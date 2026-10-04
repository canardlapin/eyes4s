/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eyes4s.laws

import eyes4s.codec.*
import eyes4s.compare.{MeasureDistance, Similarity}
import eyes4s.core.*
import eyes4s.design.SignedDifference
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json
import org.scalacheck.{Gen, Prop, Test}

import scala.reflect.ClassTag

/** Round-trip laws for the manifest and payload codecs (the cd-roundtrip
  * residual) and the four built-in score and difference codecs, each shown to
  * discriminate by deliberate mutants that a falsified property must kill.
  *
  * {{{
  * | codec                     | law                          | killed mutants                               |
  * |---------------------------|------------------------------|----------------------------------------------|
  * | eyes4s.manifest@1         | CodecLaws.roundTrip          | dropped optional relation, altered length,   |
  * |                           |                              | swapped identities                           |
  * | eyes4s.packed-recording@1 | PayloadLaws.packedRecording  | dropped sample, payload filled in from cache |
  * | eyes4s.packed-array@1     | PayloadLaws.packedArrays     | signed zero lost, float rounding, reversed   |
  * | similarity, distance,     | CodecLaws.roundTrip (bits)   | float-rounding score codec                   |
  * | scalar, signed-difference |                              |                                              |
  * }}}
  */
class ArtifactCodecLawSuite extends munit.DisciplineSuite:
  import ArtifactCodecLawSuite.*

  // -------------------------------------------------------------------------
  // eyes4s.manifest@1
  // -------------------------------------------------------------------------

  checkAll(
    "manifest",
    CodecLaws.roundTrip(ScientificManifest.codec, manifests, (a, b) => a == b)
  )

  // -------------------------------------------------------------------------
  // eyes4s.packed-recording@1 and eyes4s.packed-array@1
  // -------------------------------------------------------------------------

  private val packedCodec = PackedRecordingCodecs.recording[Px]

  checkAll(
    "packed recording",
    PayloadLaws.shippedRecording(packedCodec, recordings, sameRecording)
  )
  checkAll("packed float64", PayloadLaws.shippedArrays(arrays(doubles), PayloadLaws.sameBits))
  checkAll(
    "packed int64",
    PayloadLaws.shippedArrays(arrays(Gen.long), (a: Long, b: Long) => a == b)
  )
  checkAll(
    "packed int32",
    PayloadLaws.shippedArrays(
      arrays(Gen.choose(Int.MinValue, Int.MaxValue)),
      (a: Int, b: Int) => a == b
    )
  )
  checkAll(
    "packed uint8",
    PayloadLaws.shippedArrays(
      arrays(Gen.choose(Byte.MinValue, Byte.MaxValue)),
      (a: Byte, b: Byte) => a == b
    )
  )

  // -------------------------------------------------------------------------
  // Score and difference schemas
  // -------------------------------------------------------------------------

  private val codecs = StudyResultCodecs

  checkAll(
    "similarity",
    CodecLaws.roundTrip(
      codecs.similarity(),
      finiteDoubles.map(v => sure(Similarity.of(v))),
      (a: Similarity, b: Similarity) => PayloadLaws.sameBits(a.value, b.value)
    )
  )
  checkAll(
    "measure distance",
    CodecLaws.roundTrip(
      codecs.measureDistance(),
      finiteDoubles.map(v => sure(MeasureDistance.of(math.abs(v)))),
      (a: MeasureDistance, b: MeasureDistance) => PayloadLaws.sameBits(a.value, b.value)
    )
  )
  checkAll(
    "scalar",
    CodecLaws.roundTrip(codecs.scalar(), finiteDoubles, PayloadLaws.sameBits)
  )
  checkAll(
    "signed difference",
    CodecLaws.roundTrip(
      codecs.signedDifference(),
      finiteDoubles.map(v => sure(SignedDifference.between(v, 0.0))),
      (a: SignedDifference, b: SignedDifference) => PayloadLaws.sameBits(a.value, b.value)
    )
  )

  // -------------------------------------------------------------------------
  // Mutants
  // -------------------------------------------------------------------------

  /** Mutant checks run from a fixed seed, so every kill is reproducible evidence. */
  private val killParameters =
    Test.Parameters.default.withMinSuccessfulTests(60).withInitialSeed(0x53364b494c4cL)

  /** A property is falsified by a genuine counterexample, never by exhaustion. */
  private def falsified(prop: Prop): Boolean =
    Test.check(killParameters, prop).status match
      case Test.Failed(_, _) | Test.PropException(_, _, _) => true
      case _                                               => false

  private def killed(rules: org.typelevel.discipline.Laws#RuleSet): Boolean =
    rules.all.properties.exists((_, prop) => falsified(prop))

  /** The shipped implementation survives: every property passes outright,
    * rather than merely not failing.
    */
  private def survives(rules: org.typelevel.discipline.Laws#RuleSet): Boolean =
    rules.all.properties.forall((_, prop) => Test.check(killParameters, prop).passed)

  /** Wrap a codec so that decoding applies a deliberate change to the value. */
  private def mutant[A](codec: VersionedCodec[A])(
      change: A => Either[CodecError, A]
  ): VersionedCodec[A] =
    VersionedCodec.checked[A](codec.schema)(a =>
      codec
        .encode(a)
        .flatMap(j =>
          j.hcursor.get[Json]("value").left.map(e => CodecError.Field("value", j, e.message))
        )
    )(raw =>
      codec
        .decode(
          Json.obj(
            "schema" -> Json.obj(
              "name"    -> Json.fromString(codec.schema.name),
              "version" -> Json.fromInt(codec.schema.version)
            ),
            "value" -> raw
          )
        )
        .flatMap(change)
    )

  private def manifestMutant(
      change: ScientificManifest => Option[ScientificManifest]
  ): VersionedCodec[ScientificManifest] =
    mutant(ScientificManifest.codec)(m => Right(change(m).getOrElse(m)))

  private def rebuilt(entries: Vector[ManifestEntry], relations: Vector[ManifestRelation]) =
    ScientificManifest.of(entries, relations).toOption

  test(
    "the manifest round-trip law kills a dropped relation, an altered length and swapped identities"
  ) {
    val droppedLedger = manifestMutant(m =>
      m.relations.indexWhere(_.kind == "ledger-of") match
        case -1    => None
        case index => rebuilt(m.entries, m.relations.patch(index, Nil, 1))
    )
    assert(killed(CodecLaws.roundTrip(droppedLedger, manifests, (a, b) => a == b)))
    val longer = manifestMutant(m =>
      m.entries.indexWhere(_.layout.isEmpty) match
        case -1    => None
        case index =>
          val e = m.entries(index)
          ManifestEntry
            .of(e.name, e.role, e.schema, e.media, e.length + 1, e.sha256, e.identity, e.layout)
            .toOption
            .flatMap(changed => rebuilt(m.entries.updated(index, changed), m.relations))
    )
    assert(killed(CodecLaws.roundTrip(longer, manifests, (a, b) => a == b)))
    val swapped = manifestMutant(m =>
      val bearing = m.entries.zipWithIndex.filter(_._1.identity.isDefined)
      if bearing.size < 2 || bearing(0)._1.identity == bearing(1)._1.identity then None
      else
        val ((a, i), (b, j)) = (bearing(0), bearing(1))
        for
          x <- ManifestEntry
            .of(a.name, a.role, a.schema, a.media, a.length, a.sha256, b.identity, a.layout)
            .toOption
          y <- ManifestEntry
            .of(b.name, b.role, b.schema, b.media, b.length, b.sha256, a.identity, b.layout)
            .toOption
          m <- rebuilt(m.entries.updated(i, x).updated(j, y), m.relations)
        yield m
    )
    assert(killed(CodecLaws.roundTrip(swapped, manifests, (a, b) => a == b)))
    // The criterion does not fire spuriously: the shipped codec survives.
    assert(survives(CodecLaws.roundTrip(ScientificManifest.codec, manifests, (a, b) => a == b)))
  }

  test(
    "the packed-recording laws kill a dropped sample and a payload filled in from elsewhere"
  ) {
    val droppedSample = PayloadLaws.packedRecording[Px](
      recordings,
      sameRecording,
      packedCodec.encode,
      (document, payloads) =>
        packedCodec.decode(document, payloads).flatMap { r =>
          Recording
            .of(
              r.frame,
              r.clock,
              r.rate,
              r.eye,
              r.pupilUnit,
              r.samples.drop(1),
              r.samplingTolerance
            )
            .left
            .map(CodecError.Recording("mutant", _))
        }
    )
    assert(killed(droppedSample))
    // Remembers every payload it ever packed and quietly supplies one the
    // caller's lookup does not have.
    val remembered = scala.collection.mutable.Map.empty[PayloadRef, VerifiedPayload]
    val filledIn   = PayloadLaws.packedRecording[Px](
      recordings,
      sameRecording,
      r =>
        packedCodec.encode(r).map { p =>
          p.payloads.foreach(v => remembered.update(v.ref, v))
          p
        },
      (document, payloads) =>
        packedCodec.decode(document, ref => payloads(ref).orElse(remembered.get(ref)))
    )
    assert(killed(filledIn))
    assert(survives(PayloadLaws.shippedRecording(packedCodec, recordings, sameRecording)))
  }

  test("columns with identical bytes and layout are one content-addressed payload") {
    // One tracked sample without a pupil: support code 0 and lineage index 0
    // are the same uint8[1] payload. (Scala.js found this case first.)
    val one = sure(
      Recording.of(
        display,
        tracker,
        Rate.Irregular,
        Eye.Left,
        None,
        IArray(Sample(Instant.micros(0L), Gaze.Tracked(Pt[Px](1.0, 2.0), None))),
        sure(SamplingTolerance.of(Span.micros(2)))
      )
    )
    val packed = sure(packedCodec.encode(one))
    assertEquals(packed.payloads(1).ref, packed.payloads(2).ref)
    val remaining = packed.payloads.filterNot(_.ref == packed.payloads(1).ref)
    assert(packedCodec.decode(packed.document, ref => remaining.find(_.ref == ref)).isLeft)
    assertEquals(
      packedCodec
        .decode(packed.document, ref => packed.payloads.find(_.ref == ref))
        .map(_.contentHash),
      Right(one.contentHash)
    )
  }

  test("the packed-array laws kill a lost signed zero, float rounding and a reversed order") {
    val zeros = arrays(Gen.oneOf(Gen.const(-0.0), doubles))
    def unpacked(
        change: Double => Double
    ): VerifiedPayload => Either[PayloadError, IArray[Double]] =
      payload => PackedArrays.unpack[Double](payload).map(_.map(change))
    assert(
      killed(
        PayloadLaws.packedArrays(
          zeros,
          PayloadLaws.sameBits,
          PackedArrays.pack[Double],
          unpacked(_ + 0.0)
        )
      )
    )
    assert(
      killed(
        PayloadLaws.packedArrays(
          arrays(doubles),
          PayloadLaws.sameBits,
          PackedArrays.pack[Double],
          unpacked(_.toFloat.toDouble)
        )
      )
    )
    assert(
      killed(
        PayloadLaws.packedArrays(
          arrays(doubles),
          PayloadLaws.sameBits,
          PackedArrays.pack[Double],
          payload => PackedArrays.unpack[Double](payload).map(_.reverse)
        )
      )
    )
    assert(survives(PayloadLaws.shippedArrays(zeros, PayloadLaws.sameBits)))
  }

  test("the score round-trip law kills a codec that rounds through single precision") {
    val rounded = mutant(codecs.scalar())(v => Right(v.toFloat.toDouble))
    assert(
      killed(CodecLaws.roundTrip(rounded, finiteDoubles, PayloadLaws.sameBits))
    )
    val lostZero = mutant(codecs.signedDifference())(d =>
      SignedDifference
        .between(d.value + 0.0, 0.0)
        .left
        .map(e => CodecError.Field("mutant", Json.Null, e.message))
    )
    assert(
      killed(
        CodecLaws.roundTrip(
          lostZero,
          Gen.const(sure(SignedDifference.between(-0.0, 0.0))),
          (a: SignedDifference, b: SignedDifference) => PayloadLaws.sameBits(a.value, b.value)
        )
      )
    )
  }

object ArtifactCodecLawSuite:
  /** Generated operands satisfy every constructor invariant by construction,
    * so a `Left` here is a generator bug rather than a discarded case.
    */
  def sure[E, A](value: Either[E, A]): A =
    value.fold(
      e => throw new IllegalStateException(s"generator invariant violated: $e"),
      identity
    )

  /** Every finite double, built from its bits: any sign, a biased exponent
    * below the all-ones pattern (so neither an infinity nor a NaN) and any
    * mantissa, subnormals included. Nothing is generated only to be discarded.
    */
  val finiteBits: Gen[Double] = for
    sign     <- Gen.oneOf(0L, 1L)
    exponent <- Gen.choose(0L, 0x7feL)
    mantissa <- Gen.choose(0L, 0xfffffffffffffL)
  yield java.lang.Double.longBitsToDouble((sign << 63) | (exponent << 52) | mantissa)

  /** Finite doubles with their awkward bit patterns: both zeros, the smallest
    * subnormal, the largest finite value and decimal fractions without an
    * exact binary form, besides arbitrary bit patterns.
    */
  val finiteDoubles: Gen[Double] = Gen.frequency(
    6 -> Gen.choose(-1e6, 1e6),
    2 -> Gen.oneOf(0.1, -0.3, 1.0 / 3.0, 2.5e-7, 123456.789),
    2 -> Gen.oneOf(
      0.0,
      -0.0,
      java.lang.Double.MIN_VALUE,
      -java.lang.Double.MIN_VALUE,
      java.lang.Double.MAX_VALUE,
      -java.lang.Double.MAX_VALUE
    ),
    1 -> finiteBits
  )

  /** Finite doubles and both infinities, which a payload carries exactly.
    * NaN is excluded: its bit pattern is not portable and every payload
    * refuses it.
    */
  val doubles: Gen[Double] = Gen.frequency(
    10 -> finiteDoubles,
    1  -> Gen.oneOf(Double.PositiveInfinity, Double.NegativeInfinity)
  )

  /** A layout of one to three axes (with empty extents) and its values. */
  def arrays[A: ClassTag](values: Gen[A])(using
      element: PackedElement[A]
  ): Gen[(PayloadLayout, IArray[A])] =
    for
      axes  <- Gen.choose(1, 3)
      shape <- Gen.listOfN(axes, Gen.frequency(1 -> Gen.const(0), 8 -> Gen.choose(1, 6)))
      order <- Gen.oneOf(ArrayOrder.RowMajor, ArrayOrder.ColumnMajor)
      layout = sure(PayloadLayout.of(element.kind, shape.toVector, order))
      xs <- Gen.listOfN(layout.count, values)
    yield (layout, IArray.from(xs))

  private val display = sure(Frame.screen("law-packed", 1000, 1000))
  private val tracker = ClockId("law-tracker")

  private def gaze(pupil: Boolean): Gen[Gaze[Px]] = Gen.frequency(
    5 -> (for
      x <- Gen.choose(0.0, 999.0)
      y <- Gen.choose(0.0, 999.0)
      p <- if pupil then Gen.option(Gen.choose(0.5, 5000.0)) else Gen.const(None)
    yield Gaze.Tracked(Pt[Px](x, y), p)),
    1 -> Gen.const(Gaze.Blink[Px]()),
    1 -> Gen.const(Gaze.Lost[Px]()),
    1 -> (for
      x <- Gen.choose(1000.5, 2000.0)
      y <- Gen.choose(-100.0, 999.0)
    yield Gaze.OffScreen[Px](Pt[Px](x, y)))
  )

  private val lineages: Gen[SampleLineage] = for
    basis <- Gen.oneOf(SampleLineage.measured, SampleLineage.interpolated)
    steps <- Gen.choose(0, 2).flatMap(n => Gen.listOfN(n, Gen.oneOf(true, false)))
  yield steps.foldLeft(basis)((acc, smoothed) =>
    if smoothed then acc.smoothed else acc.projected
  )

  /** Monocular recordings of one to forty samples, fixed-rate or irregular,
    * from origins beyond JavaScript's exact integer range and at the floor.
    */
  val recordings: Gen[Recording[Px]] = for
    n      <- Gen.choose(1, 40)
    fixed  <- Gen.oneOf(true, false)
    origin <- Gen.oneOf(0L, 9007199254740993L, Long.MinValue + 1, -123456L)
    gaps   <- Gen.listOfN(n, if fixed then Gen.choose(-1L, 1L) else Gen.choose(1L, 5000L))
    times =
      if fixed then gaps.toVector.zipWithIndex.map((j, i) => origin + i * 1000L + j)
      else gaps.toVector.scanLeft(origin)(_ + _).tail
    eye   <- Gen.oneOf(Eye.Left, Eye.Right, Eye.Cyclopean)
    unit  <- Gen.option(Gen.oneOf(PupilUnit.Area, PupilUnit.Diameter, PupilUnit.Arbitrary))
    gazes <- Gen.listOfN(n, gaze(unit.isDefined))
    lines <- Gen.listOfN(n, lineages)
  yield sure(
    Recording.of(
      display,
      tracker,
      if fixed then Rate.Fixed(sure(Hz(1000.0))) else Rate.Irregular,
      eye,
      unit,
      IArray.from(times.indices.map(i => Sample(Instant.micros(times(i)), gazes(i), lines(i)))),
      sure(SamplingTolerance.of(Span.micros(2)))
    )
  )

  val sameRecording: (Recording[Px], Recording[Px]) => Boolean = (a, b) =>
    a.contentHash == b.contentHash && a.samples.toVector == b.samples.toVector &&
      a.frame == b.frame && a.clock == b.clock && a.eye == b.eye && a.rate == b.rate &&
      a.pupilUnit == b.pupilUnit && a.samplingTolerance == b.samplingTolerance

  // ---------------------------------------------------------------------------
  // Manifests
  // ---------------------------------------------------------------------------

  private def definition(name: String, version: Int): DefinitionId =
    sure(DefinitionId.of(name, version))

  private val jsonSchemas: Gen[DefinitionId] = Gen.oneOf(
    DefinitionId.study,
    DefinitionId.studyInput,
    DefinitionId.admissionLedger,
    DefinitionId.studyResult,
    DefinitionId.recording,
    DefinitionId.recordingInput,
    DefinitionId.temporalStudyInput,
    definition("example.custom-study", 3)
  )

  private val hashes: Gen[ContentHash] =
    Gen.long.map(l => sure(ContentHash.parse(f"$l%016x").toRight(l)))

  private val digests: Gen[ByteDigest] =
    Gen
      .listOfN(8, Gen.choose(Byte.MinValue, Byte.MaxValue))
      .map(bs => ByteDigest.sha256(IArray.from(bs)))

  /** Unique, strictly valid names: plain, nested, with spaces inside and
    * outside the Basic Multilingual Plane.
    */
  private def names(n: Int): Gen[Vector[ArtifactName]] =
    Gen
      .listOfN(
        n,
        Gen.oneOf("", ".json", "/nested/part", " with space", "-ünïcödé", "-数据", "-😀")
      )
      .map(_.toVector.zipWithIndex.map((suffix, i) => sure(ArtifactName.of(s"e$i$suffix"))))

  private def entry(
      name: ArtifactName,
      role: ArtifactRole,
      schemaFor: Gen[DefinitionId]
  ): Gen[ManifestEntry] =
    if role == ArtifactRole.Payload then
      for
        element <- Gen.oneOf(ElementKind.values.toIndexedSeq)
        axes    <- Gen.choose(1, 2)
        shape   <- Gen.listOfN(axes, Gen.choose(0, 5000))
        order   <- Gen.oneOf(ArrayOrder.RowMajor, ArrayOrder.ColumnMajor)
        layout = sure(PayloadLayout.of(element, shape.toVector, order))
        digest <- digests
      yield sure(
        ManifestEntry.of(
          name,
          role,
          DefinitionId.packedArray,
          MediaKind.Binary,
          layout.byteLength.toLong,
          digest,
          None,
          Some(layout)
        )
      )
    else
      for
        schema   <- schemaFor
        length   <- Gen.oneOf(Gen.choose(0L, 10000000L), Gen.const(Long.MaxValue))
        digest   <- digests
        identity <- if role.identityBearing then hashes.map(Some(_)) else Gen.const(None)
      yield sure(
        ManifestEntry.of(name, role, schema, MediaKind.JsonText, length, digest, identity, None)
      )

  /** Well-formed manifests over every role and relation kind, with entries
    * and relations in generated order: one to three inputs; plans with their
    * input; results of a plan on an input; ledgers, temporal inputs and
    * recording inputs with or without their optional relation; standalone
    * and packed recordings; payloads owned by one or more packed recordings;
    * recording and temporal plans with their input, and their results of a
    * plan on an input; analysis plans, and analysis results of a plan on one
    * or more identity-bearing inputs.
    */
  val manifests: Gen[ScientificManifest] = for
    inputs           <- Gen.choose(1, 3)
    plans            <- Gen.choose(0, 2)
    results          <- if plans == 0 then Gen.const(0) else Gen.choose(0, 2)
    ledgers          <- Gen.choose(0, 2)
    temporals        <- Gen.choose(0, 2)
    standalone       <- Gen.choose(0, 2)
    packed           <- Gen.choose(0, 2)
    recorded         <- Gen.choose(0, 2)
    payloads         <- if packed == 0 then Gen.const(0) else Gen.choose(1, 4)
    recordingPlans   <- if recorded == 0 then Gen.const(0) else Gen.choose(0, 2)
    recordingResults <- if recordingPlans == 0 then Gen.const(0) else Gen.choose(0, 2)
    temporalPlans    <- if temporals == 0 then Gen.const(0) else Gen.choose(0, 2)
    temporalResults  <- if temporalPlans == 0 then Gen.const(0) else Gen.choose(0, 2)
    analysisPlans    <- Gen.choose(0, 2)
    analysisResults  <- if analysisPlans == 0 then Gen.const(0) else Gen.choose(0, 2)
    roles = Vector.fill(inputs)(ArtifactRole.StudyInput) ++
      Vector.fill(plans)(ArtifactRole.StudyPlan) ++
      Vector.fill(results)(ArtifactRole.StudyResult) ++
      Vector.fill(ledgers)(ArtifactRole.AdmissionLedger) ++
      Vector.fill(temporals)(ArtifactRole.TemporalInput) ++
      Vector.fill(standalone + packed)(ArtifactRole.Recording) ++
      Vector.fill(recorded)(ArtifactRole.RecordingInput) ++
      Vector.fill(payloads)(ArtifactRole.Payload) ++
      Vector.fill(recordingPlans)(ArtifactRole.RecordingPlan) ++
      Vector.fill(recordingResults)(ArtifactRole.RecordingResult) ++
      Vector.fill(temporalPlans)(ArtifactRole.TemporalPlan) ++
      Vector.fill(temporalResults)(ArtifactRole.TemporalResult) ++
      Vector.fill(analysisPlans)(ArtifactRole.AnalysisPlan) ++
      Vector.fill(analysisResults)(ArtifactRole.AnalysisResult)
    named   <- names(roles.size)
    entries <- Gen.sequence[Vector[ManifestEntry], ManifestEntry](
      roles.zip(named).zipWithIndex.map { case ((role, name), index) =>
        val recordingIndex = index - (inputs + plans + results + ledgers + temporals)
        val schemas        =
          if role == ArtifactRole.Recording then
            if recordingIndex >= standalone then Gen.const(DefinitionId.packedRecording)
            else Gen.oneOf(DefinitionId.recording, DefinitionId.binocularRecording)
          else jsonSchemas
        entry(name, role, schemas)
      }
    )
    byRole = (role: ArtifactRole) => entries.filter(_.role == role).map(_.name)
    owners = entries.filter(_.schema == DefinitionId.packedRecording).map(_.name)
    planInputs <- Gen.sequence[Vector[ManifestRelation], ManifestRelation](
      byRole(ArtifactRole.StudyPlan).map(p =>
        Gen.oneOf(byRole(ArtifactRole.StudyInput)).map(ManifestRelation.PlanInput(p, _))
      )
    )
    resultOf <- Gen.sequence[Vector[ManifestRelation], ManifestRelation](
      byRole(ArtifactRole.StudyResult).map(r =>
        for
          p <- Gen.oneOf(byRole(ArtifactRole.StudyPlan))
          i <- Gen.oneOf(byRole(ArtifactRole.StudyInput))
        yield ManifestRelation.ResultOf(r, p, i)
      )
    )
    optional <- Gen.sequence[Vector[Option[ManifestRelation]], Option[ManifestRelation]](
      byRole(ArtifactRole.AdmissionLedger).map(l =>
        Gen.option(
          Gen.oneOf(byRole(ArtifactRole.StudyInput)).map(ManifestRelation.LedgerOf(l, _))
        )
      ) ++ byRole(ArtifactRole.TemporalInput).map(t =>
        Gen.option(
          Gen.oneOf(byRole(ArtifactRole.StudyInput)).map(ManifestRelation.TemporalBase(t, _))
        )
      ) ++ byRole(ArtifactRole.RecordingInput).map(r =>
        if byRole(ArtifactRole.Recording).isEmpty then Gen.const(None)
        else
          Gen.option(
            Gen.oneOf(byRole(ArtifactRole.Recording)).map(ManifestRelation.RecordingOf(r, _))
          )
      )
    )
    owned <- Gen.sequence[Vector[Vector[ManifestRelation]], Vector[ManifestRelation]](
      byRole(ArtifactRole.Payload).map(p =>
        Gen
          .choose(1, owners.size)
          .flatMap(n => Gen.pick(n, owners))
          .map(_.toVector.map(ManifestRelation.PayloadOf(_, p)))
      )
    )
    archives <- Gen.sequence[Vector[ManifestRelation], ManifestRelation](
      byRole(ArtifactRole.RecordingPlan).map(p =>
        Gen
          .oneOf(byRole(ArtifactRole.RecordingInput))
          .map(ManifestRelation.RecordingPlanInput(p, _))
      ) ++ byRole(ArtifactRole.RecordingResult).map(r =>
        for
          p <- Gen.oneOf(byRole(ArtifactRole.RecordingPlan))
          i <- Gen.oneOf(byRole(ArtifactRole.RecordingInput))
        yield ManifestRelation.RecordingResultOf(r, p, i)
      ) ++ byRole(ArtifactRole.TemporalPlan).map(p =>
        Gen
          .oneOf(byRole(ArtifactRole.TemporalInput))
          .map(ManifestRelation.TemporalPlanInput(p, _))
      ) ++ byRole(ArtifactRole.TemporalResult).map(r =>
        for
          p <- Gen.oneOf(byRole(ArtifactRole.TemporalPlan))
          i <- Gen.oneOf(byRole(ArtifactRole.TemporalInput))
        yield ManifestRelation.TemporalResultOf(r, p, i)
      )
    )
    // An analysis result cites one or more distinct identity-bearing inputs.
    identified = entries.filter(_.role.identityBearing).map(_.name)
    analyses <- Gen.sequence[Vector[ManifestRelation], ManifestRelation](
      byRole(ArtifactRole.AnalysisResult).map(r =>
        for
          p  <- Gen.oneOf(byRole(ArtifactRole.AnalysisPlan))
          n  <- Gen.choose(1, identified.size)
          on <- Gen.pick(n, identified)
        yield ManifestRelation.AnalysisResultOf(r, p, on.toVector)
      )
    )
    relations =
      planInputs ++ resultOf ++ optional.flatten ++ owned.flatten ++ archives ++ analyses
    entryOrder    <- Gen.listOfN(entries.size, Gen.long)
    relationOrder <- Gen.listOfN(relations.size, Gen.long)
  yield sure(
    ScientificManifest.of(
      entries.zip(entryOrder).sortBy(_._2).map(_._1),
      relations.zip(relationOrder).sortBy(_._2).map(_._1)
    )
  )
