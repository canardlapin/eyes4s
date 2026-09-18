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

package example

import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.*
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import io.circe.Json
import org.scalacheck.Gen
import scala.compiletime.testing.typeCheckErrors

class ConsumerSuite extends munit.DisciplineSuite:
  private val ReferenceTolerance             = Tolerance(absolute = 1e-12, relative = 0)
  private def get[E, A](e: Either[E, A]): A  = e.fold(error => fail(s"$error"), identity)
  private def id(name: String): DefinitionId = get(DefinitionId.of(name, 1))
  private val layout                         = CustomMethod.layout(id("my.lab.trial-layout"))
  private val method      = get(CustomMethod.describedMethod(id("my.lab.scaled-cosine")))
  private val parameters  = CustomMethod.parameterCodec(id("my.lab.multiplier"))
  private val keys        = CustomMethod.keyCodec(id("my.lab.trial-key"))
  private val persistence = new StudyCodec(id("my.lab.study"), layout, keys, method, parameters)
  private val detectorPersistence = get(
    CustomDetector.persistence(
      id("my.lab.recording-plan"),
      id("my.lab.conservative-ivt"),
      id("my.lab.conservative-ivt-parameters")
    )
  )
  private val frame  = get(Frame.screen("display", 2, 2))
  private val grid   = get(Grid.over(frame, 2, 2))
  private val reader = get(
    FixationKeyReader.of[TrialKey](Vector("participant", "image", "phase"))(
      fields =>
        for
          subject <- fields.get("participant").toRight("participant missing")
          image   <- fields.get("image").toRight("image missing")
          item  <- Map("a" -> 1, "b" -> 2, "c" -> 3).get(image).toRight(s"unknown image $image")
          phase <- fields.get("phase").toRight("phase missing")
        yield TrialKey(subject, item, phase),
      key => ClockId(KeyDigest[TrialKey].digest(key).render)
    )
  )
  private val columns = get(
    FixationColumns.of("fixation", "x_px", "y_px", "onset_us", "duration_us", "sample_count")
  )
  private val input = get(
    get(
      FixationCsv.read(ConsumerFixtures.csv, columns, reader, frame, TimestampUnit.Microseconds)
    ).requireComplete
  )
  private val scales = ConsumerFixtures.gaussianTargets.map { case (sigma, _) =>
    StudyEstimate.Gaussian(get(Sigma.px(sigma)), EdgePolicy.Truncate)
  }
  private def plan(multiplier: Double) = get(
    StudyPlan.of(
      input.reference,
      layout,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      scales,
      FailurePolicy.RequireAll,
      method,
      get(Multiplier.of(multiplier))
    )
  )
  private val views = get(
    ScoreColumns.of[ScaledScore, SignedDifference](Vector("value"))(
      s => Vector(s.value),
      d => Vector(d.value)
    )
  )

  test(
    "packaged typed descriptors construct and inspect an extension without an application parameter table"
  ) {
    val parameter = get(CustomMethod.parameterDescriptor)
    assert(parameter.parse(0).isLeft)
    val value = get(parameter.parse(2.0))
    assertEquals(value.value, 2.0)
    val original   = plan(value.value)
    val inspection = get(original.inspect)
    assertEquals(inspection.description, original.description)
    assertEquals(
      inspection.fields.find(_.info.id == "method.multiplier").map(_.info.version),
      Some(2)
    )
    val restored = get(persistence.codec.decode(get(persistence.codec.encode(original))))
    assertEquals(get(restored.inspect).description, inspection.description)
    val descriptor = method.descriptor.get
    assertEquals(
      get(descriptor.components(value)).map(_.range),
      Vector(eyes4s.compare.MeasureScale.Bounded(0, 2))
    )
    assertEquals(descriptor.execution, ExecutionCapability.BoundedComparison)
    assertEquals(inspection.execution, ExecutionCapability.BoundedComparison)
    val prepared = get(original.prepare(input))
    assertEquals(prepared.capability, ExecutionCapability.BoundedComparison)
    val bounded = get(prepared.boundedWork())
    assertEquals(bounded.capability, ExecutionCapability.BoundedComparison)
    assertEquals(bounded.stage, StudyStage.Estimating(0, 0))
    val cells   = get(eyes4s.compare.ComparisonQuantum.of(1))
    val finest  = WorkQuanta(get(PairQuantum.of(1)), cells)
    val stepped = get(StudyWork.complete(bounded, finest))
    val whole   = get(prepared.run)
    assertEquals(
      stepped.scales.map(s =>
        get(s.contrast).rows.map(r => r.key -> r.difference.map(_.value))
      ),
      whole.scales.map(s => get(s.contrast).rows.map(r => r.key -> r.difference.map(_.value)))
    )
    assert(descriptor.verify(value, Vector.empty, Vector("value")).isLeft)
    assert(typeCheckErrors("""
      import example.*
      CustomMethod.parameterDescriptor.toOption.get.construct("two")
    """).nonEmpty)
  }

  checkAll(
    "parameter codec",
    CodecLaws.roundTrip(
      parameters,
      Gen.choose(0.1, 5.0).map(x => get(Multiplier.of(x))),
      (a: Multiplier, b: Multiplier) => a.value == b.value
    )
  )
  checkAll(
    "custom key codec",
    CodecLaws.roundTrip(
      keys,
      Gen.zip(Gen.alphaStr, Gen.choose(1, 100), Gen.alphaStr).map { case (s, i, p) =>
        TrialKey(s, i, p)
      },
      (a: TrialKey, b: TrialKey) => a == b
    )
  )
  checkAll(
    "custom contrast",
    ContrastLaws.subtraction(
      Contrastable[ScaledScore, SignedDifference],
      Gen.choose(-100.0, 100.0).map(x => get(ScaledScore.of(x))),
      (s: ScaledScore) => Vector(s.value),
      (d: SignedDifference) => Vector(d.value),
      (get(ScaledScore.of(0.75)), get(ScaledScore.of(0.25)), Vector(0.5)),
      ReferenceTolerance
    )
  )
  checkAll(
    "custom study codec",
    CodecLaws.roundTrip(
      persistence.codec,
      Gen.choose(0.1, 5.0).map(plan),
      (
          a: StudyPlan[TrialKey, Px, Multiplier, ScaledScore, SignedDifference],
          b: StudyPlan[TrialKey, Px, Multiplier, ScaledScore, SignedDifference]
      ) => a == b
    )
  )

  test("custom typed method saves, reloads and agrees with the independent scale targets") {
    val original = plan(2.0)
    val decoded  = get(persistence.codec.decode(get(persistence.codec.encode(original))))
    val direct   = get(original.run(input))
    val restored = get(decoded.run(input))
    assertEquals(restored.scales.size, 3)
    restored.scales.zip(ConsumerFixtures.gaussianTargets).zipWithIndex.foreach {
      case ((scale, (_, expected)), index) =>
        val directRows = get(direct.scales(index).contrast).rows
        assertEquals(get(scale.contrast).rows.size, expected.size)
        assertEquals(
          get(scale.contrast).rows
            .map(r => s"${r.key.subject}/${r.key.item}/${r.key.phase}")
            .toSet,
          expected.keySet
        )
        get(scale.contrast).rows.zip(directRows).foreach { case (row, before) =>
          val label = s"${row.key.subject}/${row.key.item}/${row.key.phase}"
          assert(
            ReferenceTolerance.approxEquals(get(row.difference).value, 2 * expected(label))
          )
          assertEquals(get(row.difference).value, get(before.difference).value)
          assertEquals(row.control.map(_.contributing), Some(2))
        }
    }
    val exported = get(ContrastCsv.document(decoded, restored, persistence, views))
    assertEquals(exported.rows.size, 18)
    assert(exported.rows.forall(_.size == ContrastCsv.header.size))
    val row = ContrastCsv.header.zip(exported.rows.head).toMap
    assertEquals(get(keys.parse(row("key_json"))), TrialKey("s1", 1, "recall"))
    assertEquals(row("method_id"), "my.lab.scaled-cosine")
  }

  test(
    "registry loads the external definition and exposes missing extensions without core edits"
  ) {
    val registered = get(StudyRegistry.empty[TrialKey, Px].register(persistence.registration))
    val json       = get(persistence.codec.encode(plan(2.0)))
    val loaded     = get(registered.decode(json))
    assertEquals(loaded.description, plan(2.0).description)
    assertEquals(get(loaded.encode), json)
    assertEquals(get(loaded.run(input)).scales.size, 3)
    assertEquals(
      StudyRegistry.empty[TrialKey, Px].decode(json).left.toOption,
      Some(CodecError.MissingMethod(method.id))
    )
    assertEquals(
      registered.register(persistence.registration).left.toOption,
      Some(CodecError.DuplicateMethod(method.id))
    )
    assert(loaded.prerequisites(None).exists {
      case PlanError.MissingArtifact(_) => true; case _ => false
    })
  }

  test("a completed result with the custom score archives through the registered score codec") {
    val scores  = CustomMethod.scoreCodec(id("my.lab.scaled-score"))
    val archive = persistence.results(scores, StudyResultCodecs.signedDifference())
    val result  = get(plan(2.0).run(input))
    val json    = get(archive.codec.encode(result))
    val decoded = get(archive.codec.decode(json))
    val same    = (a: ScaledScore, b: ScaledScore) => a.value == b.value
    val sameD   = (a: SignedDifference, b: SignedDifference) => a.value == b.value
    assert(StudyResultEquivalence.same(result, decoded)(same, sameD))
    assertEquals(get(archive.codec.encode(decoded)), json)
    assertEquals(decoded.scales.size, 3)
    assertEquals(
      get(decoded.scales.head.contrast).rows.map(r => r.control.map(_.contributing)),
      Vector.fill(6)(Some(2))
    )
    val registry = get(StudyResultRegistry.empty[TrialKey, Px].register(archive.registration))
    val loaded   = get(registry.decode(json))
    assertEquals(get(loaded.encode), json)
    assertEquals(
      StudyResultRegistry.empty[TrialKey, Px].decode(json).left.toOption,
      Some(CodecError.MissingResultCodec(method.id))
    )
    assertEquals(
      registry.register(archive.registration).left.toOption,
      Some(CodecError.DuplicateResultCodec(method.id))
    )
    // Re-executing the reloaded plan reproduces the archived contrasts bit for bit.
    val reloaded = get(persistence.codec.decode(get(persistence.codec.encode(plan(2.0)))))
    assertEquals(get(archive.codec.encode(get(reloaded.run(input)))), json)
  }

  checkAll(
    "custom result codec",
    CodecLaws.roundTrip(
      persistence
        .results(
          CustomMethod.scoreCodec(id("my.lab.scaled-score")),
          StudyResultCodecs.signedDifference()
        )
        .codec,
      Gen.choose(0.1, 5.0).map(m => get(plan(m).run(input))),
      (
          a: StudyResult[TrialKey, Px, ScaledScore, SignedDifference],
          b: StudyResult[TrialKey, Px, ScaledScore, SignedDifference]
      ) =>
        StudyResultEquivalence.same(a, b)(
          (x, y) => x.value == y.value,
          (x, y) => x.value == y.value
        )
    )
  )

  // UI-S5: the saved custom study as a manifest, resolved from the application's own storage.
  private val archive =
    persistence.results(
      CustomMethod.scoreCodec(id("my.lab.scaled-score")),
      StudyResultCodecs.signedDifference()
    )
  private val inputPersistence =
    new StudyInputCodec[TrialKey, Px](
      id("my.lab.study-input"),
      id("my.lab.admission-ledger"),
      layout,
      keys
    )
  private val customDecoders = ArtifactDecoders.of(
    get(StudyRegistry.empty[TrialKey, Px].register(persistence.registration)),
    get(StudyInputRegistry.empty[TrialKey, Px].register(inputPersistence)),
    get(StudyResultRegistry.empty[TrialKey, Px].register(archive.registration))
  )
  private def savedStudy(multiplier: Double): Either[CodecError, SavedManifest] = for
    p <- StoredArtifact.plan("plan", persistence, plan(multiplier))
    i <- StoredArtifact.input("input", inputPersistence, input)
    r <- StoredArtifact.result("result", archive, get(plan(multiplier).run(input)))
    s <- SavedManifest.of(
      Vector(p, i, r),
      Vector(
        ManifestRelation.PlanInput(p.name, i.name),
        ManifestRelation.ResultOf(r.name, p.name, i.name)
      )
    )
  yield s

  test("a saved custom study resolves from an in-memory source through registered decoders") {
    val saved = get(savedStudy(2.0))
    // The application's storage: here a map, keyed by its own names.
    val store  = saved.artifacts.map(a => a.name.value -> a.bytes).toMap
    val source = ByteSource {
      case ByteRequest.Manifest(address) =>
        Either.cond(address == saved.address, saved.bytes, SourceFailure.Missing)
      case ByteRequest.Entry(entry) =>
        store.get(entry.name.value).toRight(SourceFailure.Missing)
    }
    val resolved =
      get(ArtifactResolver.resolve(saved.address, source, customDecoders).left.map(_.toVector))
    val result = get(plan(2.0).run(input))
    assertEquals(resolved.results.map(_._2.encode), Vector(archive.codec.encode(result)))
    val admitted = resolved.inputs.map(_._2)
    assertEquals(admitted.map(_.reference), Vector(input.reference))
    // The admitted input reruns the reloaded plan to the archived result.
    assertEquals(
      archive.codec.encode(get(plan(2.0).run(admitted.head))),
      archive.codec.encode(result)
    )
    // A changed byte is refused before anything is decoded, naming the entry.
    val changed = store.updated(
      "result",
      IArray.tabulate(store("result").length)(i =>
        if i == 40 then (store("result")(i) ^ 1).toByte else store("result")(i)
      )
    )
    val tampered = ByteSource {
      case ByteRequest.Manifest(_)  => Right(saved.bytes)
      case ByteRequest.Entry(entry) =>
        changed.get(entry.name.value).toRight(SourceFailure.Missing)
    }
    assert(
      ArtifactResolver
        .resolve(saved.address, tampered, customDecoders)
        .left
        .exists(errors =>
          errors.length == 1 && (errors.head match
            case ResolveError.Digest(name, _, _) => name.value == "result"
            case _                               => false)
        )
    )
    // Without the custom result registration the archive is refused by name.
    val unregistered = ArtifactDecoders.of(
      get(StudyRegistry.empty[TrialKey, Px].register(persistence.registration)),
      get(StudyInputRegistry.empty[TrialKey, Px].register(inputPersistence)),
      StudyResultRegistry.empty[TrialKey, Px]
    )
    assertEquals(
      ArtifactResolver.resolve(saved.address, source, unregistered).left.map(_.toVector),
      Left(
        Vector(
          ResolveError.Decode(
            get(ArtifactName.of("result")),
            CodecError.MissingResultCodec(method.id)
          )
        )
      )
    )
  }

  checkAll(
    "custom saved study manifest",
    ManifestLaws.verifiedResolution(
      Gen.choose(0.1, 5.0),
      (m: Double) => savedStudy(m).map(StoredGraph.of),
      customDecoders,
      (m: Double, resolved: ResolvedManifest[TrialKey, Px]) =>
        resolved.results.map(_._2.encode) ==
          Vector(archive.codec.encode(get(plan(m).run(input))))
    )
  )

  test("invalid parameters, missing schema versions and incompatible static types reject") {
    val json = get(parameters.encode(get(Multiplier.of(2.0))))
    assert(
      parameters
        .decode(json.mapObject(_.add("value", Json.obj("multiplier" -> Json.fromInt(0)))))
        .isLeft
    )
    assert(parameters.decode(json.mapObject(_.remove("schema"))).isLeft)
    assert(plan(2.0).diff(plan(3.0)).exists(_.field == "method.multiplier"))
    assert(
      typeCheckErrors(
        """import example.*; import eyes4s.codec.*; def bad(c: VersionedCodec[Multiplier]): VersionedCodec[TrialKey] = c"""
      ).nonEmpty
    )
  }

  test("an external detector method saves, registers and runs from packaged artifacts") {
    val display       = get(Frame.screen("external-display", 1000, 1000))
    val trackerClock  = ClockId("external-tracker")
    val analysisClock = ClockId("external-analysis")
    val rate          = Rate.Fixed(get(Hz(100.0)))
    val recording     = get(
      Recording.of(
        display,
        trackerClock,
        rate,
        Eye.Left,
        None,
        IArray.from(
          (0 until 10).map(index =>
            Sample(
              Instant.millis(index.toLong * 10L),
              Gaze.Tracked(Pt[Px](500.0, 500.0), None)
            )
          )
        )
      )
    )
    val marks = Vector(
      get(SyncMark.of("start", Instant.millis(0), Instant.millis(0))),
      get(SyncMark.of("end", Instant.millis(90), Instant.millis(90)))
    )
    val area = get(
      RecordingArea.of(
        "centre",
        "Central area",
        get(Bounds.of[Px](400.0, 400.0, 600.0, 600.0))
      )
    )
    val original = get(
      RecordingPlan.of(
        ArtifactRef.of(recording.contentHash),
        RecordingRef("external-recording"),
        display,
        trackerClock,
        analysisClock,
        FrameId("external-angular"),
        Some(get(Viewing.millimetres(600.0, 500.0, 500.0))),
        SyncFitMode.OffsetOnly,
        marks,
        None,
        InterpolationGap.none,
        Vector(area),
        detectorPersistence.method,
        get(LabIvtParameters.of(30.0, 20000L))
      )
    )
    val json       = get(detectorPersistence.codec.encode(original))
    val registry   = get(RecordingRegistry.empty.register(detectorPersistence.registration))
    val loaded     = get(registry.decode(json))
    val direct     = get(original.run(recording))
    val restored   = get(loaded.plan.run(recording))
    val detectorId = direct.detection.identity.detectorRef.render

    assertEquals(get(loaded.encode), json)
    assertEquals(loaded.plan.description, original.description)
    assertEquals(restored.detection.eventSeries.events, direct.detection.eventSeries.events)
    assertEquals(restored.assignment.toVector, direct.assignment.toVector)
    assertEquals(detectorId, "eyes4s.detect.ivt@1.0.0")
    assert(direct.detection.eventSeries.events.nonEmpty)
    assertEquals(
      RecordingRegistry.empty.decode(json).left.toOption,
      Some(CodecError.MissingMethod(detectorPersistence.method.id))
    )

    println(
      "EYES4S_DETECTOR_RUNTIME=" + Json
        .obj(
          "plan"           -> json,
          "recording"      -> Json.fromString(recording.contentHash.render),
          "detector"       -> Json.fromString(detectorId),
          "event_count"    -> Json.fromInt(direct.detection.eventSeries.events.size),
          "sample_classes" -> Json.arr(
            direct.detection.labels.toVector.map(value => Json.fromString(value.toString))*
          )
        )
        .noSpaces
    )
  }

  test("a packaged result row drills down to the CSV record that supplied it") {
    val imported = get(
      FixationCsv.read(ConsumerFixtures.csv, columns, reader, frame, TimestampUnit.Microseconds)
    )
    val ledger = get(
      FixationEvidence.ledger("consumer.csv", imported, AdmissionDecision.RequireComplete)
    )
    val study      = plan(2.0)
    val result     = get(study.run(input))
    val inspection = get(ResultInspection.study(study, result, input, Some(ledger)))
    val focal      = TrialKey("s1", 1, "recall")
    val row        = get(inspection.contrastRow(ResultRef.ContrastRow(0, focal)))
    val difference = get(row.outcome.left.map(_.message))
    assertEquals(difference.components.map(_.id), Vector("value"))
    assertEquals(difference.components.map(_.value), Vector(difference.value.value))
    val matched = get(inspection.reduction(get(row.matched.toRight("no matched reduction"))))
    assertEquals(matched.contributors, Vector(TrialKey("s1", 1, "encode")))
    val pair    = get(inspection.pair(matched.members.head.pair))
    val located = get(inspection.sources.fixation(pair.reference, 0))
    assertEquals((located.record, located.ordinal), (2, 0))
    val line = ConsumerFixtures.csv.linesIterator.toVector(located.record - 1)
    assertEquals(line.split(',').take(4).toVector, Vector("s1", "a", "encode", "0"))
    assertEquals(get(inspection.trial(focal)).records, Vector(26, 27, 28, 29))
    val refusal = Diagnostic.of(PlanError.ArtifactMismatch(input.reference.digest, "0" * 16))
    assertEquals(refusal.code.render, "plan.artifact-mismatch")
    assertEquals(refusal.subject, Vector(Locus.Artifact(input.reference.digest)))
    assert(DiagnosticCatalog.codes.map(_.render).contains("study-failure.comparison"))
  }

  test("emit raw portable evidence for the isolated JVM and Scala.js comparison") {
    val original = plan(2.0)
    val result   = get(original.run(input))
    val binned   = get(
      StudyPlan.of(
        input.reference,
        layout,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll,
        method,
        get(Multiplier.of(2.0))
      )
    )
    val bits = get(get(binned.run(input)).scales.head.contrast).rows.map(row =>
      Json.fromString(java.lang.Double.doubleToLongBits(get(row.difference).value).toString)
    )
    val numbers = result.scales.flatMap(scale =>
      get(scale.contrast).rows.map(row => Json.fromDoubleOrNull(get(row.difference).value))
    )
    println(
      "EYES4S_CROSS_RUNTIME=" + Json
        .obj(
          "plan"        -> get(persistence.codec.encode(original)),
          "input"       -> Json.fromString(input.reference.digest),
          "binned_bits" -> Json.arr(bits*),
          "gaussian"    -> Json.arr(numbers*)
        )
        .noSpaces
    )
  }
