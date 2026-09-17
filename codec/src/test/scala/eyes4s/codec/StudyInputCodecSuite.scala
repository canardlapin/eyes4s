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

package eyes4s.codec

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.examples.MatchedControlFixtures
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json
import scala.compiletime.testing.typeCheckErrors

/** A key whose display projections coincide for repeated presentations. */
final case class OccurrenceKey(
    participant: String,
    stimulus: String,
    phase: String,
    occurrence: Int
) derives CanEqual
object OccurrenceKey:
  given KeyDigest[OccurrenceKey] = KeyDigest.derived[OccurrenceKey]
  given Ordering[OccurrenceKey]  =
    Ordering.by(k => (k.participant, k.stimulus, k.phase, k.occurrence))
  def layout(id: DefinitionId): StudyLayout[OccurrenceKey] = new StudyLayout(
    id,
    Projection.named("participant")(_.participant),
    Projection.named("stimulus")(_.stimulus),
    Projection.named("phase")(_.phase)
  )
  def codec(schema: DefinitionId): VersionedCodec[OccurrenceKey] =
    VersionedCodec.of[OccurrenceKey](schema)(k =>
      Json.obj(
        "participant" -> Json.fromString(k.participant),
        "stimulus"    -> Json.fromString(k.stimulus),
        "phase"       -> Json.fromString(k.phase),
        "occurrence"  -> Json.fromInt(k.occurrence)
      )
    )(json =>
      for
        p <- Wire.field[String](json, "participant")
        s <- Wire.field[String](json, "stimulus")
        f <- Wire.field[String](json, "phase")
        o <- Wire.field[Int](json, "occurrence")
      yield OccurrenceKey(p, s, f, o)
    )

class StudyInputCodecSuite extends munit.FunSuite:
  private val OracleTolerance               = 1e-12
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private def id(name: String, version: Int = 1): DefinitionId = get(
    DefinitionId.of(name, version)
  )
  private val frame = get(Frame.screen("matched-control-display", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val input = StudyInputFixtures.matchedControl
  private val codec = StudyInputCodecs.study[Px]

  private def fixation(clock: ClockId, onset: Long, x: Double, y: Double) = get(
    Event.Fixation.withoutDispersion(
      get(Interval.of(clock, Instant.micros(onset), Instant.micros(onset + 100000L))),
      Pt[Px](x, y),
      10
    )
  )
  private def scanpath[K](key: K, points: Vector[(Double, Double)])(using
      digest: KeyDigest[K]
  ): Scanpath[Px] =
    val clock = ClockId(s"trial:${digest.digest(key).render}")
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      fixation(clock, i * 200000L, x, y)
    }
    get(Scanpath.of(frame, clock, IArray.from(fixes)))

  private def same[K](a: StudyInput[K, Px], b: StudyInput[K, Px]): Unit =
    assertEquals(a.reference, b.reference)
    assertEquals(a.trials.rows.map(_.key), b.trials.rows.map(_.key))
    a.trials.rows.zip(b.trials.rows).foreach { case (x, y) =>
      assertEquals(x.value.frame, y.value.frame)
      assertEquals(x.value.clock, y.value.clock)
      assertEquals(x.value.fixations.toVector, y.value.fixations.toVector)
    }

  private def plan(reference: ArtifactRef[StudyInput[StudyKey, Px]]) = get(
    StudyPlan.cosine(
      reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned()),
      FailurePolicy.RequireAll
    )
  )

  test("decoded input reproduces the prepared identity and the exact study result") {
    val json    = get(codec.input.encode(input))
    val decoded = get(codec.input.parse(json.noSpaces))
    same(input, decoded)
    assertEquals(get(codec.input.encode(decoded)), json)
    val original = plan(input.reference)
    assertEquals(
      get(original.prepare(decoded)).inputReference,
      get(original.prepare(input)).inputReference
    )
    val direct   = get(original.run(input)).scales.head
    val reloaded = get(plan(decoded.reference).run(decoded)).scales.head
    val expected = MatchedControlFixtures.reductions.map(r => r.id -> r.difference).toMap
    get(direct.contrast).rows.zip(get(reloaded.contrast).rows).foreach { case (a, b) =>
      assertEquals(a.key, b.key)
      assertEquals(get(a.difference).value, get(b.difference).value)
      val label = s"${a.key.participant}/${a.key.stimulus}/${a.key.phase}"
      assertEqualsDouble(get(b.difference).value, expected(label), OracleTolerance)
    }
    assertEquals(reloaded.estimation.size, 12)
    assertEquals(reloaded.excludedPhases, Vector.empty)
  }

  test("repeated presentations with equal display projections remain distinct trials") {
    val layout   = OccurrenceKey.layout(id("test.occurrence-layout"))
    val keyCodec = OccurrenceKey.codec(id("test.occurrence-key"))
    val custom   = new StudyInputCodec[OccurrenceKey, Px](
      id("test.occurrence-input"),
      id("test.occurrence-ledger"),
      layout,
      keyCodec
    )
    val quoted = "s1, \"lab\"\nA"
    val first  = OccurrenceKey(quoted, "a", "recall", 1)
    val second = OccurrenceKey(quoted, "a", "recall", 2)
    val encode = OccurrenceKey(quoted, "a", "encode", 1)
    val value  = StudyInput(
      Trials(
        Vector(
          Trial(first, (), scanpath(first, Vector((0.5, 0.5), (1.5, 1.5)))),
          Trial(second, (), scanpath(second, Vector((1.5, 0.5), (0.5, 1.5)))),
          Trial(encode, (), scanpath(encode, Vector((0.5, 0.5), (0.5, 1.5))))
        )
      )
    )
    val decoded = get(custom.input.decode(get(custom.input.encode(value))))
    same(value, decoded)
    assertEquals(decoded.trials.rows.map(_.key), Vector(first, second, encode))
    assertEquals(
      decoded.trials.rows.map(t => layout.participant(t.key)).distinct,
      Vector(quoted)
    )
    val prepared = get(
      get(
        StudyPlan.of(
          decoded.reference,
          layout,
          grid,
          "recall",
          "encode",
          Weight.Duration,
          Vector(StudyEstimate.Binned()),
          FailurePolicy.RequireAll,
          StudyMethod.cosine[Px](id("eyes4s.cosine")),
          ()
        )
      ).prepare(decoded)
    )
    assertEquals(prepared.focalIndices, Vector(0, 1))
    assertEquals(prepared.referenceIndices, Vector(2))
  }

  private def valueField(json: Json, name: String): Json =
    get(json.hcursor.downField("value").get[Json](name))
  private def withValue(json: Json, name: String, value: Json): Json =
    json.mapObject(
      _.add("value", get(json.hcursor.get[Json]("value")).mapObject(_.add(name, value)))
    )
  private def rows(json: Json): Vector[Json] =
    get(valueField(json, "trials").hcursor.get[Vector[Json]]("value"))
  private def withRows(json: Json, rows: Vector[Json]): Json =
    withValue(
      json,
      "trials",
      valueField(json, "trials").mapObject(_.add("value", Json.arr(rows*)))
    )
  private def mutateRow(row: Json, change: Json => Json): Json =
    val inner = get(row.hcursor.downField("value").get[Json]("value"))
    row.mapObject(
      _.add(
        "value",
        get(row.hcursor.get[Json]("value")).mapObject(_.add("value", change(inner)))
      )
    )

  test("decode errors name the offending trial, fixation or identity") {
    val json     = get(codec.input.encode(input))
    val original = rows(json)
    val badTime  = withRows(
      json,
      original.updated(
        2,
        mutateRow(
          original(2),
          inner =>
            val fixes = get(inner.hcursor.get[Vector[Json]]("fixations"))
            inner.mapObject(
              _.add(
                "fixations",
                Json.arr(
                  fixes.updated(
                    1,
                    fixes(1).mapObject(_.add("onsetMicros", Json.fromString("x")))
                  )*
                )
              )
            )
        )
      )
    )
    codec.input.decode(badTime) match
      case Left(CodecError.Entry(path, CodecError.Field("onsetMicros", _, _))) =>
        assertEquals(path, "trials.rows[2].fixations[1]")
      case other => fail(s"unexpected $other")
    val ghost = withRows(
      json,
      original.updated(
        0,
        mutateRow(original(0), _.mapObject(_.add("frame", Json.fromString("ghost"))))
      )
    )
    assertEquals(
      codec.input.decode(ghost).left.toOption,
      Some(CodecError.Entry("trials.rows[0]", CodecError.MissingIdentity("frame", "ghost")))
    )
    val overlap = withRows(
      json,
      original.updated(
        0,
        mutateRow(
          original(0),
          inner =>
            val fixes = get(inner.hcursor.get[Vector[Json]]("fixations"))
            inner.mapObject(
              _.add("fixations", Json.arr((fixes :+ fixes.head)*))
            )
        )
      )
    )
    codec.input.decode(overlap) match
      case Left(CodecError.Entry("trials.rows[0]", CodecError.Field("fixations", _, _))) => ()
      case other => fail(s"unexpected $other")
    assert(codec.input.decode(withValue(json, "unit", Json.fromString("deg"))).isLeft)
    assertEquals(
      codec.input.decode(withValue(json, "keySchema", Wire.id(id("other-key")))).left.toOption,
      Some(CodecError.Schema(DefinitionId.studyKey, id("other-key")))
    )
    assertEquals(
      codec.input.decode(withValue(json, "layout", Wire.id(id("other-layout")))).left.toOption,
      Some(CodecError.Schema(DefinitionId.studyLayout, id("other-layout")))
    )
    assertEquals(
      codec.input
        .decode(json.mapObject(_.add("schema", Wire.id(id("eyes4s.study-input", 2)))))
        .left
        .toOption,
      Some(CodecError.Schema(StudyInputCodecs.input, id("eyes4s.study-input", 2)))
    )
    assert(codec.input.parse("not JSON").isLeft)
  }

  test("reordered, dropped or tampered trials contradict the declared input identity") {
    val json            = get(codec.input.encode(input))
    val original        = rows(json)
    val reversed        = withRows(json, original.reverse)
    val reorderedDigest = StudyInput(Trials(input.trials.rows.reverse)).reference.digest
    assertEquals(
      codec.input.decode(reversed).left.toOption,
      Some(CodecError.InputIdentity(input.reference.digest, reorderedDigest))
    )
    codec.input.decode(withRows(json, original.drop(1))) match
      case Left(CodecError.InputIdentity(declared, _)) =>
        assertEquals(declared, input.reference.digest)
      case other => fail(s"unexpected $other")
    codec.input.decode(withValue(json, "input", Json.fromString("0123456789abcdef"))) match
      case Left(CodecError.InputIdentity("0123456789abcdef", actual)) =>
        assertEquals(actual, input.reference.digest)
      case other => fail(s"unexpected $other")
    assert(codec.input.decode(withValue(json, "input", Json.fromString("xyz"))).isLeft)
  }

  test("encoding refuses conflicting frame identities and source-supported summaries") {
    val key   = StudyKey("s1", "a", "recall")
    val other = StudyKey("s1", "b", "recall")
    val clash = Frame.of(FrameId(frame.id.name), get(Bounds.of[Px](0, 0, 4, 4)), YAxis.Down)
    val clock = ClockId("shared")
    val conflicting = StudyInput(
      Trials(
        Vector(
          Trial(key, (), scanpath(key, Vector((0.5, 0.5)))),
          Trial(
            other,
            (),
            get(Scanpath.of(clash, clock, IArray(fixation(clock, 0L, 3.5, 3.5))))
          )
        )
      )
    )
    codec.input.encode(conflicting) match
      case Left(CodecError.Entry("trials.rows[1]", CodecError.Field("frames", _, _))) => ()
      case other => fail(s"unexpected $other")
    val screen    = get(Frame.screen("backed", 800, 600))
    val source    = RecordingRef("scanpath-source")
    val recording = get(
      Recording.of(
        screen,
        clock,
        Rate.Fixed(get(Hz(1000.0))),
        Eye.Left,
        None,
        IArray(
          Sample(Instant.millis(0), Gaze.Tracked(Pt[Px](100.0, 100.0), None)),
          Sample(Instant.millis(1), Gaze.Tracked(Pt[Px](300.0, 100.0), None))
        )
      )
    )
    val backedFixation = get(
      Event.Fixation.of(
        get(Interval.of(clock, Instant.millis(0), Instant.millis(2))),
        Pt[Px](200.0, 100.0),
        100.0,
        DispersionMethod.RmsRadius,
        2
      )
    )
    val series = get(
      EventSeries.of(
        recording,
        source,
        Vector(backedFixation),
        Vector(get(SampleRange.of(0, 2)))
      )
    )
    val backed = StudyInput(Trials(Vector(Trial(key, (), get(Scanpath.fromEvents(series))))))
    assertEquals(
      codec.input.encode(backed).left.toOption.map {
        case CodecError.Entry(path, CodecError.Unsupported(field, _)) => path           -> field
        case other                                                    => other.toString -> ""
      },
      Some("trials.rows[0]" -> "scanpath")
    )
    val declared = StudyInput(
      Trials(Vector(Trial(key, (), get(Scanpath.of(screen, clock, IArray(backedFixation))))))
    )
    val decoded = get(codec.input.decode(get(codec.input.encode(declared))))
    assertEquals(decoded.trials.rows.head.value.fixations.toVector, Vector(backedFixation))
    assertEquals(decoded.trials.rows.head.value.first.dispersion.map(_.value), Some(100.0))
  }

  private def completeLedger: AdmissionLedger[StudyKey] =
    get(
      AdmissionLedger.decide(
        SourceRef
          .of("matched-control.csv", StudyInputFixtures.header, StudyInputFixtures.records),
        StudyInputFixtures.header,
        StudyInputFixtures.records.zipWithIndex.map { case (row, index) =>
          SourceRecord(
            index + 2,
            Disposition.Admitted(StudyKey(row(0), row(1), row(2)), row(3).toInt)
          )
        },
        AdmissionDecision.RequireComplete
      )
    )

  test("ledger round-trips, cross-checks the input and refuses inconsistent records") {
    val ledger  = completeLedger
    val json    = get(codec.ledger.encode(ledger))
    val decoded = get(codec.ledger.parse(json.noSpaces))
    assertEquals(decoded, ledger)
    assertEquals(get(codec.ledger.encode(decoded)), json)
    assertEquals(decoded.outcome, AdmissionOutcome.Complete)
    assertEquals(decoded.records.size, 48)
    assertEquals(decoded.checkAgainst(input), Right(()))
    val fewer = StudyInput(Trials(input.trials.rows.drop(1)))
    assertEquals(
      decoded.checkAgainst(fewer),
      Left(AdmissionError.UnknownTrial(Vector(2, 3, 4, 5)))
    )
    val doubled = StudyInput(Trials(input.trials.rows.head +: input.trials.rows))
    assertEquals(
      decoded.checkAgainst(doubled),
      Left(AdmissionError.AmbiguousTrial(Vector(0, 1)))
    )
    val partial = get(
      AdmissionLedger.of(ledger.source, ledger.header, ledger.records.drop(1), ledger.outcome)
    )
    assertEquals(partial.checkAgainst(input), Left(AdmissionError.FixationCount(0, 4, 3)))
    val records = ledger.records
    assertEquals(
      AdmissionLedger.of(ledger.source, ledger.header, records.reverse, ledger.outcome),
      Left(AdmissionError.RecordOrder(1, 49, 48))
    )
    assertEquals(
      AdmissionLedger.of(
        ledger.source,
        ledger.header,
        records.updated(1, SourceRecord(3, records.head.disposition)),
        ledger.outcome
      ),
      Left(AdmissionError.DuplicateOrdinal(Vector(2, 3), 0))
    )
    val rejected = SourceRecord(
      2,
      Disposition.Rejected(
        records.head.disposition match
          case Disposition.Admitted(_, _)      => StudyInputFixtures.records.head
          case Disposition.Rejected(raw, _, _) => raw,
        Some(StudyKey("s1", "a", "encode")),
        AdmissionReason.Quarantined(Vector(3), QuarantineCause.RejectedRecords)
      )
    )
    assertEquals(
      AdmissionLedger
        .of(ledger.source, ledger.header, records.updated(0, rejected), ledger.outcome),
      Left(AdmissionError.QuarantineScope(2, Vector(3)))
    )
    assertEquals(
      AdmissionLedger
        .of(ledger.source, ledger.header, records, AdmissionOutcome.ReviewedExclusions),
      Left(AdmissionError.OutcomeMismatch(AdmissionOutcome.ReviewedExclusions, 0))
    )
    val entries = get(valueField(json, "records").as[Vector[Json]])
    val unknown = withValue(
      json,
      "records",
      Json.arr(
        entries.updated(3, entries(3).mapObject(_.add("kind", Json.fromString("lost"))))*
      )
    )
    codec.ledger.decode(unknown) match
      case Left(CodecError.Entry("records[3]", CodecError.Field("kind", _, _))) => ()
      case other => fail(s"unexpected $other")
    val reversedJson = withValue(json, "records", Json.arr(entries.reverse*))
    assertEquals(
      codec.ledger.decode(reversedJson).left.toOption,
      Some(CodecError.Admission(AdmissionError.RecordOrder(1, 49, 48)))
    )
    assertEquals(
      codec.ledger.decode(withValue(json, "outcome", Json.fromString("refused"))).left.toOption,
      Some(CodecError.Admission(AdmissionError.OutcomeMismatch(AdmissionOutcome.Refused, 0)))
    )
    assert(codec.ledger.decode(withValue(json, "outcome", Json.fromString("maybe"))).isLeft)
    assert(
      codec.ledger
        .decode(
          withValue(
            json,
            "source",
            Json.obj("label" -> Json.fromString("x"), "records" -> Json.fromString("short"))
          )
        )
        .isLeft
    )
  }

  test("every typed admission reason survives the ledger payload") {
    val key     = StudyKey("s1", "a", "encode")
    val reasons = Vector(
      AdmissionReason.Width(9, 8),
      AdmissionReason.Key("Missing participant column 'participant'."),
      AdmissionReason.Number("x_px", "NaN", "a finite number"),
      AdmissionReason.Time("0", "-1", "microseconds", "duration must be positive"),
      AdmissionReason.Position(2.5, 0.5, frame.id),
      AdmissionReason.Event("Fixation support must contain at least one sample"),
      AdmissionReason.Quarantined(Vector(2, 3), QuarantineCause.RejectedRecords),
      AdmissionReason.Quarantined(Vector(2, 3), QuarantineCause.DuplicateOrdinals),
      AdmissionReason.Quarantined(Vector(2, 3), QuarantineCause.Overlap(1, "[0, 1)", "[0, 2)")),
      AdmissionReason.Quarantined(Vector(2, 3), QuarantineCause.Scanpath("extent"))
    )
    val records = reasons.zipWithIndex.map { case (reason, index) =>
      SourceRecord(
        index + 2,
        Disposition.Rejected(
          Vector("s1", "a"),
          if index % 2 == 0 then Some(key) else None,
          reason match
            case AdmissionReason.Quarantined(_, cause) =>
              AdmissionReason.Quarantined(Vector(index + 2, index + 3), cause)
            case other => other
        )
      )
    } :+ SourceRecord(reasons.size + 2, Disposition.Admitted(key, 0))
    val ledger = get(
      AdmissionLedger.decide(
        SourceRef.of("bad.csv", Vector("participant", "image"), Vector.empty),
        Vector("participant", "image"),
        records,
        AdmissionDecision.ReviewExclusions
      )
    )
    assertEquals(ledger.outcome, AdmissionOutcome.ReviewedExclusions)
    assertEquals(ledger.quarantined, Vector(key))
    val decoded = get(codec.ledger.decode(get(codec.ledger.encode(ledger))))
    assertEquals(decoded, ledger)
    assertEquals(decoded.rejected.size, reasons.size)
    assertEquals(decoded.admitted.map(_.record), Vector(reasons.size + 2))
  }

  test("registry resolves the key schema and refuses missing or duplicate registrations") {
    val registry = get(StudyInputRegistry.empty[StudyKey, Px].register(codec))
    val json     = get(codec.input.encode(input))
    same(get(registry.decodeInput(json)), input)
    assertEquals(
      get(registry.decodeLedger(get(codec.ledger.encode(completeLedger)))),
      completeLedger
    )
    assertEquals(
      registry.register(codec).left.toOption,
      Some(CodecError.DuplicateKeySchema(DefinitionId.studyKey))
    )
    assertEquals(
      StudyInputRegistry.empty[StudyKey, Px].decodeInput(json).left.toOption,
      Some(CodecError.MissingKeySchema(DefinitionId.studyKey))
    )
    val renamed = new StudyInputCodec[StudyKey, Px](
      StudyInputCodecs.input,
      StudyInputCodecs.ledger,
      codec.layout,
      StudyCodecs.key(id("eyes4s.study-key", 2))
    )
    assertEquals(
      get(registry.register(renamed)).decodeInput(get(renamed.input.encode(input))).isRight,
      true
    )
    assertEquals(
      registry.decodeInput(get(renamed.input.encode(input))).left.toOption,
      Some(CodecError.MissingKeySchema(id("eyes4s.study-key", 2)))
    )
  }

  test("absent key digests and wrong spatial units are rejected statically") {
    assert(
      typeCheckErrors(
        """import eyes4s.plan.*; import eyes4s.design.*
           final case class Opaque(read: Any => Int)
           def layout(id: DefinitionId): StudyLayout[Opaque] = new StudyLayout[Opaque](
             id, Projection.named("p")(_ => ""), Projection.named("s")(_ => ""), Projection.named("f")(_ => ""))"""
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        """import eyes4s.codec.*; import eyes4s.plan.*; import eyes4s.kernel.Unit2D.*
           def wrong(i: StudyInput[StudyKey, Deg]) = StudyInputCodecs.study[Px].input.encode(i)"""
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        """import eyes4s.codec.*; import eyes4s.plan.*; import eyes4s.kernel.Unit2D.*
           def wrong(l: AdmissionLedger[OccurrenceKey]) = StudyInputCodecs.study[Px].ledger.encode(l)"""
      ).nonEmpty
    )
  }
