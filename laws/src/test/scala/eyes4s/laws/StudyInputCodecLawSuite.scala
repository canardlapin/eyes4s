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
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json
import org.scalacheck.{Gen, Test}

/** Published round-trip laws for study-input and admission-ledger payloads,
  * with deliberate mutants that a lawful codec must not survive.
  */
class StudyInputCodecLawSuite extends munit.DisciplineSuite:
  private def checked[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private def id(name: String): DefinitionId    = checked(DefinitionId.of(name, 1))

  final case class OccurrenceKey(
      participant: String,
      stimulus: String,
      phase: String,
      occurrence: Int
  ) derives CanEqual
  private given KeyDigest[OccurrenceKey] = KeyDigest.derived[OccurrenceKey]
  private given Ordering[OccurrenceKey]  =
    Ordering.by(k => (k.participant, k.stimulus, k.phase, k.occurrence))
  private val occurrenceLayout = new StudyLayout[OccurrenceKey](
    id("test.occurrence-layout"),
    Projection.named("participant")(_.participant),
    Projection.named("stimulus")(_.stimulus),
    Projection.named("phase")(_.phase)
  )
  private def occurrenceCodec(
      read: Json => Either[CodecError, OccurrenceKey]
  ): VersionedCodec[OccurrenceKey] =
    VersionedCodec.of[OccurrenceKey](id("test.occurrence-key"))(k =>
      Json.obj(
        "participant" -> Json.fromString(k.participant),
        "stimulus"    -> Json.fromString(k.stimulus),
        "phase"       -> Json.fromString(k.phase),
        "occurrence"  -> Json.fromInt(k.occurrence)
      )
    )(read)
  private def field(json: Json, name: String): Either[CodecError, String] =
    json.hcursor.get[String](name).left.map(e => CodecError.Field(name, json, e.message))
  private val faithfulKeys = occurrenceCodec(json =>
    for
      p <- field(json, "participant")
      s <- field(json, "stimulus")
      f <- field(json, "phase")
      o <- json.hcursor
        .get[Int]("occurrence")
        .left
        .map(e => CodecError.Field("occurrence", json, e.message))
    yield OccurrenceKey(p, s, f, o)
  )

  // Adversarial text: quotes, commas, newlines and empty strings are all keys.
  private val text: Gen[String] = Gen.oneOf(
    Gen.alphaNumStr,
    Gen.const("s1, \"lab\"\nA"),
    Gen.const(""),
    Gen.const("\"quoted\""),
    Gen.const("tab\tseparated")
  )
  private val studyKeys: Gen[StudyKey] = for
    p <- text
    s <- text
    f <- Gen.oneOf("encode", "recall", "practice")
  yield StudyKey(p, s, f)
  private val occurrenceKeys: Gen[OccurrenceKey] = for
    p <- text
    s <- text
    f <- Gen.oneOf("encode", "recall")
    o <- Gen.choose(1, 5)
  yield OccurrenceKey(p, s, f, o)

  private val frames: Vector[Frame[Px]] = Vector(
    Frame.of(FrameId("display"), checked(Bounds.of[Px](0, 0, 800, 600)), YAxis.Down),
    Frame.of(FrameId("tablet"), checked(Bounds.of[Px](-10, -10, 10, 10)), YAxis.Up)
  )
  private def scanpaths(clock: ClockId): Gen[Scanpath[Px]] = for
    frame <- Gen.oneOf(frames)
    n     <- Gen.choose(1, 4)
    start <- Gen.oneOf(
      Gen.chooseNum(-1000000L, 1000000L),
      Gen.const(9007199254740993L),
      Gen.const(Long.MinValue + 1)
    )
    gaps      <- Gen.listOfN(n, Gen.choose(1L, 500000L))
    durations <- Gen.listOfN(n, Gen.choose(1L, 500000L))
    xs        <- Gen.listOfN(n, Gen.choose(0, 1000))
    ys        <- Gen.listOfN(n, Gen.choose(0, 1000))
    counts    <- Gen.listOfN(n, Gen.choose(1, 500))
    spread    <- Gen.option(Gen.choose(0, 100))
  yield
    var t     = start
    val fixes = (0 until n).map { i =>
      val onset  = t + gaps(i)
      val offset = onset + durations(i)
      t = offset
      val x  = frame.bounds.xMin + (frame.bounds.xMax - frame.bounds.xMin) * xs(i) / 1000.0
      val y  = frame.bounds.yMin + (frame.bounds.yMax - frame.bounds.yMin) * ys(i) / 1000.0
      val iv = checked(Interval.of(clock, Instant.micros(onset), Instant.micros(offset)))
      spread match
        case None    => checked(Event.Fixation.withoutDispersion(iv, Pt[Px](x, y), counts(i)))
        case Some(s) =>
          checked(
            Event.Fixation.of(iv, Pt[Px](x, y), s / 10.0, DispersionMethod.RmsRadius, counts(i))
          )
    }
    checked(Scanpath.of(frame, clock, IArray.from(fixes)))

  private def inputs[K](keys: Gen[K])(using d: KeyDigest[K]): Gen[StudyInput[K, Px]] =
    Gen
      .choose(0, 5)
      .flatMap(n =>
        Gen.listOfN(n, keys).flatMap { ks =>
          Gen
            .sequence[Vector[Trial[K, Unit, Scanpath[Px]]], Trial[K, Unit, Scanpath[Px]]](
              ks.zipWithIndex.map { case (k, i) =>
                scanpaths(ClockId(s"trial-$i:${d.digest(k).render}")).map(Trial(k, (), _))
              }
            )
            .map(rows => StudyInput(Trials(rows)))
        }
      )

  private def sameInput[K](a: StudyInput[K, Px], b: StudyInput[K, Px]): Boolean =
    a.reference == b.reference &&
      a.trials.rows.map(_.key) == b.trials.rows.map(_.key) &&
      a.trials.rows.zip(b.trials.rows).forall { case (x, y) =>
        x.value.frame == y.value.frame && x.value.clock == y.value.clock &&
        x.value.fixations.toVector == y.value.fixations.toVector
      }

  private val reasons: Gen[AdmissionReason] = Gen.oneOf(
    Gen.zip(Gen.choose(1, 9), Gen.choose(0, 12)).map(AdmissionReason.Width.apply),
    text.map(AdmissionReason.Key.apply),
    Gen.zip(text, text, text).map(AdmissionReason.Number.apply),
    Gen
      .zip(text, text, Gen.oneOf("microseconds", "seconds"), text)
      .map(AdmissionReason.Time.apply),
    Gen
      .zip(Gen.choose(-5.0, 5.0), Gen.choose(-5.0, 5.0), text.map(FrameId.apply))
      .map(AdmissionReason.Position.apply),
    text.map(AdmissionReason.Event.apply)
  )
  private val causes: Gen[QuarantineCause] = Gen.oneOf(
    Gen.const(QuarantineCause.RejectedRecords),
    Gen.const(QuarantineCause.DuplicateOrdinals),
    Gen.zip(Gen.choose(0, 5), text, text).map(QuarantineCause.Overlap.apply),
    text.map(QuarantineCause.Scanpath.apply)
  )

  private def ledgers[K](keys: Gen[K]): Gen[AdmissionLedger[K]] = for
    label    <- text
    header   <- Gen.listOfN(3, text).map(_.toVector)
    n        <- Gen.choose(1, 8)
    ks       <- Gen.listOfN(n, keys)
    kinds    <- Gen.listOfN(n, Gen.choose(0, 2))
    raws     <- Gen.listOfN(n, Gen.listOfN(3, text).map(_.toVector))
    rs       <- Gen.listOfN(n, reasons)
    cs       <- Gen.listOfN(n, causes)
    decision <- Gen.oneOf(AdmissionDecision.RequireComplete, AdmissionDecision.ReviewExclusions)
  yield
    val records = (0 until n).toVector.map { i =>
      val record                      = i + 2
      val disposition: Disposition[K] = kinds(i) match
        case 0 => Disposition.Admitted(ks(i), i)
        case 1 => Disposition.Rejected(raws(i), Some(ks(i)), rs(i))
        case _ =>
          Disposition.Rejected(
            raws(i),
            Some(ks(i)),
            AdmissionReason.Quarantined(Vector(record), cs(i))
          )
      SourceRecord(record, disposition)
    }
    checked(
      AdmissionLedger.decide(
        SourceRef.of(label, header, raws.toVector),
        header,
        records,
        decision
      )
    )

  private val standard = StudyInputCodecs.study[Px]
  private val custom   = new StudyInputCodec[OccurrenceKey, Px](
    id("test.occurrence-input"),
    id("test.occurrence-ledger"),
    occurrenceLayout,
    faithfulKeys
  )

  checkAll(
    "study input",
    CodecLaws.roundTrip(standard.input, inputs(studyKeys), sameInput[StudyKey])
  )
  checkAll(
    "occurrence input",
    CodecLaws.roundTrip(custom.input, inputs(occurrenceKeys), sameInput[OccurrenceKey])
  )
  checkAll(
    "admission ledger",
    CodecLaws.roundTrip(
      standard.ledger,
      ledgers(studyKeys),
      (a: AdmissionLedger[StudyKey], b: AdmissionLedger[StudyKey]) => a == b
    )
  )
  checkAll(
    "occurrence ledger",
    CodecLaws.roundTrip(
      custom.ledger,
      ledgers(occurrenceKeys),
      (a: AdmissionLedger[OccurrenceKey], b: AdmissionLedger[OccurrenceKey]) => a == b
    )
  )
  checkAll(
    "generic trials",
    CodecLaws.roundTrip(
      VersionedCodec.trials(
        id("test.trials"),
        StudyCodecs.key(id("test.key")),
        VersionedCodec.string(id("test.meta")),
        VersionedCodec.string(id("test.value"))
      ),
      Gen
        .listOf(Gen.zip(studyKeys, text, text).map(Trial.apply))
        .map(rows => Trials(rows.toVector)),
      (a: Trials[StudyKey, String, String], b: Trials[StudyKey, String, String]) => a == b
    )
  )

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

  private def killed[A](codec: VersionedCodec[A], gen: Gen[A], eq: (A, A) => Boolean): Boolean =
    CodecLaws.roundTrip(codec, gen, eq).all.properties.exists { case (_, prop) =>
      !Test.check(Test.Parameters.default.withMinSuccessfulTests(40), prop).passed
    }

  test("published laws kill dropped ledger rows, reordered trials and collapsed occurrences") {
    val withAdmitted = ledgers(studyKeys).suchThat(_.admitted.nonEmpty)
    val droppedRow   = mutant(standard.ledger)(l =>
      AdmissionLedger
        .of(l.source, l.header, l.records.filterNot(_ == l.admitted.head), l.outcome)
        .left
        .map(CodecError.Admission.apply)
    )
    assert(
      killed(
        droppedRow,
        withAdmitted,
        (a: AdmissionLedger[StudyKey], b: AdmissionLedger[StudyKey]) => a == b
      )
    )

    val withExclusions    = ledgers(studyKeys).suchThat(_.rejected.nonEmpty)
    val droppedExclusions = mutant(standard.ledger)(l =>
      AdmissionLedger
        .of(l.source, l.header, l.admitted, l.outcome)
        .left
        .map(CodecError.Admission.apply)
    )
    assert(
      killed(
        droppedExclusions,
        withExclusions,
        (a: AdmissionLedger[StudyKey], b: AdmissionLedger[StudyKey]) => a == b
      )
    )

    val several   = inputs(studyKeys).suchThat(_.trials.size >= 2)
    val reordered =
      mutant(standard.input)(i => Right(StudyInput(Trials(i.trials.rows.reverse))))
    assert(killed(reordered, several, sameInput[StudyKey]))

    val forgetful = new StudyInputCodec[OccurrenceKey, Px](
      id("test.occurrence-input"),
      id("test.occurrence-ledger"),
      occurrenceLayout,
      occurrenceCodec(json =>
        for
          p <- field(json, "participant")
          s <- field(json, "stimulus")
          f <- field(json, "phase")
        yield OccurrenceKey(p, s, f, 1)
      )
    )
    val repeated = inputs(occurrenceKeys).suchThat(_.trials.rows.exists(_.key.occurrence != 1))
    // The forgetful key codec contradicts the declared digest, so decoding fails.
    assert(killed(forgetful.input, repeated, sameInput[OccurrenceKey]))
    val repeatedLedgers = ledgers(occurrenceKeys).suchThat(
      _.records.exists {
        case SourceRecord(_, Disposition.Admitted(k, _))    => k.occurrence != 1
        case SourceRecord(_, Disposition.Rejected(_, k, _)) => k.exists(_.occurrence != 1)
      }
    )
    assert(
      killed(
        forgetful.ledger,
        repeatedLedgers,
        (a: AdmissionLedger[OccurrenceKey], b: AdmissionLedger[OccurrenceKey]) => a == b
      )
    )
  }
