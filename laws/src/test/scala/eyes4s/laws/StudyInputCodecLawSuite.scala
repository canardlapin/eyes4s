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
    Gen.const(QuarantineCause.NoFixations),
    Gen.zip(Gen.choose(0, 5), text, text).map(QuarantineCause.Overlap.apply),
    Gen.zip(Gen.choose(0, 5), text, text).map(QuarantineCause.WrongClock.apply),
    Gen.zip(Gen.choose(0, 5), text).map(QuarantineCause.InvalidTransition.apply),
    text.map(QuarantineCause.InvalidExtent.apply),
    Gen
      .zip(
        Gen.choose(0, 5),
        text.map(FrameId.apply),
        text.map(FrameId.apply),
        Gen.choose(-5.0, 5.0),
        Gen.choose(-5.0, 5.0)
      )
      .map(QuarantineCause.UnmappableFixation.apply),
    // Causes that arrived with the admission policy: a ledger naming one can
    // only be written as version 2.
    Gen.zip(Gen.choose(0, 3), Gen.choose(4, 6)).map(QuarantineCause.CorrectionConflict.apply),
    Gen.listOfN(2, text).map(v => QuarantineCause.ItemConflict(v.toVector)),
    Gen.const(QuarantineCause.OccurrenceConflict(Vector(1, 2)))
  )

  /** One admitted record, one rejected record, or a quarantined trial of one
    * to three consecutive records sharing a scope, the way the importer emits it.
    */
  private enum Block[K]:
    case Admitted(key: K, raw: Vector[String])
    case Rejected(key: Option[K], raw: Vector[String], reason: AdmissionReason)
    case Quarantined(
        key: K,
        raws: Vector[Vector[String]],
        reason: AdmissionReason,
        cause: QuarantineCause
    )

  private def blocks[K](keys: Gen[K], quarantine: K => K): Gen[Block[K]] =
    val raw = Gen.listOfN(3, text).map(_.toVector)
    Gen.frequency(
      3 -> Gen.zip(keys, raw).map(Block.Admitted.apply),
      2 -> Gen.zip(Gen.option(keys), raw, reasons).map(Block.Rejected.apply),
      2 -> Gen
        .zip(
          keys.map(quarantine),
          Gen.choose(1, 3).flatMap(Gen.listOfN(_, raw)),
          reasons,
          causes
        )
        .map { case (k, raws, reason, cause) =>
          Block.Quarantined(k, raws.toVector, reason, cause)
        }
    )

  private def ledgers[K](keys: Gen[K], quarantine: K => K): Gen[AdmissionLedger[K]] = for
    label    <- text
    header   <- Gen.listOfN(3, text).map(_.toVector)
    n        <- Gen.choose(1, 6)
    bs       <- Gen.listOfN(n, blocks(keys, quarantine))
    decision <- Gen.oneOf(AdmissionDecision.RequireComplete, AdmissionDecision.ReviewExclusions)
  yield
    val (records, raws) = bs.zipWithIndex.foldLeft(
      (Vector.empty[SourceRecord[K]], Vector.empty[Vector[String]])
    ) { case ((acc, rows), (block, ordinal)) =>
      val first = acc.size + 2
      block match
        case Block.Admitted(k, raw) =>
          (acc :+ SourceRecord(first, Disposition.Admitted(k, ordinal)), rows :+ raw)
        case Block.Rejected(k, raw, reason) =>
          (acc :+ SourceRecord(first, Disposition.Rejected(raw, k, reason)), rows :+ raw)
        case Block.Quarantined(k, blockRaws, reason, cause) =>
          val scope   = blockRaws.indices.map(_ + first).toVector
          val entries = blockRaws.zipWithIndex.map { case (raw, i) =>
            val disposition =
              if i == 0 && cause == QuarantineCause.RejectedRecords then
                Disposition.Rejected(raw, Some(k), reason)
              else Disposition.Rejected(raw, Some(k), AdmissionReason.Quarantined(scope, cause))
            SourceRecord(first + i, disposition)
          }
          (acc ++ entries, rows ++ blockRaws)
    }
    checked(
      AdmissionLedger.decide(SourceRef.of(label, header, raws), header, records, decision)
    )

  private val corrections: Gen[Correction] = Gen.oneOf(
    Gen.const(Correction.FlipX),
    Gen.const(Correction.FlipY),
    Gen
      .zip(Gen.choose(-500.0, 500.0), Gen.oneOf(Gen.choose(-500.0, 500.0), Gen.const(-0.0)))
      .map((dx, dy) => checked(Correction.translate(dx, dy)))
  )

  /** Ledgers under an explicit admission policy: either off-screen policy,
    * correction rules of every scope that never cover one admitted trial
    * twice, and admitted records listed as outside the frame under
    * ExcludeRecord. Nothing is discarded.
    */
  private def policyLedgers[K](keys: Gen[K], quarantine: K => K): Gen[AdmissionLedger[K]] =
    for
      base      <- ledgers(keys, quarantine)
      offScreen <- Gen.oneOf(OffScreenPolicy.values.toIndexedSeq)
      admitted = base.records.collect { case SourceRecord(r, Disposition.Admitted(k, _)) =>
        r -> k
      }
      trial <-
        if admitted.isEmpty then Gen.const(None)
        else Gen.option(Gen.oneOf(admitted.map(_._2)))
      global  <- Gen.oneOf(true, false)
      first   <- corrections
      second  <- corrections
      outside <- Gen.someOf(admitted.map(_._1))
      xs      <- Gen.listOfN(
        outside.size,
        Gen.oneOf(Gen.choose(-5000.0, -0.001), Gen.choose(801.0, 9000.0))
      )
    yield
      // One global rule, or a trial rule beside a participant rule that
      // covers no key: never two rules on one admitted trial.
      val rules =
        if global then Vector(AppliedCorrection(CorrectionScope.AllTrials[K](), first))
        else
          trial.toVector.map(k => AppliedCorrection(CorrectionScope.Trial(k), first)) :+
            AppliedCorrection(CorrectionScope.Participant[K]("\u0000nobody"), second)
      val listed =
        if offScreen == OffScreenPolicy.ExcludeRecord then
          outside.toVector.sorted
            .zip(xs)
            .map((r, x) => OutsideFrame(r, x, 12.5, FrameId("display")))
        else Vector.empty
      checked(
        AdmissionLedger.of(
          base.source,
          base.header,
          base.records,
          base.outcome,
          AdmissionPolicy(offScreen, rules),
          listed
        )
      )

  // Quarantined keys are kept disjoint from admitted keys, as the importer guarantees.
  private def quarantineStudyKey(k: StudyKey): StudyKey =
    k.copy(participant = "quarantined-" + k.participant)
  private def quarantineOccurrenceKey(k: OccurrenceKey): OccurrenceKey =
    k.copy(participant = "quarantined-" + k.participant)

  // ---------------------------------------------------------------- inventory

  private def valueOf(kind: AttributeKind): Gen[AttributeValue] =
    Gen.frequency(
      1 -> Gen.const(AttributeValue.Blank),
      3 -> (kind match
        case AttributeKind.Text    => text.map(AttributeValue.Text.apply)
        case AttributeKind.Integer =>
          Gen
            .oneOf(Gen.choose(Long.MinValue, Long.MaxValue), Gen.const(9007199254740993L))
            .map(AttributeValue.Integer.apply)
        case AttributeKind.Number =>
          Gen.oneOf(Gen.choose(-1e12, 1e12), Gen.const(-0.0)).map(AttributeValue.Number.apply))
    )
  private def columnsGen(prefix: String): Gen[Vector[AttributeColumn]] =
    Gen
      .listOfN(3, Gen.oneOf(AttributeKind.values.toIndexedSeq))
      .map(_.toVector.zipWithIndex.map((k, i) => AttributeColumn(s"$prefix$i", k)))
  private def attributesOf(columns: Vector[AttributeColumn]): Gen[Attributes] =
    Gen
      .sequence[Vector[AttributeValue], AttributeValue](columns.map(c => valueOf(c.kind)))
      .map(values => checked(Attributes.of(columns.map(_.name).zip(values))))
  private val sampleCounts: Gen[SampleCountRule] = Gen.oneOf(
    Gen.const(SampleCountRule.PositiveColumn("samples")),
    Gen.choose(1.0, 2000.0).map(r => SampleCountRule.DerivedFromDuration(checked(Hz(r))))
  )

  /** A version-3 ledger built from a generated inventory scenario the way the
    * importer records it: one disposition per inventory trial, row-level
    * rejections for records with no samples, quarantined records naming
    * their cause, and trials outside the inventory listed. Declared attribute
    * columns of every kind, and values of their kind or blank, are drawn for
    * trials and admitted records.
    */
  private val inventoryLedgers: Gen[AdmissionLedger[TrialKey]] =
    for
      scenario      <- InventoryScenario.gen
      trialColumns  <- columnsGen("t")
      recordColumns <- columnsGen("r")
      counts        <- sampleCounts
      trialValues   <- Gen.listOfN(scenario.trials.size, attributesOf(trialColumns))
      recordValues  <- Gen.listOfN(
        scenario.records.size,
        Gen.option(attributesOf(recordColumns))
      )
    yield
      val s    = scenario
      val zero = AdmissionReason.Number("samples", "0", "a positive integer")
      val numbers: Map[TrialIdentity, Vector[Int]] =
        (s.trials.map(_._1) ++ s.unlisted.map(_._1)).map(id => id -> s.recordsOf(id)).toMap
      def raw(n: Int) = Vector(n.toString, s.records(n - 2).identity.render)
      val dispositions: Vector[(Int, Disposition[TrialKey])] =
        s.trials.flatMap { (id, item, plan) =>
          val key                                 = checked(id.withItem(item))
          val rs                                  = numbers(id)
          def quarantined(cause: QuarantineCause) =
            rs.map(n =>
              n -> Disposition.Rejected(
                raw(n),
                Some(key),
                AdmissionReason.Quarantined(rs, cause)
              )
            )
          plan match
            case TrialPlan.Absent      => Vector.empty
            case TrialPlan.Admitted(_) =>
              rs.map(n => n -> Disposition.Admitted(key, s.records(n - 2).ordinal))
            case TrialPlan.AllRejected(_) =>
              rs.map(n => n -> Disposition.Rejected(raw(n), Some(key), zero))
            case TrialPlan.OneRejected(_) =>
              rs.map(n =>
                n -> (if s.records(n - 2).samples == 0 then
                        Disposition.Rejected(raw(n), Some(key), zero)
                      else
                        Disposition.Rejected(
                          raw(n),
                          Some(key),
                          AdmissionReason.Quarantined(rs, QuarantineCause.RejectedRecords)
                        ))
              )
            case TrialPlan.DuplicateOrdinals(_) =>
              quarantined(QuarantineCause.DuplicateOrdinals)
            case TrialPlan.Overlap(_) =>
              quarantined(QuarantineCause.Overlap(1, "[0,50)", "[20,70)"))
            case TrialPlan.ItemConflict(_) =>
              quarantined(QuarantineCause.InventoryItemConflict(item, Vector(item + "-other")))
            case TrialPlan.OccurrenceConflict(_) =>
              quarantined(QuarantineCause.OccurrenceConflict(Vector(1, 2)))
        } ++ s.unlisted.flatMap { (id, _) =>
          val rs    = numbers(id)
          val key   = Option.when(s.recordItems)(checked(id.withItem("unlisted")))
          val cause =
            QuarantineCause.NotInInventory(
              id.participant,
              id.phase,
              id.trial,
              id.occurrence.value
            )
          rs.map(n =>
            n -> Disposition.Rejected(raw(n), key, AdmissionReason.Quarantined(rs, cause))
          )
        }
      val records = dispositions.sortBy(_._1).map((n, d) => SourceRecord(n, d))
      val header  = Vector("participant", "phase", "trial")
      val trials  =
        s.trials.zip(trialValues).zipWithIndex.map { case (((id, item, plan), attrs), i) =>
          val named =
            if s.recordItems then
              s.records
                .filter(r => InventoryScenario.label(r.identity) == InventoryScenario.label(id))
                .map(_.item)
                .filter(_.nonEmpty)
                .distinct
                .sorted
            else Vector.empty
          checked(
            InventoryTrial.of(
              id,
              Vector(i + 2),
              Some(item),
              attrs,
              named,
              numbers(id),
              plan match
                case TrialPlan.Absent         => TrialDisposition.Absent
                case TrialPlan.Admitted(_)    => TrialDisposition.Admitted
                case TrialPlan.AllRejected(_) => TrialDisposition.NoFixations
                case TrialPlan.OneRejected(_) =>
                  TrialDisposition.Quarantined(QuarantineCause.RejectedRecords)
                case TrialPlan.DuplicateOrdinals(_) =>
                  TrialDisposition.Quarantined(QuarantineCause.DuplicateOrdinals)
                case TrialPlan.Overlap(_) =>
                  TrialDisposition.Quarantined(QuarantineCause.Overlap(1, "[0,50)", "[20,70)"))
                case TrialPlan.ItemConflict(_) =>
                  TrialDisposition.Quarantined(
                    QuarantineCause.InventoryItemConflict(item, Vector(item + "-other"))
                  )
                case TrialPlan.OccurrenceConflict(_) =>
                  TrialDisposition.Quarantined(QuarantineCause.OccurrenceConflict(Vector(1, 2)))
            )
          )
        }
      val unlisted = s.unlisted.map((id, _) =>
        checked(
          UnlistedTrial.of(
            id,
            if s.recordItems then Vector("unlisted") else Vector.empty,
            numbers(id)
          )
        )
      )
      val admitted = records.collect { case SourceRecord(n, Disposition.Admitted(_, _)) =>
        n
      }.toSet
      val perRecord = recordValues.zipWithIndex.collect {
        case (Some(attrs), i) if admitted.contains(i + 2) =>
          checked(RecordAttributes.of(i + 2, attrs))
      }
      val inventory = checked(
        InventoryLedger.of(
          SourceRef.of("t.csv", header, s.trials.map(t => Vector(t._1.render))),
          header,
          trialColumns,
          trials,
          unlisted,
          recordColumns,
          perRecord.toVector,
          counts
        )
      )
      checked(
        AdmissionLedger.decide(
          SourceRef.of("f.csv", header, s.records.map(r => Vector(r.identity.render))),
          header,
          records,
          AdmissionDecision.ReviewExclusions,
          AdmissionPolicy.default[TrialKey],
          Vector.empty,
          inventory,
          TrialIdentity.of,
          _.item
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
      ledgers(studyKeys, quarantineStudyKey),
      (a: AdmissionLedger[StudyKey], b: AdmissionLedger[StudyKey]) => a == b
    )
  )
  checkAll(
    "admission ledger with policy",
    CodecLaws.roundTrip(
      standard.ledger,
      policyLedgers(studyKeys, quarantineStudyKey),
      (a: AdmissionLedger[StudyKey], b: AdmissionLedger[StudyKey]) => a == b
    )
  )
  checkAll(
    "admission ledger with inventory",
    CodecLaws.roundTrip(
      StudyInputCodecs.trial[Px].ledger,
      inventoryLedgers,
      (a: AdmissionLedger[TrialKey], b: AdmissionLedger[TrialKey]) => a == b
    )
  )
  checkAll(
    "admission ledger versions",
    SchemaLadderLaws.ladder(
      standard.ledgerLadder,
      Gen.oneOf(
        ledgers(studyKeys, quarantineStudyKey),
        policyLedgers(studyKeys, quarantineStudyKey)
      ),
      (a: AdmissionLedger[StudyKey], b: AdmissionLedger[StudyKey]) => a == b
    )
  )
  checkAll(
    "inventory ledger versions",
    SchemaLadderLaws.ladder(
      StudyInputCodecs.trial[Px].ledgerLadder,
      inventoryLedgers,
      (a: AdmissionLedger[TrialKey], b: AdmissionLedger[TrialKey]) => a == b
    )
  )
  checkAll(
    "occurrence ledger",
    CodecLaws.roundTrip(
      custom.ledger,
      ledgers(occurrenceKeys, quarantineOccurrenceKey),
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
    // The wrapped codec's whole document is the payload, so a value keeps the
    // schema version it was written under.
    VersionedCodec.checked[A](codec.schema)(codec.encode)(raw =>
      codec.decode(raw).flatMap(change)
    )

  private def killed[A](codec: VersionedCodec[A], gen: Gen[A], eq: (A, A) => Boolean): Boolean =
    CodecLaws.roundTrip(codec, gen, eq).all.properties.exists { case (_, prop) =>
      !Test.check(Test.Parameters.default.withMinSuccessfulTests(40), prop).passed
    }

  test(
    "the policy ledger law kills a dropped rule, a flipped policy and a lost off-frame record"
  ) {
    type L = AdmissionLedger[StudyKey]
    val eq = (a: L, b: L) => a == b
    def rebuilt(l: L, policy: AdmissionPolicy[StudyKey], outside: Vector[OutsideFrame]) =
      AdmissionLedger
        .of(l.source, l.header, l.records, l.outcome, policy, outside)
        .left
        .map(CodecError.Admission.apply)
    val gen         = policyLedgers(studyKeys, quarantineStudyKey)
    val droppedRule = mutant(standard.ledger)(l =>
      rebuilt(l, l.policy.copy(corrections = l.policy.corrections.drop(1)), l.outsideFrame)
    )
    assert(killed(droppedRule, gen, eq))
    val flipped = mutant(standard.ledger)(l =>
      if l.outsideFrame.nonEmpty then Right(l)
      else
        rebuilt(
          l,
          l.policy.copy(offScreen =
            if l.policy.offScreen == OffScreenPolicy.ExcludeRecord then
              OffScreenPolicy.QuarantineTrial
            else OffScreenPolicy.ExcludeRecord
          ),
          l.outsideFrame
        )
    )
    assert(killed(flipped, gen, eq))
    val lost = mutant(standard.ledger)(l => rebuilt(l, l.policy, l.outsideFrame.drop(1)))
    assert(killed(lost, gen, eq))
  }

  test("the ledger ladder laws kill dropped or wrong upcasts and writing the latest version") {
    val ladder = standard.ledgerLadder
    val values = Gen.oneOf(
      ledgers(studyKeys, quarantineStudyKey),
      policyLedgers(studyKeys, quarantineStudyKey)
    )
    val same = (a: AdmissionLedger[StudyKey], b: AdmissionLedger[StudyKey]) => a == b
    val v1 +: v2 +: v3 +: _ = ladder.versions: @unchecked
    assert(LadderMutants.passes(ladder, values, same))
    val lifting = Vector(
      "a lifted document decodes to the value and re-encodes to the earliest document",
      "upcasting a vN payload writes exactly what each later version writes"
    )
    // Each upcast step dropped in turn.
    assertEquals(
      LadderMutants.falsified(
        LadderMutants.rebuilt(ladder)(upcast = { case `v2` => identity }),
        values,
        same
      ),
      lifting
    )
    assertEquals(
      LadderMutants.falsified(
        LadderMutants.rebuilt(ladder)(upcast = { case `v3` => identity }),
        values,
        same
      ),
      lifting
    )
    // The v1 -> v2 upcast states today's default policy, not version 1's.
    val defaultPolicy: Json => Json = json =>
      ladder
        .upcast(v1, json)
        .fold(
          _ => json,
          (_, lifted) =>
            lifted.hcursor
              .downField("offScreen")
              .set(Json.fromString("excludeRecord"))
              .top
              .getOrElse(lifted)
        )
    assertEquals(
      LadderMutants.falsified(
        LadderMutants.rebuilt(ladder)(upcast = { case `v2` => defaultPolicy }),
        values,
        same
      ),
      lifting
    )
    // Every ledger written as the latest version, and a version-2 vocabulary
    // that claims a policy ledger is a version-1 ledger.
    assertEquals(
      LadderMutants.falsified(
        LadderMutants.rebuilt(ladder)(expresses = { case `v1` | `v2` => _ => false }),
        values,
        same
      ),
      Vector(
        "a value is written under the earliest version that expresses it"
      )
    )
    assert(
      LadderMutants
        .falsified(
          LadderMutants.rebuilt(ladder)(expresses = { case `v1` => _ => true }),
          values,
          same
        )
        .contains("a value is written under the earliest version that expresses it")
    )
  }

  test("published laws kill dropped ledger rows, reordered trials and collapsed occurrences") {
    val withAdmitted = ledgers(studyKeys, quarantineStudyKey).suchThat(_.admitted.nonEmpty)
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

    val withExclusions    = ledgers(studyKeys, quarantineStudyKey).suchThat(_.rejected.nonEmpty)
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
    val repeatedLedgers = ledgers(occurrenceKeys, quarantineOccurrenceKey).suchThat(
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

  test(
    "the inventory ledger law kills a changed attribute, a dropped trial and a lost listing"
  ) {
    type L = AdmissionLedger[TrialKey]
    val eq    = (a: L, b: L) => a == b
    val codec = StudyInputCodecs.trial[Px].ledger
    def rejoin(l: L)(change: InventoryLedger => Either[InventoryError, InventoryLedger]) =
      l.inventory.fold[Either[CodecError, L]](Right(l))(inventory =>
        change(inventory).left
          .map(e => CodecError.Admission(AdmissionError.Inventory(e)))
          .flatMap(changed =>
            l.withInventory(changed, TrialIdentity.of, _.item)
              .left
              .map(CodecError.Admission.apply)
          )
      )
    def rebuilt(i: InventoryLedger)(
        trials: Vector[InventoryTrial] = i.trials,
        unlisted: Vector[UnlistedTrial] = i.unlisted
    ) = InventoryLedger.of(
      i.source,
      i.header,
      i.attributeColumns,
      trials,
      unlisted,
      i.recordAttributeColumns,
      i.recordAttributes,
      i.sampleCounts
    )
    def withAttributes(t: InventoryTrial, values: Attributes) =
      InventoryTrial.of(
        t.identity,
        t.rows,
        t.inventoryItem,
        values,
        t.recordItems,
        t.records,
        t.disposition
      )
    val changedAttribute = mutant(codec)(l =>
      rejoin(l)(i =>
        i.trials
          .foldLeft[Either[InventoryError, Vector[InventoryTrial]]](Right(Vector.empty))(
            (acc, t) =>
              acc.flatMap(done =>
                withAttributes(
                  t,
                  checked(
                    Attributes.of(
                      t.attributes.entries.map((n, v) =>
                        n -> (v match
                          case AttributeValue.Integer(x) => AttributeValue.Integer(x ^ 1L)
                          case AttributeValue.Text(x)    => AttributeValue.Text(x + "!")
                          case other                     => other)
                      )
                    )
                  )
                ).map(done :+ _)
              )
          )
          .flatMap(trials => rebuilt(i)(trials = trials))
      )
    )
    assert(killed(changedAttribute, inventoryLedgers, eq))
    val droppedTrial = mutant(codec)(l =>
      rejoin(l)(i =>
        rebuilt(i)(trials = i.trials.filterNot(_.disposition == TrialDisposition.Absent))
      )
    )
    assert(killed(droppedTrial, inventoryLedgers, eq))
    val lostListing = mutant(codec)(l => rejoin(l)(i => rebuilt(i)(unlisted = Vector.empty)))
    assert(
      killed(
        lostListing,
        inventoryLedgers.suchThat(_.inventory.exists(_.unlisted.nonEmpty)),
        eq
      )
    )
  }

  /** Forge a member of an encoded version-3 ledger's inventory. */
  private def forged(json: Json)(change: io.circe.ACursor => io.circe.ACursor): Option[Json] =
    change(json.hcursor.downField("value").downField("inventory")).top

  test("the version-3 decoder refuses forged inventories the importer could not produce") {
    val codec = StudyInputCodecs.trial[Px].ledger
    val prop  = org.scalacheck.Prop.forAll(inventoryLedgers) { ledger =>
      val json      = checked(codec.encode(ledger))
      val inventory = ledger.inventory.get
      val forgeries = Vector(
        // An absent trial that names record items.
        inventory.trials.indexWhere(_.disposition == TrialDisposition.Absent) match
          case -1 => None
          case i  =>
            forged(json)(
              _.downField("trials")
                .downN(i)
                .downField("recordItems")
                .withFocus(_ => Json.arr(Json.fromString("zzz")))
            )
        ,
        // One trial's attribute of another kind than its column declares.
        forged(json)(
          _.downField("attributeColumns")
            .downN(0)
            .downField("kind")
            .withFocus(k =>
              Json.fromString(if k.asString.contains("text") then "integer" else "text")
            )
        ).filter(_ =>
          inventory.trials.exists(t =>
            t.attributes.entries.headOption.exists(_._2 != AttributeValue.Blank)
          )
        ),
        // An admitted trial that names a conflicting item.
        inventory.trials.indexWhere(_.disposition == TrialDisposition.Admitted) match
          case -1 => None
          case i  =>
            forged(json)(
              _.downField("trials")
                .downN(i)
                .downField("recordItems")
                .withFocus(_ => Json.arr(Json.fromString("aaa"), Json.fromString("zzz")))
            )
      ).flatten
      // A null inventory under version 3 is the lift of a ledger without one:
      // refused when a record names an inventory cause, and otherwise read as
      // that ledger, which is written under an earlier version.
      val inventoryCause = ledger.records.exists {
        case SourceRecord(_, Disposition.Rejected(_, _, AdmissionReason.Quarantined(_, c))) =>
          QuarantineCause.version(c) == 3
        case _ => false
      }
      val nullified =
        json.hcursor.downField("value").downField("inventory").set(Json.Null).top.get
      val withoutInventory = codec.decode(nullified) match
        case Left(_)        => inventoryCause
        case Right(decoded) =>
          !inventoryCause && decoded.inventory.isEmpty && decoded.version <= 2 &&
          decoded.records == ledger.records
      org.scalacheck.Prop.all(
        ((org.scalacheck
          .Prop(withoutInventory) :| s"null inventory read as ${codec.decode(nullified)}") +:
          forgeries.map(f =>
            org.scalacheck.Prop(codec.decode(f).isLeft) :| s"accepted ${f.noSpaces.take(200)}"
          ))*
      )
    }
    assert(Test.check(Test.Parameters.default.withMinSuccessfulTests(100), prop).passed)
  }
