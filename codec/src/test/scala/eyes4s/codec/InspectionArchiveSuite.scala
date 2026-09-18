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

import eyes4s.compare.Similarity
import eyes4s.core.{Recording, RecordingRef}
import eyes4s.design.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.{ACursor, Json}

/** Result inspection over pinned and decoded archives: the frozen
  * study-result-v1 drills down through its reductions and pairs to the
  * logical records of the matched-control CSV; the pinned admission ledger
  * names every rejected record; a decoded archive inspects exactly as the
  * result it was written from.
  */
class InspectionArchiveSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private val results = StudyResultCodecs.cosine[Px]
  private val inputs  = StudyInputCodecs.study[Px]
  private val plans   = StudyCodecs.cosine[Px]

  private val pinnedResult = get(results.codec.parse(StudyResultFixtures.resultVersionOne))
  private val pinnedInput  = get(inputs.input.parse(StudyInputFixtures.inputVersionOne))
  private val pinnedPlan   = get(plans.codec.parse(SavedStudyFixtures.versionOne))

  private def key(p: String, s: String, phase: String) = StudyKey(p, s, phase)

  /** The pinned admission-ledger-complete-v1: every matched-control record
    * admitted, the header being record 1 and fixture row r record r + 2.
    */
  private val complete = ManifestFixtures.ledger

  /** The CSV fields of one logical record (the header is record 1). */
  private def fields(record: Int): Vector[String] = StudyInputFixtures.records(record - 2)

  test("the pinned study-result-v1 drills down to specific CSV record numbers") {
    assertEquals(complete.outcome, AdmissionOutcome.Complete)
    assertEquals(
      get(StoredArtifact.ledger("ledger", inputs, complete)).entry.sha256.hex,
      ManifestV1Fixtures.completeLedgerSha256
    )
    val inspection =
      get(ResultInspection.study(pinnedPlan, pinnedResult, pinnedInput, Some(complete)))
    val focal = key("s1", "a", "recall")
    val row   = get(inspection.contrastRow(ResultRef.ContrastRow(0, focal)))
    assertEquals(get(row.outcome.left.map(_.message)).value.value, 0.28)

    val matched = get(inspection.reduction(get(row.matched.toRight("matched"))))
    assertEquals(
      matched.members.map(m => m.pair -> m.status),
      Vector(
        ResultRef.PairRow(0, StudyDesign.Matched, focal, key("s1", "a", "encode")) ->
          Membership.Contributing
      )
    )
    val control = get(inspection.reduction(get(row.control.toRight("control"))))
    assertEquals(
      control.contributors,
      Vector(key("s1", "b", "encode"), key("s1", "c", "encode"))
    )
    assertEquals((control.selected, control.successful, control.contributing), (2, 2, 2))

    val pair = get(inspection.pair(matched.members.head.pair))
    assertEquals(get(inspection.trial(pair.focal)).records, Vector(26, 27, 28, 29))
    assertEquals(get(inspection.trial(pair.reference)).records, Vector(2, 3, 4, 5))
    assertEquals(
      control.members.map(m =>
        get(inspection.trial(get(inspection.pair(m.pair)).reference)).records
      ),
      Vector(Vector(6, 7, 8, 9), Vector(10, 11, 12, 13))
    )

    // Each record is the CSV line that supplied that fixation: its key
    // fields, ordinal and coordinates are the fixation's own.
    val fixation = get(inspection.sources.fixation(focal, 2))
    assertEquals(fixation, FixationSource(focal, 2, 28, 2))
    assertEquals(fields(28).take(4), Vector("s1", "a", "recall", "2"))
    val scanpath = pinnedInput.trials.rows.find(_.key == focal).get.value
    assertEquals(fields(28)(4).toDouble, scanpath.fixations(2).centre.x)
    assertEquals(fields(28)(5).toDouble, scanpath.fixations(2).centre.y)
    assertEquals(inspection.sources.trialOf(28), Some(focal))
    assertEquals(inspection.failures, Vector.empty)
  }

  test("the pinned admission ledger names every rejected record, key and typed reason") {
    val ledger   = get(inputs.ledger.parse(StudyInputFixtures.ledgerVersionOne))
    val dropped  = key("s1", "a", "encode")
    val accepted = StudyInput(
      eyes4s.design.Trials(
        StudyInputFixtures.matchedControl.trials.rows.filterNot(_.key == dropped)
      )
    )
    val sources = get(StudySources.of(accepted, ledger))
    assertEquals(
      sources.rejections.map(d => (d.code.render, d.subject.take(2), d.sources)),
      Vector(
        (
          "admission-reason.time",
          Vector(Locus.Trial(dropped), Locus.Record(2)),
          Vector(SourceLink.Record(ledger.source, 2))
        )
      ) ++ Vector(3, 4, 5).map(n =>
        (
          "admission-reason.quarantined",
          Vector(Locus.Trial(dropped), Locus.Record(n)),
          Vector(SourceLink.Record(ledger.source, n))
        )
      )
    )
    assertEquals(
      sources.rejections.drop(1).map(_.causes.map(_.code.render)),
      Vector.fill(3)(Vector("quarantine.rejected-records"))
    )
    assertEquals(get(sources.trial(dropped)).rejected.map(_.record), Vector(2, 3, 4, 5))

    // Running on the accepted trials, the focal trial that lost its matched
    // reference fails its reduction; the failure names the trial and its lines.
    val plan = get(
      StudyPlan.cosine(
        accepted.reference,
        pinnedPlan.grid,
        pinnedPlan.focalPhase,
        pinnedPlan.referencePhase,
        pinnedPlan.weight,
        pinnedPlan.estimates,
        pinnedPlan.policy
      )
    )
    val result     = get(plan.run(accepted))
    val inspection = get(ResultInspection.study(plan, result, accepted, Some(ledger)))
    val focal      = key("s1", "a", "recall")
    val reduced = get(inspection.reduction(ResultRef.Reduction(0, StudyDesign.Matched, focal)))
    val failure = get(reduced.outcome.swap.left.map(_ => "expected a failure"))
    assertEquals(failure.code.render, "reduction.no-selected-scores")
    assertEquals(
      failure.subject,
      Vector(Locus.Scale(0), Locus.Design(StudyDesign.Matched), Locus.Trial(focal))
    )
    assertEquals(
      failure.sources,
      Vector(26, 27, 28, 29).map(SourceLink.Record(ledger.source, _))
    )
    assert(inspection.failures.contains(failure))
  }

  test("the refused ledger, checked against the full input, names the trial it did not admit") {
    val refused = get(inputs.ledger.parse(StudyInputFixtures.ledgerVersionOne))
    val dropped = key("s1", "a", "encode")
    val refusal =
      get(StudySources.of(pinnedInput, refused).swap.left.map(_ => "expected a refusal"))
    assertEquals(refusal.error, AdmissionError.UnadmittedTrial(0))
    val diagnostic = Diagnostics.ledgerRefusal(refusal)
    assertEquals(diagnostic.code.render, "admission.unadmitted-trial")
    assertEquals(diagnostic.subject, Vector(Locus.Trial(dropped)))
    assertEquals(diagnostic.keys, Vector(dropped))
    assertEquals(
      diagnostic.sources,
      Vector(2, 3, 4, 5).map(SourceLink.Record(refused.source, _))
    )
    val inspection =
      ResultInspection.study(pinnedPlan, pinnedResult, pinnedInput, Some(refused))
    assertEquals(
      inspection.left.map(e => Diagnostic.of(e).subject),
      Left(Vector(Locus.Trial(dropped)))
    )
  }

  test("an inspection of a decoded archive equals the inspection of the original result") {
    val ledger   = get(inputs.ledger.parse(StudyInputFixtures.ledgerVersionOne))
    val accepted = StudyInput(
      eyes4s.design.Trials(
        StudyInputFixtures.matchedControl.trials.rows
          .filterNot(_.key == key("s1", "a", "encode"))
      )
    )
    val cases = Vector(
      (pinnedPlan, pinnedInput, complete),
      (
        get(
          StudyPlan.cosine(
            accepted.reference,
            pinnedPlan.grid,
            pinnedPlan.focalPhase,
            pinnedPlan.referencePhase,
            pinnedPlan.weight,
            pinnedPlan.estimates,
            get(FailurePolicy.successfulOnly(1))
          )
        ),
        accepted,
        ledger
      )
    )
    cases.foreach { case (plan, input, evidence) =>
      val original = get(plan.run(input))
      val decoded  = get(results.codec.decode(get(results.codec.encode(original))))
      val left     = get(ResultInspection.study(plan, original, input, Some(evidence)))
      val right    = get(ResultInspection.study(plan, decoded, input, Some(evidence)))
      assertEquals(summary(right), summary(left))
      assertEquals(right.failures, left.failures)
    }
    val pinned =
      get(ResultInspection.study(pinnedPlan, pinnedResult, pinnedInput, Some(complete)))
    val rerun = get(pinnedPlan.run(pinnedInput))
    val fresh = get(ResultInspection.study(pinnedPlan, rerun, pinnedInput, Some(complete)))
    assertEquals(summary(pinned), summary(fresh))
  }

  test("source-supported fixations link to the recording samples that support them") {
    val decoded = get(inputs.input.parse(InputPayloadFixtures.sourceSupportedVersionOne))
    val sources = StudySources.unledgered(decoded)
    val trial   = key("s1", "a", "encode")
    val digest = ArtifactRef.of[Recording[Px]](InputPayloadFixtures.sourceRecording.contentHash)
    val ref    = RecordingRef("synthetic-right-headfixed-1000")
    assertEquals(
      sources.fixationLinks(trial, 1),
      Vector(
        SourceLink.Fixation(trial, 1),
        SourceLink.Missing(MissingSource.NoLedger),
        SourceLink.Samples(ref, digest, 5, 10)
      )
    )
    assertEquals(sources.samples(trial, 0), Right(SourceLink.Samples(ref, digest, 0, 5)))
    assertEquals(sources.samples(trial, 2), Left(MissingSource.FixationOutOfRange(trial, 2, 2)))
    assertEquals(
      get(sources.trial(trial)).support.map(_.ranges.map(r => r.from -> r.until)),
      Some(Vector(0 -> 5, 5 -> 10))
    )
  }

  test("a malformed archive is refused with a coded diagnostic naming the key") {
    val json                                       = get(results.codec.encode(pinnedResult))
    def edit(path: String*)(f: Json => Json): Json =
      def go(cursor: ACursor, rest: List[String]): ACursor = rest match
        case Nil          => cursor.withFocus(f)
        case head :: tail =>
          head.toIntOption match
            case Some(index) => go(cursor.downN(index), tail)
            case None        => go(cursor.downField(head), tail)
      go(json.hcursor, path.toList).top.getOrElse(fail(s"no JSON at $path"))
    val tampered =
      edit("value", "scales", "0", "analyses", "matched", "entries", "0", "contributing")(_ =>
        Json.fromInt(5)
      )
    val diagnostic = results.codec.decode(tampered) match
      case Left(CodecError.Entry(_, CodecError.Reconstruction(error))) =>
        Diagnostics.reconstruction(error): Diagnostic[Any]
      case other => fail(s"unexpected $other")
    assertEquals(diagnostic.code.render, "reconstruction.denominator")
    assertEquals(diagnostic.subject, Vector(Locus.Trial(key("s1", "a", "recall"))))
    assertEquals(diagnostic.operand("contributing"), Some(Operand.Integer(BigInt(5))))
  }

  private type Inspection = StudyInspection[StudyKey, Px, Similarity, SignedDifference]

  /** Every entry of every listing, reached through reference-keyed pages. */
  private def summary(inspection: Inspection): Vector[Any] =
    val size                                               = get(PageSize.of(3))
    def pages[A](listing: Listing[StudyKey, A]): Vector[A] =
      Iterator
        .iterate(Option(listing.first(size)))(
          _.flatMap(_.next).map(r => get(listing.page(r, size)))
        )
        .takeWhile(_.isDefined)
        .flatten
        .flatMap(_.entries)
        .toVector
    inspection.scales.flatMap { scale =>
      Vector(scale.index, scale.estimate, scale.excludedPhases) ++
        pages(scale.estimation) ++
        StudyDesign.values.toVector.flatMap(d =>
          Vector(scale.pairing(d), scale.report(d)) ++
            pages(scale.pairs(d)) ++ pages(scale.reductions(d))
        ) ++
        (scale.contrast match
          case ScaleContrast.Rows(rows) => pages(rows)
          case ScaleContrast.Failed(d)  => Vector(d))
    }
