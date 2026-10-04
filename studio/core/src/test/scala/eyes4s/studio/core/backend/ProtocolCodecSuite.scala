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

package eyes4s.studio.core.backend

import eyes4s.plan.{SegmentTotal, StudyDesign, StudySegment}
import eyes4s.studio.core.preview.{
  PreviewCandidates,
  PreviewCounts,
  PreviewError,
  PreviewId,
  QueryCount
}
import io.circe.Json
import io.circe.syntax.*

/** The wire contract: every message kind round-trips on this platform and
  * encodes exactly as pinned, so a rename fails here; validated values are
  * validated again when decoded; each value is shaped like the eyes4s value
  * it wraps.
  */
class ProtocolCodecSuite extends munit.FunSuite:
  import ProtocolSamples.*

  test("every message kind and case is sampled") {
    assertEquals(requests.map(_.ordinal), requests.indices.toVector)
    assertEquals(requests.size, 22)
    assertEquals(responses.map(_.ordinal), responses.indices.toVector)
    assertEquals(responses.size, 18)
    assertEquals(errors.map(_.ordinal), errors.indices.toVector)
    assertEquals(errors.size, 19)
    assertEquals(causes.map(_.ordinal), causes.indices.toVector)
    assertEquals(causes.size, 14)
    assertEquals(loci.map(_.ordinal), loci.indices.toVector)
    assertEquals(loci.size, 31)
    assertEquals(runStates.map(_.ordinal), runStates.indices.toVector)
    assertEquals(queryStatuses.map(_.ordinal), queryStatuses.indices.toVector)
    assertEquals(inspections.map(_.ordinal), inspections.indices.toVector)
    assertEquals(all.map(_.name).distinct.size, all.size)
  }

  test("every sample round-trips") {
    val failures = all.flatMap(s => s.roundTrips.left.toOption)
    assertEquals(failures, Vector.empty)
  }

  test("every sample encodes exactly as pinned") {
    val actual = all.map(s => s.name -> s.json.noSpaces).toMap
    val drift  =
      actual.toVector.sortBy(_._1).filter((n, j) => !ProtocolPins.pins.get(n).contains(j))
    // A deliberate protocol change updates ProtocolPins from these lines.
    drift.foreach((n, j) => println(s"PIN\t$n\t$j"))
    assertEquals(drift.map(_._1), Vector.empty)
    assertEquals(ProtocolPins.pins.keySet, actual.keySet)
  }

  test("preview handles keep unsafe Long values as decimal text") {
    val id = PreviewId(9007199254740992L)
    assertEquals(id.asJson.noSpaces, "\"9007199254740992\"")
    assertEquals(id.asJson.as[PreviewId], Right(id))
  }

  test("an unknown quarantine code decodes as Other and re-encodes unchanged") {
    val wire = Json.obj(
      "code"    -> "quarantine.from-the-future".asJson,
      "message" -> "a newer cause".asJson,
      "extra"   -> 1.asJson
    )
    val cause = wire.as[QuarantineCause]
    assertEquals(
      cause,
      Right(QuarantineCause.Other("quarantine.from-the-future", "a newer cause"))
    )
    assertEquals(cause.map(_.code), Right("quarantine.from-the-future"))
    assert(Json.obj("message" -> "no code".asJson).as[QuarantineCause].isLeft)
  }

  test("every eyes4s quarantine cause code is mirrored") {
    val core = eyes4s.plan.DiagnosticCatalog.quarantine.codes.map(_.render)
    assertEquals(causes.filterNot(_.isInstanceOf[QuarantineCause.Other]).map(_.code), core)
  }

  test("validated values are validated again when decoded") {
    val beyond = progress.meter.asJson.deepMerge(Json.obj("done" -> Json.fromLong(8513L)))
    assert(beyond.as[StageMeter].isLeft, beyond)
    val pairs =
      progress.totals.asJson.deepMerge(Json.obj("completedPairs" -> Json.fromLong(44846L)))
    assert(pairs.as[RunTotals].isLeft, pairs)
    val mismatch =
      progress.asJson.deepMerge(Json.obj("segment" -> (Segment.Estimating(0): Segment).asJson))
    assert(mismatch.as[JobProgress].isLeft, mismatch)
    val negative = progress.asJson.deepMerge(Json.obj("step" -> Json.fromLong(-1L)))
    assert(negative.as[JobProgress].isLeft, negative)
    assert(
      Json.obj("offset" -> Json.fromInt(0), "size" -> Json.fromInt(0)).as[PageRequest].isLeft
    )
    assertEquals(PageRequest.of(0, 4097), Left(PageError.SizeOutOfRange(4097, 4096)))
    assertEquals(PageRequest.of(-1, 1), Left(PageError.NegativeOffset(-1)))
    assertEquals(
      StageMeter.of(StageKind.Reducing, CountUnit.Keys, 5L, ProgressTotal.AtMost(4L)),
      Left(ProgressError.BeyondTotal("meter", 5L, ProgressTotal.AtMost(4L)))
    )
    assert(
      StageMeter.of(StageKind.Reducing, CountUnit.Keys, 1000000L, ProgressTotal.Unknown).isRight
    )
  }

  test("refusals have distinct stable codes, typed subjects and rendered messages") {
    assertEquals(errors.map(_.code).distinct.size, errors.size)
    errors.foreach(e => assert(e.code.startsWith("studio-backend."), e.code))
    val unknownRun = errors
      .collectFirst { case e: BackendError.UnknownRun => e }
      .getOrElse(fail("unknown run sample missing"))
    val unavailable = errors
      .collectFirst { case e: BackendError.Unavailable => e }
      .getOrElse(fail("unavailable sample missing"))
    val unknownReference = errors
      .collectFirst { case e: BackendError.UnknownReference => e }
      .getOrElse(fail("unknown reference sample missing"))
    assert(unknownRun.message.contains("run 9") && unknownRun.message.contains("run 7"))
    assertEquals(
      unavailable.diagnostic.subject,
      Vector(DiagnosticLocus.Dataset(DatasetRevision(2)))
    )
    assertEquals(unavailable.message, "The backend holds no data for dataset r2.")
    assert(!unknownReference.message.contains("PairRow"), unknownReference.message)
    // Protocol 1.6: a trial outside the revision's dataset names both.
    val unknownTrial = errors
      .collectFirst { case e: BackendError.UnknownTrial => e }
      .getOrElse(fail("unknown trial sample missing"))
    assertEquals(unknownTrial.code, "studio-backend.unknown-trial")
    assertEquals(unknownTrial.message, "P99 · enc_01 is not a trial of dataset r3.")
    assertEquals(
      unknownTrial.diagnostic.subject,
      Vector(
        DiagnosticLocus.Dataset(DatasetRevision(3)),
        DiagnosticLocus.Trial(TrialKey("P99", Phase.Encoding, "enc_01", 1))
      )
    )
  }

  test(
    "1.8: a diagnostic without its affected trials, class and remedy is refused on the wire"
  ) {
    val full  = ProtocolSamples.diagnostic.asJson
    val older = full.mapObject(_.remove("affected").remove("category").remove("remedy"))
    assert(full.as[StudioDiagnostic].isRight)
    older.as[StudioDiagnostic] match
      case Left(e)  => assert(e.history.toString.contains("affected"), e)
      case Right(d) => fail(s"a pre-1.8 diagnostic decoded: $d")
  }

  test("protocol values are shaped like the eyes4s values they wrap") {
    assertEquals(ProgressTotal.of(SegmentTotal.Exact(3L)), ProgressTotal.Exact(3L))
    assertEquals(ProgressTotal.of(SegmentTotal.AtMost(4L)), ProgressTotal.AtMost(4L))
    assertEquals(ProgressTotal.of(SegmentTotal.Unknown), ProgressTotal.Unknown)
    assertEquals(ProgressTotal.of(SegmentTotal.Counting), ProgressTotal.Counting)
    assertEquals(Segment.of(StudySegment.Estimating(0)), Segment.Estimating(0))
    assertEquals(
      Segment.of(StudySegment.Comparing(1, StudyDesign.Control)),
      Segment.Comparing(1, PairDesign.Control)
    )
    assertEquals(
      Segment.of(StudySegment.Reducing(1, StudyDesign.Matched)),
      Segment.Reducing(1, PairDesign.Matched)
    )
    assertEquals(Segment.of(StudySegment.Contrasting(2)), Segment.Contrasting(2))
    val keys = Map("q" -> query, "r" -> matched)
    assertEquals(
      ResultAddress.of(eyes4s.plan.ResultRef.PairRow(2, StudyDesign.Control, "q", "r"), keys),
      Some(address)
    )
    assertEquals(ResultAddress.of(eyes4s.plan.ResultRef.Event(0, 1), keys), None)
    val planDiagnostic = eyes4s.plan.Diagnostic.of(eyes4s.plan.PlanError.EmptyScales(0))
    val studio         = StudioDiagnostic.of(planDiagnostic, (k: Nothing) => k)
    assertEquals(studio.code, "plan.empty-scales")
    assertEquals(
      (studio.level, studio.origin),
      (DiagnosticLevel.Error, DiagnosticOrigin.EyesCore)
    )
    assertEquals(studio.message, planDiagnostic.message)
    assertEquals((studio.affected, studio.category, studio.remedy), (Vector.empty, None, None))
    // 1.8: a finding's affected trials, class and remedy are eyes4s's.
    val p11  = TrialKey("P11", Phase.Retrieval, "ret_05", 1)
    val refs = Vector(1, 2).map(o => TrialKey("P11", Phase.Encoding, "enc_04", o))
    val cardinality: eyes4s.plan.StudyFinding[TrialKey, eyes4s.kernel.Unit2D.Px] =
      eyes4s.plan.StudyFinding
        .MatchedCardinality(p11, refs, eyes4s.plan.MatchedReferences.RequireOne)
    val finding = eyes4s.plan.Diagnostic.of(cardinality)
    val wired   = StudioDiagnostic.of(finding, identity[TrialKey])
    assertEquals(wired.affected, p11 +: refs)
    assertEquals(wired.remedy, Some("ChooseMatchedReference"))
    assertEquals(wired.category, finding.category.map(_.toString))
    assert(wired.category.isDefined, wired)
    assertEquals(
      DiagnosticLocus.of(eyes4s.plan.Locus.Pair("q", "r"), keys),
      DiagnosticLocus.Pair(query, matched)
    )
    assertEquals(
      DiagnosticLocus.of(eyes4s.plan.Locus.Samples(1, 3), keys),
      DiagnosticLocus.Samples(1, 3)
    )
    assertEquals(
      QuarantineCause.of(
        eyes4s.plan.QuarantineCause.NotInInventory("P01", "Encoding", "enc_21", 1)
      ),
      QuarantineCause.NotInInventory("P01", "Encoding", "enc_21", 1)
    )
    assertEquals(
      TrialDisposition.of(eyes4s.plan.TrialDisposition.NoFixations),
      TrialDisposition.NoFixations
    )
  }

  test(
    "Counting is unbounded but still refuses negative progress and remains distinct from Unknown"
  ) {
    assertEquals(ProgressTotal.Counting.bound, None)
    val meter = StageMeter
      .of(StageKind.Comparing, CountUnit.Pairs, 1000000L, ProgressTotal.Counting)
      .fold(e => fail(e.message), identity)
    assertEquals(meter.asJson.as[StageMeter], Right(meter))
    assertEquals(
      StageMeter.of(StageKind.Comparing, CountUnit.Pairs, -1L, ProgressTotal.Counting),
      Left(ProgressError.Negative("meter", -1L))
    )
    assert(RunTotals.of(1L, ProgressTotal.Counting, 2L, ProgressTotal.Counting).isRight)
    assertNotEquals(
      (ProgressTotal.Counting: ProgressTotal).asJson,
      (ProgressTotal.Unknown: ProgressTotal).asJson
    )
    val invalid = progress.meter.asJson.deepMerge(
      Json.obj(
        "done"  -> Json.fromLong(-1L),
        "total" -> (ProgressTotal.Counting: ProgressTotal).asJson
      )
    )
    assert(invalid.as[StageMeter].isLeft)
  }

  // Frozen protocol 1.0 and 1.1 total cases. This is the legacy decoder reached inside
  // an event body, not the current ProgressTotal decoder under another version.
  private enum Protocol11Total derives CanEqual, io.circe.Decoder:
    case Exact(units: Long)
    case AtMost(units: Long)
    case Unknown

  private def legacyMeterTotal(wire: Json): io.circe.Decoder.Result[Protocol11Total] =
    wire.hcursor
      .downField("body")
      .downField("Event")
      .downField("event")
      .downField("Advanced")
      .downField("progress")
      .downField("meter")
      .downField("total")
      .as[Protocol11Total]

  test(
    "1.9: a trial-failed placement carries its window tally, and a broken tally is refused"
  ) {
    val failed = trialFixations.fixations.map(_.placement).collect {
      case p @ eyes4s.plan.MapPlacement.TrialFailed(_) => p
    }
    assertEquals(failed.size, 1)
    val wire = TrialViewCodecs.placement(failed.head)
    assertEquals(wire.as(using TrialViewCodecs.placementDecoder), Right(failed.head))
    // A total above the safe JSON integer range travels as a decimal string.
    assertEquals(
      wire.hcursor.downField("TrialFailed").downField("tally").get[String]("totalMicros"),
      Right("9007199254740993")
    )
    // In the window keeps its earlier wire name.
    assertEquals(
      TrialViewCodecs.placement(eyes4s.plan.MapPlacement.InWindow),
      Json.obj("InMap" -> Json.obj())
    )
    val broken = wire.hcursor
      .downField("TrialFailed")
      .downField("tally")
      .downField("outsideWindow")
      .withFocus(_ => Json.fromInt(99))
      .top
      .getOrElse(fail("wire"))
    assert(broken.as(using TrialViewCodecs.placementDecoder).isLeft, broken.noSpaces)
  }

  test("protocol 1.2 Counting requires coordinated peers, not a relabelled 1.1 frame") {
    assertEquals(ProtocolVersion.Current, ProtocolVersion(1, 9))
    val previous = Envelope(RequestId(41), ServerFrame.Event(JobEvent.Advanced(progress)))
    assertEquals(legacyMeterTotal(previous.asJson), Right(Protocol11Total.Exact(8512L)))
    val counting = progress.asJson
      .deepMerge(
        Json.obj("meter" -> Json.obj("total" -> (ProgressTotal.Counting: ProgressTotal).asJson))
      )
      .as[JobProgress]
      .fold(e => fail(e.message), identity)
    val envelope = Envelope(RequestId(41), ServerFrame.Event(JobEvent.Advanced(counting)))
    val wire     = envelope.asJson
    assertEquals(WireFormat.parse[ServerFrame](wire.noSpaces), Right(envelope))
    assert(legacyMeterTotal(wire).isLeft)
    val relabelled = wire.deepMerge(Json.obj("version" -> ProtocolVersion(1, 1).asJson))
    assert(legacyMeterTotal(relabelled).isLeft)
  }

  test(
    "protocol 1.5: preview query counts are required, non-negative, and by design is optional"
  ) {
    val candidates = ProtocolSamples.previewReady.candidates
    val wire       = candidates.asJson
    assertEquals(wire.as[PreviewCandidates], Right(candidates))
    // A recipe without a by-design category says so: null, not zero.
    assertEquals(wire.hcursor.downField("byDesignQueries").focus, Some(Json.Null))
    // A 1.4 preview (no query counts) is not read as 1.5.
    val legacy = wire.mapObject(
      _.remove("requestedQueries").remove("queriesNotAdmitted").remove("byDesignQueries")
    )
    assert(legacy.as[PreviewCandidates].isLeft)
    val counts = ProtocolSamples.previewReady.counts.asJson
    assert(counts.mapObject(_.remove("eligibleQueries")).as[PreviewCounts].isLeft)
    assert(counts.mapObject(_.add("eligibleQueries", (-1).asJson)).as[PreviewCounts].isLeft)
    assert(
      wire.mapObject(_.add("byDesignQueries", (-1).asJson)).as[PreviewCandidates].isLeft
    )
    assertEquals(
      QueryCount.of(-1).map(_.value),
      Left(PreviewError.Negative("query count", -1L))
    )
    assertEquals(QueryCount.of(14).map(_.asJson), Right(14.asJson))
  }

  test("protocol 1.3: an admission summary without an inventory says absent is not counted") {
    val summary = ProtocolSamples.admission.copy(inventory = InventoryJoin.Undeclared)
    val wire    = (summary: AdmissionSummary).asJson
    assertEquals(
      wire.hcursor.downField("inventory").focus,
      Some(Json.obj("Undeclared" -> Json.obj()))
    )
    assertEquals(wire.as[AdmissionSummary], Right(summary))
    assertEquals((summary.inventoryTrials, summary.absent), (None, None))
    // A 1.2 summary (inventoryTrials and absent as numbers) is not read as 1.3.
    val legacy = wire.mapObject(
      _.remove("inventory").add("inventoryTrials", 960.asJson).add("absent", 6.asJson)
    )
    assert(legacy.as[AdmissionSummary].isLeft)
  }
