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
    assertEquals(requests.size, 15)
    assertEquals(responses.map(_.ordinal), responses.indices.toVector)
    assertEquals(responses.size, 13)
    assertEquals(errors.map(_.ordinal), errors.indices.toVector)
    assertEquals(errors.size, 9)
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
    assert(errors(2).message.contains("run 9") && errors(2).message.contains("run 7"))
    assertEquals(
      errors(4).diagnostic.subject,
      Vector(DiagnosticLocus.Dataset(DatasetRevision(2)))
    )
    assertEquals(errors(4).message, "The backend holds no data for dataset r2.")
    assert(!errors(6).message.contains("PairRow"), errors(6).message)
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
