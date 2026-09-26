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

import eyes4s.plan.{SegmentTotal, StudyDesign, StudyStage}
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/** Every protocol value survives a JSON round trip on this platform, the
  * validated ones are validated again when decoded, and each value is shaped
  * like the eyes4s value it wraps.
  */
class ProtocolCodecSuite extends munit.FunSuite:

  private def roundTrip[A: Encoder: Decoder](value: A)(using munit.Location): Unit =
    val text = value.asJson.noSpaces
    assertEquals(io.circe.parser.decode[A](text), Right(value), text)

  private def right[A](e: Either[?, A]): A = e.fold(l => throw new AssertionError(l), identity)

  private val query   = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  private val matched = TrialKey("P17", Phase.Encoding, "enc_03", 1)
  private val job     = JobId(1)
  private val run     = RunId(8)
  private val p       =
    right(JobProgress.of(job, run, 2L, JobStage.Comparing, 21400L, ProgressTotal.Exact(44845L)))
  private val page = right(PageRequest.of(10, 20))
  private val diag = StudioDiagnostic(
    "study-failure.estimation",
    DiagnosticLevel.Error,
    Vector("P05 · ret_04"),
    "empty map"
  )
  private val address = ResultAddress.PairRow(2, PairDesign.Control, query, matched)
  private val status  =
    JobStatus(job, run, AnalysisRevision(5), DatasetRevision(3), JobState.Running(p))

  private val outcomes = Vector(
    JobOutcome.Completed(job, run, p),
    JobOutcome.Cancelled(job, run, Some(p)),
    JobOutcome.Cancelled(job, run, None),
    JobOutcome.Failed(job, run, Vector(diag), None)
  )

  private val errors = Vector(
    BackendError
      .UnknownDataset(DatasetRevision(9), Vector(DatasetRevision(2), DatasetRevision(3))),
    BackendError.UnknownRevision(AnalysisRevision(9), Vector(AnalysisRevision(4))),
    BackendError.UnknownRun(RunId(9), Vector(RunId(7))),
    BackendError.UnknownJob(JobId(9), Vector.empty),
    BackendError.Unavailable("r2", "not in the fixture"),
    BackendError.NoResult(RunId(5), RunState.Stale),
    BackendError.UnknownReference(RunId(7), address),
    BackendError.AlreadyRunning(AnalysisRevision(5), job)
  )

  private val addresses = Vector(
    ResultAddress.Estimation(0, query),
    address,
    ResultAddress.Reduction(1, PairDesign.Matched, query),
    ResultAddress.ContrastRow(3, query)
  )

  private val info    = PageInfo(10, 480, Some(30))
  private val preview = PreviewSummary(
    AnalysisRevision(5),
    DatasetRevision(3),
    Vector("0.5°", "8°"),
    480,
    480,
    480,
    457,
    230400L,
    8969L,
    44845L
  )
  private val admission = AdmissionSummary(
    DatasetRevision(3),
    DatasetState.Admitted,
    960,
    937,
    QuarantineCause.values.toVector.map(QuarantineCount(_, 1)),
    6,
    11520,
    543,
    409,
    259,
    257,
    Vector(MissingImage("forest-044", Vector("P01", "P24"), 2)),
    "onset declared ms"
  )
  private val statuses = Vector(
    QueryStatus.Contributing(Vector(0.41), Vector(0.22), Vector(0.19)),
    QueryStatus.Failed("empty map"),
    QueryStatus.NoMatch("matched Encoding trial not admitted"),
    QueryStatus.NotAdmitted("absent")
  )
  private val result = ResultSummary(
    RunId(7),
    AnalysisRevision(4),
    DatasetRevision(3),
    Vector("2°"),
    8969L,
    35876L,
    457,
    QueryContrasts(480, 14, 9, 3, 454),
    0.26,
    Vector(0.26),
    GroupSummary(24, 0.3, Vector(0.3)),
    GroupSummary(24, 0.15, Vector(0.15)),
    24,
    2,
    17,
    Vector(
      ParticipantSummary(
        "P17",
        20,
        19,
        0,
        0,
        1,
        ScoreMeans(0.73, 0.35, 0.38, Vector(0.38)),
        GroupMeans(17, 0.74, 0.35, 0.38),
        GroupMeans(2, 0.64, 0.32, 0.32)
      )
    )
  )

  test("requests round-trip") {
    Vector(
      BackendRequest.Admission(DatasetRevision(3)),
      BackendRequest.Ledger(DatasetRevision(3), page),
      BackendRequest.Preview(AnalysisRevision(5)),
      BackendRequest.PreviewRows(AnalysisRevision(5), page),
      BackendRequest.Runs,
      BackendRequest.Submit(AnalysisRevision(5)),
      BackendRequest.Jobs,
      BackendRequest.Job(job),
      BackendRequest.Cancel(job),
      BackendRequest.Outcome(job),
      BackendRequest.Result(run),
      BackendRequest.Queries(run, page),
      BackendRequest.Inspect(run, address),
      BackendRequest.ProvenanceOf(run, address)
    ).foreach(roundTrip(_))
  }

  test("responses round-trip, refusals included") {
    val responses = Vector(
      BackendResponse.Admission(admission),
      BackendResponse.Ledger(
        LedgerPage(
          DatasetRevision(3),
          info,
          Vector(
            LedgerEntry(
              query,
              "beach-042",
              Some(Response.Remembered),
              TrialDisposition.Admitted
            ),
            LedgerEntry(matched, "beach-042", None, TrialDisposition.Absent),
            LedgerEntry(
              matched,
              "x",
              None,
              TrialDisposition.Quarantined(QuarantineCause.Overlap)
            )
          )
        )
      ),
      BackendResponse.Preview(preview),
      BackendResponse.PreviewRows(
        PreviewPage(
          AnalysisRevision(5),
          info,
          Vector(
            PreviewRow(
              query,
              "beach-042",
              Response.Remembered,
              matched,
              Some(19),
              Eligibility.Eligible
            ),
            PreviewRow(
              query,
              "b",
              Response.Forgotten,
              matched,
              None,
              Eligibility.QueryNotAdmitted("absent")
            ),
            PreviewRow(query, "b", Response.Forgotten, matched, None, Eligibility.NoMatch("no"))
          )
        )
      ),
      BackendResponse.Runs(
        Vector(
          RunState.Current,
          RunState.Stale,
          RunState.Running(job),
          RunState.Cancelled(Some(JobStage.Comparing)),
          RunState.Cancelled(None),
          RunState.Failed,
          RunState.Completed
        )
          .map(RunSummary(run, AnalysisRevision(5), DatasetRevision(3), _))
      ),
      BackendResponse.Job(status),
      BackendResponse.Jobs(
        Vector(JobState.Queued, JobState.Running(p)).map(s => status.copy(state = s)) ++
          outcomes.map(o => status.copy(state = JobState.Finished(o)))
      ),
      BackendResponse.Result(result),
      BackendResponse.Queries(
        QueryPage(
          run,
          info,
          statuses.map(QueryRow(query, "beach-042", Response.Remembered, matched, Some(19), _))
        )
      ),
      BackendResponse.ProvenanceOf(
        Provenance(
          address,
          Vector(
            ProvenanceStep.Run(run),
            ProvenanceStep.Analysis(AnalysisRevision(4)),
            ProvenanceStep.Dataset(DatasetRevision(3)),
            ProvenanceStep.Scale(2, "2°"),
            ProvenanceStep.Design(PairDesign.Control),
            ProvenanceStep.Trial(query, "beach-042")
          )
        )
      )
    ) ++ outcomes.map(BackendResponse.Outcome(_)) ++ errors.map(BackendResponse.Refused(_)) ++
      (addresses.map(a => Inspection.Unscored(a, statuses(1))) ++ Vector(
        Inspection.Contrast(address, 0.41, 0.22, 0.19),
        Inspection.Reduction(address, 0.22, 19),
        Inspection.Pair(address, "street-112", 0.61)
      )).map(BackendResponse.Inspected(_))
    responses.foreach(roundTrip(_))
  }

  test("progress events round-trip") {
    (JobEvent.Advanced(p) +: outcomes.map(JobEvent.Finished(_))).foreach(roundTrip(_))
    Vector(ProgressTotal.Exact(1L), ProgressTotal.AtMost(2L), ProgressTotal.Unknown)
      .foreach(roundTrip(_))
  }

  test("validated values are validated again when decoded") {
    val beyond = p.asJson.deepMerge(Json.obj("done" -> Json.fromLong(44846L)))
    assert(beyond.as[JobProgress].isLeft, beyond)
    val negative = p.asJson.deepMerge(Json.obj("step" -> Json.fromLong(-1L)))
    assert(negative.as[JobProgress].isLeft, negative)
    assert(
      Json.obj("offset" -> Json.fromInt(0), "size" -> Json.fromInt(0)).as[PageRequest].isLeft
    )
    assert(
      Json.obj("offset" -> Json.fromInt(0), "size" -> Json.fromInt(4097)).as[PageRequest].isLeft
    )
    assert(
      Json.obj("offset" -> Json.fromInt(-1), "size" -> Json.fromInt(1)).as[PageRequest].isLeft
    )
    assertEquals(
      JobProgress.of(job, run, 0L, JobStage.Reducing, 5L, ProgressTotal.AtMost(4L)),
      Left(ProgressError.BeyondTotal(job, JobStage.Reducing, 5L, ProgressTotal.AtMost(4L)))
    )
    assert(
      JobProgress.of(job, run, 0L, JobStage.Reducing, 1000000L, ProgressTotal.Unknown).isRight
    )
  }

  test("every refusal has a distinct stable code and names its operands") {
    // One sample per case.
    assertEquals(errors.map(_.ordinal), errors.indices.toVector)
    assertEquals(errors.map(_.code).distinct.size, errors.size)
    errors.foreach(e => assert(e.code.startsWith("studio-backend."), e.code))
    assert(errors(2).message.contains("run 9"), errors(2).message)
    assert(errors(2).message.contains("run 7"), errors(2).message)
  }

  test("protocol values are shaped like the eyes4s values they wrap") {
    assertEquals(ProgressTotal.of(SegmentTotal.Exact(3L)), ProgressTotal.Exact(3L))
    assertEquals(ProgressTotal.of(SegmentTotal.AtMost(4L)), ProgressTotal.AtMost(4L))
    assertEquals(ProgressTotal.of(SegmentTotal.Unknown), ProgressTotal.Unknown)
    assertEquals(JobStage.of(StudyStage.Estimating(0, 5)), JobStage.Estimating)
    assertEquals(JobStage.of(StudyStage.Comparing(1, StudyDesign.Control)), JobStage.Comparing)
    assertEquals(JobStage.of(StudyStage.Reducing(1, StudyDesign.Matched)), JobStage.Reducing)
    assertEquals(JobStage.of(StudyStage.Contrasting(2)), JobStage.Contrasting)
    assertEquals(PairDesign.of(StudyDesign.Matched), PairDesign.Matched)
    val ref  = eyes4s.plan.ResultRef.PairRow(2, StudyDesign.Control, "q", "r")
    val keys = Map("q" -> query, "r" -> matched)
    assertEquals(ResultAddress.of(ref, keys), Some(address))
    assertEquals(ResultAddress.of(eyes4s.plan.ResultRef.Event(0, 1), keys), None)
    val planDiagnostic = eyes4s.plan.Diagnostic.of(eyes4s.plan.PlanError.EmptyScales(0))
    val studio         = StudioDiagnostic.of(planDiagnostic, (k: Nothing) => k)
    assertEquals(studio.code, "plan.empty-scales")
    assertEquals(studio.level, DiagnosticLevel.Error)
    assertEquals(studio.message, planDiagnostic.message)
  }
