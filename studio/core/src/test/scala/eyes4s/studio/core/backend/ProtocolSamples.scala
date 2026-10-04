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

import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import eyes4s.studio.core.execution.RunStamp
import eyes4s.studio.core.preview.*
import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.document.{Source, SourcePath, SourceRole}
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, StudioRef}

/** One named protocol value: its JSON is pinned in [[ProtocolPins]]. */
final case class Sample[A](name: String, value: A)(using
    val encoder: Encoder[A],
    val decoder: Decoder[A]
):
  def json: Json = value.asJson(using encoder)

  def roundTrips: Either[String, Unit] =
    io.circe.parser
      .decode[A](json.noSpaces)(using decoder)
      .left
      .map(_.getMessage)
      .flatMap(back => Either.cond(back == value, (), s"$name decoded as $back"))

/** A sample of every message kind and of every case the messages carry. */
object ProtocolSamples:

  private def right[E, A](e: Either[E, A]): A =
    e.fold(l => throw new AssertionError(l), identity)

  val query: TrialKey   = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  val matched: TrialKey = TrialKey("P17", Phase.Encoding, "enc_03", 1)
  val job: JobId        = JobId(1)
  val run: RunId        = RunId(8)
  val page: PageRequest = right(PageRequest.of(10, 20))
  val info: PageInfo    = PageInfo(10, 480, Some(30))

  val progress: JobProgress = right(
    for
      meter <- StageMeter
        .of(StageKind.Comparing, CountUnit.Pairs, 3005L, ProgressTotal.Exact(8512L))
      totals <- RunTotals.of(
        2811L,
        ProgressTotal.Exact(4685L),
        21400L,
        ProgressTotal.Exact(44845L)
      )
      p <- JobProgress.of(job, run, 2L, Segment.Comparing(2, PairDesign.Control), meter, totals)
    yield p
  )

  val diagnostic: StudioDiagnostic = StudioDiagnostic(
    "study-failure.off-window",
    DiagnosticLevel.Error,
    DiagnosticOrigin.EyesCore,
    Vector(DiagnosticLocus.Trial(query)),
    "empty map",
    // Protocol 1.8: the trials it names, its finding class and remedy.
    Vector(query, TrialKey("P17", Phase.Encoding, "enc_03", 1)),
    Some("DataDependent"),
    Some("ReviewAnalysisWindow")
  )

  val previewBudget: PreviewBudget = right(PreviewBudget.of(24))
  val previewReady: PreviewReady   = right(
    PreviewReady.of(
      PreviewId(1L),
      PreviewStamp.fake(AnalysisRevision(5), DatasetRevision(3)),
      right(PreviewCandidates.of(466, 471, 24, 219486L, 480, 14, None)),
      right(PreviewCounts.of(8969L, 44845L, 457, 9, 0)),
      Vector(diagnostic)
    )
  )

  /** `r` with another id, stamp or counts, as a backend or a tamperer would send it. */
  def remade(r: PreviewReady)(
      id: PreviewId = r.id,
      stamp: RunStamp = r.stamp,
      counts: PreviewCounts = r.counts
  ): PreviewReady =
    right(PreviewReady.of(id, stamp, r.candidates, counts, r.diagnostics))

  val address: ResultAddress = ResultAddress.PairRow(2, PairDesign.Control, query, matched)

  val status: JobStatus =
    JobStatus(job, run, AnalysisRevision(5), DatasetRevision(3), JobState.Running(progress))

  val outcomes: Vector[JobOutcome] = Vector(
    JobOutcome.Completed(job, run, progress),
    JobOutcome.Cancelled(job, run, Some(progress)),
    JobOutcome.Failed(job, run, Vector(diagnostic), None)
  )

  val causes: Vector[QuarantineCause] = Vector(
    QuarantineCause.RejectedRecords,
    QuarantineCause.DuplicateOrdinals,
    QuarantineCause.NoFixations,
    QuarantineCause.Overlap(3, "[70.0ms, 150.0ms) on c", "[110.0ms, 300.0ms) on c"),
    QuarantineCause.WrongClock(1, "a", "b"),
    QuarantineCause.InvalidTransition(2, "negative"),
    QuarantineCause.InvalidExtent("empty"),
    QuarantineCause.UnmappableFixation(4, "screen", "image", -5.5, 2.5),
    QuarantineCause.CorrectionConflict(0, 1),
    QuarantineCause.ItemConflict(Vector("a", "b")),
    QuarantineCause.OccurrenceConflict(Vector(1, 2)),
    QuarantineCause.NotInInventory("P01", "Encoding", "enc_21", 1),
    QuarantineCause.InventoryItemConflict("beach-007", Vector("beach-008")),
    QuarantineCause.Other("quarantine.future-cause", "a cause from a newer backend")
  )

  val dispositions: Vector[TrialDisposition] =
    Vector(TrialDisposition.Admitted, TrialDisposition.NoFixations, TrialDisposition.Absent) ++
      causes.map(TrialDisposition.Quarantined(_))

  val loci: Vector[DiagnosticLocus] = Vector(
    DiagnosticLocus.Artifact("abc"),
    DiagnosticLocus.Definition("eyes4s.study", 1),
    DiagnosticLocus.Field("x"),
    DiagnosticLocus.Scale(2),
    DiagnosticLocus.Design(PairDesign.Matched),
    DiagnosticLocus.Repetition("r1"),
    DiagnosticLocus.Window("w"),
    DiagnosticLocus.Trial(query),
    DiagnosticLocus.Trials(Vector(query, matched)),
    DiagnosticLocus.TrialDigest("d1"),
    DiagnosticLocus.Pair(query, matched),
    DiagnosticLocus.Fixation(6),
    DiagnosticLocus.Record(7214),
    DiagnosticLocus.Records(Vector(1, 2)),
    DiagnosticLocus.InputTrial(3),
    DiagnosticLocus.Recording("rec"),
    DiagnosticLocus.Area("face"),
    DiagnosticLocus.Event(4),
    DiagnosticLocus.Sample(5),
    DiagnosticLocus.Samples(5, 9),
    DiagnosticLocus.Entry("result"),
    DiagnosticLocus.Path(".a"),
    DiagnosticLocus.Relation("input", "result"),
    DiagnosticLocus.Line("s.asc", 12L),
    DiagnosticLocus.Participant("P17"),
    DiagnosticLocus.Group(Vector(GroupLevel("response", "Remembered"))),
    DiagnosticLocus.Dataset(DatasetRevision(2)),
    DiagnosticLocus.Revision(AnalysisRevision(3)),
    DiagnosticLocus.Run(RunId(5)),
    DiagnosticLocus.Job(job),
    DiagnosticLocus.Address(address)
  )

  val errors: Vector[BackendError] = Vector(
    BackendError
      .UnknownDataset(DatasetRevision(9), Vector(DatasetRevision(2), DatasetRevision(3))),
    BackendError.UnknownRevision(AnalysisRevision(9), Vector(AnalysisRevision(4))),
    BackendError.UnknownRun(RunId(9), Vector(RunId(7))),
    BackendError.UnknownJob(JobId(9), Vector.empty),
    BackendError.UnknownPreview(PreviewId(9L), Vector(PreviewId(1L))),
    BackendError.PreviewNotReady(
      PreviewId(1L),
      right(ParticipantCount.of(3)),
      right(ParticipantCount.of(24))
    ),
    BackendError.StalePreview(
      PreviewId(1L),
      previewReady.stamp,
      previewReady.stamp.copy(dataset = DatasetRevision(4))
    ),
    BackendError.TamperedPreview(
      remade(previewReady)(counts = right(PreviewCounts.of(8969L, 44845L, 457, 9, 1))),
      previewReady
    ),
    BackendError.Unavailable(DiagnosticLocus.Dataset(DatasetRevision(2))),
    BackendError.NoResult(RunId(5), RunState.Stale),
    BackendError.UnknownReference(RunId(7), address),
    BackendError.AlreadyRunning(AnalysisRevision(5), job),
    BackendError.UnsupportedVersion(ProtocolVersion(2, 0), ProtocolVersion(1, 0)),
    BackendError.Malformed("{\"id\":3,\"body\":{\"Runz\":{}}}", "no such request"),
    BackendError.DuplicateSubscription(RequestId(41)),
    BackendError.InventoryRefused(
      DatasetRevision(4),
      Vector(
        InventoryIssue.Conflict(
          TrialLabel("P01", "Encoding", "enc_01"),
          Vector(2, 9),
          Vector("response")
        ),
        InventoryIssue.Width(5, 8, 7),
        InventoryIssue.Field(6, "occurrence", "x", "a positive integer occurrence"),
        InventoryIssue.Other(
          "DuplicateAttribute",
          "Attribute names [a] are declared more than once."
        )
      )
    ),
    BackendError.UnknownTrial(DatasetRevision(3), TrialKey("P99", Phase.Encoding, "enc_01", 1)),
    BackendError.TrialViewRefused(TrialViewError.TrialFails(query, Vector(4, 5))),
    BackendError.SourceRecordsRefused(
      AnalysisRevision(4),
      SourceRecordsError.RangeInvalid(0, 501, SourceRecordPage.Limit)
    )
  )

  val runStates: Vector[RunState] = Vector(
    RunState.Current,
    RunState.Stale,
    RunState.Running(job),
    RunState.Cancelled(Some(StageKind.Comparing)),
    RunState.Failed,
    RunState.Completed
  )

  val queryStatuses: Vector[QueryStatus] = Vector(
    QueryStatus.Contributing(Vector(0.41), Vector(0.22), Vector(0.19)),
    QueryStatus.Failed(diagnostic),
    QueryStatus.NoMatch(diagnostic.copy(code = "study-finding.unmatched-focal")),
    QueryStatus.NotAdmitted(TrialDisposition.Absent)
  )

  val admission: AdmissionSummary = AdmissionSummary(
    DatasetRevision(3),
    DatasetState.Admitted,
    InventoryJoin.Joined(960, 6),
    937,
    Vector(QuarantineCount("quarantine.overlap", 6)),
    5,
    11520,
    WindowTotals(543, 0, 11311, 409, 0, 937, 0, Some(11520), 159142000L, 0L, 3282108000L),
    259,
    257,
    Vector(MissingImage("forest-044", Vector("P01", "P24"), 2)),
    "onset declared ms"
  )

  val result: ResultSummary = ResultSummary(
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
    Vector(GroupSummary("response", Response.Remembered, 24, 0.3, Vector(0.3))),
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
        Vector(GroupMeans(Response.Forgotten, 2, 0.64, 0.32, 0.32))
      )
    )
  )

  val inspections: Vector[Inspection] = Vector(
    Inspection.Contrast(ResultAddress.ContrastRow(2, query), 0.73, 0.35, 0.38),
    Inspection.Reduction(ResultAddress.Reduction(2, PairDesign.Control, query), 0.35, 19),
    Inspection.Pair(address, "street-112", 0.61),
    Inspection.Unscored(ResultAddress.Estimation(0, query), queryStatuses(3))
  )

  private def fixation(
      position: Int,
      record: Int,
      x: Double,
      y: Double,
      onset: Double,
      duration: Double,
      placement: MapPlacement
  ): AdmittedFixation =
    val index = FixationIndex.of(position).toOption.get
    AdmittedFixation
      .of(StudioRef.Fixation(query, index), record, x, y, onset, duration, placement)
      .toOption
      .get

  /** Protocol 1.6: one fixation of each placement. */
  val trialFixations: TrialFixations = TrialFixations
    .of(
      AnalysisRevision(4),
      DatasetRevision(3),
      query,
      Vector(
        fixation(1, 7209, 960.5, 540.25, 0.5, 212.5, MapPlacement.InMap),
        fixation(
          2,
          7210,
          1500.5,
          540.75,
          230.5,
          180.25,
          MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)
        ),
        fixation(3, 7211, -4.5, 20.25, 420.5, 96.5, MapPlacement.OutsideScreen),
        fixation(4, 7212, 600.5, 400.5, 530.5, 140.5, MapPlacement.DroppedInitial),
        fixation(
          5,
          7213,
          610.5,
          410.5,
          680.5,
          160.5,
          MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial)
        )
      )
    )
    .toOption
    .get

  /** Protocol 1.6: a 3 × 2 preview, top row first, one cell without a value. */
  val trialPreview: TrialPreview = TrialPreview
    .of(
      AnalysisRevision(4),
      query,
      2.5,
      ScreenRegion.of(query, 448.5, 156.5, 1472.5, 924.5).toOption.get,
      3,
      2,
      RowOrder.TopFirst,
      Vector(Some(0.1), Some(0.25), None, Some(0.3), Some(0.2), Some(0.15)),
      Vector(0.2, 0.12)
    )
    .toOption
    .get

  /** Protocol 1.7: a placed record of an admitted scanpath, a record outside
    * the image no scanpath holds, and a record whose cells are not numbers.
    */
  val sourceRecordPage: SourceRecordPage =
    def point(n: Int, f: String, x: Double, y: Double) = PlanePoint.of(n, f, x, y).toOption.get
    def ref(n: Int, fixation: Option[Int]): StudioRef.SourceRecord = StudioRef.SourceRecord(
      query,
      fixation.map(FixationIndex.of(_).toOption.get),
      SourceRole.Fixations,
      RecordNumber.of(n).toOption.get
    )
    SourceRecordPage
      .of(
        AnalysisRevision(4),
        DatasetRevision(3),
        Source(
          SourceRole.Fixations,
          SourcePath.of("fixations.csv").toOption.get,
          ByteDigest.parse("ab" * 32).toOption.get,
          None
        ),
        35.5,
        ScaleSource.Recipe,
        11520,
        7214,
        3,
        Vector(
          SourceRecordRow
            .of(
              ref(7214, Some(6)),
              Some(6),
              Some(2160.5),
              Some(412.5),
              Some(206),
              Some(point(7214, "screen", 1148.5, 456.5)),
              Some(ImagePosition(point(7214, "image", 700.5, 300.5), true)),
              Some(point(7214, "degrees", 5.375, 2.385)),
              Some(MapPlacement.InMap),
              "P17,Retrieval,ret_07,1,6,1148.5,456.5,2160.5,412.5,206"
            )
            .toOption
            .get,
          SourceRecordRow
            .of(
              ref(7215, None),
              Some(7),
              Some(2650.5),
              Some(200.5),
              Some(0),
              Some(point(7215, "screen", 120.5, 80.5)),
              Some(ImagePosition(point(7215, "image", -327.5, -75.5), false)),
              Some(point(7215, "degrees", -24.25, 13.75)),
              None,
              "P17,Retrieval,ret_07,1,7,120.5,80.5,2650.5,200.5,0"
            )
            .toOption
            .get,
          SourceRecordRow
            .of(
              ref(7216, None),
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              "P17,Retrieval,ret_07,1,x,,,,,"
            )
            .toOption
            .get
        )
      )
      .toOption
      .get

  val requests: Vector[BackendRequest] = Vector(
    BackendRequest.Admission(DatasetRevision(3)),
    BackendRequest.Ledger(DatasetRevision(3), page),
    BackendRequest.Preview(AnalysisRevision(5)),
    BackendRequest.PreviewRows(AnalysisRevision(5), page),
    BackendRequest.PreviewCounting(AnalysisRevision(5), previewBudget),
    BackendRequest.ContinuePreview(PreviewId(1L), previewBudget),
    BackendRequest.SubmitPreview(previewReady),
    BackendRequest.Runs,
    BackendRequest.Submit(AnalysisRevision(5)),
    BackendRequest.Jobs,
    BackendRequest.Job(job),
    BackendRequest.Subscribe(job),
    BackendRequest.Cancel(job),
    BackendRequest.Outcome(job),
    BackendRequest.Result(run),
    BackendRequest.Queries(run, page),
    BackendRequest.Inspect(run, address),
    BackendRequest.ProvenanceOf(run, address),
    BackendRequest.Unsubscribe(RequestId(41)),
    BackendRequest.TrialFixationsOf(AnalysisRevision(4), query),
    BackendRequest.TrialPreviewOf(AnalysisRevision(4), query),
    BackendRequest.SourceRecordsOf(AnalysisRevision(4), 7214, 60)
  )

  val responses: Vector[BackendResponse] = Vector(
    BackendResponse.Refused(errors(2)),
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
            TrialDisposition.Admitted,
            Vector(OutsideFrame(12, -5.5, 2.5, "screen"))
          )
        )
      )
    ),
    BackendResponse.Preview(
      PreviewSummary(
        AnalysisRevision(5),
        DatasetRevision(3),
        Vector("8°"),
        466,
        471,
        480,
        457,
        219486L,
        8969L,
        44845L
      )
    ),
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
            Eligibility.QueryNotAdmitted(TrialDisposition.NoFixations)
          ),
          PreviewRow(
            query,
            "b",
            Response.Forgotten,
            matched,
            None,
            Eligibility.NoMatch(diagnostic)
          )
        )
      )
    ),
    BackendResponse.PreviewAccepted,
    BackendResponse.Runs(
      Vector(RunSummary(RunId(7), AnalysisRevision(4), DatasetRevision(3), RunState.Current))
    ),
    BackendResponse.Job(status),
    BackendResponse.Jobs(Vector(status.copy(state = JobState.Queued))),
    BackendResponse.Outcome(job, Some(outcomes(1))),
    BackendResponse.Result(result),
    BackendResponse.Queries(
      QueryPage(
        run,
        info,
        Vector(
          QueryRow(query, "beach-042", Response.Remembered, matched, Some(19), queryStatuses(0))
        )
      )
    ),
    BackendResponse.Inspected(inspections(0)),
    BackendResponse.ProvenanceOf(
      Provenance(
        address,
        Vector(
          ProvenanceStep.Run(RunId(7)),
          ProvenanceStep.Analysis(AnalysisRevision(4)),
          ProvenanceStep.Dataset(DatasetRevision(3)),
          ProvenanceStep.Scale(2, "2°"),
          ProvenanceStep.Design(PairDesign.Control),
          ProvenanceStep.Trial(query, "beach-042")
        )
      )
    ),
    BackendResponse.Unsubscribed(RequestId(41), true),
    BackendResponse.TrialFixationsOf(trialFixations),
    BackendResponse.TrialPreviewOf(trialPreview),
    BackendResponse.SourceRecordsOf(sourceRecordPage)
  )

  val events: Vector[JobEvent] =
    JobEvent.Advanced(progress) +: outcomes.map(JobEvent.Finished(_))

  val segments: Vector[Segment] = Vector(
    Segment.Estimating(0),
    Segment.Comparing(1, PairDesign.Matched),
    Segment.Reducing(2, PairDesign.Control),
    Segment.Contrasting(3)
  )

  private def named[A: Encoder: Decoder](kind: String, values: Vector[A])(label: A => String) =
    values.map(v => Sample(s"$kind.${label(v)}", v))

  private def prefix(p: Product): String = p.productPrefix

  /** Every sample, named `<type>.<case>`: the request and response kinds, the
    * server frames and envelopes, and each case of the enums they carry.
    */
  val all: Vector[Sample[?]] =
    named("request", requests)(prefix) ++
      named("response", responses)(prefix) ++
      named("event", events)(e =>
        e match
          case JobEvent.Advanced(_) => "Advanced"
          case JobEvent.Finished(o) => s"Finished.${o.productPrefix}"
      ) ++
      named(
        "preview-event",
        Vector[PreviewEvent](
          PreviewEvent.Initial(previewReady.id, previewReady.stamp, previewReady.candidates),
          PreviewEvent.Counting(previewReady.id, right(PreviewProgress.of(1, 24))),
          PreviewEvent.Ready(previewReady)
        )
      )(prefix) ++
      Vector(
        Sample("frame.Response", ServerFrame.Response(responses(0))),
        Sample("frame.Event", ServerFrame.Event(events(0))),
        Sample("frame.Preview", ServerFrame.Preview(PreviewEvent.Ready(previewReady))),
        Sample(
          "envelope.request",
          Envelope(RequestId(41), BackendRequest.Subscribe(job): BackendRequest)
        ),
        Sample("envelope.frame", Envelope(RequestId(41), ServerFrame.Event(events(1)))),
        Sample("state.Queued", JobState.Queued: JobState),
        Sample("state.Running", JobState.Running(progress): JobState),
        Sample("state.Finished", JobState.Finished(outcomes(0)): JobState),
        Sample("total.Exact", ProgressTotal.Exact(1L): ProgressTotal),
        Sample("total.AtMost", ProgressTotal.AtMost(2L): ProgressTotal),
        Sample("total.Unknown", ProgressTotal.Unknown: ProgressTotal),
        Sample("total.Counting", ProgressTotal.Counting: ProgressTotal)
      ) ++
      named("cause", causes)(prefix) ++
      named("disposition", dispositions.take(3))(prefix) ++
      named("locus", loci)(prefix) ++
      named("error", errors)(prefix) ++
      named("run-state", runStates)(prefix) ++
      named("query-status", queryStatuses)(prefix) ++
      named("inspection", inspections)(prefix) ++
      named("segment", segments)(prefix)
