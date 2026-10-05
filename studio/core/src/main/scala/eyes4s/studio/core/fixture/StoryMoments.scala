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

package eyes4s.studio.core.fixture

import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.plan.{AdmissionDecision as CoreAdmissionDecision, TrialKeyDefinitions}
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  JobId,
  Phase,
  RunId,
  StageKind
}
import eyes4s.studio.core.document.*

/** The documents of the three story moments of docs/studio/fixture/FIXTURE.md
  * (DESIGN_SPEC section 12), built through the document's own constructors.
  *
  *  - t1 Data · verify: dataset r3 is a draft re-import of r2; run 5 (rev 3,
  *    data r2) exists; no run on r3.
  *  - t2 Explore/Analysis/Compare/Figures: r3 admitted; rev 4 · run 7 current;
  *    draft rev 5 adds σ 8°, not run; Figure 1 = run 7, Figure 2 = run 5.
  *  - t3 Compare · summary: rev 5 saved and run 8 running; the view stays on
  *    run 7.
  *
  * Nothing here is bound to an eyes4s artifact: the fake backend has none, so
  * every [[CoreBinding]] is `Unbound` and no digest is invented. The source
  * byte digests are the real SHA-256 of fixtures/studio-golden.
  */
object StoryMoments:

  val r2: DatasetRevision    = DatasetRevision(2)
  val r3: DatasetRevision    = DatasetRevision(3)
  val rev3: AnalysisRevision = AnalysisRevision(3)
  val rev4: AnalysisRevision = AnalysisRevision(4)
  val rev5: AnalysisRevision = AnalysisRevision(5)
  val run5: RunId            = RunId(5)
  val run6: RunId            = RunId(6)
  val run7: RunId            = RunId(7)
  val run8: RunId            = RunId(8)
  val run8Job: JobId         = JobId(1)

  /** FIXTURE.md: σ 0.5°, 1°, 2°, 4°; rev 5 adds 8°. */
  val scalesRev4: Vector[Double] = Vector(0.5, 1.0, 2.0, 4.0)
  val scalesRev5: Vector[Double] = scalesRev4 :+ 8.0

  private def sources: Either[String, Sources] =
    for
      fixationsPath <- SourcePath.of("inputs/fixations.csv").leftMap(_.message)
      trialsPath    <- SourcePath.of("inputs/trials.csv").leftMap(_.message)
      fixations     <- ByteDigest.parse(GoldenInventory.fixationsSha256).leftMap(_.message)
      trials        <- ByteDigest.parse(GoldenInventory.trialsSha256).leftMap(_.message)
      sources       <- Sources
        .of(
          Vector(
            Source(SourceRole.Fixations, fixationsPath, fixations, None),
            Source(SourceRole.Trials, trialsPath, trials, None)
          )
        )
        .leftMap(_.message)
    yield sources

  /** The fixation source's columns. r2 left the occurrence column unmapped;
    * r3 maps it (FIXTURE.md: "Block → occurrence"; the golden file names the
    * column `occurrence`).
    */
  private def mapping(occurrence: Boolean): Either[DocumentError, ColumnMapping] =
    val columns = Vector(
      ColumnRole.Participant -> "participant",
      ColumnRole.Phase       -> "phase",
      ColumnRole.Trial       -> "trial",
      ColumnRole.Ordinal     -> "ordinal",
      ColumnRole.X           -> "x",
      ColumnRole.Y           -> "y",
      ColumnRole.Onset       -> "onset_ms",
      ColumnRole.Duration    -> "duration_ms",
      ColumnRole.SampleCount -> "sample_count"
    ) ++ Option.when(occurrence)(ColumnRole.Occurrence -> "occurrence")
    columns
      .traverse((role, name) => ColumnName.of(name).map(ColumnBinding(role, _)))
      .flatMap(ColumnMapping.of)

  /** trials.csv's columns (S5.4): the trial identity, the occurrence when the
    * fixation key reads it (r3), the item and response, and the display
    * columns as text attributes.
    */
  def inventory(occurrence: Boolean): Either[DocumentError, InventoryMapping] =
    val roles = Vector(
      ColumnRole.Participant -> "participant",
      ColumnRole.Phase       -> "phase",
      ColumnRole.Trial       -> "trial",
      ColumnRole.Item        -> "item",
      ColumnRole.Response    -> "response"
    ) ++ Option.when(occurrence)(ColumnRole.Occurrence -> "occurrence")
    val attributes = Option.unless(occurrence)("occurrence").toVector ++
      Vector("display_kind", "image_file")
    for
      bindings <- roles.traverse((role, name) =>
        ColumnName.of(name).map(ColumnBinding(role, _))
      )
      declared <- attributes
        .traverse(name =>
          ColumnName.of(name).map(AttributeBinding(_, AttributeKindChoice.Text))
        )
        .flatMap(DeclaredAttributes.of)
      mapping <- InventoryMapping.of(bindings, declared)
    yield mapping

  /** Screen 1920×1080; image 1024×768 centred; declared 35 px/°. */
  val geometry: Either[DocumentError, Geometry] =
    for
      screen <- ScreenSize.of(1920, 1080)
      image  <- ImagePlacement.of(448, 156, 1024, 768)
      ppd    <- DeclaredPixelsPerDegree.of(35.0)
      g      <- Geometry.of(screen, image, ppd)
    yield g

  /** An admitted revision was admitted reviewing its exclusions: the story's
    * ledger quarantines trials, which `RequireComplete` refuses.
    */
  private val admitted: AdmissionDecision =
    AdmissionDecision.Admitted(
      Some(CoreAdmissionDecision.ReviewExclusions),
      CoreBinding.unbound,
      CoreBinding.unbound
    )

  /** r2: onset units undeclared, occurrence unmapped. */
  private def datasetR2(sources: Sources): Either[DocumentError, DatasetRevisionSpec] =
    (mapping(occurrence = false), geometry, inventory(occurrence = false)).mapN((m, g, i) =>
      DatasetRevisionSpec(
        r2,
        None,
        sources,
        m,
        DeclaredUnits(None),
        g,
        AdmissionChoice.default,
        admitted,
        inventory = Some(i)
      )
    )

  /** r3: a re-import of r2 with onsets declared in ms and occurrence mapped. */
  private def datasetR3(
      sources: Sources,
      decision: AdmissionDecision
  ): Either[DocumentError, DatasetRevisionSpec] =
    (mapping(occurrence = true), geometry, inventory(occurrence = true)).mapN((m, g, i) =>
      DatasetRevisionSpec(
        r3,
        Some(r2),
        sources,
        m,
        DeclaredUnits(Some(TimeUnit.Milliseconds)),
        g,
        AdmissionChoice.default,
        decision,
        inventory = Some(i)
      )
    )

  /** The recipe of every story revision: the trial-keyed (trialLayout)
    * layout, cosine similarity, duration weighting, the image frame as the
    * analysis window (off-window fixations excluded), grid 64×48 and declared
    * 35 px/°. The input is unbound on the fake backend.
    */
  def recipe(scales: Vector[Double]): Either[DocumentError, Recipe] =
    for
      sigmas <- scales.traverse(Sigma.of)
      set    <- ScaleSet.of(sigmas)
      grid   <- GridSize.of(64, 48)
      layout = DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout)
      cosine <- DefinitionRef.of("eyes4s.cosine", 1)
      window <- AnalysisWindow.of(448.0, 156.0, 1472.0, 924.0)
      ppd    <- DeclaredPixelsPerDegree.of(35.0)
    yield Recipe(
      None,
      layout,
      MethodSpec(cosine, Vector.empty),
      PhasePair(Phase.Retrieval, Phase.Encoding),
      WeightChoice.Duration,
      FailureChoice.RequireAll,
      grid,
      Some(window),
      Some(OffWindowChoice.Exclude),
      set,
      Some(ppd),
      MatchedChoice.RequireOne,
      ControlChoice.SameSelection,
      UnmatchedChoice.ReportNoMatch,
      InitialFixationChoice.KeepAll
    )

  private def analysis(
      id: AnalysisRevision,
      dataset: DatasetRevision,
      scales: Vector[Double]
  ): Either[DocumentError, AnalysisRevisionSpec] =
    for
      r    <- recipe(scales)
      name <- RevisionName.of("Encoding → retrieval reinstatement")
    yield AnalysisRevisionSpec(
      id,
      dataset,
      CoreBinding.unbound,
      r,
      StudioFields(Preset.EncodingRetrieval, name, "")
    )

  private def run(
      id: RunId,
      analysis: AnalysisRevision,
      dataset: DatasetRevision,
      state: RunLifecycle
  ): RunRef = RunRef(id, analysis, dataset, state, CoreBinding.unbound)

  val run5Ref: RunRef = run(run5, rev3, r2, RunLifecycle.Completed)
  val run6Ref: RunRef =
    run(run6, rev4, r3, RunLifecycle.Cancelled(Some(StageKind.Comparing)))
  val run7Ref: RunRef = run(run7, rev4, r3, RunLifecycle.Completed)
  val run8Ref: RunRef = run(run8, rev5, r3, RunLifecycle.Running)

  val byResponseId: Either[DocumentError, ReportingId] = ReportingId.of("by-retrieval-response")

  /** "By retrieval response": grouped on the response attribute, no filter,
    * no minimum per group, participant means.
    */
  val byResponse: Either[DocumentError, ReportingSpec] =
    for
      id        <- byResponseId
      covariate <- Covariate.of("response")
      spec      <- ReportingSpec.grouped(id, "By retrieval response", Some(covariate))
    yield spec

  private def panel(
      letter: String,
      title: String,
      scale: PanelScale,
      selection: PanelSelection
  ): Either[DocumentError, PanelSpec] =
    PanelLetter.of(letter).map(PanelSpec(_, title, scale, selection))

  private val sigma2 = Sigma.of(2.0)

  /** Figure 1: run 7 + by retrieval response, panels A–E (Figures board). */
  val figure1: Either[DocumentError, FigureSpec] =
    for
      id  <- FigureId.of(1)
      rep <- byResponseId
      s2  <- sigma2
      ps  <- Vector(
        panel(
          "A",
          "Encoding gaze",
          PanelScale.Unscaled,
          PanelSelection.Trial(MockStudy.key("P17", "enc_03"))
        ),
        panel(
          "B",
          "Retrieval gaze",
          PanelScale.Unscaled,
          PanelSelection.Trial(MockStudy.key("P17", "ret_07"))
        ),
        panel(
          "C",
          "Density maps",
          PanelScale.At(s2),
          PanelSelection.QueryWithReferences(MockStudy.key("P17", "ret_07"))
        ),
        panel("D", "Participant D by response", PanelScale.At(s2), PanelSelection.AllQueries),
        panel("E", "Scale profile by response", PanelScale.AllScales, PanelSelection.AllQueries)
      ).sequence
      f <- FigureSpec.of(id, run7, rep, ps)
    yield f

  /** Figure 2: bound to run 5 (rev 3, data r2), stale since r3. The board
    * shows only its binding; its one panel is illustrative.
    */
  val figure2: Either[DocumentError, FigureSpec] =
    for
      id  <- FigureId.of(2)
      rep <- byResponseId
      s2  <- sigma2
      p <- panel("A", "Participant D by response", PanelScale.At(s2), PanelSelection.AllQueries)
      f <- FigureSpec.of(id, run5, rep, Vector(p))
    yield f

  private def presentation(perspective: Perspective, shown: RunId) =
    val d = PresentationState.default
    PresentationState.of(
      perspective,
      d.theme,
      d.stage,
      d.mapOpacity,
      d.underlay,
      Some(shown),
      d.layouts
    )

  /** t1 Data · verify. */
  def t1: Either[String, StudioDocument] =
    sources.flatMap { s =>
      (for
        d2   <- datasetR2(s)
        d3   <- datasetR3(s, AdmissionDecision.Pending)
        a3   <- analysis(rev3, r2, scalesRev4)
        rep  <- byResponse
        f2   <- figure2
        view <- presentation(Perspective.Data, run5)
        doc  <- StudioDocument.of(
          Vector(d2, d3),
          Vector(a3),
          None,
          Vector(run5Ref),
          Vector(rep),
          Vector(f2),
          view,
          Vector.empty
        )
      yield doc).leftMap(_.message)
    }

  /** t2 Explore / Analysis / Compare · query / Figures. */
  def t2: Either[String, StudioDocument] =
    sources.flatMap { s =>
      (for
        d2    <- datasetR2(s)
        d3    <- datasetR3(s, admitted)
        a3    <- analysis(rev3, r2, scalesRev4)
        a4    <- analysis(rev4, r3, scalesRev4)
        next  <- recipe(scalesRev5)
        draft <- Draft.between(rev5, a4, next)
        rep   <- byResponse
        f1    <- figure1
        f2    <- figure2
        view  <- presentation(Perspective.Compare, run7)
        doc   <- StudioDocument.of(
          Vector(d2, d3),
          Vector(a3, a4),
          Some(draft),
          Vector(run5Ref, run6Ref, run7Ref),
          Vector(rep),
          Vector(f1, f2),
          view,
          Vector.empty
        )
      yield doc).leftMap(_.message)
    }

  /** t3 Compare · summary: Save & run made rev 5; run 8 is running. */
  def t3: Either[String, StudioDocument] =
    sources.flatMap { s =>
      (for
        d2   <- datasetR2(s)
        d3   <- datasetR3(s, admitted)
        a3   <- analysis(rev3, r2, scalesRev4)
        a4   <- analysis(rev4, r3, scalesRev4)
        a5   <- analysis(rev5, r3, scalesRev5)
        rep  <- byResponse
        f1   <- figure1
        f2   <- figure2
        view <- presentation(Perspective.Compare, run7)
        doc  <- StudioDocument.of(
          Vector(d2, d3),
          Vector(a3, a4, a5),
          None,
          Vector(run5Ref, run6Ref, run7Ref, run8Ref),
          Vector(rep),
          Vector(f1, f2),
          view,
          Vector(JobHandle(run8, run8Job))
        )
      yield doc).leftMap(_.message)
    }
