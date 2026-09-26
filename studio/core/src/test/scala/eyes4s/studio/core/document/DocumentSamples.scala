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

package eyes4s.studio.core.document

import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.fixture.StoryMoments
import DocumentGen.right

/** A named sample of every document type and of every enum case, pinned in
  * [[DocumentPins]].
  */
object DocumentSamples:

  private def story(e: Either[String, StudioDocument]): StudioDocument =
    e.fold(m => throw new AssertionError(m), identity)

  val t1: StudioDocument = story(StoryMoments.t1)
  val t2: StudioDocument = story(StoryMoments.t2)
  val t3: StudioDocument = story(StoryMoments.t3)

  private val digest: CanonicalDigest[StudyPlanArtifact] =
    CanonicalDigest.parse[StudyPlanArtifact]("0123456789abcdef" * 4).toOption.get

  private val r3: DatasetRevisionSpec    = t2.datasets(1)
  private val rev4: AnalysisRevisionSpec = t2.analyses(1)
  private val draft: Draft               = t2.draft.get
  private val query: TrialKey            = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  private val s2: Sigma                  = right(Sigma.of(2.0))
  private val s8: Sigma                  = right(Sigma.of(8.0))
  private val response: Covariate        = right(Covariate.of("response"))
  private val keep: ReportingFilter      =
    ReportingFilter.Keep(response, right(ValueSet.of(response, Vector("Remembered"))))
  private val outside: ReportingFilter =
    ReportingFilter.OutsideWindowAtMost(right(Share.of(0.25)))
  private val translate: CoordinateCorrection =
    CoordinateCorrection.Translate(right(Offset.of(-2.5, 4.0)))
  private val rules: AdmissionChoice = AdmissionChoice(
    OffScreenChoice.QuarantineTrial,
    Vector(
      CorrectionRule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipY),
      CorrectionRule(CorrectionTarget.Participant(right(ParticipantId.of("P05"))), translate),
      CorrectionRule(CorrectionTarget.Trial(query), CoordinateCorrection.FlipX)
    )
  )
  private val base: Recipe = rev4.recipe
  private val otherGrid    = right(GridSize.of(32, 24))
  private val pick         = OccurrencePick.At(right(Occurrence.of(2)))
  private val drop   = InitialFixationChoice.DropLeadingNearCross(right(CrossRadius.of(1.5)))
  private val strict = right(
    ReportingSpec.of(
      right(ReportingId.of("strict")),
      "Remembered, window ≤ 25%",
      Some(response),
      Vector(keep, outside),
      Some(right(MinimumPerGroup.of(3))),
      ReportingWeight.PooledQueries
    )
  )

  val all: Vector[Sample[?]] = Vector(
    Sample("binding.Unbound", CoreBinding.unbound[StudyPlanArtifact]),
    Sample("binding.Bound", CoreBinding.Bound(digest)),
    Sample("source-role.Fixations", SourceRole.Fixations),
    Sample("source-role.Trials", SourceRole.Trials),
    Sample(
      "source.semantic",
      r3.sources
        .entries(0)
        .copy(semantic = Some(right(SemanticIdentity.of("0123456789abcdef"))))
    ),
    Sample("sources", r3.sources),
    Sample("mapping.r3", r3.mapping),
    Sample("units.undeclared", DeclaredUnits(None)),
    Sample("units.ms", r3.units),
    Sample("time-unit.Seconds", TimeUnit.Seconds),
    Sample("time-unit.Microseconds", TimeUnit.Microseconds),
    Sample("geometry", r3.geometry),
    Sample("admission.default", AdmissionChoice.default),
    Sample("admission.rules", rules),
    Sample("decision.Pending", AdmissionDecision.Pending),
    Sample("decision.Admitted", r3.decision),
    Sample("dataset.r3", r3),
    Sample("sigma", s2),
    Sample("scales", rev4.recipe.scales),
    Sample("grid", rev4.recipe.grid),
    Sample("initial.KeepAll", InitialFixationChoice.KeepAll),
    Sample("initial.DropFirst", InitialFixationChoice.DropFirst),
    Sample("initial.DropLeadingNearCross", drop),
    Sample("matched.RequireOne", MatchedChoice.RequireOne),
    Sample("matched.SameOccurrence", MatchedChoice.SameOccurrence),
    Sample("matched.Select.First", MatchedChoice.Select(OccurrencePick.First)),
    Sample("matched.Select.Last", MatchedChoice.Select(OccurrencePick.Last)),
    Sample("matched.Select.At", MatchedChoice.Select(pick)),
    Sample("matched.MeanOfAll", MatchedChoice.MeanOfAll),
    Sample("controls.AllOccurrences", ControlChoice.AllOccurrences),
    Sample("unmatched.Refuse", UnmatchedChoice.Refuse),
    Sample("recipe.rev4", base),
    Sample(
      "change.Phases",
      RecipeChange.Phases(base.phases, PhasePair(Phase.Encoding, Phase.Encoding))
    ),
    Sample("change.Grid", RecipeChange.Grid(base.grid, otherGrid)),
    Sample("change.Scales", draft.changes(0)),
    Sample("change.Matched", RecipeChange.Matched(base.matched, MatchedChoice.Select(pick))),
    Sample(
      "change.Controls",
      RecipeChange.Controls(base.controls, ControlChoice.AllOccurrences)
    ),
    Sample("change.Unmatched", RecipeChange.Unmatched(base.unmatched, UnmatchedChoice.Refuse)),
    Sample(
      "change.InitialFixations",
      RecipeChange.InitialFixations(base.initialFixations, drop)
    ),
    Sample("draft.rev5", draft),
    Sample("preset.Recognition", Preset.Recognition),
    Sample("preset.Custom", Preset.Custom),
    Sample("analysis.rev4", rev4),
    Sample("run.Running", t3.runs(3)),
    Sample("run.Cancelled", t2.runs(1)),
    Sample("run.Completed", t2.runs(2)),
    Sample("lifecycle.Failed", RunLifecycle.Failed),
    Sample("lifecycle.Cancelled.none", RunLifecycle.Cancelled(None)),
    Sample("filter.Keep", keep),
    Sample("filter.OutsideWindowAtMost", outside),
    Sample("reporting.by-response", t2.reporting(0)),
    Sample("reporting.strict", strict),
    Sample("panel-scale.At", PanelScale.At(s8)),
    Sample("figure.1", t2.figures(0)),
    Sample("figure.2", t2.figures(1)),
    Sample("presentation.default", PresentationState.default),
    Sample(
      "presentation.layouts",
      right(
        PresentationState.of(
          Perspective.Figures,
          Theme.Dark,
          StageAppearance.Mid,
          right(MapOpacity.of(0.4)),
          true,
          Some(RunId(7)),
          Vector(
            SavedLayout(Perspective.Figures, LayoutBlob("""{"root":"figures"}""")),
            SavedLayout(Perspective.Data, LayoutBlob("""{"root":"data"}"""))
          )
        )
      )
    )
  )
