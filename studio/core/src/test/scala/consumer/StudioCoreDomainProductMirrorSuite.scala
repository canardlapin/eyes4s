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

package consumer

import scala.compiletime.testing.typeCheckErrors

class StudioCoreDomainProductMirrorSuite extends munit.FunSuite:
  test("eyes4s.studio.core.artifacts.NativeArtifactBudget rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.artifacts.NativeArtifactBudget; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.artifacts.NativeArtifactBudget]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.artifacts.NativeBindingFacts rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.artifacts.NativeBindingFacts; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.artifacts.NativeBindingFacts]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.artifacts.NativeDatasetDefinition rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.artifacts.NativeDatasetDefinition; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.artifacts.NativeDatasetDefinition]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.preferences.RecentProjects rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.preferences.RecentProjects; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.preferences.RecentProjects]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.preferences.UserPreferences rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.preferences.UserPreferences; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.preferences.UserPreferences]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.platform.HostPath rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.platform.HostPath; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.platform.HostPath]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.platform.FileKind rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.platform.FileKind; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.platform.FileKind]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.platform.ExternalUrl rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.platform.ExternalUrl; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.platform.ExternalUrl]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.platform.PreferenceKey rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.platform.PreferenceKey; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.platform.PreferenceKey]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.platform.FontRequest rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.platform.FontRequest; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.platform.FontRequest]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.platform.ClockReading rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.platform.ClockReading; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.platform.ClockReading]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.bundle.BundlePath rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.bundle.BundlePath; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.bundle.BundlePath]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.bundle.InputEntry rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.bundle.InputEntry; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.bundle.InputEntry]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.bundle.ProjectManifest rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.bundle.ProjectManifest; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.bundle.ProjectManifest]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.bundle.LockOwner rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.bundle.LockOwner; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.bundle.LockOwner]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.diff.StatusChanges rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.diff.StatusChanges; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.diff.StatusChanges]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.diff.DatasetDiff rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.diff.DatasetDiff; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.diff.DatasetDiff]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.StageMeter rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.StageMeter; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.StageMeter]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.RunTotals rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.RunTotals; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.RunTotals]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.JobProgress rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.JobProgress; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.JobProgress]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.PageRequest rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.PageRequest; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.PageRequest]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.DensityGrid rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.DensityGrid; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.DensityGrid]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.PlanePoint rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.PlanePoint; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.PlanePoint]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.SourceRecordRow rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.SourceRecordRow; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.SourceRecordRow]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.SourceRecordPage rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.SourceRecordPage; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.SourceRecordPage]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.AdmittedFixation rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.AdmittedFixation; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.AdmittedFixation]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.TrialFixations rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.TrialFixations; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.TrialFixations]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.ScreenRegion rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.ScreenRegion; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.ScreenRegion]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.TrialPreview rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.TrialPreview; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.TrialPreview]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.TrialExtentEvidence rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.TrialExtentEvidence; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.TrialExtentEvidence]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.TrialExtentFromStart rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.TrialExtentFromStart; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.TrialExtentFromStart]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.TrialPlacement rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.TrialPlacement; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.TrialPlacement]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.PlacementDensityGrid rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.PlacementDensityGrid; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.PlacementDensityGrid]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.backend.PlacementPreview rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.backend.PlacementPreview; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.backend.PlacementPreview]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.AssetRelinks rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.AssetRelinks; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.AssetRelinks]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.Sigma rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.Sigma; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.Sigma]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ScaleSet rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ScaleSet; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ScaleSet]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.GridSize rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.GridSize; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.GridSize]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.CrossRadius rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.CrossRadius; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.CrossRadius]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ScreenCross rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ScreenCross; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ScreenCross]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.Occurrence rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.Occurrence; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.Occurrence]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.DefinitionRef rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.DefinitionRef; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.DefinitionRef]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.MinimumSuccessful rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.MinimumSuccessful; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.MinimumSuccessful]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.AnalysisWindow rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.AnalysisWindow; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.AnalysisWindow]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.DraftContext rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.DraftContext; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.DraftContext]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.RevisionName rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.RevisionName; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.RevisionName]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.InventoryDurationColumn rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.InventoryDurationColumn; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.InventoryDurationColumn]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.InventoryMapping rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.InventoryMapping; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.InventoryMapping]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.MapOpacity rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.MapOpacity; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.MapOpacity]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.PresentationState rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.PresentationState; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.PresentationState]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ReportingId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ReportingId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ReportingId]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.Covariate rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.Covariate; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.Covariate]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ValueSet rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ValueSet; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ValueSet]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.Share rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.Share; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.Share]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.MinimumPerGroup rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.MinimumPerGroup; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.MinimumPerGroup]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ReportingContrast rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ReportingContrast; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ReportingContrast]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ReportingSpec rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ReportingSpec; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ReportingSpec]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.FigureId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.FigureId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.FigureId]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.PanelLetter rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.PanelLetter; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.PanelLetter]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.FigureMethodsDraft rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.FigureMethodsDraft; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.FigureMethodsDraft]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.FigureSpec rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.FigureSpec; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.FigureSpec]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.SourcePath rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.SourcePath; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.SourcePath]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.SemanticIdentity rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.SemanticIdentity; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.SemanticIdentity]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.Sources rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.Sources; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.Sources]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ColumnName rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ColumnName; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ColumnName]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ColumnMapping rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ColumnMapping; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ColumnMapping]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.DeclaredAttributes rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.DeclaredAttributes; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.DeclaredAttributes]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ScreenSize rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ScreenSize; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ScreenSize]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ImagePlacement rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ImagePlacement; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ImagePlacement]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.DeclaredPixelsPerDegree rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.DeclaredPixelsPerDegree; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.DeclaredPixelsPerDegree]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.Geometry rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.Geometry; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.Geometry]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.Offset rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.Offset; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.Offset]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.document.ParticipantId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.document.ParticipantId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ParticipantId]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.execution.RunShelf rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.execution.RunShelf; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.execution.RunShelf]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.execution.ExecutionTracker rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.execution.ExecutionTracker; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.execution.ExecutionTracker]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.figures.Tally rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.figures.Tally; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.figures.Tally]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.preview.PreviewBudget rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.preview.PreviewBudget; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.preview.PreviewBudget]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.preview.ParticipantCount rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.preview.ParticipantCount; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.preview.ParticipantCount]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.preview.QueryCount rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.preview.QueryCount; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.preview.QueryCount]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.preview.PreviewCandidates rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.preview.PreviewCandidates; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.preview.PreviewCandidates]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.preview.PreviewProgress rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.preview.PreviewProgress; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.preview.PreviewProgress]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.preview.PreviewCounts rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.preview.PreviewCounts; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.preview.PreviewCounts]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.preview.PreviewReady rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.preview.PreviewReady; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.preview.PreviewReady]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.selection.ScaleIndex rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.selection.ScaleIndex; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.selection.ScaleIndex]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.selection.FixationIndex rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.selection.FixationIndex; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.selection.FixationIndex]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.selection.RecordNumber rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.selection.RecordNumber; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.selection.RecordNumber]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.selection.CheckedAddress rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.selection.CheckedAddress; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.selection.CheckedAddress]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.selection.ViewId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.selection.ViewId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.selection.ViewId]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.selection.SelectionState rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.selection.SelectionState; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.selection.SelectionState]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.command.UndoStack rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.command.UndoStack; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.command.UndoStack]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.command.History rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.command.History; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.command.History]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.geometry.SourcePositions rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.geometry.SourcePositions; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.geometry.SourcePositions]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.assets.SourceCheck rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.assets.SourceCheck; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.assets.SourceCheck]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.assets.AssetFile rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.assets.AssetFile; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.assets.AssetFile]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.assets.MatchItem rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.assets.MatchItem; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.assets.MatchItem]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.assets.AssetRegistry rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.assets.AssetRegistry; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.assets.AssetRegistry]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.runs.ArchiveEntry rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.runs.ArchiveEntry; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.runs.ArchiveEntry]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.runs.ArchiveIndex rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.runs.ArchiveIndex; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.runs.ArchiveIndex]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.runs.RetentionBasis rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.runs.RetentionBasis; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.runs.RetentionBasis]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.runs.RunStorage rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.runs.RunStorage; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.runs.RunStorage]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.runs.PrunePlan rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.runs.PrunePlan; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.runs.PrunePlan]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.runs.ConfirmedPrune rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.runs.ConfirmedPrune; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.runs.ConfirmedPrune]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.importing.MappingDraft rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.importing.MappingDraft; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.importing.MappingDraft]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.importing.TrialMetadataDraft rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.importing.TrialMetadataDraft; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.importing.TrialMetadataDraft]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.importing.PresetName rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.importing.PresetName; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.importing.PresetName]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.importing.ImportPreset rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.importing.ImportPreset; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.importing.ImportPreset]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.importing.ImportPresets rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.importing.ImportPresets; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.importing.ImportPresets]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.core.session.SessionOptions rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.core.session.SessionOptions; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.session.SessionOptions]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("trusted internal copies are unavailable to external consumers") {
    assert(
      typeCheckErrors(
        "def forbidden(value: eyes4s.studio.core.command.History): eyes4s.studio.core.command.History = value.copy()"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "def forbidden(value: eyes4s.studio.core.document.InventoryMapping): eyes4s.studio.core.document.InventoryMapping = value.copy()"
      ).nonEmpty
    )
  }
