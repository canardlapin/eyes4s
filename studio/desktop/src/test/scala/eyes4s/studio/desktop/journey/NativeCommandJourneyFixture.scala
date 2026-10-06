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

package eyes4s.studio.desktop.journey

import cats.effect.IO
import eyes4s.codec.ByteDigest
import eyes4s.plan.TrialKeyDefinitions
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.real.{DatasetSources, RealAdmission, RealPrepared}
import eyes4s.studio.desktop.runtime.DatasetSourceHosts
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** Explicit native inputs shared by command and FX journeys. No saved story
  * document, rounded illustrative score or fabricated input is installed.
  */
object NativeCommandJourneyFixture:
  def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new AssertionError(e.toString), identity)
  val golden: Path                = FixtureDoc.root.resolve("fixtures/studio-golden")
  val dataset: DatasetRevision    = DatasetRevision(1)
  val revision: AnalysisRevision  = AnalysisRevision(1)
  val run: RunId                  = RunId(1)
  val focus: TrialKey             = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  val scale: Int                  = 2
  val sources: DatasetSources[IO] = DatasetSourceHosts.golden(golden)
  val ScoresTolerance: Double     = 5e-7 + 1e-12

  private def bindings(fields: Vector[(ColumnRole, String)]) =
    fields.map((role, name) => ColumnBinding(role, get(ColumnName.of(name))))
  val mapping: ColumnMapping = get(
    ColumnMapping.of(
      bindings(
        Vector(
          ColumnRole.Participant -> "participant",
          ColumnRole.Phase       -> "phase",
          ColumnRole.Trial       -> "trial",
          ColumnRole.Occurrence  -> "occurrence",
          ColumnRole.Ordinal     -> "ordinal",
          ColumnRole.X           -> "x",
          ColumnRole.Y           -> "y",
          ColumnRole.Onset       -> "onset_ms",
          ColumnRole.Duration    -> "duration_ms",
          ColumnRole.SampleCount -> "sample_count"
        )
      )
    )
  )
  val inventory: InventoryMapping = get(
    InventoryMapping.withDisplays(
      get(
        InventoryMapping.of(
          bindings(
            Vector(
              ColumnRole.Participant -> "participant",
              ColumnRole.Phase       -> "phase",
              ColumnRole.Trial       -> "trial",
              ColumnRole.Occurrence  -> "occurrence",
              ColumnRole.Item        -> "item",
              ColumnRole.Response    -> "response"
            )
          ),
          DeclaredAttributes.empty
        )
      ),
      Some(
        DisplayColumns(
          get(ColumnName.of("display_kind")),
          Some(get(ColumnName.of("image_file")))
        )
      )
    )
  )
  val units: DeclaredUnits = DeclaredUnits(Some(TimeUnit.Milliseconds))
  val geometry: Geometry   = get(
    Geometry.of(
      get(ScreenSize.of(1920, 1080)),
      get(ImagePlacement.of(448, 156, 1024, 768)),
      get(DeclaredPixelsPerDegree.of(35))
    )
  )
  val recipe: Recipe = Recipe(
    None,
    DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout),
    MethodSpec(get(DefinitionRef.of("eyes4s.cosine", 1)), Vector.empty),
    PhasePair(Phase.Retrieval, Phase.Encoding),
    WeightChoice.Duration,
    FailureChoice.RequireAll,
    get(GridSize.of(64, 48)),
    Some(get(AnalysisWindow.of(448, 156, 1472, 924))),
    Some(OffWindowChoice.Exclude),
    get(ScaleSet.of(Vector(0.5, 1.0, 2.0, 4.0).map(s => get(Sigma.of(s))))),
    Some(get(DeclaredPixelsPerDegree.of(35))),
    MatchedChoice.RequireOne,
    ControlChoice.SameSelection,
    UnmatchedChoice.ReportNoMatch,
    InitialFixationChoice.KeepAll
  )
  val reporting: ReportingSpec = get(
    ReportingSpec.of(
      get(ReportingId.of("native-by-response")),
      "Native retrieval response",
      Some(get(Covariate.of("response"))),
      Vector.empty,
      None,
      ReportingWeight.ParticipantMeans,
      Some(get(ReportingContrast.of("Remembered", "Forgotten")))
    )
  )
  val panels: Vector[PanelSpec] = Vector(
    PanelSpec(
      get(PanelLetter.of("A")),
      "Participant D",
      PanelScale.At(get(Sigma.of(2))),
      PanelSelection.AllQueries
    )
  )

  final case class Inputs(entries: Vector[(Source, Vector[Byte])]):
    val declared: Sources      = get(Sources.of(entries.map(_._1)))
    val importCommand: Command = Command.ImportSources(
      None,
      declared,
      mapping,
      units,
      geometry,
      DeclaredAttributes.empty,
      None,
      Some(inventory)
    )
    def direct(document: StudioDocument): IO[RealPrepared] =
      val spec = document
        .dataset(dataset)
        .getOrElse(throw new AssertionError("Imported dataset missing"))
      for
        assets <- sources
          .assets(spec)
          .map(_.getOrElse(throw new AssertionError("Native inventory assets missing")))
        byRole   = entries.map((s, b) => s.role -> String(b.toArray, UTF_8)).toMap
        admitted = get(
          RealAdmission
            .admit(spec, byRole(SourceRole.Fixations), byRole(SourceRole.Trials), assets)
        )
      yield get(RealPrepared.of(revision, dataset, recipe, admitted))

  def load: IO[Inputs] = IO.blocking {
    Inputs(
      Vector(SourceRole.Fixations -> "fixations.csv", SourceRole.Trials -> "trials.csv").map {
        (role, name) =>
          val data = Files.readAllBytes(golden.resolve(name))
          Source(
            role,
            get(SourcePath.of(name)),
            ByteDigest.sha256(IArray.from(data)),
            None
          ) -> data.toVector
      }
    )
  }
