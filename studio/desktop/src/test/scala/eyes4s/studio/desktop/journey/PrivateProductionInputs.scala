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

import eyes4s.codec.ByteDigest
import eyes4s.plan.{ExpectedUnmatchedRule, TrialKeyDefinitions}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.importing.CsvSniffer
import eyes4s.studio.core.real.{AdmittedDataset, RealAdmission}
import io.circe.{Json, parser}
import java.nio.file.{Files, Path, StandardOpenOption}

/** Explicitly invoked local inputs. Nothing discovers or opens a private dataset in CI. */
object PrivateProductionInputs:
  def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new AssertionError(e.toString), identity)

  def write(path: Path, value: Json): Unit =
    Files.writeString(path, value.noSpaces, StandardOpenOption.CREATE_NEW): Unit

  def external(path: Path): Path =
    val real = path.toRealPath()
    require(
      !Iterator
        .iterate(Option(real))(_.flatMap(p => Option(p.getParent)))
        .takeWhile(_.isDefined)
        .flatten
        .exists(p => Files.exists(p.resolve(".git"))),
      "Private evidence must be outside every Git checkout"
    )
    real

  final class Inputs(val root: Path):
    val request = get(parser.parse(Files.readString(root.resolve("request.json")))).hcursor
    def number(name: String): Double = get(request.get[Double](name))
    def text(name: String): String   = get(request.get[String](name))
    val fixations: String            = Files.readString(root.resolve("fixations.csv"))
    val trials: String               = Files.readString(root.resolve("trials.csv"))
    val entries: Vector[(Source, IArray[Byte])] =
      Vector(SourceRole.Fixations -> "fixations.csv", SourceRole.Trials -> "trials.csv").map {
        (role, name) =>
          val bytes = IArray.from(Files.readAllBytes(root.resolve(name)))
          Source(role, get(SourcePath.of(name)), ByteDigest.sha256(bytes), None) -> bytes
      }
    private def binding(role: ColumnRole, column: String) =
      ColumnBinding(role, get(ColumnName.of(column)))
    val mapping: ColumnMapping = get(
      ColumnMapping.of(
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
        ).map(binding)
      )
    )
    private val trialBindings = Vector(
      ColumnRole.Participant -> "participant",
      ColumnRole.Phase       -> "phase",
      ColumnRole.Trial       -> "trial",
      ColumnRole.Occurrence  -> "occurrence",
      ColumnRole.Item        -> "item",
      ColumnRole.Response    -> "response"
    ).map(binding)
    private def attributes(name: String, source: String, mapped: Set[String]) =
      val header = get(CsvSniffer.chooseDelimiter(name, source))._2.head
      get(
        DeclaredAttributes.of(
          header
            .filterNot(mapped)
            .map(c => AttributeBinding(get(ColumnName.of(c)), AttributeKindChoice.Text))
        )
      )
    val fixationAttributes: DeclaredAttributes =
      attributes("fixations.csv", fixations, mapping.bindings.map(_.column.value).toSet)
    val inventory: InventoryMapping = get(
      InventoryMapping.withDisplays(
        get(
          InventoryMapping.of(
            trialBindings,
            attributes("trials.csv", trials, trialBindings.map(_.column.value).toSet)
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
        get(ScreenSize.of(number("screen_width_px").toInt, number("screen_height_px").toInt)),
        get(
          ImagePlacement
            .of(0, 0, number("screen_width_px").toInt, number("screen_height_px").toInt)
        ),
        get(DeclaredPixelsPerDegree.of(number("pixels_per_degree")))
      )
    )
    val spec: DatasetRevisionSpec = DatasetRevisionSpec(
      DatasetRevision(1),
      None,
      get(Sources.of(entries.map(_._1))),
      mapping,
      units,
      geometry,
      AdmissionChoice.default,
      AdmissionDecision.Pending,
      fixationAttributes,
      Some(inventory)
    )
    def admit: AdmittedDataset =
      val assets = get(
        AssetRegistry.fromInventory(spec, entries(1)._2, Vector.empty, Vector.empty)
      )
      get(RealAdmission.admit(spec, fixations, trials, assets))
    def recipe(columns: Int, rows: Int): Recipe = Recipe(
      None,
      DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout),
      MethodSpec(get(DefinitionRef.of("eyes4s.cosine", 1)), Vector.empty),
      PhasePair(Phase(text("focal_phase")), Phase(text("reference_phase"))),
      WeightChoice.Duration,
      FailureChoice.RequireAll,
      get(GridSize.of(columns, rows)),
      Some(get(AnalysisWindow.of(0, 0, number("screen_width_px"), number("screen_height_px")))),
      Some(OffWindowChoice.Exclude),
      get(ScaleSet.of(Vector(get(Sigma.of(number("sigma_px") / number("pixels_per_degree")))))),
      Some(geometry.pixelsPerDegree),
      MatchedChoice.Select(OccurrencePick.First),
      ControlChoice.SameSelection,
      UnmatchedChoice.ReportNoMatch,
      InitialFixationChoice.KeepAll,
      Some(
        get(
          ExpectedUnmatchedRule.of(
            text("expected_unmatched_column"),
            Vector(text("expected_unmatched_value"))
          )
        )
      )
    )

  def load: Inputs =
    require(
      !sys.env.contains("CI") && !sys.env.contains("GITHUB_ACTIONS"),
      "Private data disabled in CI"
    )
    new Inputs(external(Path.of(sys.env("EYES4S_PRIVATE_RUN"))))
