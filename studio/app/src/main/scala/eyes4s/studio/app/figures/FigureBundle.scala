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

package eyes4s.studio.app.figures

import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.core.backend.{ReportRole, ReportView}
import eyes4s.studio.core.selection.ScaleIndex
import eyes4s.studio.app.text.Format
import eyes4s.studio.core.engine.StudioBuild
import eyes4s.studio.core.figures.FigureSource

/** One file of a figure's export bundle (Figures board, Bundle). */
enum BundleItem derives CanEqual:
  case Figure, Results, Comparisons, Participants, Methods, Snapshot

object BundleItem:
  /** On until the author turns them off; the project snapshot is off. */
  val Default: Set[BundleItem] = Set(Figure, Results, Comparisons, Participants, Methods)

/** One row of the bundle list: its file, what it holds, whether it is
  * chosen, and why it cannot be written, if it cannot.
  */
final case class BundleRowVM(
    item: BundleItem,
    file: String,
    detail: String,
    chosen: Boolean,
    unavailable: Option[String]
) derives CanEqual

final case class BundleVM(rows: Vector[BundleRowVM], action: String, status: Option[String])
    derives CanEqual:
  /** The items that will be written: chosen and available. */
  def written: Vector[BundleItem] =
    rows.filter(r => r.chosen && r.unavailable.isEmpty).map(_.item)

/** What the platform writes for "Export bundle…": the bound figure, its page
  * in the chosen format, the methods text as shown (edits included), and
  * the chosen items, into the folder `folder`.
  */
final case class BundleRequest(
    source: FigureSource,
    page: PageVM,
    format: ExportFormat,
    methods: Option[String],
    items: Vector[BundleItem],
    includeImages: Boolean,
    folder: String,
    omitted: Vector[(String, String)] = Vector.empty,
    participantScale: Option[ScaleIndex] = None
) derives CanEqual

object FigureBundle:
  val Action: String = "Export bundle…"

  def folder(page: PageVM): String = s"figure-${page.figure.number}-bundle"

  def file(item: BundleItem, page: PageVM, format: ExportFormat): String = item match
    case BundleItem.Figure       => ComposerText.fileName(page, format)
    case BundleItem.Results      => "results.csv"
    case BundleItem.Comparisons  => "comparisons.csv"
    case BundleItem.Participants => "participants.csv"
    case BundleItem.Methods      => "methods.md"
    case BundleItem.Snapshot     => "project snapshot"

  def view(
      chosen: Set[BundleItem],
      source: FigureSource,
      page: PageVM,
      format: ExportFormat,
      summary: Option[SummaryAnswer],
      report: Option[Either[String, ReportView]],
      methods: Option[MethodsVM],
      includeImages: Boolean,
      status: Option[String]
  ): BundleVM =
    val result  = summary.collect { case SummaryAnswer.Answered(r) => r }
    val reading = ComposerText.reading(source.run.id)
    def detail(item: BundleItem): (String, Option[String]) = item match
      case BundleItem.Figure  => ("", None)
      case BundleItem.Results =>
        result.fold(("", Some(reading)))(r =>
          (s"${Format.count(r.contrasts.requested)} queries × ${r.scaleLabels.size} σ", None)
        )
      case BundleItem.Comparisons =>
        result.fold(("", Some(reading)))(r => (s"${Format.count(r.pairRows)} pair rows", None))
      case BundleItem.Participants =>
        (result, report) match
          case (Some(r), Some(Right(view))) if r.scaleLabels.isDefinedAt(view.scale) =>
            val groups =
              view.cells.filter(_.role == ReportRole.Difference).map(_.group).distinct
            (
              s"${r.participants.size} × ${groups.size} groups at ${r.scaleLabels(view.scale)}",
              None
            )
          case (_, Some(Left(why)))   => ("", Some(why))
          case (_, Some(Right(view))) =>
            ("", Some(s"The summary of ${view.run.label} has no scale ${view.scale}."))
          case _ => ("", Some(reading))
      case BundleItem.Methods =>
        methods.map(_.text) match
          case Some(Right(_))  => ("", None)
          case Some(Left(why)) => ("", Some(why))
          case None            => ("", Some(MethodsCopy.NoFigure))
      case BundleItem.Snapshot =>
        (
          if includeImages then "includes images"
          else "without image bytes; stimulus file names remain in the trial inventory",
          None
        )
    BundleVM(
      BundleItem.values.toVector.map { item =>
        val (d, unavailable) = detail(item)
        BundleRowVM(item, file(item, page, format), d, chosen.contains(item), unavailable)
      },
      Action,
      status
    )

  /** The bundle's README.txt: what it holds, what it is bound to, and each
    * chosen file it does not hold, with why.
    */
  def readme(request: BundleRequest): String =
    val s     = request.source
    val files = request.items.map(i => s"- ${file(i, request.page, request.format)}")
    val left  = request.omitted.map((f, why) => s"- $f: $why")
    (Vector(
      s"${s.figure.id.label} export bundle",
      s"${s.run.id.label} · analysis ${s.bound.analysis.id.label} · data " +
        s"${s.bound.dataset.id.label} · reporting “${s.reporting.name}” · studio build eyes4s " +
        StudioBuild.eyes4sBaseVersion,
      s"reporting spec ${s.reporting.id.value} " +
        FigureCaption.specDigest(s.reporting, short = false)
    ) ++ request.participantScale.toVector.flatMap { scale =>
      s.bound.analysis.recipe.scales.values
        .lift(scale.value)
        .toVector
        .map(sigma =>
          s"participants.csv and reporting counts: ${sigma.render} (scale index ${scale.value}); " +
            "the first participant panel's scale, or the first declared scale for a figure without one."
        )
    } ++ Vector(
      "",
      "Files:"
    ) ++ files ++
      (if request.items.contains(BundleItem.Snapshot) && !request.includeImages then
         Vector(
           "",
           "The project snapshot withholds stimulus image bytes. The trial inventory is included",
           "with its image_file column, so stimulus file names remain visible."
         )
       else Vector.empty) ++
      (if left.isEmpty then Vector.empty else Vector("", "Not included:") ++ left))
      .mkString("", "\n", "\n")

  def exported(where: Either[String, String]): String =
    where.fold(why => s"The bundle was not exported: $why", w => s"Bundle exported to $w.")
