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

package eyes4s.studio.app.plot

import eyes4s.studio.app.text.{ParticipantText, ParticipantTextId, SummaryText, SummaryTextId}

/** The columns of the slopegraphs' value source. */
final case class SlopeColumns(
    scale: ColumnId,
    group: ColumnId,
    meanOf: ColumnId,
    d: ColumnId,
    n: ColumnId
) derives CanEqual

object SlopeColumns:
  val standard: Either[PlotSourceError, SlopeColumns] =
    for
      scale  <- ColumnId.of("scale")
      group  <- ColumnId.of("group")
      meanOf <- ColumnId.of("mean-of")
      d      <- ColumnId.of("d")
      n      <- ColumnId.of("n")
    yield SlopeColumns(scale, group, meanOf, d, n)

/** The contrast at one σ as the summary's "Δ by σ" draws it: the mean over
  * paired participants of their difference, and how many of the `paired`
  * are above zero. Computed by the paired descriptive, never by the plot.
  */
final case class SlopeDelta(scale: String, mean: Option[Double], higher: Int, paired: Int)
    derives CanEqual

/** What the slopegraphs draw beyond their source (overhaul v4, Results ›
  * summary): the σ the report is at, the selected participant, the
  * participants with fewer than [[Slopegraphs.ThinQueries]] queries in a
  * group (drawn dotted), each group's short name under its column, and the
  * contrast at every σ.
  */
final case class SlopeFrame(
    reported: Option[String],
    selected: Option[String],
    thin: Set[String],
    shortNames: Vector[String],
    deltas: Vector[SlopeDelta]
) derives CanEqual

object SlopeFrame:
  val empty: SlopeFrame = SlopeFrame(None, None, Set.empty, Vector.empty, Vector.empty)

object Slopegraphs:

  /** Fewer queries than this in a group make a participant's line dotted. */
  val ThinQueries: Int = 3

  /** The σ panel's label: "σ 2°". */
  def panel(means: ParticipantMeans): String =
    SummaryText(SummaryTextId.Scale, means.scaleLabel)

  /** The participants with fewer than [[ThinQueries]] queries in a group, at
    * any σ.
    */
  def thin(means: Vector[ParticipantMeans]): Set[String] =
    means.flatMap(_.cells).filter(_.n.exists(_ < ThinQueries)).map(_.participant).toSet

  /** Every σ's participant means as one source, σ by σ in order: for each
    * group, every participant's mean and then the group's grand mean, as
    * [[ParticipantMeans.source]] lists one σ, with the σ panel first.
    */
  def source(
      means: Vector[ParticipantMeans],
      columns: SlopeColumns
  ): Either[PlotSourceError, PlotSource] =
    def text(s: String)   = PlotValue.Text(s)
    def number(v: Double) = PlotValue.Number(v)
    val rows              = means.flatMap { m =>
      val scale = text(panel(m))
      m.groups.flatMap { g =>
        m.cells
          .filter(_.group == g.group)
          .map(c =>
            PlotRow(
              c.ref,
              Vector(
                scale,
                text(c.group.label),
                text(c.participant),
                c.d.fold(PlotValue.Missing)(number),
                c.n.fold(PlotValue.Missing)(n => text(ParticipantText.queries(n)))
              )
            )
          ) :+ PlotRow(
          g.ref,
          Vector(
            scale,
            text(g.group.label),
            text(ParticipantText(ParticipantTextId.AllParticipants)),
            number(g.d),
            text(ParticipantText.participants(g.n))
          )
        )
      }
    }
    PlotSource(
      ParticipantText(ParticipantTextId.SlopeCaption),
      Vector(
        PlotColumn(
          columns.scale,
          ParticipantText(ParticipantTextId.ScaleHeader),
          ColumnFormat.Label
        ),
        PlotColumn(
          columns.group,
          ParticipantText(ParticipantTextId.GroupHeader),
          ColumnFormat.Label
        ),
        PlotColumn(
          columns.meanOf,
          ParticipantText(ParticipantTextId.MeanOfHeader),
          ColumnFormat.Label
        ),
        PlotColumn(
          columns.d,
          ParticipantText(ParticipantTextId.DHeader),
          ColumnFormat.Signed(2)
        ),
        PlotColumn(columns.n, ParticipantText(ParticipantTextId.NHeader), ColumnFormat.Label)
      ),
      rows
    )
