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

package eyes4s.results

import cats.syntax.all.*
import eyes4s.plan.{
  CellKey,
  ContrastKey,
  DiagnosticFamily,
  Fact,
  FactError,
  FactSlot,
  FactSource,
  FactValue,
  GroupCell
}

/** The reporting facts of a methods text (CR6d), read from a [[Report]]:
  * which report, its minimum, n per group, paired n, the queries per
  * participant and group, and the cells below the minimum or with the fewest
  * queries. Each fact's source names the report cells, contrast or
  * specification it is read from, at the report's scale.
  */
object ReportFacts:
  /** The facts of `report` for one `role` and `component` (by default its
    * first selected ones), with `name` as the host's display name of the
    * reporting specification:
    *
    *   - `ReportingSpec`: `name`, from the specification's id;
    *   - `MinimumQueries`: the participant-means minimum, when it is above
    *     one;
    *   - `GroupN(level)`: the participants contributing to each group's cell,
    *     when the report groups;
    *   - `PairedN`: the paired participants of the level contrast, when it has
    *     one stratum;
    *   - `GroupSizeRange`: the fewest to the most queries a participant has in
    *     a group;
    *   - `BelowMinimumQueries`, when a minimum above one applies: the
    *     participant-and-group cells it left out (possibly none);
    *     `SmallestGroups` otherwise: the cells with the fewest queries.
    *
    * A group's level is its levels' labels joined by " · ". The facts are
    * refused only as [[Fact.of]] refuses them (a blank name or level).
    */
  def of[K](
      report: Report[K],
      name: String,
      role: Option[Role] = None,
      component: Option[String] = None
  ): Either[FactError, Vector[Fact]] =
    val spec  = report.spec
    val r     = role.getOrElse(spec.selection.roles.head)
    val c     = component.getOrElse(spec.selection.components.head)
    val cells = report.cells.filter(x => x.role == r && x.component == c)
    def key(cell: Cell[K], participant: Option[String]) =
      CellKey(
        cell.group.levels,
        DiagnosticFamily.slug(r.toString),
        c,
        spec.scale,
        participant
      )
    val all     = FactSource.ReportCells(cells.map(key(_, None)))
    val minimum = spec.reduce match
      case ReducePolicy.ParticipantMeans(m) if m.value > 1 => Some(m.value)
      case _                                               => None
    val sizes = for
      cell <- cells
      p    <- cell.perParticipant
    yield (cell, p)
    def breakdown(entries: Vector[(Cell[K], String, Int)]) =
      FactValue.Breakdown(entries.map { (cell, participant, queries) =>
        GroupCell(
          participant,
          level(cell.group),
          queries,
          FactSource.ReportCells(Vector(key(cell, Some(participant))))
        )
      })
    val contrasts = report.contrasts.filter(x => x.role == r && x.component == c)
    val facts     = Vector(
      Some(
        (FactSlot.ReportingSpec, FactSource.ReportSpec(spec.id.value), FactValue.Label(name))
      ),
      minimum.map(m =>
        (FactSlot.MinimumQueries, FactSource.ReportSpec(spec.id.value), FactValue.Count(m))
      )
    ).flatten ++
      Option
        .when(spec.groupBy.nonEmpty)(
          cells.map(cell =>
            (
              FactSlot.GroupN(level(cell.group)),
              FactSource.ReportCells(Vector(key(cell, None))),
              FactValue.Count(cell.participants.toLong)
            )
          )
        )
        .toVector
        .flatten ++
      (contrasts match
        case Vector(one) =>
          Vector(
            (
              FactSlot.PairedN,
              FactSource.ReportContrast(
                ContrastKey(
                  one.stratum.levels,
                  one.term,
                  one.minuend,
                  one.subtrahend,
                  spec.scale
                )
              ),
              FactValue.Count(one.paired.size.toLong)
            )
          )
        case _ => Vector.empty) ++
      Option
        .when(sizes.nonEmpty) {
          val counts = sizes.map(_._2.queries.toLong)
          (FactSlot.GroupSizeRange, all, FactValue.Range(counts.min, counts.max))
        }
        .toVector ++
      (minimum match
        case Some(m) =>
          val below = sizes.collect {
            case (cell, p) if p.queries < m => (cell, p.participant, p.queries)
          }
          Vector((FactSlot.BelowMinimumQueries, all, breakdown(below)))
        case None =>
          sizes.map(_._2.queries).minOption.toVector.map { fewest =>
            val smallest = sizes.collect {
              case (cell, p) if p.queries == fewest => (cell, p.participant, p.queries)
            }
            (FactSlot.SmallestGroups, all, breakdown(smallest))
          })
    facts.traverse((slot, source, value) => Fact.of(slot, source, value))

  /** A group's level as a methods text names it: its labels joined by " · ",
    * or "all" for the ungrouped report.
    */
  def level(group: GroupKey): String =
    if group.levels.isEmpty then "all" else group.levels.map(_._2).mkString(" · ")
