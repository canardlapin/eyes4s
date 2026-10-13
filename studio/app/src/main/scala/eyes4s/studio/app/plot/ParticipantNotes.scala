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

import eyes4s.studio.app.text.{Format, ParticipantText, ParticipantTextId}
import eyes4s.studio.core.backend.{ParticipantCounts, QueryRow, Response, RunId}
import eyes4s.studio.core.selection.StudioRef

/** What a line of the participant plot's annotation column is to its
  * block: the heading names the participant, a fact follows it. A heading
  * is set in the medium mono face, a fact in the regular one.
  */
enum NoteRole derives CanEqual:
  case Heading, Fact

/** A line's colour, by what it says rather than by a colour: `Primary`
  * (ink) for what the block is about, `Secondary` (ink-2) for the numbers
  * that back it, `Failure` (the fail colour) for a participant or a value
  * that failed.
  */
enum NoteTone derives CanEqual:
  case Primary, Secondary, Failure

/** One line of a participant's notes: its text by id in the participant
  * plot's catalogue, how it is set, and how it is spoken.
  */
final case class NoteLine(
    id: ParticipantTextId,
    args: Vector[String],
    role: NoteRole,
    tone: NoteTone,
    spoken: String
) derives CanEqual:

  /** The line as the catalogue writes it. */
  def text: String = ParticipantText(id, args*)

/** One participant's notes, top to bottom, and the plot row they are
  * joined to, `anchor`, whose D is `d` (missing when the participant has
  * no mean there).
  */
final case class NoteBlock(
    participant: String,
    anchor: StudioRef,
    d: Option[Double],
    lines: Vector[NoteLine]
) derives CanEqual

/** The participant plot's annotation column as the app composes it from
  * served values (review ticket 26; the Results board's notes beside the
  * participant D plot). `blocks` are in priority order; `omitted` counts the
  * participants that qualified but did not fit.
  */
final case class ParticipantNotes(blocks: Vector[NoteBlock], omitted: Int) derives CanEqual:
  def isEmpty: Boolean = blocks.isEmpty

object ParticipantNotes:

  val empty: ParticipantNotes = ParticipantNotes(Vector.empty, 0)

  /** The slots of the board's frame: 216 px tall in 14 px slots (the
    * column's `SlotPx`), so 15 whole lines.
    */
  val BoardSlots: Int = 15

  /** The empty slots between two blocks (the column's `BlockGapSlots`). */
  val BlockGapSlots: Int = 1

  /** The participant a selection names: its first participant summary,
    * report participant or participant entity, for `run` where the ref
    * names a run.
    */
  def selectedParticipant(selected: Vector[StudioRef], run: RunId): Option[String] =
    selected.collectFirst {
      case StudioRef.ParticipantSummary(`run`, _, _, _, p)   => p
      case StudioRef.ReportParticipant(`run`, _, _, _, _, p) => p
      case StudioRef.Participant(p)                          => p
    }

  /** A group's short name in the column: its leading letters through its
    * first run of vowels and the run of consonants after it, one closed
    * syllable ("Remembered" → "Rem", "Forgotten" → "Forg", as the board and
    * FIXTURE.md's table headers write them). When two groups would share a
    * short name, every group keeps its whole name.
    */
  def shortNames(groups: Vector[Response]): Map[Response, String] =
    def vowel(c: Char)               = "aeiouAEIOU".indexOf(c) >= 0
    def short(label: String): String =
      val lead   = label.takeWhile(c => c.isLetter && !vowel(c))
      val vowels = label.drop(lead.length).takeWhile(vowel)
      val after  =
        label.drop(lead.length + vowels.length).takeWhile(c => c.isLetter && !vowel(c))
      val cut = lead + vowels + after
      if cut.isEmpty then label else cut
    val shorts = groups.map(g => g -> short(g.label))
    if shorts.map(_._2).distinct.size == shorts.size then shorts.toMap
    else groups.map(g => g -> g.label).toMap

  /** The notes of the participant plot drawn from `means`, in at most
    * `slots` lines.
    *
    * The board's rule: the selected participant, when it is plotted, and
    * every plotted participant with failed queries each get a block. A
    * block reads, top down:
    *
    *   - the heading: "P17 · selected" for the selected participant,
    *     otherwise "P05 · 17 of 20 contributing" in the fail colour;
    *   - for the selected participant, "19 of 20 contributing" (in the fail
    *     colour when it has failed queries);
    *   - its mean in each group, "Rem +0.38 (n 17)", or "Rem no value";
    *   - when it has failed queries: "3 failed queries:", by group "2
    *     Remembered, 1 Forgotten" (only when the query rows' failures add
    *     up to the served count), and "no value, not zero" in the fail
    *     colour.
    *
    * Every number is served: the counts are `counts`' (the participant
    * table's Requested, Contributing and Failed columns), the means and
    * their n are `means`' cells, and the failures by group count the
    * served `queries` rows that failed, by response.
    *
    * A block is joined to the participant's mean in the plot's last group,
    * the column nearest the notes, or failing that its last group with a
    * row.
    *
    * Overflow: blocks take their lines plus [[BlockGapSlots]] between two.
    * They are kept in priority order, the selected participant first, then
    * the others by failed queries, most first (ties in the plot's order),
    * while they fit in `slots` with one more line, "+3 more", left for the
    * blocks that do not; that line ends the block drawn lowest. When not
    * even the first block fits, there are no notes.
    */
  def of(
      means: ParticipantMeans,
      counts: Vector[ParticipantCounts],
      queries: Option[Vector[QueryRow]],
      selected: Option[String],
      slots: Int
  ): ParticipantNotes =
    import ParticipantTextId.*
    val groups              = means.groups.map(_.group)
    val shorts              = shortNames(groups)
    val people              = means.cells.map(_.participant).distinct
    def countOf(p: String)  = counts.find(_.participant == p)
    def failedOf(p: String) = countOf(p).fold(0)(_.failed)
    def line(id: ParticipantTextId, role: NoteRole, tone: NoteTone, args: String*) =
      val l = NoteLine(id, args.toVector, role, tone, "")
      l.copy(spoken = l.text)
    def anchorOf(p: String): Option[ParticipantCell] =
      val own = means.cells.filter(_.participant == p)
      groups.reverseIterator.flatMap(g => own.find(_.group == g)).nextOption()
    def groupLines(p: String): Vector[NoteLine] =
      groups.flatMap { g =>
        means.cells.find(c => c.participant == p && c.group == g).map { c =>
          (c.d, c.n) match
            case (Some(d), Some(n)) =>
              val args = Vector(Format.signed(d, 2), Format.count(n.toLong))
              NoteLine(
                NoteGroupMean,
                shorts(g) +: args,
                NoteRole.Fact,
                NoteTone.Secondary,
                ParticipantText(NoteGroupMeanSpoken, (g.label +: args)*)
              )
            case _ =>
              line(NoteGroupNoValue, NoteRole.Fact, NoteTone.Secondary, shorts(g))
                .copy(spoken = ParticipantText(NoteGroupNoValue, g.label))
        }
      }
    def failureLines(p: String, failed: Int): Vector[NoteLine] =
      // Spoken without the colon that leads the eye to the next line.
      val total =
        if failed == 1 then
          line(NoteOneFailed, NoteRole.Fact, NoteTone.Primary)
            .copy(spoken = ParticipantText(NoteOneFailedSpoken))
        else
          val n = Format.count(failed.toLong)
          line(NoteFailed, NoteRole.Fact, NoteTone.Primary, n)
            .copy(spoken = ParticipantText(NoteFailedSpoken, n))
      val rows   = queries.getOrElse(Vector.empty).filter(q => q.query.participant == p)
      val byKind = groups.map(g => g -> rows.count(q => q.response == g && q.status.isFailed))
      val parts  = byKind.collect {
        case (g, n) if n > 0 =>
          ParticipantText(NoteFailedInGroup, Format.count(n.toLong), g.label)
      }
      val byGroup = Option.when(queries.isDefined && byKind.map(_._2).sum == failed)(
        line(
          NoteFailedByGroup,
          NoteRole.Fact,
          NoteTone.Primary,
          parts.mkString(ParticipantText(NoteSeparator))
        )
      )
      Vector(total) ++ byGroup.toVector :+ line(NoteNoValue, NoteRole.Fact, NoteTone.Failure)
    def of20(x: ParticipantCounts) =
      Vector(Format.count(x.contributing.toLong), Format.count(x.requested.toLong))
    def selectedBlock(p: String): Option[NoteBlock] =
      anchorOf(p).map { anchor =>
        val failed = failedOf(p)
        val tone   = if failed > 0 then NoteTone.Failure else NoteTone.Secondary
        val head   = line(NoteSelected, NoteRole.Heading, NoteTone.Primary, p) +:
          countOf(p).toVector.map(x => line(NoteContributing, NoteRole.Fact, tone, of20(x)*))
        val tail = if failed > 0 then failureLines(p, failed) else Vector.empty
        NoteBlock(p, anchor.ref, anchor.d, head ++ groupLines(p) ++ tail)
      }
    def failedBlock(p: String): Option[NoteBlock] =
      for
        anchor <- anchorOf(p)
        x      <- countOf(p)
      yield NoteBlock(
        p,
        anchor.ref,
        anchor.d,
        (line(NoteFailedHeading, NoteRole.Heading, NoteTone.Failure, (p +: of20(x))*) +:
          groupLines(p)) ++ failureLines(p, x.failed)
      )
    val chosen   = selected.filter(people.contains)
    val failures = people
      .filter(p => failedOf(p) > 0 && !chosen.contains(p))
      .zipWithIndex
      .sortBy((p, i) => (-failedOf(p), i))
      .map(_._1)
    val ranked = chosen.flatMap(selectedBlock).toVector ++ failures.flatMap(failedBlock)
    def needs(kept: Vector[NoteBlock]): Int =
      kept.map(_.lines.size).sum + BlockGapSlots * (kept.size - 1).max(0) +
        (if kept.size < ranked.size then 1 else 0)
    val fit  = (ranked.size to 1 by -1).find(k => needs(ranked.take(k)) <= slots).getOrElse(0)
    val kept = ranked.take(fit)
    val left = ranked.size - fit
    if fit == 0 then empty
    else if left == 0 then ParticipantNotes(kept, 0)
    else
      // The block drawn lowest: the column stacks blocks by their anchor's D,
      // highest first, a missing mean below every value.
      val lowest = kept.indices.maxBy(i => (-kept(i).d.getOrElse(Double.NegativeInfinity), i))
      val more   = NoteLine(
        NoteMore,
        Vector(Format.count(left.toLong)),
        NoteRole.Fact,
        NoteTone.Secondary,
        ParticipantText(NoteMoreSpoken, Format.count(left.toLong))
      )
      ParticipantNotes(
        kept.updated(lowest, kept(lowest).copy(lines = kept(lowest).lines :+ more)),
        left
      )
