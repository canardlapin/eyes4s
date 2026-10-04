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

package eyes4s.studio.app.text

/** The strings of the participant plot and its table (ticket S4.5c; the
  * Results board's participant D plot). They are kept apart from
  * [[MessageId]] so the plot adds its own ids without editing the shell's
  * catalogue. Templates name their arguments by position, as [[Messages]]
  * does.
  */
enum ParticipantTextId derives CanEqual:
  /** The source's caption, the plot's tab title and its accessible summary. */
  case Caption, Title, Summary

  /** The table's column headers. */
  case GroupHeader, MeanOfHeader, DHeader, NHeader

  /** The "mean of" cell of a group's grand mean. */
  case AllParticipants

  /** The D axis's title, the label of the band of missing values, and the
    * per-group n under each group's column.
    */
  case DAxis, NoValue, GroupN

  /** The n cell: a participant's mean is over queries, a grand mean over
    * participants.
    */
  case Queries, OneQuery, Participants, OneParticipant

/** The participant plot's strings in the boards' wording. */
object ParticipantText:

  /** The reference English template of `id`. */
  def english(id: ParticipantTextId): String =
    import ParticipantTextId.*
    id match
      case Caption => "Participant D at {0}"
      case Title   => "Participants"
      case Summary =>
        "Participant plot of {0}: each participant's mean D in {1} groups, lines join a " +
          "participant's means, a tick marks each group's grand mean"
      case GroupHeader     => "Group"
      case MeanOfHeader    => "Mean of"
      case DHeader         => "D"
      case NHeader         => "n"
      case AllParticipants => "all participants"
      case DAxis           => "D (Δ cosine), participant mean"
      case NoValue         => "no value"
      case GroupN          => "n = {0}"
      case Queries         => "{0} queries"
      case OneQuery        => "1 query"
      case Participants    => "{0} participants"
      case OneParticipant  => "1 participant"

  /** "2 queries": the n of a participant's mean in a group. */
  def queries(n: Int): String =
    if n == 1 then apply(ParticipantTextId.OneQuery)
    else apply(ParticipantTextId.Queries, Format.count(n.toLong))

  /** "24 participants": the n of a group's grand mean. */
  def participants(n: Int): String =
    if n == 1 then apply(ParticipantTextId.OneParticipant)
    else apply(ParticipantTextId.Participants, Format.count(n.toLong))

  /** `id`'s English template with `args` filled in. */
  def apply(id: ParticipantTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
