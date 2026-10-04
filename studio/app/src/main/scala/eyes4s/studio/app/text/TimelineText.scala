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

/** The strings of the timeline plot and its table (ticket S4.5e; the
  * Explore board's timeline). They are kept apart from [[MessageId]] so the
  * plot adds its own ids without editing the shell's catalogue. Templates
  * name their arguments by position, as [[Messages]] does.
  */
enum TimelineTextId derives CanEqual:
  /** The source's caption, the plot's tab title and its accessible summary. */
  case Caption, Title, Summary

  /** The table's column headers. */
  case FixationHeader, OnsetHeader, DurationHeader

  /** The time axis's title. */
  case TimeAxis

/** The timeline's strings in the boards' wording. */
object TimelineText:

  /** The reference English template of `id`. */
  def english(id: TimelineTextId): String =
    import TimelineTextId.*
    id match
      case Caption => "{0} fixations"
      case Title   => "Timeline"
      case Summary =>
        "Timeline of {0}: {1} fixation intervals, each bar as long and as tall as its duration"
      case FixationHeader => "Fixation"
      case OnsetHeader    => "Onset ms"
      case DurationHeader => "Duration ms"
      case TimeAxis       => "Onset (ms)"

  /** `id`'s English template with `args` filled in. */
  def apply(id: TimelineTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
