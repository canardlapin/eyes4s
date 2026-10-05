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

/** The strings of Explore's source records table (ticket S6.4;
  * Explore.dc.html, bottom). Templates name their arguments by position, as
  * [[Messages]] does.
  */
enum RecordTextId derives CanEqual:
  /** The column headers. */
  case Record, Trial, Ordinal, Onset, Duration, Screen, Image, Degrees, Samples, Window

  /** A position's two coordinates, and a value the record does not have. */
  case Pair, None

  /** Where the record falls. */
  case Inside, Outside, OffScreen, DroppedInitial, NotAdmitted, InsideTrialFails

  /** A row's accessible name, the table's (Explore.dc.html's grid label),
    * and the raw record toggle.
    */
  case RowName, TableName, ShowRaw

  /** The table's states. */
  case NoRun, Reading, NotServed, Empty, Misaligned

object RecordText:

  def english(id: RecordTextId): String =
    import RecordTextId.*
    id match
      case Record           => "Record"
      case Trial            => "Trial"
      case Ordinal          => "Ordinal"
      case Onset            => "Onset ms"
      case Duration         => "Dur ms"
      case Screen           => "Screen x,y"
      case Image            => "Image x,y"
      case Degrees          => "Degrees x,y"
      case Samples          => "Samples"
      case Window           => "Window"
      case Pair             => "{0},{1}"
      case None             => "—"
      case Inside           => "inside"
      case Outside          => "outside"
      case OffScreen        => "off screen"
      case NotAdmitted      => "not admitted"
      case InsideTrialFails =>
        "inside, trial fails ({0} of {1} outside)"
      case DroppedInitial => "dropped (initial)"
      case RowName        => "Record {0}, {1}, fixation {2}"
      case TableName      =>
        "Source records from fixations.csv. One focus stop; arrow keys move the row cursor."
      case ShowRaw    => "Show raw record"
      case NoRun      => "No run is shown; source records follow the shown run's dataset"
      case Reading    => "Reading the source records…"
      case Empty      => "The fixation table has no records"
      case Misaligned => "Page {0} of the source records starts at row {1}, not row {2}"
      case NotServed  => "Source records are not served in this window"

  def apply(id: RecordTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
