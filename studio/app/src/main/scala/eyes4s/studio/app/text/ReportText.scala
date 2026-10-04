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

/** The error report dialog's strings (ticket S1.12). Templates name their
  * arguments by position, as [[Messages]] does.
  */
enum ReportTextId derives CanEqual:
  case Title, Heading, Summary, WhereUi, WhereJob, WhereBackground, Logged, NotLogged, Copy,
    Copied, Close, DetailsAccessible

object ReportText:

  def english(id: ReportTextId): String =
    import ReportTextId.*
    id match
      case Title   => "Eyes Studio error"
      case Heading => "Something went wrong in Eyes Studio"
      case Summary =>
        "An unexpected failure on the {0}. Your project is unchanged by this report. " +
          "Copy the report below to share it; it holds no participant data."
      case WhereUi           => "user interface"
      case WhereJob          => "job ({0})"
      case WhereBackground   => "background thread {0}"
      case Logged            => "The same report is in the log: {0}"
      case NotLogged         => "The log could not be written: {0}"
      case Copy              => "Copy report"
      case Copied            => "Copied"
      case Close             => "Close"
      case DetailsAccessible => "Error report text"

  def apply(id: ReportTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
