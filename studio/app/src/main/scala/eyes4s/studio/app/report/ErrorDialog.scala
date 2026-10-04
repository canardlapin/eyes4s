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

package eyes4s.studio.app.report

import eyes4s.studio.app.text.{ReportText, ReportTextId}
import eyes4s.studio.core.report.{ErrorBundle, FailureOrigin}

/** Where the report was logged: the log file as the user sees it (home
  * abbreviated to `~`), or why it could not be written.
  */
enum LogState derives CanEqual:
  case Written(location: String)
  case Unavailable(reason: String)

/** The error report dialog (ticket S1.12): what happened, where it was
  * logged, and the copyable report, which is exactly [[ErrorBundle.render]].
  */
final case class ErrorDialogVM(
    title: String,
    heading: String,
    summary: String,
    log: String,
    report: String,
    copy: String,
    copied: String,
    close: String,
    reportAccessible: String
) derives CanEqual

object ErrorDialog:

  def vm(bundle: ErrorBundle, log: LogState): ErrorDialogVM =
    import ReportTextId.*
    val where = bundle.origin match
      case FailureOrigin.UiThread(_)   => ReportText(WhereUi)
      case FailureOrigin.Job(effect)   => ReportText(WhereJob, effect)
      case FailureOrigin.Background(t) => ReportText(WhereBackground, t)
    ErrorDialogVM(
      title = ReportText(Title),
      heading = ReportText(Heading),
      summary = ReportText(Summary, where),
      log = log match
        case LogState.Written(at)    => ReportText(Logged, at)
        case LogState.Unavailable(r) => ReportText(NotLogged, r)
      ,
      report = bundle.render,
      copy = ReportText(Copy),
      copied = ReportText(Copied),
      close = ReportText(Close),
      reportAccessible = ReportText(DetailsAccessible)
    )
