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

enum AnalysesTextId derives CanEqual:
  case NewAnalysis, Draft, NotRun, Current, Stale, Running, Cancelled, CancelledAt, Failed,
    HeldDraft, Immutable

object AnalysesText:
  def english(id: AnalysesTextId): String =
    import AnalysesTextId.*
    id match
      case NewAnalysis => "New analysis…"
      case Draft       => "Draft {0}"
      case NotRun      => "data {0} · not run"
      case Current     => "current"
      case Stale       => "stale"
      case Running     => "running"
      case Cancelled   => "cancelled"
      case CancelledAt => "cancelled at {0}"
      case Failed      => "failed"
      case HeldDraft => "Finish or discard the existing draft before starting another analysis."
      case Immutable =>
        "Revisions are immutable. Recipe edits start a draft; existing runs keep their revision."
  def apply(id: AnalysesTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
