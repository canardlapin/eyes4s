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

enum ParticipantsNavTextId derives CanEqual:
  case NoReport
  case Heading, Accounting, Row, Mean, Spoken, Reading, NoRun, Empty, Invalid, MissingCounts,
    MissingValue

/** Results navigator text; overall means and run accounting are named separately. */
object ParticipantsNavText:
  def english(id: ParticipantsNavTextId): String =
    import ParticipantsNavTextId.*
    id match
      case Heading    => "Overall D · σ {0}"
      case Accounting => "Run: contributing / requested · failed"
      case Mean       => "{0} (n {1})"
      case Row        => "{0} / {1} · {2} failed"
      case Spoken     =>
        "{0}, overall D {1}, n {2}; run queries {3} contributing of {4}, {5} failed"
      case Reading  => "Reading overall participant values for {0}."
      case NoReport => "No reporting specification for {0}."
      case NoRun    => "No run shown."
      case Empty    => "No overall participant values in {0}."
      case Invalid  =>
        "{0}, reporting {1}, scale {2}: the served participant context is inconsistent."
      case MissingCounts => "{0}: no query accounting was served for participant {1}."
      case MissingValue  => "no value"
  def apply(id: ParticipantsNavTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
