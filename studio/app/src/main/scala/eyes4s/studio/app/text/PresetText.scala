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

/** The recipe presets' strings (ticket S7.1; Analysis.dc.html, recipe).
  * Templates name their arguments by position, as [[Messages]] does.
  */
enum PresetTextId derives CanEqual:
  case Heading, Title
  case DetailEncodingRetrieval, DetailPerceptionImagery, DetailRecognition
  case CustomNote, NoAnalysis, StartInitial, Unchanged, Changes, OptionAccessible

object PresetText:

  def english(id: PresetTextId): String =
    import PresetTextId.*
    id match
      case Heading => "Preset · sets defaults and wording; one engine underneath"
      case Title   => "{0} → {1}"
      case DetailEncodingRetrieval => "Query: {0} · Reference: {1} · match on item"
      case DetailPerceptionImagery => "Query: {0} on blank · Reference: {1}"
      case DetailRecognition       => "Old probes only. Lures and novel: {0}"
      case CustomNote              => "Custom: the recipe holds no preset's phases"
      case NoAnalysis              => "No analysis revision to apply a preset to yet."
      case StartInitial            => "Starts the first analysis on {0}."
      case Unchanged               => "no change"
      case Changes                 => "changes {0}"
      case OptionAccessible        => "{0} preset, {1}; {2}"

  def apply(id: PresetTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
