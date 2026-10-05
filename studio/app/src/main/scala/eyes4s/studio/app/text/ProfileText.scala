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

/** The strings of the scale profile plot and its table (ticket S4.5d; the
  * Results board's scale profile and the Figures board's panel E). They are
  * kept apart from [[MessageId]] so the plot adds its own ids without
  * editing the shell's catalogue. Templates name their arguments by
  * position, as [[Messages]] does.
  */
enum ProfileTextId derives CanEqual:
  /** The source's caption, the plot's tab title and its accessible summary. */
  case Caption, Title, Summary

  /** The table's column headers. */
  case SeriesHeader, ScaleHeader, SigmaHeader, DHeader, NHeader

  /** The axes' titles. */
  case DAxis, SigmaAxis

/** The scale profile's strings in the boards' wording. */
object ProfileText:

  /** The reference English template of `id`. */
  def english(id: ProfileTextId): String =
    import ProfileTextId.*
    id match
      case Caption => "Scale profile of run {0}"
      case Title   => "Scale profile"
      case Summary =>
        "{0}: each group's grand mean D at {1} scales, bold, over each " +
          "participant's mean D, faint"
      case SeriesHeader => "Mean of"
      case ScaleHeader  => "Scale"
      case SigmaHeader  => "σ (°)"
      case DHeader      => "D"
      case NHeader      => "n"
      case DAxis        => "D (Δ cosine)"
      case SigmaAxis    => "Gaussian σ (log spacing)"

  /** `id`'s English template with `args` filled in. */
  def apply(id: ProfileTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
