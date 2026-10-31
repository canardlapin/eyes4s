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

/** The strings of the Results headline's paired descriptive (design review
  * ticket 27; DESIGN_SPEC §15, "Paired descriptive"). They are kept apart
  * from [[SummaryTextId]] so the block adds its own ids without editing the
  * summary's catalogue. Templates name their arguments by position, as
  * [[Messages]] does.
  */
enum PairedTextId derives CanEqual:
  /** The block's key: the contrast and the σ it is at. */
  case Heading

  /** How many paired participants are strictly higher in the minuend. */
  case Higher

  /** The range of the paired differences, each end with its participant. */
  case Range

  /** The rounding note and the descriptive-only note. */
  case Rounding, DescriptiveOnly

  /** Participants with means in one group only, left out of the pairing. */
  case Unpaired, OneUnpaired

  /** No participant has means in both groups: nothing is paired. */
  case NonePaired

  /** The block read as one sentence, for assistive technology. */
  case Spoken, SpokenNonePaired

  /** A spoken block followed by its unpaired count. */
  case SpokenUnpaired

  /** The separator between the note's parts. */
  case Separator

/** The paired descriptive's strings in the Results board's wording. */
object PairedText:

  /** The reference English template of `id`. */
  def english(id: PairedTextId): String =
    import PairedTextId.*
    id match
      case Heading         => "Δ D, {0} − {1} at {2}"
      case Higher          => "Higher for {0} in {1} of {2} participants"
      case Range           => "range {0} ({1}) to {2} ({3})"
      case Rounding        => "computed before rounding participant means"
      case DescriptiveOnly => "descriptive, no tests"
      case Unpaired        => "{0} participants with means in one group only are not paired"
      case OneUnpaired     => "1 participant with means in one group only is not paired"
      case NonePaired      => "No participant has means in both {0} and {1}; nothing is paired"
      case Separator       => " · "
      case Spoken          =>
        "{0} minus {1}, paired over {2} participants: mean difference {3}, higher in {4} " +
          "of {5} participants, range {6} {7} to {8} {9}, computed before rounding participant " +
          "means. Descriptive only, no intervals or tests."
      case SpokenNonePaired =>
        "{0} minus {1}: no participant has means in both groups, so nothing is paired."
      case SpokenUnpaired => "{0} {1}."

  /** "2 participants with means in one group only are not paired". */
  def unpaired(n: Int): String =
    if n == 1 then apply(PairedTextId.OneUnpaired)
    else apply(PairedTextId.Unpaired, Format.count(n.toLong))

  /** `id`'s English template with `args` filled in. */
  def apply(id: PairedTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
