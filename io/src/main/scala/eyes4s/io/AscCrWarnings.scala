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

package eyes4s.io

/** Absence of an exported warning token is not evidence of no warnings. */
enum AscCrWarningEvidence:
  case NotDeclared
  case NotExported
  case Observed(flags: AscCrWarningFlags)

final class AscCrEyeWarnings private[io] (
    val missing: Boolean,
    val recovering: Boolean
)

/** Native warning evidence, separate from missing coordinates and blink intervals. */
final class AscCrWarningFlags private (
    val raw: String,
    val interpolated: Boolean,
    val left: Option[AscCrEyeWarnings],
    val right: Option[AscCrEyeWarnings]
)

object AscCrWarningFlags:
  private[io] def parse(eyes: AscEyeLayout, raw: String): Option[AscCrWarningFlags] =
    val expected = if eyes == AscEyeLayout.Binocular then 5 else 3
    if raw.length != expected || (raw(0) != '.' && raw(0) != 'I') then None
    else
      def eye(offset: Int): Option[AscCrEyeWarnings] =
        if (raw(offset) == '.' || raw(offset) == 'C') &&
          (raw(offset + 1) == '.' || raw(offset + 1) == 'R')
        then Some(new AscCrEyeWarnings(raw(offset) == 'C', raw(offset + 1) == 'R'))
        else None
      eyes match
        case AscEyeLayout.Left =>
          eye(1).map(value => new AscCrWarningFlags(raw, raw(0) == 'I', Some(value), None))
        case AscEyeLayout.Right =>
          eye(1).map(value => new AscCrWarningFlags(raw, raw(0) == 'I', None, Some(value)))
        case AscEyeLayout.Binocular =>
          for
            left  <- eye(1)
            right <- eye(3)
          yield new AscCrWarningFlags(raw, raw(0) == 'I', Some(left), Some(right))
