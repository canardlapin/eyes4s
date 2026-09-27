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

package eyes4s.plan

import eyes4s.core.{FixationBoundary, Weight}
import eyes4s.kernel.{SyncFitMode, YAxis}
import eyes4s.surface.EdgePolicy

/** The stable token and the English display label of each alternative of a
  * closed choice. Tokens are what descriptions and raw form values carry;
  * labels are what a form shows, and a host localises by token.
  */
trait Labelled[A]:
  def token(value: A): String
  def label(value: A): String

object Labelled:
  def apply[A](using l: Labelled[A]): Labelled[A] = l

  private def of[A](tokenOf: A => String)(labelOf: A => String): Labelled[A] =
    new Labelled[A]:
      def token(value: A): String = tokenOf(value)
      def label(value: A): String = labelOf(value)

  given Labelled[Weight] = of[Weight](_.toString) {
    case Weight.Uniform  => "Every fixation counts once"
    case Weight.Duration => "Weighted by fixation duration"
  }

  given Labelled[EdgePolicy] = of[EdgePolicy](_.toString) {
    case EdgePolicy.Truncate    => "Truncate at the grid edge"
    case EdgePolicy.Renormalise => "Renormalise each fixation's mass"
  }

  given Labelled[YAxis] = of[YAxis](_.toString) {
    case YAxis.Down => "y increases downward"
    case YAxis.Up   => "y increases upward"
  }

  given Labelled[OffWindowPolicy] = of[OffWindowPolicy](_.toString) {
    case OffWindowPolicy.Exclude   => "Leave fixations outside the window out of the map"
    case OffWindowPolicy.FailTrial => "Fail a trial with any fixation outside the window"
  }

  given Labelled[ControlReferences] = of[ControlReferences](_.name) {
    case ControlReferences.SameSelection  => "Controls chosen like the matched reference"
    case ControlReferences.AllOccurrences => "Every occurrence of other items as controls"
  }

  given Labelled[UnmatchedFocalPolicy] = of[UnmatchedFocalPolicy](_.name) {
    case UnmatchedFocalPolicy.ReportNoMatch => "Report as no match"
    case UnmatchedFocalPolicy.Refuse        => "Refuse the study"
  }

  given Labelled[FixationBoundary] = of[FixationBoundary](_.toString) {
    case FixationBoundary.ClipDuration   => "Clip durations to the window"
    case FixationBoundary.FullyContained => "Only fixations fully inside the window"
  }

  given Labelled[SyncFitMode] = of[SyncFitMode](_.toString) {
    case SyncFitMode.OffsetOnly => "Constant offset between the clocks"
    case SyncFitMode.Affine     => "Offset and drift between the clocks"
  }
