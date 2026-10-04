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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.plot.{HalfOpenSpan, PlotBrush}
import eyes4s.studio.core.selection.StudioRef
import intaglio.DevicePoint

/** A drag across a plot as a brush (ticket S4.5e): the span of the plot's
  * numeric x column between the drag's two ends, and the rows the span
  * holds ([[PlotBrush.rows]]). The span is read through the plot's one
  * transform, so a brush selects exactly the rows whose values lie under
  * it on screen.
  */
final case class Brushed(span: HalfOpenSpan, refs: Vector[StudioRef]) derives CanEqual

object PlotBrushing:

  /** The brush from device point `from` to `to` on `targets`, if the plot's
    * x axis is numeric and the two ends are at different values of it.
    */
  def brushed(targets: PlotTargets, from: DevicePoint, to: DevicePoint): Option[Brushed] =
    val plot = targets.plot
    plot.encoding.x match
      case Axis.Numeric(column, scale) =>
        def value(d: DevicePoint) =
          val x = targets.transform.deviceToData(d).x
          scale match
            case AxisScale.Linear => x
            case AxisScale.Log10  => math.pow(10.0, x)
        HalfOpenSpan
          .between(value(from), value(to))
          .map(span => Brushed(span, PlotBrush.rows(plot.source, column, span)))
      case Axis.Category(_, _) => None
