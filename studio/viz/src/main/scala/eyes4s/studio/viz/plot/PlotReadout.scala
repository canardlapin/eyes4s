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

import eyes4s.studio.core.selection.StudioRef

/** What a plot's readout line says (ticket S4.5c): the words of the
  * hovered mark or, with none hovered, of the mark of the first selected
  * row the plot draws ([[BuiltPlot.readout]]). A host passes the hover
  * intent's key and the bus's selection and only binds the result, so a
  * mark of several rows says them all.
  */
object PlotReadout:

  def of(
      plot: BuiltPlot,
      hovered: Option[StudioRef],
      selected: Vector[StudioRef]
  ): Option[String] =
    hovered
      .flatMap(plot.markOf)
      .orElse(selected.iterator.flatMap(plot.markOf).nextOption())
      .flatMap(plot.readout)
