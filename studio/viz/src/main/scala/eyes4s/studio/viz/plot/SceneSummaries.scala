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

import intaglio.{GraphicsName, Grob, PlotSemantics, Scene, SemanticId}

/** The semantic side of studio scenes (ticket S4.6): Intaglio's
  * [[PlotSemantics]] for a plot or a trial scene (its title, its alt text
  * and a text summary a host reads aloud or an SVG export carries), and the
  * named grobs of a scene, through which every drawn mark resolves to the
  * scientific identity it shows (a [[eyes4s.studio.core.selection.StudioRef]]),
  * never to a canvas position.
  */
object SceneSummaries:

  /** A scene's semantic id: `studio-` and its [[SceneId]], every character
    * outside Intaglio's portable identifier set (letters, digits, `-`, `_`,
    * `.`) written `-`.
    */
  def semanticId(id: SceneId): SemanticId =
    val safe = id.value.map(c =>
      if c.isLetterOrDigit && c < '\u0080' || c == '-' || c == '_' || c == '.' then c else '-'
    )
    SemanticId.unsafe(s"studio-$safe")

  /** A scene's semantics: its title; its alt text, what it shows in a
    * sentence; and its text summary, what every mark accounts for.
    */
  def semantics(id: SceneId, title: String, altText: String, summary: String): PlotSemantics =
    PlotSemantics(
      semanticId(id),
      Some(title),
      Some(summary),
      Some(altText),
      Vector.empty,
      Vector.empty,
      Vector.empty
    )

  /** Every named grob of `scene`, depth first, a name once for each grob
    * that bears it.
    */
  def namedGrobs(scene: Scene): Vector[GraphicsName] =
    def walk(g: Grob): Vector[GraphicsName] = g.name.toVector ++ g.children.flatMap(walk)
    scene.grobs.flatMap(walk)

  /** Names that more than one grob of `scene` bears: each must be drawn once,
    * so a pick of it resolves to one mark.
    */
  def duplicateNames(scene: Scene): Vector[GraphicsName] =
    val names = namedGrobs(scene)
    names.diff(names.distinct).distinct
