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

package eyes4s.studio.desktop.icons

import eyes4s.studio.app.icons.{AccessibleText, Icon, IconPaint}
import javafx.scene.Group
import javafx.scene.control.{Button, ContentDisplay, Tooltip}
import javafx.scene.layout.{Pane, Region}
import javafx.scene.shape.{FillRule, SVGPath, StrokeLineCap, StrokeLineJoin}
import javafx.scene.transform.Scale

/** The displayed size of an icon, in logical pixels. The boards draw icons at
  * these sizes; the 16x16 drawing is scaled, stroke included, as SVG scales a
  * view box.
  */
enum IconSize(val px: Int) derives CanEqual:
  case Px10 extends IconSize(10)
  case Px12 extends IconSize(12)
  case Px14 extends IconSize(14)
  case Px16 extends IconSize(16)
  case Px18 extends IconSize(18)
  case Px20 extends IconSize(20)

/** JavaFX nodes for the studio icon set (ticket S1.3).
  *
  * Icons paint in the looked-up colour `-es-current`, the studio's
  * `currentColor`: `studio-icons.css` defines it as `-es-ink` on the root, and
  * a control recolours its icon by redefining it (`-es-current: -es-accent`),
  * ideally together with `-fx-text-fill: -es-current`. The stroke width, caps
  * and joins are set here, so an icon draws the same without a stylesheet;
  * only its colour comes from CSS.
  */
object StudioIcons:

  /** The style class of every icon node, and of each layer by its paint. */
  val iconClass: String = "icon"

  def paintClass(paint: IconPaint): String = paint match
    case IconPaint.Stroke      => "icon-stroke"
    case IconPaint.Fill        => "icon-fill"
    case IconPaint.FillEvenOdd => "icon-fill"

  /** The style class of an icon-only button. */
  val iconButtonClass: String = "icon-button"

  /** One layer as an `SVGPath` in view-box coordinates. */
  def path(layer: eyes4s.studio.app.icons.IconLayer): SVGPath =
    val p = SVGPath()
    p.setContent(layer.pathData)
    p.getStyleClass.add(paintClass(layer.paint))
    layer.paint match
      case IconPaint.Stroke =>
        p.setStrokeWidth(Icon.strokeWidth)
        p.setStrokeLineCap(StrokeLineCap.BUTT)
        p.setStrokeLineJoin(StrokeLineJoin.MITER)
      case IconPaint.Fill        => p.setFillRule(FillRule.NON_ZERO)
      case IconPaint.FillEvenOdd => p.setFillRule(FillRule.EVEN_ODD)
    p

  /** The icon as a fixed-size region: `size` square, style classes `icon` and
    * `icon-<id>`. Decorative: it is not a focus stop and says nothing to a
    * screen reader; the control that shows it carries the name.
    */
  def graphic(icon: Icon, size: IconSize = IconSize.Px16): Region =
    val k     = size.px.toDouble / Icon.viewBox
    val group = Group(icon.layers.map(path)*)
    group.getTransforms.add(Scale(k, k, 0, 0))
    val pane = Pane(group)
    pane.getStyleClass.addAll(iconClass, s"icon-${icon.id}")
    pane.setMinSize(size.px, size.px)
    pane.setPrefSize(size.px, size.px)
    pane.setMaxSize(size.px, size.px)
    pane.setFocusTraversable(false)
    pane.setMouseTransparent(true)
    pane

  /** An icon-only button. `label` is required: it is the accessible text a
    * screen reader announces and the tooltip a pointer shows.
    */
  def button(icon: Icon, label: AccessibleText, size: IconSize = IconSize.Px14): Button =
    val b = Button()
    b.setGraphic(graphic(icon, size))
    b.setContentDisplay(ContentDisplay.GRAPHIC_ONLY)
    b.setAccessibleText(label.text)
    b.setTooltip(Tooltip(label.text))
    b.getStyleClass.add(iconButtonClass)
    b
