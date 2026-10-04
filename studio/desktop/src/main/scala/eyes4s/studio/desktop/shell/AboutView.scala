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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.about.{AboutBox, AboutFacts, AboutVM}
import javafx.geometry.Insets
import javafx.scene.control.{Button, Label, ScrollPane}
import javafx.scene.layout.{GridPane, VBox}

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.control.NonFatal

/** What the desktop knows for the About box (ticket S1.14): the running
  * Java and JavaFX, and the components the build's generated notices list
  * (`notices/components.tsv`, a resource of this jar).
  */
object DesktopAbout:

  /** The generated component list's resource. */
  val ComponentsResource: String = "eyes4s/studio/desktop/notices/components.tsv"

  /** The generated list's text, or why it is not there. */
  def componentsText: Either[String, String] =
    Option(getClass.getClassLoader.getResourceAsStream(ComponentsResource)) match
      case None     => Left(s"$ComponentsResource is not in the application")
      case Some(in) =>
        try Right(String(in.readAllBytes, UTF_8))
        catch case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString))
        finally in.close()

  /** This JVM's facts. */
  def facts: AboutFacts =
    AboutFacts(
      sys.props.getOrElse("java.version", "unknown"),
      sys.props.getOrElse("java.vendor", "unknown"),
      sys.props.get("javafx.runtime.version"),
      componentsText.flatMap(AboutBox.parse)
    )

/** The About box's content: it only binds [[AboutVM]]. `close` closes its
  * window. Use on the JavaFX thread.
  */
final class AboutView(vm: AboutVM, close: () => Unit):

  private def label(text: String, styles: String*): Label =
    val l = Label(text)
    l.getStyleClass.addAll(styles*)
    l.setWrapText(true)
    l

  private val lines = GridPane()
  lines.setHgap(12.0)
  lines.setVgap(4.0)
  vm.lines.zipWithIndex.foreach { (l, i) =>
    lines.add(label(l.label, "t12", "inspector-key"), 0, i)
    lines.add(label(l.value, "t12", "mono"), 1, i)
  }

  private val table = GridPane()
  table.setHgap(12.0)
  table.setVgap(2.0)
  vm.components.zipWithIndex.foreach { (c, i) =>
    table.add(label(c.name, "t11"), 0, i)
    table.add(label(c.version, "t11", "mono"), 1, i)
    table.add(label(c.licence, "t11"), 2, i)
  }
  private val tableScroll = ScrollPane(table)
  tableScroll.setFitToWidth(true)
  tableScroll.setPrefViewportHeight(260.0)
  tableScroll.setFocusTraversable(false)

  private val closeButton = Button(vm.close)
  closeButton.getStyleClass.add("btn")
  closeButton.setAccessibleText(vm.close)
  closeButton.setDefaultButton(true)
  closeButton.setOnAction(_ => close())

  /** The window's content. */
  val node: VBox = VBox(
    10.0,
    (Vector(
      label(vm.title, "t13"),
      lines,
      label(vm.componentsTitle, "t13")
    ) ++ vm.problem.map(p => label(p, "t12", "inspector-note")) ++
      Vector(tableScroll, label(vm.notices, "t11", "inspector-note"), closeButton))*
  )
  node.setPadding(Insets(16.0))
  node.getStyleClass.add("about-box")

  /** The lines and component rows shown, as text, for tests. */
  def shownLines: Vector[(String, String)] = vm.lines.map(l => (l.label, l.value))
  def shownRows: Int                       = table.getChildren.size / 3
