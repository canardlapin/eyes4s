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

import eyes4s.studio.app.Intent
import eyes4s.studio.app.explore.{NavigatorPaneVM, NavigatorRowKind, NavigatorRowVM}
import eyes4s.studio.app.plot.{
  ColumnFormat,
  ColumnId,
  PlotColumn,
  PlotRow,
  PlotSource,
  PlotValue
}
import eyes4s.studio.app.tokens.{Theme, ThemedToken, Tokens}
import eyes4s.studio.core.selection.{
  InputCause,
  InputStamp,
  SelectionInput,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.admission.AdmissionLedgerView
import eyes4s.studio.desktop.analysis.ResolvedDesignView
import eyes4s.studio.desktop.explore.{
  ExploreTimelineHost,
  ExploreTrialViewPane,
  TrialsNavigatorView
}
import eyes4s.studio.desktop.figures.FiguresHost
import eyes4s.studio.desktop.harness.StudioFxSuite
import eyes4s.studio.desktop.plot.TableTwinView
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.collections.FXCollections
import javafx.css.PseudoClass
import javafx.scene.Parent
import javafx.scene.control.{Button, Label, Labeled, ListCell, ListView, ToggleButton}
import javafx.scene.layout.{HBox, Region, StackPane, VBox}
import javafx.scene.paint.Color

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/** Selection is a fill; hover is lighter (DESIGN_SPEC section 15, REVIEW
  * ticket 13). A selected row or item draws the themed selection fill with
  * its text in ink at weight 600, the semibold face, and its mono text in
  * Mono 500; a hovered row draws surface-2, and a selected row stays selected
  * when hovered. The accent is focus's: no selection is drawn with it.
  *
  * `:hover` and `:focused` are set explicitly, as A11yTreeSuite sets
  * `:focused`: a headless run moves no pointer and cannot guarantee focus.
  * Each check reads the rendered background fills and fonts.
  */
class SelectionFillFxSuite extends StudioFxSuite:

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val Hover    = PseudoClass.getPseudoClass("hover")
  private val Focused  = PseudoClass.getPseudoClass("focused")
  private val Selected = PseudoClass.getPseudoClass("selected")

  private val SansSemibold = "IBM Plex Sans SmBld"
  private val MonoMedium   = "IBM Plex Mono Medm"

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def colour(t: ThemedToken): Color =
    val c = Tokens.themed(Theme.Light, t)
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  private val accent   = colour(ThemedToken.Accent)
  private val soft     = colour(ThemedToken.AccentSoft)
  private val surface  = colour(ThemedToken.Surface)
  private val surface2 = colour(ThemedToken.Surface2)
  private val surface3 = colour(ThemedToken.Surface3)
  private val hairline = colour(ThemedToken.Hairline)
  private val selFill  = colour(ThemedToken.SelFill)
  private val ink      = colour(ThemedToken.Ink)

  /** The background fill colours of `r`, outermost first. */
  private def fills(r: Region): List[Color] =
    Option(r.getBackground).toList
      .flatMap(_.getFills.asScala.toList.map(_.getFill.asInstanceOf[Color]))

  /** `node` under `pseudo` (set or cleared); `read` runs after CSS. */
  private def drawn[A](node: Region, pseudo: (PseudoClass, Boolean)*)(read: => A): A = runOnFx {
    pseudo.foreach((p, on) => node.pseudoClassStateChanged(p, on))
    node.getScene.getRoot.applyCss()
    read
  }

  private def face(l: Labeled): String  = runOnFx(l.getFont.getFamily)
  private def textFill(l: Labeled): Any = runOnFx(l.getTextFill)

  private def themed(content: Parent, sheets: String*): StackPane = runOnFx {
    val root = StackPane(content)
    root.getStylesheets.setAll(right(StudioStyles.stylesheets(Theme.Light))*)
    sheets.foreach(s =>
      Option(getClass.getClassLoader.getResource(s))
        .foreach(url => root.getStylesheets.add(url.toExternalForm))
    )
    root.setPrefSize(640, 400)
    root
  }

  private def cell[A](list: ListView[A], index: Int): ListCell[?] = runOnFx {
    list
      .lookupAll(".list-cell")
      .asScala
      .collectFirst { case c: ListCell[?] if c.getIndex == index => c }
      .getOrElse(fail(s"no cell for row $index"))
  }

  fxStage.test("a selected list row draws the selection fill, ink and the semibold face") {
    fx =>
      // A plain list (the shell's rules), and the ledger's trials list in place
      // under its view's own sheet, which outranks the shell's.
      Vector(None, Some("ledger-trials")).foreach { style =>
        val l = runOnFx {
          val l =
            ListView(FXCollections.observableArrayList("enc_01", "enc_02", "enc_03", "enc_04"))
          style.foreach(l.getStyleClass.add)
          l
        }
        val host = runOnFx {
          val h = StackPane(l)
          style.foreach(_ =>
            Option(getClass.getClassLoader.getResource(AdmissionLedgerView.stylesheetResource))
              .foreach(url => h.getStylesheets.add(url.toExternalForm))
          )
          h
        }
        fx.show(themed(host))
        fx.awaitLayout()
        // The cursor is kept off the rows read: its frame is CompositeCursorFxSuite's.
        runOnFx {
          l.getSelectionModel.select(1)
          l.getFocusModel.focus(3)
        }
        fx.awaitLayout()
        val selected = cell(l, 1)
        val plain    = cell(l, 2)
        Vector(false, true).foreach { focused =>
          val clue = s"list ${style.getOrElse("plain")} focused=$focused"
          drawn(l, Focused -> focused)(())
          assertEquals(drawn(selected)(fills(selected)), List(selFill), clue)
          assertEquals(textFill(selected), ink, clue)
          assertEquals(face(selected), SansSemibold, clue)
          assertEquals(face(plain), "IBM Plex Sans", clue)
          // Hover is surface-2, and never replaces the selection fill.
          assertEquals(drawn(plain, Hover -> true)(fills(plain)), List(surface2), clue)
          assertEquals(drawn(selected, Hover -> true)(fills(selected)), List(selFill), clue)
          drawn(plain, Hover -> false)(())
          drawn(selected, Hover -> false)(())
        }
      }
  }

  private def navRow(kind: NavigatorRowKind, label: String): NavigatorRowVM =
    NavigatorRowVM(
      kind,
      0,
      label,
      "street-112",
      "1.2 s",
      None,
      false,
      None,
      false,
      label,
      None,
      None
    )

  fxStage.test("a selected navigator row: the selection fill, Sans 600 and Mono 500") { fx =>
    val view = runOnFx {
      val v = TrialsNavigatorView(_ => (), _ => eyes4s.studio.app.explore.NavigatorIntent.Retry)
      v.render(
        NavigatorPaneVM(
          "Trials",
          "Filter",
          "filter",
          "",
          "Trials",
          Vector(
            navRow(NavigatorRowKind.Participant, "P01"),
            navRow(NavigatorRowKind.Trial, "enc_01"),
            navRow(NavigatorRowKind.Trial, "enc_02")
          ),
          None,
          None,
          Vector.empty,
          ""
        )
      )
      v
    }
    fx.show(themed(view.node))
    fx.awaitLayout()
    runOnFx(view.rows.getSelectionModel.select(1))
    fx.awaitLayout()
    def labels(c: ListCell[?], style: String): Label = runOnFx(
      c.lookup(s".$style").asInstanceOf[Label]
    )
    val selected = cell(view.rows, 1)
    val plain    = cell(view.rows, 2)
    val group    = cell(view.rows, 0)
    assertEquals(drawn(selected)(fills(selected)), List(selFill))
    assertEquals(face(labels(selected, "nav-item")), SansSemibold)
    assertEquals(face(labels(selected, "nav-label")), MonoMedium)
    assertEquals(textFill(labels(selected, "nav-item")), ink)
    assertEquals(face(labels(plain, "nav-item")), "IBM Plex Sans")
    assertEquals(face(labels(plain, "nav-label")), "IBM Plex Mono")
    // A participant's label is semibold Sans already, selected or not.
    runOnFx(view.rows.getSelectionModel.select(0))
    fx.awaitLayout()
    assertEquals(face(labels(group, "nav-label")), SansSemibold)
    assertEquals(drawn(plain, Hover -> true)(fills(plain)), List(surface2))
    assertEquals(drawn(group, Hover -> true)(fills(group)), List(selFill))
    // In place, under the view's own sheet, the focused list's cursor row
    // keeps its frame around the selection fill (CompositeCursorFxSuite).
    runOnFx(view.rows.getFocusModel.focus(0))
    assertEquals(drawn(view.rows, Focused -> true)(fills(group)), List(accent, selFill))
  }

  private val view = right(ViewId.of("test.selection"))

  private val pairs: PlotSource = right(
    PlotSource(
      "Pairs",
      Vector(
        PlotColumn(right(ColumnId.of("query")), "Query", ColumnFormat.Label),
        PlotColumn(right(ColumnId.of("cosine")), "Cosine", ColumnFormat.Decimal(2))
      ),
      Vector.tabulate(4)(i =>
        PlotRow(
          StudioRef.Participant(f"P$i%02d"),
          Vector(PlotValue.Text(f"P$i%02d"), PlotValue.Number(i / 10.0))
        )
      )
    )
  )

  fxStage.test("a selected table row: the selection fill, Sans 600 and Mono 500 numerals") {
    fx =>
      val intents = ArrayBuffer.empty[Intent]
      val table   = runOnFx {
        val t = TableTwinView.attach(view, SelectionState.empty, intents += _)
        t.show(pairs)
        t
      }
      fx.show(themed(table))
      fx.awaitLayout()
      val ref   = pairs.rows(1).ref
      val empty = SelectionState.empty
      val input = SelectionInput(
        InputStamp(empty.context, view, 1L, InputCause.Pointer),
        SelectionMode.Replace,
        Vector(ref)
      )
      runOnFx(table.project(right(empty.submit(input))))
      fx.awaitLayout()
      def row(i: Int): Region          = runOnFx(table.rowNode(i).getOrElse(fail(s"no row $i")))
      def cells(i: Int): Vector[Label] = runOnFx(
        row(i).lookupAll(".table-twin-cell").asScala.toVector.collect { case l: Label => l }
      )
      assert(runOnFx(row(1).getPseudoClassStates.contains(Selected)))
      // The row's bottom rule, then its fill.
      assertEquals(drawn(row(1))(fills(row(1))), List(surface3, selFill))
      assertEquals(drawn(row(2))(fills(row(2))), List(surface3, surface))
      val Vector(name, number) = cells(1)
      assertEquals((face(name), face(number)), (SansSemibold, MonoMedium))
      assertEquals((textFill(name), textFill(number)), (ink, ink))
      val Vector(plainName, plainNumber) = cells(2)
      assertEquals((face(plainName), face(plainNumber)), ("IBM Plex Sans", "IBM Plex Mono"))
      assertEquals(drawn(row(2), Hover -> true)(fills(row(2))), List(surface3, surface2))
      assertEquals(drawn(row(1), Hover -> true)(fills(row(1))), List(surface3, selFill))
      runOnFx(table.dispose())
  }

  private def labelled(styles: String*): Label = runOnFx {
    val l = Label("enc_01")
    l.getStyleClass.addAll(styles*)
    l
  }

  private def box(styles: String*)(children: javafx.scene.Node*): HBox = runOnFx {
    val b = HBox(children*)
    b.getStyleClass.addAll(styles*)
    b
  }

  fxStage.test(
    "selected chips, toggles and rows draw the selection fill and the heavier face"
  ) { fx =>
    val chip = runOnFx {
      val b = ToggleButton("Eligible 12")
      b.getStyleClass.addAll("design-chip", "t11")
      b
    }
    val toggle = runOnFx {
      val b = ToggleButton("Fixations")
      b.getStyleClass.addAll("explore-toggle", "t12")
      b
    }
    val speed = runOnFx {
      val b = ToggleButton("1×")
      b.getStyleClass.addAll("timeline-speed", "mono", "t11")
      b
    }
    val figureTitle = labelled("t12")
    val figure      = runOnFx {
      val r = VBox(HBox(figureTitle), labelled("t11"))
      r.getStyleClass.addAll("figures-row", "figures-row-selected")
      r
    }
    val ledgerLabel = labelled("ledger-label", "t12")
    val ledgerCount = runOnFx {
      val b = Button("12")
      b.getStyleClass.addAll("ledger-count", "mono", "t12")
      b
    }
    val ledger      = box("ledger-row", "cause", "opened")(VBox(ledgerLabel), ledgerCount)
    val recordCell  = labelled("mono", "t11", "records-cell")
    val record      = box("records-row")(recordCell)
    val plainRecord = box("records-row")(labelled("mono", "t11", "records-cell"))
    // Nothing takes real focus: :focused is set where it is read.
    Vector(chip, toggle, speed, ledgerCount).foreach(n => runOnFx(n.setFocusTraversable(false)))
    fx.show(
      themed(
        VBox(chip, toggle, speed, figure, ledger, record, plainRecord),
        ResolvedDesignView.stylesheetResource,
        ExploreTrialViewPane.stylesheetResource,
        ExploreTimelineHost.stylesheetResource,
        FiguresHost.stylesheetResource,
        AdmissionLedgerView.stylesheetResource,
        TableTwinView.stylesheetResource
      )
    )
    fx.awaitLayout()
    runOnFx {
      Vector(chip, toggle, speed).foreach(_.setSelected(true))
      record.pseudoClassStateChanged(Selected, true)
    }
    val cases: Vector[(String, Region, List[Color], Labeled, String)] = Vector(
      ("design chip", chip, List(hairline, selFill), chip, SansSemibold),
      ("explore toggle", toggle, List(hairline, selFill), toggle, SansSemibold),
      ("timeline speed", speed, List(hairline, selFill), speed, MonoMedium),
      ("figure row", figure, List(selFill), figureTitle, SansSemibold),
      ("ledger row", ledger, List(surface3, selFill), ledgerLabel, SansSemibold),
      ("ledger count", ledger, List(surface3, selFill), ledgerCount, MonoMedium),
      ("source record", record, List(selFill), recordCell, MonoMedium)
    )
    cases.foreach { (clue, node, expected, text, family) =>
      assertEquals(drawn(node)(fills(node)), expected, clue)
      assertEquals(face(text), family, clue)
      if clue != "ledger count" then assertEquals(textFill(text), ink, clue)
    }
    // Hover: surface-2, and a selected record or figure stays selected.
    assertEquals(drawn(plainRecord, Hover -> true)(fills(plainRecord)), List(surface2))
    assertEquals(drawn(record, Hover -> true)(fills(record)), List(selFill))
    assertEquals(drawn(figure, Hover -> true)(fills(figure)), List(selFill))
    // Focused and selected shows both: the ring around the selection fill.
    assertEquals(drawn(speed, Focused -> true)(fills(speed)), List(accent, selFill))
    assertEquals(drawn(toggle, Focused -> true)(fills(toggle)), List(accent, selFill))
    assertEquals(drawn(chip, Focused -> true)(fills(chip)), List(accent, selFill))
    assertEquals(drawn(figure, Focused -> true)(fills(figure)), List(accent, selFill))
    assert(cases.forall((_, n, _, _, _) => !drawn(n)(fills(n)).contains(soft)))
  }

  /** `(selector, declarations)` of every rule in `css`, comments removed. */
  private def rules(css: String): Vector[(String, String)] =
    css
      .replaceAll("(?s)/\\*.*?\\*/", "")
      .split("}")
      .toVector
      .flatMap(r =>
        r.split("\\{", 2) match
          case Array(selector, body) => Some((selector.trim, body))
          case _                     => None
      )

  test("no stylesheet draws a selected or hovered state with the accent's soft fill") {
    // The shell stylesheet's directory holds every studio stylesheet.
    val shell =
      Option(getClass.getClassLoader.getResource(StudioStyles.shellStylesheetResource))
        .getOrElse(fail(s"no ${StudioStyles.shellStylesheetResource} on the classpath"))
    val sheets = Files
      .list(Paths.get(shell.toURI).getParent)
      .iterator
      .asScala
      .toVector
      .filter(_.getFileName.toString.endsWith(".css"))
    assert(sheets.size > 10, sheets)
    val states  = Vector(":selected", "-selected", ".opened", ":hover", ".current")
    val offends = for
      sheet            <- sheets
      (selector, body) <- rules(Files.readString(sheet))
      if states.exists(selector.contains) && body.contains("-es-accent-soft")
    yield s"${sheet.getFileName}: $selector"
    assertEquals(offends, Vector.empty)
  }
