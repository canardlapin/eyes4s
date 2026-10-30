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

import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.app.plot.RovingMove
import eyes4s.studio.viz.plot.{MarkInputState, RovingTarget, RovingTargets}
import javafx.scene.input.KeyCode
import javafx.scene.{AccessibleRole, Node}

/** The one resting focus ring each 2026-10-11 board depicts (its `.kf`
  * node, REVIEW ticket 28), on the desktop node that carries it. The
  * desktop draws no ring at rest: a parity moment reaches that node by
  * keyboard ([[KeyboardFocus.byKeyboard]], then Home and Page Down to the
  * mark a board's cursor rests on) before its snapshot, so the snapshot shows the ring the
  * board shows.
  *
  * Boards whose `.kf` node has no desktop counterpart, so no moment takes
  * a resting focus for them:
  *  - Data.dc.html: the column mapping's FixStart row, which carries the
  *    row cursor; the desktop's mapping rows are not Tab stops.
  *  - MainDark.dc.html: depicts none (the dark Compare snapshots take none).
  *  - System.dc.html: a specimen sheet; FocusVisibleFxSuite and
  *    ControlsFxSuite check the rings it draws.
  * DataEmpty's shared preset group now has a composite Tab stop; its own
  * first-run suites cover it rather than this populated-story helper.
  * And Main.dc.html's ring is the reference trial's fixation cursor: the
  * desktop's Compare trial panels carry no cursor, so its counterpart is
  * the Reference trial pane's own ring, on the pane's edge.
  */
object RestingFocus:

  /** A board with a resting focus ring on the desktop. */
  enum Board(val stop: Stop):

    /** Main.dc.html (t2, Compare query): the Reference trial pane. */
    case Main extends Board(Stop.named(AccessibleRole.IMAGE_VIEW, "Reference trial"))

    /** Explore.dc.html (t2 Explore): the trial canvas, its cursor on
      * fixation 6.
      */
    case Explore
        extends Board(
          Stop
            .named(AccessibleRole.PARENT, "Fixations of P17 · enc_03")
            .withCursor(
              w => (w.explore.input.state, w.explore.input.targets),
              {
                case StudioRef.Fixation(_, i) => i.value == 6
                case _                        => false
              }
            )
        )

    /** Analysis.dc.html (t2 Analysis): the scale chip "8° · new". */
    case Analysis extends Board(Stop.named(AccessibleRole.RADIO_BUTTON, "8° · new"))

    /** Figures.dc.html (t2 Figures): panel D's frame on the page. */
    case Figures
        extends Board(Stop.named(AccessibleRole.BUTTON, "Panel D, Participant D by response"))

    /** Results.dc.html (Compare summary): the participant plot, its cursor on
      * P17's Remembered mark, the first of P17's.
      */
    case Results
        extends Board(
          Stop
            .named(AccessibleRole.PARENT, "Participant plot of Participant D")
            .withCursor(
              w =>
                (
                  w.summary.participantPlot.input.state,
                  w.summary.participantPlot.input.targets
                ),
              {
                case r: StudioRef.ReportParticipant => r.participant == "P17"
                case _                              => false
              }
            )
        )

  /** AppShellFxSuite's boards, by its names for them. */
  def appShell(board: String): Option[Board] = board match
    case "Explore"         => Some(Board.Explore)
    case "Analysis"        => Some(Board.Analysis)
    case "Compare summary" => Some(Board.Results)
    case "Compare query"   => Some(Board.Main)
    case "Figures"         => Some(Board.Figures)
    case _                 => None

  /** ShellMomentsFxSuite's moments that show a board's state. Its t2
    * Explore comes from Compare and explores no trial, unlike the board.
    */
  def moment(story: StoryMoment, p: Perspective): Option[Board] = (story, p) match
    case (StoryMoment.T2, Perspective.Results)  => Some(Board.Main)
    case (StoryMoment.T2, Perspective.Analysis) => Some(Board.Analysis)
    case (StoryMoment.T2, Perspective.Report)   => Some(Board.Figures)
    case (StoryMoment.T3, Perspective.Results)  => Some(Board.Results)
    case _                                      => None

  /** A composite's cursor as a window holds it: its input state and the
    * marks of its drawn frame.
    */
  type Composite = StudioWindow => (MarkInputState[?], Option[RovingTargets[?, ?]])

  /** Where a board rests a composite's cursor: the first mark, in the
    * composite's order, whose ref `on` accepts.
    */
  final case class Cursor(composite: Composite, on: StudioRef => Boolean)

  /** A Tab stop as the tab-order goldens name it (its role and the start of
    * its accessible name), and the cursor the board rests in it, if any.
    */
  final case class Stop(role: AccessibleRole, name: String, cursor: Option[Cursor]):
    def matches(n: Node): Boolean =
      n.getAccessibleRole == role && Option(n.getAccessibleText).exists(_.startsWith(name))
    def withCursor(c: Composite, on: StudioRef => Boolean): Stop =
      copy(cursor = Some(Cursor(c, on)))
    override def toString: String = s"$role '$name…'"

  object Stop:
    def named(role: AccessibleRole, name: String): Stop = Stop(role, name, None)

  /** The marks of `targets` in order: Home, then Page Down to the last. */
  private def marks(targets: RovingTargets[?, ?]): Vector[RovingTarget[?]] =
    val out  = Vector.newBuilder[RovingTarget[?]]
    var next = targets.step(None, RovingMove.First)
    var last = -1
    while next.exists(_.order > last) do
      next.foreach { m =>
        out += m
        last = m.order
        next = targets.step(Some(m.ref), RovingMove.Next)
      }
    out.result()

  private val TimeoutNanos = 20_000_000_000L

  /** Puts keyboard focus on `board`'s resting node in `w`, once its stop is
    * shown (a plot takes its name when drawn): Tab to the stop, then Home and
    * Page Down to the mark of the board's cursor.
    */
  def show(fx: FxStage, w: StudioWindow, board: Board): Unit =
    val s        = board.stop
    val deadline = System.nanoTime + TimeoutNanos
    def find     = fx.runOnFx(A11yChecks.focusable(w.root).find(s.matches))
    var found    = find
    while found.isEmpty do
      if System.nanoTime > deadline then
        val stops = fx.runOnFx(A11yChecks.focusable(w.root).map(_.getAccessibleText))
        throw AssertionError(s"$board: no Tab stop $s among $stops")
      Thread.sleep(25)
      fx.awaitLayout()
      found = find
    found.foreach(n => fx.runOnFx(KeyboardFocus.byKeyboard(n)))
    fx.awaitLayout()
    // The marks may still be redrawn (another scale arriving): the cursor
    // is placed again until it holds the board's mark on the marks drawn.
    s.cursor.foreach { c =>
      def mark: Option[RovingTarget[?]] =
        fx.runOnFx(c.composite(w)._2.toVector.flatMap(marks).find(m => c.on(m.ref)))
      def placed: Boolean =
        mark.exists(m => fx.runOnFx(c.composite(w)._1.focus).contains(m.ref))
      // Placed, and still placed after another frame has had time to come.
      def settled: Boolean = placed && {
        Thread.sleep(250)
        fx.awaitLayout()
        placed
      }
      while !settled do
        if System.nanoTime > deadline then
          val all = fx.runOnFx(c.composite(w)._2.toVector.flatMap(marks).map(_.ref))
          throw AssertionError(s"$board: the cursor does not rest on its mark among $all")
        mark.foreach { m =>
          fx.robot.press(KeyCode.HOME)
          (0 until m.order).foreach(_ => fx.robot.press(KeyCode.PAGE_DOWN))
        }
        if !placed then
          Thread.sleep(25)
          fx.awaitLayout()
    }

  /** Moves the focus to the scene root, which draws no ring: for a board
    * that depicts none (the dark Main board).
    */
  def clear(fx: FxStage, w: StudioWindow): Unit =
    fx.runOnFx(w.root.requestFocus())
    fx.awaitLayout()
