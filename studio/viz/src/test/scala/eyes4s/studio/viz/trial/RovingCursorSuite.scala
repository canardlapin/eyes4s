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

package eyes4s.studio.viz.trial

import eyes4s.studio.app.Intent
import eyes4s.studio.app.text.TrialText
import eyes4s.studio.app.tokens.{StageVariant, Theme}
import eyes4s.studio.core.assets.Display
import eyes4s.studio.core.selection.{
  ContextRevision,
  FixationIndex,
  InputCause,
  InputStamp,
  SelectionInput,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}
import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.viz.plot.{PlotSurface, PlotTransform}
import eyes4s.studio.viz.trial.TrialSamples.*
import intaglio.interaction.NamedPicking
import intaglio.{DevicePoint, RenderPlan, value}
import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}

/** The roving cursor and trial input model (S4.2): picking resolves to one
  * fixation per mark (casing included), the resolved panel frame and its
  * inverse agree with the scene's transform, arrows go to the nearest mark on
  * that side, stacks never trap, every mark is reachable, Enter and Escape
  * are selection intents, projected selection emits nothing, and the focused
  * mark's accessible text comes from its semantic id.
  */
class RovingCursorSuite extends ScalaCheckSuite:

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val view = right(ViewId.of("trial.query"))

  private def scene(
      fixations: Vector[TrialFixation] = ret07Fixations,
      marks: MarkStyle = MarkStyle.Neutral,
      d: Display = Display.BlankWithFixationCross
  ): TrialScene =
    right(
      TrialScene(
        TrialSceneInput(
          display(ret07, d),
          screen,
          fixations,
          marks,
          Theme.Light,
          StageVariant.Dark
        )
      )
    )

  private def targetsOf(s: TrialScene, surface: PlotSurface): TrialTargets =
    val context = right(surface.renderContext(s.plot.id))
    val device  = right(RenderPlan(s.plot.scene, context).deviceScene)
    val picking = right(NamedPicking.compile(s.plot.scene, context))
    right(TrialTargets.resolve(s, device, picking, surface.deviceScale))

  private def targets(
      fixations: Vector[TrialFixation] = ret07Fixations,
      marks: MarkStyle = MarkStyle.Neutral,
      scale: Double = 1.0
  ): TrialTargets = targetsOf(scene(fixations, marks), right(PlotSurface(800, 600, scale)))

  private def fixationsAt(points: Seq[(Double, Double)]): Vector[TrialFixation] =
    points.zipWithIndex.map { case ((x, y), i) =>
      right(
        TrialFixation.of(ret07, right(FixationIndex.of(i + 1)), x, y, 200, MapPlacement.InMap)
      )
    }.toVector

  // ---------------------------------------------------------------------------
  // Frames and picking
  // ---------------------------------------------------------------------------

  test("marks are placed through the panel's resolved frame, and its inverse maps back") {
    for scale <- List(1.0, 2.0) do
      val s         = scene()
      val surface   = right(PlotSurface(800, 600, scale))
      val t         = targetsOf(s, surface)
      val transform = right(PlotTransform.resolve(s.plot, surface))
      assertEquals(t.frame.name.value, TrialScene.PanelName)
      assertEquals(t.targets.map(_.mark), s.marks)
      t.targets.foreach { target =>
        val want = right(transform.dataToDevice(target.mark.at))
        assertEqualsDouble(target.anchor.x, want.x, 1e-6)
        assertEqualsDouble(target.anchor.y, want.y, 1e-6)
        val back = right(t.toData(target.anchor))
        assertEqualsDouble(back.x, target.mark.at.x, 1e-6)
        assertEqualsDouble(back.y, target.mark.at.y, 1e-6)
        assertEqualsDouble(target.radiusDevicePx, target.mark.radiusPx * scale, 1e-9)
      }
  }

  test("a point outside the panel has no data position (typed, names the frame)") {
    val t = targets()
    t.toData(DevicePoint(-5.0, -5.0)) match
      case Left(TrialTargetError.Frame(id, what, _)) =>
        assertEquals(id, t.sceneId)
        assert(what.startsWith("device point (-5"), what)
      case other => fail(s"unexpected $other")
  }

  test("every mark is picked where it is drawn on top, at 1x and 2x, in every style") {
    for
      scale <- List(1.0, 2.0)
      style <- MarkStyle.Neutral +: TrialRole.values.toList.map(MarkStyle.Role(_))
    do
      val t                         = targets(marks = style, scale = scale)
      def hollowMark(m: MarkTarget) =
        m.mark.placement != MapPlacement.InMap || style == MarkStyle.Role(TrialRole.Control)
      def reaches(o: MarkTarget, p: DevicePoint) =
        math.hypot(o.anchor.x - p.x, o.anchor.y - p.y) <= (o.mark.reachPx + 0.5) * scale
      // The marks that may take `p` from `target`: any mark drawn later whose
      // reach covers `p`, and, inside a hollow mark (where nothing of it is
      // painted), any other mark whose reach covers `p`.
      def covering(target: MarkTarget, p: DevicePoint) = t.targets.filter { o =>
        val interior = hollowMark(target) &&
          math.hypot(target.anchor.x - p.x, target.anchor.y - p.y) < 0.9 * target.radiusDevicePx
        o.mark.order != target.mark.order && (o.mark.order > target.mark.order || interior) &&
        reaches(o, p)
      }
      t.targets.foreach { target =>
        // A filled mark is its whole area: probe its centre and half radius. A
        // hollow mark is picked on its outline and, through the centre
        // fallback, inside it: probe the centre, half radius and outline.
        val hollow          = hollowMark(target)
        def ring(r: Double) = (0 until 8).map { i =>
          val a = math.Pi * i / 4.0
          DevicePoint(target.anchor.x + r * math.cos(a), target.anchor.y + r * math.sin(a))
        }
        val probes = target.anchor +: (ring(0.5 * target.radiusDevicePx) ++
          (if hollow then ring(target.radiusDevicePx) else Vector.empty))
        val hits = probes.map(p => p -> right(t.pick(p, 0.5 * scale)).map(_.ref))
        // Each probe is this mark, or a mark drawn over it there.
        hits.foreach { (p, hit) =>
          assert(
            hit.contains(target.ref) || covering(target, p).exists(o => hit.contains(o.ref)),
            s"$style at ${scale}x: $p on ${target.ref} picked $hit"
          )
        }
        // An unoccluded probe always picks this mark.
        hits.filter((p, _) => covering(target, p).isEmpty).foreach { (p, hit) =>
          assertEquals(hit, Some(target.ref), s"$style at ${scale}x: $p")
        }
        assert(hits.exists(_._2.contains(target.ref)), s"$style at ${scale}x: ${target.ref}")
      }
  }

  test("inside two overlapping hollow rings, the later-drawn mark wins the centre fallback") {
    // Two 900 ms control rings (radius 12 px) 5 px apart: the earlier ring's
    // centre is inside both rings and on neither outline.
    val fs = Vector((900.0, 500.0), (905.0, 500.0)).zipWithIndex.map { case ((x, y), i) =>
      right(
        TrialFixation.of(ret07, right(FixationIndex.of(i + 1)), x, y, 900, MapPlacement.InMap)
      )
    }
    for scale <- List(1.0, 2.0) do
      val t                      = targets(fs, MarkStyle.Role(TrialRole.Control), scale)
      val Vector(earlier, later) = t.targets: @unchecked
      assert(
        math.hypot(later.anchor.x - earlier.anchor.x, later.anchor.y - earlier.anchor.y) <
          later.radiusDevicePx - 2.0 * scale
      )
      assertEquals(right(t.pick(earlier.anchor, 0.5 * scale)).map(_.ref), Some(later.ref))
      assertEquals(right(t.pick(later.anchor, 0.5 * scale)).map(_.ref), Some(later.ref))
      // On the earlier ring's own outline, away from the later ring, the
      // painted hit still wins.
      val onEarlier = DevicePoint(earlier.anchor.x - earlier.radiusDevicePx, earlier.anchor.y)
      assertEquals(right(t.pick(onEarlier, 0.5 * scale)).map(_.ref), Some(earlier.ref))
  }

  test("a hit on a control mark's casing resolves to that mark, not to a casing target") {
    for scale <- List(1.0, 2.0) do
      val t = targets(marks = MarkStyle.Role(TrialRole.Control), scale = scale)
      t.targets.filter(_.mark.placement == MapPlacement.InMap).foreach { target =>
        // 1.5 px outside the ring's centre line: on the halo casing, beyond the
        // 2 px outline itself.
        val onCasing = DevicePoint(
          target.anchor.x,
          target.anchor.y - target.radiusDevicePx - 1.5 * scale
        )
        val hit = right(t.pick(onCasing, 1.0 * scale))
        assertEquals(hit.map(_.ref), Some(target.ref))
      }
  }

  test("a pick on the image, the order lines or the stage is no fixation") {
    val t      = targets()
    val corner = t.frame.frame
    assertEquals(right(t.pick(DevicePoint(corner.x + 2.0, corner.y + 2.0), 0.0)), None)
    assertEquals(right(t.pick(DevicePoint(1.0, 1.0), 0.0)), None)
    assertEquals(
      t.pick(DevicePoint(1.0, 1.0), -1.0),
      Left(TrialTargetError.InvalidTolerance(t.sceneId, -1.0))
    )
  }

  // ---------------------------------------------------------------------------
  // Roving navigation
  // ---------------------------------------------------------------------------

  private val genPoints: Gen[Vector[(Double, Double)]] =
    for
      n  <- Gen.choose(1, 30)
      xs <- Gen.listOfN(n, Gen.oneOf(Gen.choose(500.0, 1400.0), Gen.const(900.0)))
      ys <- Gen.listOfN(n, Gen.oneOf(Gen.choose(200.0, 900.0), Gen.const(500.0)))
    yield xs.zip(ys).toVector

  private def oracle(t: TrialTargets, from: MarkTarget, move: RovingMove): Option[MarkTarget] =
    val a     = from.anchor
    val stack = t.targets.filter(o =>
      o.mark.order != from.mark.order && math.abs(o.anchor.x - a.x) <= TrialTargets.Epsilon &&
        math.abs(o.anchor.y - a.y) <= TrialTargets.Epsilon
    )
    val forward = move == RovingMove.Right || move == RovingMove.Down
    val inStack =
      if forward then stack.filter(_.mark.order > from.mark.order).minByOption(_.mark.order)
      else stack.filter(_.mark.order < from.mark.order).maxByOption(_.mark.order)
    inStack.orElse {
      val side = t.targets.filter { o =>
        val dx = o.anchor.x - a.x
        val dy = o.anchor.y - a.y
        move match
          case RovingMove.Right => dx > TrialTargets.Epsilon
          case RovingMove.Left  => dx < -TrialTargets.Epsilon
          case RovingMove.Down  => dy > TrialTargets.Epsilon
          case _                => dy < -TrialTargets.Epsilon
      }
      val best = side.map(o => math.hypot(o.anchor.x - a.x, o.anchor.y - a.y)).minOption
      best.flatMap(d =>
        side
          .filter(o => math.hypot(o.anchor.x - a.x, o.anchor.y - a.y) == d)
          .minByOption(_.mark.order)
      )
    }

  property("an arrow goes to the nearest mark strictly on that side; a stack steps in order") {
    Prop.forAll(genPoints) { points =>
      val t = targets(fixationsAt(points))
      for
        from <- t.targets
        move <- List(RovingMove.Left, RovingMove.Right, RovingMove.Up, RovingMove.Down)
      do assertEquals(t.step(Some(from.ref), move), oracle(t, from, move), s"$move from $from")
      true
    }
  }

  property(
    "Next reaches every mark in fixation order and Previous returns; stacks never trap"
  ) {
    Prop.forAll(genPoints) { points =>
      val t       = targets(fixationsAt(points))
      val forward =
        Iterator
          .iterate(t.step(None, RovingMove.Next))(
            _.flatMap(m => t.step(Some(m.ref), RovingMove.Next))
          )
          .take(t.targets.size + 1)
          .takeWhile(_.isDefined)
          .flatten
          .toVector
      assertEquals(forward.map(_.ref), t.targets.map(_.ref))
      val back = Iterator
        .iterate(t.step(None, RovingMove.Last))(
          _.flatMap(m => t.step(Some(m.ref), RovingMove.Previous))
        )
        .take(t.targets.size + 1)
        .takeWhile(_.isDefined)
        .flatten
        .toVector
      assertEquals(back.map(_.ref), t.targets.map(_.ref).reverse)
      // Right through a stack visits every stacked mark before leaving it.
      t.targets.groupBy(m => (m.anchor.x, m.anchor.y)).values.filter(_.size > 1).foreach {
        stack =>
          val first   = stack.minBy(_.mark.order)
          val visited = Iterator
            .iterate(Option(first))(_.flatMap(m => t.step(Some(m.ref), RovingMove.Right)))
            .take(stack.size + 1)
            .takeWhile(_.exists(s => stack.contains(s)))
            .flatten
            .toVector
          assertEquals(visited.map(_.ref), stack.sortBy(_.mark.order).map(_.ref))
      }
      true
    }
  }

  test("with no focus the cursor starts at the first mark (End: the last)") {
    val t = targets()
    RovingMove.values.foreach { move =>
      val want = if move == RovingMove.Last then t.targets.last else t.targets.head
      assertEquals(t.step(None, move), Some(want), move.toString)
    }
    assertEquals(t.step(Some(t.targets.last.ref), RovingMove.Next), None)
    assertEquals(t.step(Some(t.targets.head.ref), RovingMove.Previous), None)
  }

  test("every golden mark is reachable by arrows alone from the first mark") {
    for style <- List(MarkStyle.Neutral, MarkStyle.Role(TrialRole.Matched)) do
      val t      = targets(marks = style)
      val arrows = List(RovingMove.Left, RovingMove.Right, RovingMove.Up, RovingMove.Down)
      @annotation.tailrec
      def reach(seen: Set[StudioRef], frontier: List[MarkTarget]): Set[StudioRef] =
        frontier match
          case Nil          => seen
          case next :: rest =>
            val out = arrows.flatMap(m => t.step(Some(next.ref), m)).filterNot(o => seen(o.ref))
            reach(seen ++ out.map(_.ref), rest ++ out)
      assertEquals(
        reach(Set(t.targets.head.ref), List(t.targets.head)),
        t.targets.map(_.ref).toSet
      )
  }

  // ---------------------------------------------------------------------------
  // The input state: intents, projection, overlay, accessible text
  // ---------------------------------------------------------------------------

  private def key(k: RovingKey) = TrialInputEvent.Key(k)

  private def run(
      state: TrialInputState,
      t: TrialTargets,
      events: TrialInputEvent*
  ): (TrialInputState, Vector[Intent]) =
    events.foldLeft((state, Vector.empty[Intent])) { case ((s, out), e) =>
      val step = right(s.handle(e, t, 1.0))
      (step.state, out ++ step.intents)
    }

  test("Enter selects the focused mark through a stamped intent; Escape clears") {
    val t        = targets()
    val initial  = TrialInputState.initial(view, SelectionState.empty)
    val (s1, i1) =
      run(initial, t, key(RovingKey.Move(RovingMove.Next)), key(RovingKey.Activate(false)))
    val first = t.targets.head.ref
    assertEquals(s1.focus, Some(first))
    assertEquals(
      i1,
      Vector(
        Intent.Select(
          SelectionInput(
            InputStamp(ContextRevision(0), view, 0L, InputCause.Keyboard),
            SelectionMode.Replace,
            Vector(first)
          )
        )
      )
    )
    val (_, i2) = run(s1, t, key(RovingKey.Activate(true)), key(RovingKey.Clear))
    assertEquals(
      i2,
      Vector(
        Intent.Select(
          SelectionInput(
            InputStamp(ContextRevision(0), view, 1L, InputCause.Keyboard),
            SelectionMode.Toggle,
            Vector(first)
          )
        ),
        Intent.Select(
          SelectionInput(
            InputStamp(ContextRevision(0), view, 2L, InputCause.Keyboard),
            SelectionMode.Clear,
            Vector.empty
          )
        )
      )
    )
    // Nothing focused: Enter selects nothing.
    assertEquals(run(initial, t, key(RovingKey.Activate(false)))._2, Vector.empty)
  }

  test("keyboard alone selects any mark, and the bus accepts every input") {
    val t = targets(enc03Fixations)
    t.targets.foreach { target =>
      var bus                    = SelectionState.empty
      var state: TrialInputState = TrialInputState.initial(view, bus)
      val presses                =
        key(RovingKey.Move(RovingMove.First)) +:
          Vector.fill(target.mark.order)(key(RovingKey.Move(RovingMove.Next))) :+
          key(RovingKey.Activate(false))
      presses.foreach { e =>
        val step = right(state.handle(e, t, 1.0))
        state = step.state
        step.intents.foreach {
          case Intent.Select(input) =>
            bus = right(bus.submit(input))
            state = state.project(bus).state
          case other => fail(s"unexpected intent $other")
        }
      }
      assertEquals(bus.selected, Vector(target.ref))
    }
  }

  test("a click picks, focuses and selects; a modifier click toggles; hover is local") {
    val t       = targets()
    val target  = t.targets(3)
    val initial = TrialInputState.initial(view, SelectionState.empty)
    val moved   = right(initial.handle(TrialInputEvent.PointerMoved(target.anchor), t, 1.0))
    assertEquals(moved.intents, Vector(Intent.HoverOver(view, Some(target.ref))))
    assertEquals(moved.state.hover, Some(target.ref))
    // The same hover again is no change and no intent.
    val again = right(moved.state.handle(TrialInputEvent.PointerMoved(target.anchor), t, 1.0))
    assertEquals((again.intents, again.redraw), (Vector.empty, false))
    val clicked =
      right(moved.state.handle(TrialInputEvent.PointerClicked(target.anchor, true), t, 1.0))
    assertEquals(clicked.state.focus, Some(target.ref))
    clicked.intents match
      case Vector(Intent.Select(SelectionInput(stamp, SelectionMode.Toggle, Vector(ref)))) =>
        assertEquals(ref, target.ref)
        assertEquals(stamp.cause, InputCause.Pointer)
      case other => fail(s"unexpected $other")
    val left = right(clicked.state.handle(TrialInputEvent.PointerExited, t, 1.0))
    assertEquals(left.intents, Vector(Intent.HoverOver(view, None)))
    // A click on no mark changes nothing.
    val miss = right(
      initial.handle(TrialInputEvent.PointerClicked(DevicePoint(1.0, 1.0), false), t, 1.0)
    )
    assertEquals((miss.intents, miss.state), (Vector.empty, initial))
  }

  test("a projected selection redraws without any intent, and only when it changes") {
    val t     = targets()
    val ref   = t.targets(2).ref
    val other = right(ViewId.of("compare.ladder"))
    val bus   = right(
      SelectionState.empty.submit(
        SelectionInput(
          InputStamp(ContextRevision(0), other, 0L, InputCause.Pointer),
          SelectionMode.Replace,
          Vector(ref)
        )
      )
    )
    val initial = TrialInputState.initial(view, SelectionState.empty)
    val step    = initial.project(bus)
    assertEquals((step.intents, step.redraw), (Vector.empty, true))
    assertEquals(
      step.state.overlay(t).map(r => (r.kind, r.ref)),
      Vector((RingKind.Selected, ref))
    )
    val same = step.state.project(bus)
    assertEquals((same.intents, same.redraw), (Vector.empty, false))
  }

  test("a view re-attached under the same id continues after its last applied input") {
    val t        = targets()
    val first    = TrialInputState.initial(view, SelectionState.empty)
    val (_, out) =
      run(first, t, key(RovingKey.Move(RovingMove.First)), key(RovingKey.Activate(false)))
    val bus = out.foldLeft(SelectionState.empty) {
      case (b, Intent.Select(input)) => right(b.submit(input))
      case (b, _)                    => b
    }
    val (_, again) = run(
      TrialInputState.initial(view, bus),
      t,
      key(RovingKey.Move(RovingMove.Last)),
      key(RovingKey.Activate(false))
    )
    again.foreach {
      case Intent.Select(input) => assert(bus.submit(input).isRight, input.toString)
      case other                => fail(s"unexpected $other")
    }
  }

  test("the focus ring shows only while focused, outside a selected mark's selection ring") {
    val t      = targets(scale = 2.0)
    val target = t.targets(4)
    val (s, _) = run(
      TrialInputState.initial(view, SelectionState.empty),
      t,
      key(RovingKey.Move(RovingMove.First)),
      key(RovingKey.Move(RovingMove.Next)),
      key(RovingKey.Move(RovingMove.Next)),
      key(RovingKey.Move(RovingMove.Next)),
      key(RovingKey.Move(RovingMove.Next))
    )
    assertEquals(s.focus, Some(target.ref))
    assertEquals(s.overlay(t), Vector.empty)
    val focused = s.focusChanged(true)
    assert(focused.redraw)
    val ring = focused.state.overlay(t) match
      case Vector(r) => r
      case other     => fail(s"unexpected $other")
    assertEquals(
      (ring.kind, ring.ref, ring.centre),
      (RingKind.Focus, target.ref, target.anchor)
    )
    assertEqualsDouble(ring.radius, (target.mark.reachPx + OverlayRings.GapPx) * 2.0, 1e-9)
    val bus = right(
      SelectionState.empty.submit(
        SelectionInput(
          InputStamp(ContextRevision(0), view, 0L, InputCause.Keyboard),
          SelectionMode.Replace,
          Vector(target.ref)
        )
      )
    )
    val both = focused.state.project(bus).state.overlay(t)
    assertEquals(both.map(_.kind), Vector(RingKind.Selected, RingKind.Focus))
    assertEqualsDouble(
      both(1).radius - both(0).radius,
      2.0 * OverlayRings.SelectedBandPx * 2.0,
      1e-9
    )
  }

  test("the focused mark's accessible text comes from its semantic id") {
    val t      = targets()
    val (s, _) = run(
      TrialInputState.initial(view, SelectionState.empty),
      t,
      key(RovingKey.Move(RovingMove.First)),
      key(RovingKey.Move(RovingMove.Next))
    )
    assertEquals(s.accessibleText(ret07, t), "Fixation 2 of P17 · ret_07 · in map")
    val selected = s.project(
      right(
        SelectionState.empty.submit(
          SelectionInput(
            InputStamp(ContextRevision(0), view, 0L, InputCause.Keyboard),
            SelectionMode.Replace,
            Vector(t.targets(1).ref)
          )
        )
      )
    )
    assertEquals(
      selected.state.accessibleText(ret07, t),
      "Fixation 2 of P17 · ret_07 · in map, selected"
    )
    assert(
      TrialInputState
        .initial(view, SelectionState.empty)
        .accessibleText(ret07, t)
        .startsWith("Fixations of P17 · ret_07. Arrow keys")
    )
  }

  test("focused-mark text names each core placement") {
    val ref: StudioRef.Fixation = StudioRef.Fixation(ret07, right(FixationIndex.of(1)))
    assertEquals(
      TrialText.mark(ref, MapPlacement.InMap, false),
      "Fixation 1 of P17 · ret_07 · in map"
    )
    assertEquals(
      TrialText.mark(ref, MapPlacement.DroppedInitial, false),
      "Fixation 1 of P17 · ret_07 · dropped by initial-fixation policy"
    )
    assertEquals(
      TrialText.mark(ref, MapPlacement.OutsideScreen, false),
      "Fixation 1 of P17 · ret_07 · outside screen"
    )
    assertEquals(
      TrialText.mark(ref, MapPlacement.OutsideWindow(OffWindowPolicy.Exclude), false),
      "Fixation 1 of P17 · ret_07 · outside window, excluded from map"
    )
    assertEquals(
      TrialText.mark(ref, MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial), false),
      "Fixation 1 of P17 · ret_07 · outside window, trial fails"
    )
  }

  test("new targets drop a focus and hover the trial no longer draws") {
    val t      = targets()
    val (s, _) = run(
      TrialInputState.initial(view, SelectionState.empty),
      t,
      TrialInputEvent.PointerMoved(t.targets.last.anchor),
      key(RovingKey.Move(RovingMove.Last))
    )
    assertEquals(s.focus, Some(t.targets.last.ref))
    val fewer = targets(ret07Fixations.take(3))
    val step  = s.retarget(fewer)
    assertEquals((step.state.focus, step.state.hover), (None, None))
    assertEquals(step.intents, Vector(Intent.HoverOver(view, None)))
    val kept = s.retarget(t)
    assertEquals((kept.state.focus, kept.intents), (s.focus, Vector.empty))
  }
