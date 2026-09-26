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

package eyes4s.sessionconsumer

import eyes4s.design.*
import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.compiletime.testing.typeCheckErrors

class SessionSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                               = right(Frame.screen("display", 100, 100))
  private val clock                               = ClockId("tracker")
  private val key                                 = right(SessionKey.of("trial"))
  private val second                              = right(SessionKey.of("second"))
  private val absent                              = right(SessionKey.of("absent"))
  private val empty                               = Session.empty(frame)
  private def rec(f: Frame[Px] = frame, c: ClockId = clock): Recording[Px] =
    right(
      Recording.of(
        f,
        c,
        Rate.Fixed(right(Hz(1000))),
        Eye.Left,
        None,
        IArray(Sample(Instant.millis(0), Gaze.Tracked(Pt[Px](1, 1), None)))
      )
    )
  private def grid(f: Frame[Px] = frame, id: String = "grid", nx: Int = 2): Grid[Px] =
    right(Grid.of(GridId(id), f, nx, 2))
  private def areas(f: Frame[Px] = frame): AoiSet[Px] =
    right(
      AoiSet.of(
        Vector(right(Aoi.of("a", "area", f, right(Region.rect(Pt[Px](0, 0), Pt[Px](10, 10))))))
      )
    )

  test(
    "empty container and absent lookups are total; blank keys are rejected without trimming"
  ) {
    assertEquals(empty.recording(key), None)
    assertEquals(empty.aoiSet(key), None)
    assertEquals(empty.grid(key), None)
    assertEquals(empty.recordings, Vector.empty)
    assertEquals(empty.aoiSets, Vector.empty)
    assertEquals(empty.grids, Vector.empty)
    assert(SessionKey.of("  ").isLeft)
    assertEquals(right(SessionKey.of(" trial ")).value, " trial ")
    assert(AoiSet.of(Vector.empty[Aoi[Px]]).isLeft)
    assert(SessionError.InvalidKey(" ").message.contains("' '"))
  }

  test("role-local insertion returns exactly the admitted objects and rejects duplicates") {
    val r = rec(); val a = areas(); val g = grid()
    val s = right(right(right(empty.addRecording(key, r)).addAoiSet(key, a)).addGrid(key, g))
    assertEquals(s.recording(key), Some(r))
    assertEquals(s.aoiSet(key), Some(a))
    assertEquals(s.grid(key), Some(g))
    assertEquals(
      s.addRecording(key, r),
      Left(SessionError.DuplicateKey(SessionRole.Recording, key))
    )
    assertEquals(s.addAoiSet(key, a), Left(SessionError.DuplicateKey(SessionRole.AoiSet, key)))
    assertEquals(s.addGrid(key, g), Left(SessionError.DuplicateKey(SessionRole.Grid, key)))
    assertEquals(empty.recording(key), None)
    s.recordings.foreach { case (_, value) =>
      assert(Agreement.frames(s.frame, value.frame).isRight)
    }
    s.aoiSets.foreach { case (_, value) =>
      assert(Agreement.frames(s.frame, value.frame).isRight)
    }
    s.grids.foreach { case (_, value) =>
      assert(Agreement.frames(s.frame, value.frame).isRight)
    }
  }

  test("every admission refuses foreign frame identity and conflicting bounds or axes") {
    val foreign = right(Frame.screen("other", 100, 100))
    val bounds  = right(Frame.screen("display", 200, 100))
    val axes    = Frame.of(frame.id, frame.bounds, YAxis.Up)
    Vector(foreign, bounds, axes).foreach { f =>
      val errors = Vector(
        empty.addRecording(key, rec(f)),
        empty.addAoiSet(key, areas(f)),
        empty.addGrid(key, grid(f))
      )
      errors.foreach { result =>
        assert(result.isLeft)
        val error = result.swap.toOption.get
        assert(error.isInstanceOf[SessionError.Frame])
        assert(error.message.contains(key.value))
        assert(error.message.contains("display"))
      }
    }
  }

  test("independently acquired recordings keep their own clocks in one shared frame") {
    assert(typeCheckErrors("Session.empty(frame, clock)").nonEmpty)
    val first = rec(c = ClockId("tracker-a"))
    val other = rec(c = ClockId("tracker-b"))
    val s     = right(right(empty.addRecording(key, first)).addRecording(second, other))
    assertEquals(s.recording(key).map(_.clock), Some(ClockId("tracker-a")))
    assertEquals(s.recording(second).map(_.clock), Some(ClockId("tracker-b")))
    // Membership is not a synchronisation claim: the clocks remain distinct timelines.
    assert(Agreement.clocks(first.clock, other.clock).isLeft)
    assertEquals(
      right(s.replaceRecording(key, rec(c = ClockId("tracker-c")))).recordings.size,
      2
    )
  }

  test("grid aliases agree in specification; distinct discretisations remain admissible") {
    val s     = right(empty.addGrid(key, grid()))
    val alias = right(s.addGrid(second, grid()))
    assertEquals(alias.grids.map(_._1), Vector(key, second))
    assert(s.addGrid(second, grid(nx = 3)).isLeft)
    val conflict = alias.replaceGrid(key, grid(nx = 3)).swap.toOption.get
    assert(conflict.isInstanceOf[SessionError.GridIdentity])
    assert(conflict.message.contains(second.value))
    assert(right(s.addGrid(second, grid(id = "fine", nx = 3))).grids.size == 2)
    assertEquals(right(s.replaceGrid(key, grid(nx = 3))).grid(key).map(_.nx), Some(3))
  }

  test(
    "replacement preserves order and invalid or absent replacements preserve original state"
  ) {
    val s = right(
      right(right(empty.addRecording(key, rec())).addAoiSet(key, areas())).addGrid(key, grid())
    )
    val two = right(
      right(right(s.addRecording(second, rec())).addAoiSet(second, areas()))
        .addGrid(second, grid(id = "fine"))
    )
    val changed = right(
      right(right(two.replaceRecording(key, rec())).replaceAoiSet(key, areas()))
        .replaceGrid(key, grid(nx = 3))
    )
    assertEquals(changed.recordings.map(_._1), Vector(key, second))
    assertEquals(changed.aoiSets.map(_._1), Vector(key, second))
    assertEquals(changed.grids.map(_._1), Vector(key, second))
    val foreign = right(Frame.screen("foreign", 100, 100))
    assert(two.replaceRecording(key, rec(foreign)).isLeft)
    assertEquals(
      right(two.replaceRecording(key, rec(c = ClockId("foreign")))).recording(key).map(_.clock),
      Some(ClockId("foreign"))
    )
    assert(two.replaceAoiSet(key, areas(foreign)).isLeft)
    assert(two.replaceGrid(key, grid(foreign)).isLeft)
    assertEquals(
      two.replaceRecording(absent, rec()),
      Left(SessionError.MissingKey(SessionRole.Recording, absent))
    )
    assertEquals(
      two.replaceAoiSet(absent, areas()),
      Left(SessionError.MissingKey(SessionRole.AoiSet, absent))
    )
    assertEquals(
      two.replaceGrid(absent, grid()),
      Left(SessionError.MissingKey(SessionRole.Grid, absent))
    )
    assertEquals(two.grid(key).map(_.nx), Some(2))
    assertEquals(two.recording(key).map(_.clock), Some(clock))
    assertEquals(two.aoiSet(key).map(_.frame), Some(frame))
  }

  test("removal is checked, stable and persistent for every role") {
    val s = right(
      right(right(empty.addRecording(key, rec())).addAoiSet(key, areas())).addGrid(key, grid())
    )
    val two = right(
      right(right(s.addRecording(second, rec())).addAoiSet(second, areas()))
        .addGrid(second, grid(id = "fine"))
    )
    val removed =
      right(right(right(two.removeRecording(key)).removeAoiSet(key)).removeGrid(key))
    assertEquals(removed.recordings.map(_._1), Vector(second))
    assertEquals(removed.aoiSets.map(_._1), Vector(second))
    assertEquals(removed.grids.map(_._1), Vector(second))
    assertEquals(
      removed.removeRecording(key),
      Left(SessionError.MissingKey(SessionRole.Recording, key))
    )
    assertEquals(
      removed.removeAoiSet(key),
      Left(SessionError.MissingKey(SessionRole.AoiSet, key))
    )
    assertEquals(removed.removeGrid(key), Left(SessionError.MissingKey(SessionRole.Grid, key)))
    val cleared = right(
      right(right(removed.removeRecording(second)).removeAoiSet(second)).removeGrid(second)
    )
    assert(cleared.recordings.isEmpty && cleared.aoiSets.isEmpty && cleared.grids.isEmpty)
    assert(two.recording(key).nonEmpty && two.aoiSet(key).nonEmpty && two.grid(key).nonEmpty)
  }

  test("external consumers cannot forge construction, keys, units or mutable access") {
    assert(
      typeCheckErrors(
        """new eyes4s.design.Session(null, Vector.empty, Vector.empty, Vector.empty)"""
      ).nonEmpty
    )
    assert(typeCheckErrors("""val key: eyes4s.design.SessionKey = "forged"""").nonEmpty)
    assert(typeCheckErrors("""
      import eyes4s.design.*
      import eyes4s.kernel.*
      val frame = Frame.screen("px", 100, 100).toOption.get
      val angular = Frame.angular("deg", 10, 10).toOption.get
      val s = Session.empty(frame)
      s.addGrid(SessionKey.of("key").toOption.get, Grid.over(angular, 2, 2).toOption.get)
    """).nonEmpty)
    assert(typeCheckErrors("""
      def mutate(s: eyes4s.design.Session[eyes4s.kernel.Unit2D.Px], k: eyes4s.design.SessionKey): Unit =
        s.recording(k).foreach(r => r.samples(0) = r.samples(0))
    """).nonEmpty)
    assert(typeCheckErrors("""
      def forge(s: eyes4s.design.Session[eyes4s.kernel.Unit2D.Px]): Unit =
        s.recordings = Vector.empty
    """).nonEmpty)
  }

  test("safe array conversion and persistent collections cannot mutate admitted data") {
    val source = Array(Sample(Instant.millis(0), Gaze.Tracked(Pt[Px](1, 1), None)))
    val r      = right(
      Recording.of(
        frame,
        clock,
        Rate.Fixed(right(Hz(1000))),
        Eye.Left,
        None,
        IArray.from(source)
      )
    )
    val s = right(empty.addRecording(key, r))
    source(0) = Sample(Instant.millis(999), Gaze.Lost[Px]())
    assertEquals(s.recording(key).map(_.samples(0).t), Some(Instant.millis(0)))
    val outside = s.recordings.updated(0, second -> rec())
    assertEquals(outside.head._1, second)
    assertEquals(s.recordings.head._1, key)
  }
