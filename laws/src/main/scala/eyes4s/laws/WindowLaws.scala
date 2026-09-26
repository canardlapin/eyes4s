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

package eyes4s.laws

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Laws for analysis windows, declared angular scales, recorded coordinate
  * corrections and per-trial window tallies.
  *
  * ==Generators straddle the edges==
  *
  * A window's containment is half-open, so the positions that decide whether
  * an implementation is right are exactly the ones on and next to its edges:
  * the minimum corner (inside), the maximum edge (outside), the largest
  * double below it (inside) and the largest double below the minimum
  * (outside). [[WindowLaws.straddling]] draws those deliberately, alongside
  * uniform interior and exterior positions, so a closed interval or a
  * clamped edge cannot pass by luck. Windows have integral pixel corners, as
  * display geometry does, and sometimes coincide with their parent frame.
  */
trait WindowLaws extends Laws:
  import WindowLaws.*

  /** Containment, entry and exit of a window of its parent frame. */
  def subframe(tol: Tolerance = Tolerance.exactish): RuleSet =
    new SimpleRuleSet(
      "subframe",
      "inside exactly when the parent region contains the position (half-open)" -> forAll(
        genPlaced
      ) { case (w, p) =>
        Prop(w.locate(p).isInside == w.region.contains(p)) :| s"${w.region} at $p"
      },
      "an inside position lies in the window's own frame" -> forAll(genPlaced) { case (w, p) =>
        w.locate(p) match
          case HalfOpenPlacement.Inside(local) => Prop(w.frame.contains(local))
          case HalfOpenPlacement.Outside(q)    => Prop(q == p)
      },
      "entering agrees with the located position and exit returns it" -> forAll(genPlaced) {
        case (w, p) =>
          (w.enter(p), w.locate(p)) match
            case (Some(local), HalfOpenPlacement.Inside(located)) =>
              Prop(tol.approxEquals(local, located)) && Prop(
                w.exit(located).exists(back => tol.approxEquals(back, p))
              )
            case (Some(_), HalfOpenPlacement.Outside(q)) => Prop(q == p)
            case (None, _) => Prop(false) :| "translation undefined"
      },
      "the window's frame has the region's extent and the parent's axis" -> forAll(genWindow) {
        w =>
          Prop(w.frame.bounds.width == w.region.width) &&
          Prop(w.frame.bounds.height == w.region.height) &&
          Prop(w.frame.yAxis == w.parent.yAxis) && Prop(w.frame.id != w.parent.id)
      },
      "a region reaching outside the parent is refused, naming both" -> forAll(genWindow) { w =>
        val beyond =
          Bounds.of[Px](w.region.xMin, w.region.yMin, w.parent.bounds.xMax + 1, w.region.yMax)
        beyond.flatMap(Subframe.of(w.parent, w.frame.id, _)) match
          case Left(GeometryError.SubframeOutsideParent(window, _, _, _, _, parent, _)) =>
            Prop(window == w.frame.id && parent == w.parent.id)
          case other => Prop(false) :| s"accepted $other"
      }
    )

  /** A declared linear angular scale: bandwidths and positions in degrees. */
  def angularScale(tol: Tolerance = Tolerance.exactish): RuleSet =
    new SimpleRuleSet(
      "linearAngularScale",
      "a bandwidth in degrees is its product with units per degree" -> forAll(
        genScale,
        Gen.choose(0.01, 20.0)
      ) { (s, degrees) =>
        s.sigma(Sigma.deg(degrees).toOption.get) match
          case Right(px) => Prop(tol.approxEquals(px.value, degrees * s.unitsPerDegree))
          case Left(e)   => Prop(false) :| e.message
      },
      "the frame centre is zero degrees, x right and y up" -> forAll(genScale) { s =>
        val c  = s.frame.centre
        val up =
          if s.frame.yAxis == YAxis.Down then Pt[Px](c.x, c.y - s.unitsPerDegree)
          else Pt[Px](c.x, c.y + s.unitsPerDegree)
        s.angular(FrameId("degrees")) match
          case Right(w) =>
            Prop(w(c).exists(tol.approxEquals(_, Pt[Deg](0, 0)))) &&
            Prop(
              w(Pt[Px](c.x + s.unitsPerDegree, c.y)).exists(tol.approxEquals(_, Pt[Deg](1, 0)))
            ) &&
            Prop(w(up).exists(tol.approxEquals(_, Pt[Deg](0, 1))))
          case Left(e) => Prop(false) :| e.message
      },
      "a window keeps the scale and refuses another frame" -> forAll(genWindow, genScaleValue) {
        (w, ppd) =>
          val scale = LinearAngularScale.of(w.parent, ppd).toOption.get
          val other = LinearAngularScale.of(w.frame, ppd).toOption.get
          Prop(scale.on(w).map(_.unitsPerDegree) == Right(ppd)) &&
          Prop(scale.on(w).map(_.frame) == Right(w.frame)) &&
          Prop(other.on(w).isLeft)
      },
      "a non-positive or non-finite scale is refused, naming it" -> forAll(
        genWindow,
        Gen.oneOf(0.0, -1.0, Double.NaN, Double.PositiveInfinity)
      ) { (w, bad) =>
        LinearAngularScale.of(w.parent, bad) match
          case Left(GeometryError.NonPositiveAngularScale(id, v)) =>
            Prop(id == w.parent.id && (v.isNaN == bad.isNaN))
          case other => Prop(false) :| s"accepted $other"
      }
    )

  /** Recorded corrections: flips are involutions, a translation is undone by
    * its negation, and every correction is an affine endomorphism.
    */
  def correction(tol: Tolerance = Tolerance.exactish): RuleSet =
    new SimpleRuleSet(
      "correction",
      "a flip applied twice is the identity" -> forAll(
        genPlaced,
        Gen.oneOf(Correction.FlipX, Correction.FlipY)
      ) { case ((w, p), flip) =>
        val f = flip.warp(w.parent, w.parent)
        Prop(f(p).flatMap(f.apply).exists(tol.approxEquals(_, p)))
      },
      "a flip reflects about the frame's centre line" -> forAll(genPlaced) { case (w, p) =>
        val c = w.parent.centre
        Prop(
          Correction.FlipY
            .warp(w.parent, w.parent)(p)
            .exists(q => tol.approxEquals(q, Pt[Px](p.x, 2 * c.y - p.y)))
        ) && Prop(
          Correction.FlipX
            .warp(w.parent, w.parent)(p)
            .exists(q => tol.approxEquals(q, Pt[Px](2 * c.x - p.x, p.y)))
        )
      },
      "a translation is undone by its negation" -> forAll(
        genPlaced,
        Gen.choose(-500.0, 500.0),
        Gen.choose(-500.0, 500.0)
      ) { case ((w, p), dx, dy) =>
        val there = Correction.translate(dx, dy).toOption.get.warp(w.parent, w.parent)
        val back  = Correction.translate(-dx, -dy).toOption.get.warp(w.parent, w.parent)
        Prop(there(p).exists(q => tol.approxEquals(q, Pt[Px](p.x + dx, p.y + dy)))) &&
        Prop(there(p).flatMap(back.apply).exists(tol.approxEquals(_, p)))
      },
      "a non-finite translation is refused" -> forAll(
        Gen.oneOf(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)
      ) { bad =>
        Prop(Correction.translate(bad, 0).isLeft) && Prop(Correction.translate(0, bad).isLeft)
      }
    )

  /** A trial's tally partitions its fixations, by count and by duration, and
    * classifies each exactly as the window and the screen do.
    */
  def tally: RuleSet =
    new SimpleRuleSet(
      "windowTally",
      "inside, outside the window and outside the screen partition the trial" -> forAll(
        genTallied
      ) { case (w, path) =>
        WindowTally.window(w, path) match
          case Right(t) =>
            Prop(t.inside + t.outsideWindow + t.outsideScreen == path.n) &&
            Prop(t.total == path.n) &&
            Prop(t.insideDuration + t.outsideDuration == path.dwellTotal) &&
            Prop(t.totalDuration == path.dwellTotal)
          case Left(e) => Prop(false) :| e.message
      },
      "each fixation is classified by the screen first, then the window" -> forAll(genTallied) {
        case (w, path) =>
          val fixations = path.fixations.toVector
          val screen    = fixations.filterNot(f => w.parent.contains(f.centre))
          val window    =
            fixations.filter(f => w.parent.contains(f.centre) && !w.locate(f.centre).isInside)
          WindowTally.window(w, path) match
            case Right(t) =>
              Prop(t.outsideScreen == screen.size) && Prop(t.outsideWindow == window.size) &&
              Prop(t.outsideScreenDuration.toMicros == screen.map(_.duration.toMicros).sum) &&
              Prop(t.outsideWindowDuration.toMicros == window.map(_.duration.toMicros).sum)
            case Left(e) => Prop(false) :| e.message
      },
      "a whole-frame tally has nothing outside a window" -> forAll(genTallied) {
        case (w, path) =>
          WindowTally.screen(w.parent, path) match
            case Right(t) =>
              Prop(t.outsideWindow == 0) &&
              Prop(t.outsideScreen == path.fixations.count(f => !w.parent.contains(f.centre)))
            case Left(e) => Prop(false) :| e.message
      },
      "a scanpath in another frame is refused" -> forAll(genTallied) { case (w, path) =>
        Prop(WindowTally.screen(w.frame, path).isLeft)
      },
      "a stored tally must partition its trial" -> forAll(
        Gen.choose(0, 5),
        Gen.choose(0, 5),
        Gen.choose(0, 12)
      ) { (screen, window, total) =>
        val result = WindowTally.of(
          screen,
          window,
          total,
          Span.micros(screen.toLong),
          Span.micros(window.toLong),
          Span.micros(total.toLong)
        )
        Prop(result.isRight == (screen + window <= total))
      }
    )

end WindowLaws

object WindowLaws extends WindowLaws:
  /** Units per degree, including the Studio's declared 35. */
  val genScaleValue: Gen[Double] = Gen.oneOf(Gen.choose(0.5, 120.0), Gen.const(35.0))

  /** A display frame with integral pixel bounds, either axis direction. */
  val genParent: Gen[Frame[Px]] =
    for
      x0   <- Gen.oneOf(Gen.const(0), Gen.choose(-2000, 2000))
      y0   <- Gen.oneOf(Gen.const(0), Gen.choose(-2000, 2000))
      w    <- Gen.oneOf(Gen.const(1920), Gen.choose(2, 4000))
      h    <- Gen.oneOf(Gen.const(1080), Gen.choose(2, 4000))
      axis <- Gen.oneOf(YAxis.Down, YAxis.Up)
    yield Frame.of(
      FrameId("display"),
      Bounds
        .of[Px](x0.toDouble, y0.toDouble, (x0 + w).toDouble, (y0 + h).toDouble)
        .toOption
        .get,
      axis
    )

  /** A window with integral corners inside its parent; sometimes the whole
    * parent, sometimes touching one of its edges.
    */
  val genWindow: Gen[Subframe[Px]] =
    for
      parent <- genParent
      b = parent.bounds
      x0 <- Gen.oneOf(Gen.const(b.xMin), Gen.choose(b.xMin, b.xMax - 1).map(math.floor))
      y0 <- Gen.oneOf(Gen.const(b.yMin), Gen.choose(b.yMin, b.yMax - 1).map(math.floor))
      x1 <- Gen.oneOf(Gen.const(b.xMax), Gen.choose(x0 + 1, b.xMax).map(math.ceil))
      y1 <- Gen.oneOf(Gen.const(b.yMax), Gen.choose(y0 + 1, b.yMax).map(math.ceil))
    yield Subframe
      .of(parent, FrameId("image"), Bounds.of[Px](x0, y0, x1, y1).toOption.get)
      .toOption
      .get

  /** One coordinate on or beside a half-open interval's edges, or uniform. */
  def straddling(lo: Double, hi: Double, outerLo: Double, outerHi: Double): Gen[Double] =
    Gen.frequency(
      1 -> Gen.const(lo),
      1 -> Gen.const(hi),
      1 -> Gen.const(java.lang.Math.nextDown(hi)),
      1 -> Gen.const(java.lang.Math.nextDown(lo)),
      2 -> Gen.choose(lo, hi),
      2 -> Gen.choose(outerLo, outerHi)
    )

  /** A window and a position that straddles its edges or lies anywhere near. */
  val genPlaced: Gen[(Subframe[Px], Pt[Px])] =
    for
      w <- genWindow
      r = w.region
      b = w.parent.bounds
      x <- straddling(r.xMin, r.xMax, b.xMin - 50, b.xMax + 50)
      y <- straddling(r.yMin, r.yMax, b.yMin - 50, b.yMax + 50)
    yield (w, Pt[Px](x, y))

  val genScale: Gen[LinearAngularScale[Px]] =
    for
      parent <- genParent
      ppd    <- genScaleValue
    yield LinearAngularScale.of(parent, ppd).toOption.get

  /** A window and a scanpath on its parent frame whose fixations straddle
    * the window's and the screen's edges.
    */
  val genTallied: Gen[(Subframe[Px], Scanpath[Px])] =
    for
      w <- genWindow
      r = w.region
      b = w.parent.bounds
      n         <- Gen.choose(1, 12)
      xs        <- Gen.listOfN(n, straddling(r.xMin, r.xMax, b.xMin - 50, b.xMax + 50))
      ys        <- Gen.listOfN(n, straddling(r.yMin, r.yMax, b.yMin - 50, b.yMax + 50))
      durations <- Gen.listOfN(n, Gen.choose(1L, 500000L))
    yield
      val clock = ClockId("tally")
      var onset = 0L
      val fixes = xs.zip(ys).zip(durations).map { case ((x, y), d) =>
        val span =
          Interval.of(clock, Instant.micros(onset), Instant.micros(onset + d)).toOption.get
        onset += d + 1
        Event.Fixation.withoutDispersion(span, Pt[Px](x, y), 1).toOption.get
      }
      (w, Scanpath.of(w.parent, clock, IArray.from(fixes)).toOption.get)
