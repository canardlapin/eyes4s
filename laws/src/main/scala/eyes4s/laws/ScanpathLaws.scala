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

import eyes4s.core.{Scanpath, Weight}
import eyes4s.kernel.*

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** The invariants every [[Scanpath]] carries, stated as laws so that a
  * downstream author with their own way of producing scanpaths -- a detector,
  * a file reader, a generator -- can check that what comes out is what the
  * type promises.
  *
  * These are the promises `Scanpath.of` makes at admission: `n` fixations and
  * `n - 1` transitions, ordered and never overlapping, one clock throughout,
  * an extent that is exactly the first onset to the last offset. A value that
  * reached a consumer without them would make every path measure quietly
  * wrong, which is why they are checked here rather than assumed.
  */
trait ScanpathLaws extends Laws:

  def wellFormed[U <: Unit2D](gen: Gen[Scanpath[U]], tol: Tolerance): RuleSet =
    new SimpleRuleSet(
      "scanpath.wellFormed",
      "n fixations, n - 1 transitions" -> forAll(gen) { sp =>
        Prop(sp.n >= 1 && sp.transitions.length == sp.n - 1) :|
          s"n=${sp.n}, transitions=${sp.transitions.length}"
      },
      "fixations are ordered and never overlap" -> forAll(gen) { sp =>
        Prop.all((1 until sp.n).map { i =>
          val prev = sp.fixations(i - 1).span
          val cur  = sp.fixations(i).span
          Prop(cur.onset.toMicros >= prev.offset.toMicros) :|
            s"fixation $i at ${cur.render} begins before ${prev.render} ends"
        }*)
      },
      "every fixation is on the scanpath's clock" -> forAll(gen) { sp =>
        Prop.all((0 until sp.n).map { i =>
          Prop(sp.fixations(i).span.clock == sp.clock) :| s"fixation $i is on another clock"
        }*)
      },
      "transitions join consecutive fixations exactly" -> forAll(gen) { sp =>
        Prop.all(sp.transitions.indices.map { i =>
          val t    = sp.transitions(i)
          val from = sp.fixations(i)
          val to   = sp.fixations(i + 1)
          Prop(
            t.span.clock == sp.clock &&
              t.span.onset == from.span.offset &&
              t.span.offset == to.span.onset &&
              t.from == from.centre &&
              t.to == to.centre
          ) :| s"transition $i does not join fixations $i and ${i + 1}"
        }*)
      },
      "the extent runs from the first onset to the last offset" -> forAll(gen) { sp =>
        Prop(
          sp.extent.clock == sp.clock &&
            sp.extent.onset == sp.first.span.onset &&
            sp.extent.offset == sp.last.span.offset
        ) :| s"extent ${sp.extent.render} vs ${sp.first.span.render}..${sp.last.span.render}"
      },
      "dwell never exceeds the extent" -> forAll(gen) { sp =>
        Prop(sp.dwellTotal.toMicros <= sp.extent.duration.toMicros) :|
          s"dwell ${sp.dwellTotal.toMicros} > extent ${sp.extent.duration.toMicros}"
      },
      "path length is invariant under translation and scales under a uniform rescale" ->
        forAll(gen) { sp =>
          val b       = sp.frame.bounds
          val shifted = Frame.of(
            FrameId(sp.frame.id.name + "-shifted"),
            Bounds
              .of[U](b.xMin + b.width, b.yMin - b.height, b.xMax + b.width, b.yMax - b.height)
              .toOption
              .get,
            sp.frame.yAxis
          )
          val doubled = Frame.of(
            FrameId(sp.frame.id.name + "-doubled"),
            Bounds.of[U](2 * b.xMin, 2 * b.yMin, 2 * b.xMax, 2 * b.yMax).toOption.get,
            sp.frame.yAxis
          )
          val moved  = sp.warp(Warp.rescale(sp.frame, shifted).toOption.get)
          val scaled = sp.warp(Warp.rescale(sp.frame, doubled).toOption.get)
          (moved, scaled) match
            case (Right(m), Right(s)) =>
              Prop(tol.approxEquals(m.pathLength, sp.pathLength)) :|
                s"translated ${m.pathLength} vs ${sp.pathLength}" &&
                Prop(tol.approxEquals(s.pathLength, 2 * sp.pathLength)) :|
                s"doubled ${s.pathLength} vs ${2 * sp.pathLength}"
            case (Left(e), _) => Prop(false) :| e.message
            case (_, Left(e)) => Prop(false) :| e.message
        },
      "re-admitting the fixations reproduces the scanpath" -> forAll(gen) { sp =>
        Scanpath.of(sp.frame, sp.clock, sp.fixations) match
          case Left(e)      => Prop(false) :| e.message
          case Right(again) =>
            Prop(again.n == sp.n && again.extent == sp.extent && again.frame == sp.frame) :|
              "re-admission changed the value"
      },
      "duration-weighted occupancy carries the whole dwell" -> forAll(gen) { sp =>
        sp.occupancy(Weight.Duration) match
          case Left(e)  => Prop(false) :| e.message
          case Right(m) =>
            Prop(m.size == sp.n && tol.approxEquals(m.total, sp.dwellTotal.toSeconds)) :|
              s"total ${m.total} vs dwell ${sp.dwellTotal.toSeconds}"
      },
      "uniform occupancy counts every fixation once" -> forAll(gen) { sp =>
        sp.occupancy(Weight.Uniform) match
          case Left(e)  => Prop(false) :| e.message
          case Right(m) =>
            Prop(m.size == sp.n && tol.approxEquals(m.total, sp.n.toDouble)) :|
              s"total ${m.total} vs n ${sp.n}"
      }
    )

end ScanpathLaws

object ScanpathLaws extends ScanpathLaws
