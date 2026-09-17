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

import eyes4s.core.Scanpath
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Norm, Px}

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

/** The published scanpath generator against the published scanpath laws, and
  * the one property a generator's shrink must have: every candidate it offers
  * is itself a value the law could have been stated about.
  */
class ScanpathLawsSuite extends munit.DisciplineSuite:

  import Generators.given

  checkAll(
    "Scanpath.wellFormed[Norm]",
    ScanpathLaws.wellFormed(Generators.genScanpath[Norm], Tolerance.exactish)
  )
  checkAll(
    "Scanpath.wellFormed[Px]",
    ScanpathLaws.wellFormed(Generators.genScanpath[Px], Tolerance.exactish)
  )

  private val screen = Frame.screen("shrink-frame", 1280, 1024).toOption.get

  checkAll(
    "Scanpath.wellFormed[fixed frame]",
    ScanpathLaws.wellFormed(Generators.genScanpathIn(screen), Tolerance.exactish)
  )

  property("the generator reaches abutting fixations and both dispersion states") {
    val samples = LazyList
      .continually(Generators.genScanpath[Norm].sample)
      .flatten
      .take(200)
      .toVector
    val abutting = samples.exists(_.transitions.exists(_.duration.toMicros == 0L))
    val reported = samples.exists(_.fixations.exists(_.dispersion.isDefined))
    val missing  = samples.exists(_.fixations.exists(_.dispersion.isEmpty))
    val single   = samples.exists(_.n == 1)
    val long     = samples.exists(_.n >= 8)
    Prop(abutting) :| "no abutting fixations" &&
    Prop(reported) :| "no reported dispersion" &&
    Prop(missing) :| "no missing dispersion" &&
    Prop(single) :| "no single-fixation scanpath" &&
    Prop(long) :| "no long scanpath"
  }

  property(
    "shrinking yields only valid, strictly smaller scanpaths on the same frame and clock"
  ) {
    forAll(Generators.genScanpath[Norm]) { sp =>
      val candidates = Generators.shrinkScanpath(sp).toVector
      // Two halves plus one candidate per dropped fixation, all admissible.
      val expected = if sp.n <= 1 then 0 else sp.n + 2
      Prop(if sp.n <= 1 then candidates.isEmpty else candidates.nonEmpty) :|
        s"n=${sp.n} offered ${candidates.length} candidates" &&
        Prop(sp.n <= 1 || candidates.length == expected) :|
        s"n=${sp.n} offered ${candidates.length}, expected $expected" &&
        Prop.all(candidates.map { c =>
          Prop(c.n < sp.n) :| s"candidate with n=${c.n} is not smaller than ${sp.n}" &&
          Prop(
            c.frame == sp.frame && c.clock == sp.clock
          ) :| "candidate changed frame or clock" &&
          Prop(c.transitions.length == c.n - 1) :| "candidate has the wrong transition count" &&
          Prop(
            Scanpath.of(c.frame, c.clock, c.fixations).isRight
          ) :| "candidate is not admissible" &&
          Prop(
            c.fixations.forall(f => sp.fixations.contains(f))
          ) :| "candidate invented a fixation"
        }*)
    }
  }

  property("shrinking terminates at a single fixation") {
    forAll(Generators.genScanpath[Norm]) { sp =>
      val chain = LazyList
        .iterate(Option(sp))(_.flatMap(s => Generators.shrinkScanpath(s).headOption))
        .takeWhile(_.isDefined)
        .flatten
      Prop(chain.last.n == 1) :| s"chain ended at n=${chain.last.n}" &&
      Prop(chain.length <= sp.n + 1) :| s"chain of ${chain.length} for n=${sp.n}"
    }
  }
