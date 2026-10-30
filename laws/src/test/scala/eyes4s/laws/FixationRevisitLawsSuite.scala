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

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import org.scalacheck.Gen

class FixationRevisitLawsSuite extends munit.DisciplineSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(e.toString), identity)
  private val frame                         = get(Frame.screen("revisit-laws", 100, 100))
  private val clock                         = ClockId("revisit-laws")
  private val numerical                     = Tolerance(absolute = 1e-13, relative = 1e-13)
  private val point                         = for
    x <- Gen.choose(-3, 3)
    y <- Gen.choose(-3, 3)
  yield Pt[Px](x.toDouble, y.toDouble)
  private def path(points: Vector[Pt[Px]]): Scanpath[Px] =
    get(
      Scanpath.of(
        frame,
        clock,
        IArray.from(points.zipWithIndex.map { (p, i) =>
          get(
            Event.Fixation.withoutDispersion(
              get(
                Interval
                  .of(clock, Instant.micros(i.toLong * 20), Instant.micros(i.toLong * 20 + 10))
              ),
              p,
              1
            )
          )
        })
      )
    )
  private val inputs = for
    n         <- Gen.choose(1, 24)
    points    <- Gen.listOfN(n, point)
    m         <- Gen.choose(0, 20)
    refs      <- Gen.listOfN(m, point)
    weights   <- Gen.listOfN(m, Gen.choose(0, 7).map(_.toDouble))
    radius    <- Gen.oneOf(1.0, 2.0, 3.0, 5.0)
    minimum   <- Gen.choose(1, 4)
    maximum   <- Gen.option(Gen.choose(minimum, minimum + 5))
    departure <- Gen.oneOf(
      RevisitDeparture.Unrestricted,
      RevisitDeparture.IncomingStepAtLeastRadius
    )
    metric <- Gen.oneOf(FixationGroundDistance.Euclidean, FixationGroundDistance.Manhattan)
    useReference <- Gen.oneOf(true, false)
  yield (
    path(points.toVector),
    get(RevisitConfig.of(get(Distance.px(radius)), minimum, maximum, departure, metric)),
    Option.when(useReference)(
      get(PointMeasure.of(frame, IArray.from(refs), IArray.from(weights)))
    )
  )

  checkAll("weighted revisits", FixationRevisitLaws.analysis(inputs, numerical))

  // Deterministic witnesses make the important falsifiers independent of a random seed.
  private val witness = (
    path(
      Vector(Pt[Px](0, 0), Pt[Px](0, 0), Pt[Px](3, 0), Pt[Px](6, 0), Pt[Px](0, 0), Pt[Px](3, 0))
    ),
    get(RevisitConfig.of(get(Distance.px(3)), 2, Some(3), RevisitDeparture.Unrestricted)),
    Some(
      get(
        PointMeasure.of(
          frame,
          IArray(Pt[Px](0, 0), Pt[Px](3, 0), Pt[Px](10, 0)),
          IArray(2.0, 3.0, 5.0)
        )
      )
    )
  )
  checkAll(
    "boundary, overlap and expiry witness",
    FixationRevisitLaws.analysis(Gen.const(witness), numerical)
  )
