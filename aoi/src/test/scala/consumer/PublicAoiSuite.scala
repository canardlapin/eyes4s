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

package consumer

import eyes4s.aoi.*
import eyes4s.kernel.*

class PublicAoiSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  test(
    "equal independently constructed areas hash consistently and sets preserve cardinality"
  ) {
    val frame = get(Frame.screen("aoi-consumer", 3, 2))
    def area  = get(Aoi.of("stimulus", "Stimulus", frame, Region.fromBounds(frame.bounds)))
    assertEquals(area.hashCode, area.hashCode)
    assert(Set(area).contains(area))
    assertEquals(get(AoiSet.of(Vector(area))).size, 1)
    assert(AoiSet.of(Vector(area, area)).isLeft)
  }
