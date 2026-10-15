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

package eyes4s.studio.core.artifacts

import scala.collection.immutable.ArraySeq

class NativeArtifactBytesSuite extends munit.FunSuite:
  test("accepted bytes cannot be changed through the caller's array") {
    val source = Array[Byte](1, 2, 3, 4)
    val owned  = NativeArtifactBytes.copyOf(IArray.unsafeFromArray(source))
    source(0) = 9
    assertEquals(owned.toVector, Vector[Byte](1, 2, 3, 4))
    val exported = owned.toArray
    exported(1) = 9
    assertEquals(owned.toVector, Vector[Byte](1, 2, 3, 4))
  }
  test("immutable sequence admission snapshots an unsafe externally wrapped array") {
    val source = Array[Byte](5, 6, 7)
    val owned  = NativeArtifactBytes.copyOf(ArraySeq.unsafeWrapArray(source))
    source(0) = 9
    assertEquals(owned.toVector, Vector[Byte](5, 6, 7))
    assertEquals(owned.hashCode, Vector[Byte](5, 6, 7).hashCode)
    assert(owned == Vector[Byte](5, 6, 7))
  }
  test("already-owned immutable storage is shared internally") {
    val owned = NativeArtifactBytes.copyOf(IArray[Byte](8, 9))
    val again = NativeArtifactBytes.copyOf(owned)
    assert(owned.asInstanceOf[AnyRef] eq again.asInstanceOf[AnyRef])
    assertEquals(NativeArtifactBytes.view(owned).toVector, Vector[Byte](8, 9))
  }
