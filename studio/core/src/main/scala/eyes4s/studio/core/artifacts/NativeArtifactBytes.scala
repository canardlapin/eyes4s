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

/** Owned primitive storage behind the ordinary immutable sequence interface.
  * No array is exposed to callers; internal immutable views may share ownership.
  */
private[core] final class NativeArtifactBytes private (private val data: Array[Byte])
    extends scala.collection.immutable.IndexedSeq[Byte]:
  def length: Int                                 = data.length
  def apply(index: Int): Byte                     = data(index)
  private[artifacts] def ownedArray: IArray[Byte] = IArray.unsafeFromArray(data)

private[core] object NativeArtifactBytes:
  def copyOf(bytes: IndexedSeq[Byte]): IndexedSeq[Byte] = bytes match
    case owned: NativeArtifactBytes => owned
    case _                          =>
      val owned = new Array[Byte](bytes.length)
      var i     = 0
      while i < owned.length do
        owned(i) = bytes(i)
        i += 1
      new NativeArtifactBytes(owned)

  def copyOf(bytes: IArray[Byte]): IndexedSeq[Byte] =
    val owned = new Array[Byte](bytes.length)
    var i     = 0
    while i < owned.length do
      owned(i) = bytes(i)
      i += 1
    new NativeArtifactBytes(owned)

  def view(bytes: IndexedSeq[Byte]): IArray[Byte] = bytes match
    case owned: NativeArtifactBytes => owned.ownedArray
    case _                          => IArray.from(bytes)
