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

package eyes4s.codec

import io.circe.Json

/** The collision-resistant identity of a value of type `A`: the SHA-256 of
  * its canonical document under its versioned codec, schema envelope
  * included (`VersionedCodec#digest`). This is the identity to persist or cite across files and runs:
  * a plan revision, the input a run used, the result a report or figure is
  * bound to, and every stale-run comparison between them.
  *
  * Writing is canonical (one document per value, under the earliest version
  * that expresses it; see `SchemaLadder` and the canonical wire forms), so
  * equal values have equal digests and a changed value has a different one
  * except with SHA-256's negligible probability. The digest is taken over a
  * portable binary rendering of the document's JSON value, not over printed
  * text, so the JVM and Scala.js compute the same digest although they print
  * some doubles differently.
  *
  * [[eyes4s.kernel.ContentHash]] and `eyes4s.plan.ArtifactRef` are a 64-bit,
  * non-cryptographic key: an in-memory change detector and the semantic
  * cross-check version-1 documents already carry. They are not this type and
  * do not convert to it. [[ByteDigest]] is the digest of exact stored bytes,
  * which a manifest verifies; this is the digest of the value those bytes
  * decode to.
  */
final class CanonicalDigest[A] private (val sha256: ByteDigest):
  /** The same identity of the same kind of value. */
  def sameAs(other: CanonicalDigest[A]): Boolean = sha256 == other.sha256

  /** The digest as it is shown: `sha256:` and 64 lowercase hexadecimal digits. */
  def display: String = s"sha256:${sha256.hex}"

  override def equals(other: Any): Boolean = other match
    case that: CanonicalDigest[?] => sha256 == that.sha256
    case _                        => false
  override def hashCode: Int    = sha256.hashCode
  override def toString: String = display

object CanonicalDigest:
  given [A]: CanEqual[CanonicalDigest[A], CanonicalDigest[A]] = CanEqual.derived

  /** Read a persisted digest: exactly 64 lowercase hexadecimal digits. */
  def parse[A](hex: String): Either[ByteDigestError, CanonicalDigest[A]] =
    ByteDigest.parse(hex).map(new CanonicalDigest(_))

  private[codec] def document[A](json: Json): CanonicalDigest[A] =
    new CanonicalDigest(ByteDigest.sha256(CanonicalBytes.of(json)))

/** A prefix-free binary rendering of a JSON value: a tag byte per node,
  * lengths before contents, strings as UTF-16 code units and object members
  * in document order. A number is its IEEE-754 double bits, or its exact
  * 64-bit integer where the double would round it, so a number has one
  * rendering whichever platform printed or parsed it.
  */
private[codec] object CanonicalBytes:
  private val Exact = 1L << 53

  def of(json: Json): IArray[Byte] =
    val out                    = Array.newBuilder[Byte]
    def byte(value: Int): Unit =
      out += value.toByte
      ()
    def int(value: Int): Unit =
      var shift = 24
      while shift >= 0 do
        byte(value >>> shift)
        shift -= 8
    def long(value: Long): Unit =
      var shift = 56
      while shift >= 0 do
        byte((value >>> shift).toInt)
        shift -= 8
    def string(value: String): Unit =
      int(value.length)
      var i = 0
      while i < value.length do
        val c = value.charAt(i).toInt
        byte(c >>> 8)
        byte(c)
        i += 1
    def number(value: io.circe.JsonNumber): Unit =
      value.toLong match
        case Some(l) if l > Exact || l < -Exact =>
          byte(4)
          long(l)
        case _ =>
          byte(3)
          long(java.lang.Double.doubleToLongBits(value.toDouble))
    def write(value: Json): Unit =
      value.fold[Unit](
        byte(0),
        b => byte(if b then 2 else 1),
        number,
        s =>
          byte(5)
          string(s)
        ,
        items =>
          byte(6)
          int(items.size)
          items.foreach(write)
        ,
        members =>
          byte(7)
          int(members.size)
          members.toIterable.foreach { (key, member) =>
            string(key)
            write(member)
          }
      )
    write(json)
    IArray.unsafeFromArray(out.result())
