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

package eyes4s.kernel

/** A digest of the data a derived value came from.
  *
  * ==Why provenance needs this== (bead `q-provenance-cache`)
  *
  * A record of *parameters* cannot serve as a cache key. Two different datasets
  * smoothed with the same bandwidth onto the same grid have identical parameter
  * records and different results, so "equal provenance implies equal result"
  * is false until input identity is part of it. That was settled by inspection
  * rather than debate.
  *
  * ==Not cryptographic, deliberately==
  *
  * This answers "has the input changed?", not "can an adversary forge an
  * input?". FNV-1a over the raw bit patterns is a few nanoseconds per value and
  * good enough to make a collision an irrelevance in practice. A SHA over a
  * hundred megabytes of samples would be a security-grade answer to a
  * cache-invalidation question.
  *
  * ==Identical on every platform==
  *
  * All arithmetic is on `Long` and on `doubleToLongBits`, both of which are
  * exact under Scala.js. A digest computed in a browser must equal one computed
  * on the JVM or a cached result is not portable (DET-2, DET-4).
  */
opaque type ContentHash = Long

object ContentHash:

  private val Offset = 0xcbf29ce484222325L
  private val Prime  = 0x100000001b3L

  val empty: ContentHash = Offset

  private def mixByte(h: Long, b: Long): Long = (h ^ (b & 0xffL)) * Prime

  private def mixLong(h: Long, v: Long): Long =
    var acc = h
    var i   = 0
    while i < 8 do
      acc = mixByte(acc, v >>> (i * 8))
      i += 1
    acc

  def of(values: IArray[Double]): ContentHash =
    var h = Offset
    var i = 0
    while i < values.length do
      // Canonicalise the two zeros and every NaN, so that bitwise-distinct but
      // numerically equal inputs do not produce different digests.
      val d = values(i)
      val v =
        if d.isNaN then 0x7ff8000000000000L
        else if d == 0.0 then 0L
        else java.lang.Double.doubleToLongBits(d)
      h = mixLong(h, v)
      i += 1
    mixLong(h, values.length.toLong)

  def ofString(s: String): ContentHash =
    var h = Offset
    var i = 0
    while i < s.length do
      h = mixLong(h, s.charAt(i).toLong)
      i += 1
    h

  def combine(a: ContentHash, b: ContentHash): ContentHash = mixLong(a, b)

  def combineAll(hs: Seq[ContentHash]): ContentHash =
    hs.foldLeft(empty)(combine)

  extension (h: ContentHash)
    def value: Long    = h
    def render: String = f"${h}%016x"

  given cats.kernel.Order[ContentHash] =
    cats.kernel.Order.from((a, b) => java.lang.Long.compare(a, b))

end ContentHash

/** How a derived value was produced.
  *
  * Carried by every [[Surface]] so that a map can report its own bandwidth,
  * weighting and normalisation history. `eyesim`'s `Ops.eye_density` drops
  * `sigma` on every operation, which is why its multiscale entropy method needs
  * a `%||% NA_real_` fallback to survive its own arithmetic.
  */
final case class Provenance(inputs: ContentHash, steps: Vector[Provenance.Step]):

  def andThen(step: Provenance.Step): Provenance = Provenance(inputs, steps :+ step)

  /** The cache key: inputs and the whole derivation, together. */
  def digest: ContentHash =
    ContentHash.combineAll(
      inputs +: steps.map(s => ContentHash.ofString(s.operation + "|" + s.detail))
    )

  def render: String =
    if steps.isEmpty then s"raw(${inputs.render})"
    else s"${inputs.render}: ${steps.map(_.render).mkString(" -> ")}"

object Provenance:

  final case class Step(operation: String, detail: String):
    def render: String = if detail.isEmpty then operation else s"$operation($detail)"

  def raw(inputs: ContentHash): Provenance = Provenance(inputs, Vector.empty)
