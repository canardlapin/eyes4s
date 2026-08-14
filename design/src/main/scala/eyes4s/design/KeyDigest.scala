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

package eyes4s.design

import eyes4s.kernel.ContentHash

import scala.annotation.implicitNotFound
import scala.deriving.Mirror

/** The only route from a key to a sampling identity (PRD X-10).
  *
  * ==Why not `hashCode`==
  *
  * `hashCode` is unspecified across platforms and unstable across releases. A
  * control sample derived from it would differ between a JVM run and a browser
  * run of the same study, and could change under a Scala upgrade with nothing
  * in the code having moved. `toString` is worse: it is a rendering, and
  * `Double.toString` alone already differs between platforms -- a lesson this
  * project learned when a provenance digest turned out not to be portable.
  *
  * ==Separators are structural, not decorative==
  *
  * Fields are digested with a domain tag and a position index, so
  * `("ab", "c")` and `("a", "bc")` differ, and so do two products whose fields
  * happen to hold the same values in different roles. Concatenating without
  * separators is the classic way to make distinct keys collide.
  *
  * ==The law==
  *
  * `Eq[K].eqv(a, b)` implies `digest(a) == digest(b)`. The converse is not
  * claimed: this is a digest, not an injection.
  */
@implicitNotFound(
  "No KeyDigest[${K}] is available. Define one explicitly, or derive it for a case class " +
    "whose fields all have KeyDigest instances."
)
trait KeyDigest[K]:
  def digest(k: K): ContentHash

object KeyDigest:

  def apply[K](using k: KeyDigest[K]): KeyDigest[K] = k

  private def tagged(domain: String, h: ContentHash): ContentHash =
    ContentHash.combine(ContentHash.ofString(domain), h)

  given KeyDigest[String] with
    def digest(k: String): ContentHash = tagged("s", ContentHash.ofString(k))

  given KeyDigest[Int] with
    def digest(k: Int): ContentHash = tagged("i", ContentHash.of(IArray(k.toDouble)))

  given KeyDigest[Long] with
    def digest(k: Long): ContentHash =
      tagged("l", ContentHash.ofString(java.lang.Long.toString(k)))

  given KeyDigest[Boolean] with
    def digest(k: Boolean): ContentHash =
      tagged("b", ContentHash.ofString(if k then "1" else "0"))

  /** Doubles are digested by bit pattern, never by rendering. */
  given KeyDigest[Double] with
    def digest(k: Double): ContentHash = tagged("d", ContentHash.of(IArray(k)))

  given [A](using a: KeyDigest[A]): KeyDigest[Option[A]] with
    def digest(k: Option[A]): ContentHash = k match
      case None    => tagged("o0", ContentHash.empty)
      case Some(v) => tagged("o1", a.digest(v))

  /** Products derive structurally, with each field's position mixed in.
    *
    * This is what lets a study key be a case class -- `Key(subject, image)` --
    * rather than a string built by pasting fields together, which is what
    * `eyesim` requires of its users and what makes its `match_on` join a
    * stringly-typed operation.
    *
    * Derivation summons a [[KeyDigest]] for every field at compile time.
    * Unsupported fields therefore fail where the key type is declared rather
    * than throwing when an analysis happens to sample its first pair.
    */
  given derived[K <: Product](using
      m: Mirror.ProductOf[K],
      fields: ProductDigest[m.MirroredElemTypes]
  ): KeyDigest[K] =
    new KeyDigest[K]:
      def digest(k: K): ContentHash =
        val tuple: m.MirroredElemTypes = Tuple.fromProductTyped(k)
        tagged("p" + k.productArity, ContentHash.combineAll(fields.digest(tuple)))

  /** Compiler-facing support for product derivation.
    *
    * Keeping the tuple typed is the important part: each recursive step knows
    * the field's declared type and must summon its `KeyDigest`. There is no
    * `Any` match, reflective fallback, or pure-path exception.
    */
  sealed trait ProductDigest[Values <: Tuple]:
    def digest(values: Values, position: Int = 0): Vector[ContentHash]

  given productEmpty: ProductDigest[EmptyTuple] with
    def digest(values: EmptyTuple, position: Int): Vector[ContentHash] = Vector.empty

  given productCons[Head, Tail <: Tuple](using
      headDigest: KeyDigest[Head],
      tailDigest: ProductDigest[Tail]
  ): ProductDigest[Head *: Tail] with
    def digest(values: Head *: Tail, position: Int): Vector[ContentHash] =
      ContentHash.combine(
        ContentHash.ofString("f" + position),
        headDigest.digest(values.head)
      ) +: tailDigest.digest(values.tail, position + 1)

end KeyDigest
