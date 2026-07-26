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
    */
  // Not `inline`: the Mirror is a GUARD that K is a case class, and the digest
  // itself walks productIterator at runtime. Marking it inline would duplicate
  // an anonymous class at every use site for no benefit.
  given derived[K <: Product](using m: Mirror.ProductOf[K]): KeyDigest[K] =
    new KeyDigest[K]:
      def digest(k: K): ContentHash =
        val fields = k.productIterator.toVector
        val parts  = fields.zipWithIndex.map { (f, i) =>
          ContentHash.combine(
            ContentHash.ofString("f" + i),
            anyDigest(f)
          )
        }
        tagged("p" + fields.length, ContentHash.combineAll(parts))

  /** Digest a field whose static type is erased by `productIterator`.
    *
    * Restricted to the primitives and identifiers the instances above cover; a
    * field of any other type is a compile-time-invisible hazard, so it fails
    * loudly rather than silently digesting a rendering.
    */
  private def anyDigest(a: Any): ContentHash = a match
    case v: String  => KeyDigest[String].digest(v)
    case v: Int     => KeyDigest[Int].digest(v)
    case v: Long    => KeyDigest[Long].digest(v)
    case v: Double  => KeyDigest[Double].digest(v)
    case v: Boolean => KeyDigest[Boolean].digest(v)
    case v: Product =>
      // A nested product: recurse, preserving position separators.
      val parts = v.productIterator.toVector.zipWithIndex.map { (f, i) =>
        ContentHash.combine(ContentHash.ofString("f" + i), anyDigest(f))
      }
      tagged("p" + v.productArity, ContentHash.combineAll(parts))
    case other =>
      throw new IllegalArgumentException(
        s"KeyDigest has no instance for ${other.getClass.getSimpleName}. " +
          "Keys must be built from strings, integers, longs, doubles, booleans, " +
          "options and case classes of those. Falling back to hashCode or toString " +
          "would make sampling depend on the platform and the release."
      )

end KeyDigest
