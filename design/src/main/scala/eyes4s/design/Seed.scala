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

/** A source of reproducible randomness, threaded explicitly.
  *
  * ==No global RNG, ever== (PRD X-11)
  *
  * `eyesim` calls `set.seed` inside a fold-construction helper, which mutates
  * the caller's global stream: running an analysis changes the results of
  * whatever the caller does next, and running two analyses in the other order
  * changes both. A seed passed as a value cannot do that.
  *
  * ==Derivation, not sequencing==
  *
  * [[derive]] produces an independent seed from a label rather than advancing a
  * shared one. That is what makes a result independent of evaluation order: a
  * stratum's sampling depends on the stratum's name, not on how many strata
  * happened to be processed first.
  */
opaque type Seed = Long

object Seed:

  def apply(value: Long): Seed = value

  extension (s: Seed)
    def value: Long = s

    /** An independent seed, named.
      *
      * Deriving by label rather than by position means adding a participant to
      * a study does not reshuffle everyone else's sampling.
      */
    def derive(label: String): Seed =
      ContentHash.combine(
        ContentHash.ofString("seed:" + label),
        ContentHash.of(IArray.empty)
      ) match
        case h => mix(s ^ h.value)

    /** A generator that starts from this seed. */
    def generator: Rng = new Rng(s)

  /** SplitMix64's finalising mix. Long-only arithmetic, so the result is
    * identical under Scala.js, where `Long` is emulated exactly.
    */
  private[design] def mix(z0: Long): Long =
    var z = z0
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)

  given cats.kernel.Order[Seed] =
    cats.kernel.Order.from((a, b) => java.lang.Long.compare(a, b))

end Seed

/** A splittable pseudo-random generator (SplitMix64).
  *
  * ==Why not `scala.util.Random`==
  *
  * Its algorithm is not part of its specification, so nothing guarantees a JVM
  * run and a browser run agree. A study whose control sampling differs between
  * the machine that ran it and the machine that reproduces it is not
  * reproducible, whatever the seed says. Every operation here is `Long`
  * arithmetic, exact on both platforms (DET-2).
  *
  * Mutable, and deliberately not shared: hold one per computation.
  */
final class Rng private[design] (private var state: Long):

  def nextLong(): Long =
    state += 0x9e3779b97f4a7c15L
    Seed.mix(state)

  /** Uniform in `[0, 1)`, using the top 53 bits -- the ones a `Double` can
    * represent exactly.
    */
  def nextDouble(): Double =
    (nextLong() >>> 11).toDouble * (1.0 / (1L << 53).toDouble)

  def nextInt(bound: Int): Int =
    if bound <= 0 then 0 else ((nextLong() >>> 1) % bound).toInt

end Rng
