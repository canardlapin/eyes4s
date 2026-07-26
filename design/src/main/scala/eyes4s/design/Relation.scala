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

/** A named, typed projection out of a key.
  *
  * The name is not decoration: it is what lets a diagnostic say *which* field
  * excluded a pair, and what a serialised plan stores instead of a closure.
  */
final case class Projection[K, V](name: String, extract: K => V):
  def apply(k: K): V = extract(k)

object Projection:

  /** Name a projection at the point where it is defined.
    *
    * The curried shape keeps the readable part first and lets Scala infer both
    * type parameters from the extractor:
    * `Projection.named("image")(_.image)`.
    */
  def named[K, V](name: String)(extract: K => V): Projection[K, V] =
    Projection(name, extract)

/** Which pairs of keys are eligible to be compared.
  *
  * ==Structure, not a predicate== (PRD X-2)
  *
  * The obvious design is `(L, R) => Boolean`. It is also a dead end. A closure
  * cannot be executed as a hash join, cannot explain why a pair was excluded,
  * and cannot be written into a saved plan. So the relation is a sealed ADT
  * whose cases name what they compare, and `accepts` is an *interpreter* over
  * that structure rather than the structure itself.
  *
  * There is deliberately no `Where(f: (L, R) => Boolean)` case. Adding one
  * would collapse every optimisation and every explanation back to opacity the
  * moment a caller reached for it, and callers reach for the escape hatch --
  * that is what escape hatches are.
  *
  * ==What eyesim does instead==
  *
  * A `match()` on a column name: first-match, silent, unidirectional. Its own
  * flagship vignette compares every participant's retrieval map against
  * participant s1's encoding map because of it. A relation that names its
  * projections cannot express that by accident.
  */
sealed trait Relation[L, R]:

  /** The derived interpreter. Not the representation. */
  def accepts(l: L, r: R): Boolean

  /** Human-readable structure, for diagnostics and for a plan's display. */
  def render: String

  def and(that: Relation[L, R]): Relation[L, R] =
    (this, that) match
      case (Relation.All(), _) => that
      case (_, Relation.All()) => this
      case _                   => Relation.And(this, that)

  /** Equality projections, which an execution strategy can hash-join on. */
  def joinKeys: Vector[(String, String)] = this match
    case Relation.SameOn(l, r) => Vector(l.name -> r.name)
    case Relation.And(a, b)    => a.joinKeys ++ b.joinKeys
    case _                     => Vector.empty

object Relation:

  /** Every pair is eligible. */
  final case class All[L, R]() extends Relation[L, R]:
    def accepts(l: L, r: R): Boolean = true
    def render: String               = "all"

  /** Eligible when two projections agree. */
  final case class SameOn[L, R, V](left: Projection[L, V], right: Projection[R, V])
      extends Relation[L, R]:
    def accepts(l: L, r: R): Boolean = left(l) == right(r)
    def render: String               = s"${left.name} == ${right.name}"

  /** Eligible when two projections differ.
    *
    * The control-pair relation, and the reason it is a first-class case rather
    * than `not(SameOn)`: a negated join is a different execution strategy, and
    * a diagnostic explaining an exclusion wants to say "same image" rather than
    * "failed the negation of an equality".
    */
  final case class DifferentOn[L, R, V](left: Projection[L, V], right: Projection[R, V])
      extends Relation[L, R]:
    def accepts(l: L, r: R): Boolean = left(l) != right(r)
    def render: String               = s"${left.name} != ${right.name}"

  final case class And[L, R](a: Relation[L, R], b: Relation[L, R]) extends Relation[L, R]:
    def accepts(l: L, r: R): Boolean = a.accepts(l, r) && b.accepts(l, r)
    def render: String               = s"(${a.render} and ${b.render})"

  def all[L, R]: Relation[L, R] = All()

  /** The common case: the same projection on both sides of a within-collection
    * design.
    */
  def sameOn[K, V](p: Projection[K, V]): Relation[K, K] = SameOn(p, p)

  def differentOn[K, V](p: Projection[K, V]): Relation[K, K] = DifferentOn(p, p)

end Relation

/** Whether a key may be paired with itself, in a within-collection design. */
enum SelfPolicy derives CanEqual:
  case Exclude, Include

/** Which sample of a repeated design this is.
  *
  * A distinct identifier requests an independent priority field from the same
  * seed, so a second control sample of the same data is genuinely independent
  * rather than a reshuffle of the first (PRD X-9).
  */
final case class SampleId(value: String) derives CanEqual

/** A positive per-focal pair limit.
  *
  * `BottomK(0, ...)` is not a useful design: it selects no pairs and can only
  * produce an empty analysis. Keeping the positivity in this type means the
  * [[Selection.BottomK]] case itself cannot represent that state, while the
  * fluent [[Pairing]] facade still accepts an ordinary `Int` and reports a
  * construction error.
  */
opaque type PairLimit = Int

object PairLimit:
  def of(value: Int): Either[PairingError, PairLimit] =
    if value > 0 then Right(value) else Left(PairingError.NonPositiveLimit(value))

  extension (limit: PairLimit) def value: Int = limit

/** Failures constructing a pair design from user-facing values. */
enum PairingError derives CanEqual:
  case NonPositiveLimit(value: Int)

  def message: String = this match
    case NonPositiveLimit(value) =>
      s"A bottom-k pair design needs a positive limit, got $value. " +
        "Use Selection.All when every eligible pair should be retained."

/** How many of the eligible candidates to keep. */
enum Selection derives CanEqual:

  /** Every eligible candidate. */
  case All

  /** A bounded, keyed sample of the eligible candidates.
    *
    * ==Cap does not enter the priority, and that is the whole design==
    *
    * Each candidate's priority comes from `(seed, sampleId, focal key,
    * candidate key)` alone. The cap decides how many are *taken*, never what
    * they *are*. Three consequences follow, and each is a test:
    *
    *   - raising the cap yields a superset, so a reviewer asking for more
    *     controls sees the original ones plus more, not a different sample;
    *   - the result does not depend on the order the candidates arrive in;
    *   - adding candidates in an unrelated stratum changes nothing here.
    *
    * `eyesim` gets all three wrong in the same few lines: it samples first and
    * removes the true match afterwards, so the realised count is `n` or `n-1`
    * unpredictably -- which is why it has to return an `n_perm` column at all.
    * It also implements the whole procedure three times with different
    * semantics.
    */
  case BottomK(cap: PairLimit, seed: Seed, sampleId: SampleId)

object Selection:

  /** The priority of one directed candidate.
    *
    * Deliberately excludes the cap and every other selection parameter.
    */
  def priority[KL, KR](
      seed: Seed,
      sampleId: SampleId,
      focal: KL,
      candidate: KR
  )(using dl: KeyDigest[KL], dr: KeyDigest[KR]): Long =
    val h = eyes4s.kernel.ContentHash.combineAll(
      Vector(
        eyes4s.kernel.ContentHash.ofString("bottomk"),
        eyes4s.kernel.ContentHash.ofString(sampleId.value),
        dl.digest(focal),
        dr.digest(candidate)
      )
    )
    Seed.mix(seed.value ^ h.value)

  /** Take the `cap` lowest-priority candidates.
    *
    * Ties break on the priority alone, so the result is a function of the keys
    * and never of the input order. Exclusion of ineligible candidates -- the
    * true match among them -- happens BEFORE this is called, so the realised
    * count is `min(cap, eligible)` and is knowable rather than reported after
    * the fact.
    */
  private[design] def bottomK[KL, KR](
      focal: KL,
      candidates: Vector[KR],
      cap: Int,
      seed: Seed,
      sampleId: SampleId
  )(using KeyDigest[KL], KeyDigest[KR]): Vector[KR] =
    bottomKBy(focal, candidates, identity, cap, seed, sampleId)

  /** Apply the same keyed sampler while retaining a richer candidate value.
    *
    * Pair construction needs the complete [[Trial]], but priorities are defined
    * only by its key. Keeping that projection here prevents a second baseline
    * sampler from drifting away from [[bottomK]].
    */
  private[design] def bottomKBy[KL, C, KR](
      focal: KL,
      candidates: Vector[C],
      keyOf: C => KR,
      cap: Int,
      seed: Seed,
      sampleId: SampleId
  )(using KeyDigest[KL], KeyDigest[KR]): Vector[C] =
    if cap <= 0 then Vector.empty
    else
      candidates
        .map(candidate => (priority(seed, sampleId, focal, keyOf(candidate)), candidate))
        .sortBy(_._1)
        .take(cap)
        .map(_._2)

end Selection

/** The space of pairs an analysis draws from.
  *
  * ==Only meaningful combinations are inhabitants== (PRD X-3)
  *
  * A self policy is meaningless between two different collections -- there is
  * no "self" to include or exclude. Per-focal sampling is meaningless for a
  * canonical-undirected design, where an edge is stored once and has no focal
  * side. Rather than accept all four parameters everywhere and validate at run
  * time, the sum type simply has no case in which the meaningless combination
  * can be written.
  */
sealed trait PairDesign[L, R]:
  def relation: Relation[L, R]
  def render: String

object PairDesign:

  /** Two distinct collections; every pair is directed left-to-right. */
  final case class BetweenDirected[L, R](
      relation: Relation[L, R],
      selection: Selection
  ) extends PairDesign[L, R]:
    def render: String = s"between-directed[${relation.render}, $selection]"

  /** One collection compared with itself, orientation retained. */
  final case class WithinDirected[K](
      relation: Relation[K, K],
      self: SelfPolicy,
      selection: Selection
  ) extends PairDesign[K, K]:
    def render: String = s"within-directed[${relation.render}, self=$self, $selection]"

  /** One collection compared with itself, each unordered edge stored once.
    *
    * Has no `Selection`: there is no focal side to sample per. Evaluating one
    * requires a `SymmetricCompare`, because storing an edge once and reporting
    * it as though orientation did not matter is only sound if it genuinely does
    * not.
    */
  final case class WithinUndirected[K](
      relation: Relation[K, K],
      self: SelfPolicy
  ) extends PairDesign[K, K]:
    def render: String = s"within-undirected[${relation.render}, self=$self]"

  /** Start a readable between-collection design.
    *
    * This delegates to [[Pairing]] so there remains one facade and one algebra.
    */
  def between[L, R]: Pairing.Between[L, R] = Pairing.between[L, R]

  /** Start a readable within-collection design. */
  def within[K]: Pairing.Within[K] = Pairing.within[K]

end PairDesign

/** A fluent, typed facade over [[Relation]] and [[PairDesign]].
  *
  * The facade is intentionally thin: every terminal method returns one of the
  * three legal `PairDesign` inhabitants, and it owns no execution path. The
  * common call site reads as the scientific design:
  *
  * {{{
  * val target =
  *   Pairing.between[Key, Key]
  *     .sameOn(subjectImage, subjectImage)
  *     .all
  *
  * val controls =
  *   Pairing.between[Key, Key]
  *     .sameOn(subject, subject)
  *     .differentOn(image, image)
  *     .bottomK(50, seed, SampleId("controls"))
  * }}}
  *
  * The final expression is an `Either` only where raw configuration can be
  * invalid. Relation composition and legal orientation choices remain total.
  */
object Pairing:

  final case class Between[L, R] private[design] (relation: Relation[L, R]):

    def sameOn[V](
        left: Projection[L, V],
        right: Projection[R, V]
    ): Between[L, R] =
      copy(relation = relation.and(Relation.SameOn(left, right)))

    def differentOn[V](
        left: Projection[L, V],
        right: Projection[R, V]
    ): Between[L, R] =
      copy(relation = relation.and(Relation.DifferentOn(left, right)))

    /** Retain every eligible directed pair. */
    def all: PairDesign.BetweenDirected[L, R] =
      PairDesign.BetweenDirected(relation, Selection.All)

    /** Retain a deterministic positive number of candidates per left key. */
    def bottomK(
        cap: Int,
        seed: Seed,
        sampleId: SampleId
    ): Either[PairingError, PairDesign.BetweenDirected[L, R]] =
      PairLimit
        .of(cap)
        .map(limit =>
          PairDesign.BetweenDirected(relation, Selection.BottomK(limit, seed, sampleId))
        )

  final case class Within[K] private[design] (
      relation: Relation[K, K],
      self: SelfPolicy
  ):

    def sameOn[V](projection: Projection[K, V]): Within[K] =
      copy(relation = relation.and(Relation.sameOn(projection)))

    def differentOn[V](projection: Projection[K, V]): Within[K] =
      copy(relation = relation.and(Relation.differentOn(projection)))

    def excludingSelf: Within[K] = copy(self = SelfPolicy.Exclude)
    def includingSelf: Within[K] = copy(self = SelfPolicy.Include)

    /** Retain orientation and every eligible pair. */
    def directed: PairDesign.WithinDirected[K] =
      PairDesign.WithinDirected(relation, self, Selection.All)

    /** Retain orientation and a deterministic positive sample per focal key. */
    def bottomK(
        cap: Int,
        seed: Seed,
        sampleId: SampleId
    ): Either[PairingError, PairDesign.WithinDirected[K]] =
      PairLimit
        .of(cap)
        .map(limit =>
          PairDesign.WithinDirected(relation, self, Selection.BottomK(limit, seed, sampleId))
        )

    /** Store every unordered edge once. Sampling is absent by construction. */
    def canonicalUndirected: PairDesign.WithinUndirected[K] =
      PairDesign.WithinUndirected(relation, self)

  def between[L, R]: Between[L, R] = Between(Relation.all[L, R])

  def within[K]: Within[K] = Within(Relation.all[K, K], SelfPolicy.Exclude)

  /** Match equal keys across two collections. */
  def matched[K]: PairDesign.BetweenDirected[K, K] =
    matchedOn(Projection.named[K, K]("key")(identity))

  /** Match equal projected values across two collections. */
  def matchedOn[K, V](projection: Projection[K, V]): PairDesign.BetweenDirected[K, K] =
    PairDesign.BetweenDirected(Relation.sameOn(projection), Selection.All)

  /** Directed within-collection controls drawn from the same stratum. */
  def mismatchedWithin[K, V](
      stratum: Projection[K, V]
  ): PairDesign.WithinDirected[K] =
    PairDesign.WithinDirected(Relation.sameOn(stratum), SelfPolicy.Exclude, Selection.All)

  /** Sample eligible directed pairs from an explicit relation.
    *
    * This is the task-named convenience constructor required by PRD X-4. It
    * delegates to the same validated [[Between.bottomK]] path used by the
    * fluent facade, so the library still has exactly one baseline sampler.
    */
  def sampled[L, R](
      relation: Relation[L, R],
      cap: Int,
      seed: Seed,
      sampleId: SampleId
  ): Either[PairingError, PairDesign.BetweenDirected[L, R]] =
    Between(relation).bottomK(cap, seed, sampleId)

end Pairing
