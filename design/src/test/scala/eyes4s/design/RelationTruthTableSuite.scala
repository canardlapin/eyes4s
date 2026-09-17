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
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

/** Truth tables for every [[Relation]] constructor and every legal
  * [[PairDesign]] inhabitant (PRD X-2, X-3, V-9).
  *
  * ==The tables are hand-derived==
  *
  * Each expected grid below is written as literal characters, rows indexed by
  * the left key and columns by the right key, over the fixed four-key space
  * `p1a, p1b, p2a, p2b` (participant x image). Nothing in an expected column
  * is computed by calling the relation; the interpreter is only ever on the
  * observed side of the comparison.
  *
  * ==What the algebra does and does not claim==
  *
  * The ADT has `All`, `SameOn`, `DifferentOn` and `And`, and one combinator,
  * `and`, which absorbs `All`. There is no `Or` and no `Not`, so there are no
  * De Morgan laws to state. `DifferentOn` is a first-class case rather than a
  * negation, and its only law is complementarity with `SameOn` on the same
  * projections. Commutativity, associativity and idempotence of `and` hold at
  * the interpreter (`accepts`) and are checked there; they do NOT hold
  * structurally, because `render` and `joinKeys` are order-preserving by
  * design, and that is also pinned.
  *
  * ==Table sensitivity: alternative relation and design values==
  *
  * These "mutants" are alternative `Relation` and `PairDesign` VALUES, not
  * alternative interpreter implementations: each is a plausible mis-stated
  * design, and the row records the table cell that tells it apart from the
  * intended one. (The sampling suite's mutants, by contrast, are alternative
  * sampler implementations.)
  *
  * {{{
  * | mutant                         | changed expression                          | killed by                    |
  * |--------------------------------|---------------------------------------------|------------------------------|
  * | drop-participant-conjunct      | controls without SameOn(participant)        | controls table, p1a->p2b     |
  * | drop-image-conjunct            | controls without DifferentOn(image)         | controls table, p1a->p1a     |
  * | invert-phase-equality          | reinstatement with SameOn(phase)            | reinstatement table, p1e->p1e|
  * | include-self-edges             | WithinDirected self=Include                 | within-directed pair list    |
  * | store-edges-twice              | WithinUndirected mirrored on storage        | undirected edge list         |
  * | asymmetric-under-undirected    | SymmetricEvaluator over (l, r) => l - r     | endpoint reversal invariance |
  * }}}
  */
class RelationTruthTableSuite extends ScalaCheckSuite:

  final case class Key(participant: String, image: String) derives CanEqual

  private given KeyDigest[Key] = KeyDigest.derived[Key]

  private val participant = Projection.named[Key, String]("participant")(_.participant)
  private val image       = Projection.named[Key, String]("image")(_.image)

  /** The enumerated key space. Row and column order of every table. */
  private val labels = Vector("p1a", "p1b", "p2a", "p2b")
  private val space  = Vector(Key("p1", "a"), Key("p1", "b"), Key("p2", "a"), Key("p2", "b"))

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(200).withInitialSeed(0x52454c4eL)

  // ---------------------------------------------------------------------------
  // Tables
  // ---------------------------------------------------------------------------

  /** A 4x4 grid of accept ('1') / reject ('0'), left key by row. */
  final case class Table(rows: Vector[String]):
    require(rows.size == 4 && rows.forall(r => r.length == 4 && r.forall("01".contains(_))))
    def cell(l: Int, r: Int): Boolean = rows(l).charAt(r) == '1'

  private def table(p1a: String, p1b: String, p2a: String, p2b: String): Table =
    Table(Vector(p1a, p1b, p2a, p2b))

  private def observed[L, R](relation: Relation[L, R], ls: Vector[L], rs: Vector[R]): Table =
    Table(ls.map(l => rs.map(r => if relation.accepts(l, r) then '1' else '0').mkString))

  private def mismatches(
      actual: Table,
      expected: Table,
      names: Vector[String] = labels
  ): Vector[String] =
    for
      l <- (0 until 4).toVector
      r <- (0 until 4).toVector
      if actual.cell(l, r) != expected.cell(l, r)
    yield s"${names(l)}->${names(r)}: expected ${expected.cell(l, r)}, got ${actual.cell(l, r)}"

  private def assertTable(relation: Relation[Key, Key], expected: Table): Unit =
    val bad = mismatches(observed(relation, space, space), expected)
    assert(bad.isEmpty, clue((relation.render, bad)))

  //                            right:  p1a p1b p2a p2b
  val allTable: Table = table("1111", "1111", "1111", "1111")

  val sameParticipant: Table = table("1100", "1100", "0011", "0011")
  val sameImage: Table       = table("1010", "0101", "1010", "0101")
  val diffParticipant: Table = table("0011", "0011", "1100", "1100")
  val diffImage: Table       = table("0101", "1010", "0101", "1010")

  /** same participant AND same image: the identity, a matched target pair. */
  val matched: Table = table("1000", "0100", "0010", "0001")

  /** same participant AND different image: the within-participant control. */
  val controls: Table = table("0100", "1000", "0001", "0010")

  /** different participant AND same image: another participant's view. */
  val otherViewer: Table = table("0010", "0001", "1000", "0100")

  /** different participant AND different image: unrelated. */
  val unrelated: Table = table("0001", "0010", "0100", "1000")

  test("All accepts every ordered pair") {
    assertTable(Relation.all[Key, Key], allTable)
    assertTable(Relation.All(), allTable)
  }

  test("SameOn on each projection") {
    assertTable(Relation.sameOn(participant), sameParticipant)
    assertTable(Relation.sameOn(image), sameImage)
    assertTable(Relation.SameOn(participant, participant), sameParticipant)
  }

  test("DifferentOn on each projection") {
    assertTable(Relation.differentOn(participant), diffParticipant)
    assertTable(Relation.differentOn(image), diffImage)
    assertTable(Relation.DifferentOn(image, image), diffImage)
  }

  test("And of every leaf combination over two projections") {
    assertTable(Relation.And(Relation.sameOn(participant), Relation.sameOn(image)), matched)
    assertTable(
      Relation.And(Relation.sameOn(participant), Relation.differentOn(image)),
      controls
    )
    assertTable(
      Relation.And(Relation.differentOn(participant), Relation.sameOn(image)),
      otherViewer
    )
    assertTable(
      Relation.And(Relation.differentOn(participant), Relation.differentOn(image)),
      unrelated
    )
    // Contradictions on one projection reject everything.
    assertTable(
      Relation.And(Relation.sameOn(image), Relation.differentOn(image)),
      table("0000", "0000", "0000", "0000")
    )
  }

  test("the and combinator and the fluent facade produce the same tables") {
    assertTable(Relation.sameOn(participant).and(Relation.differentOn(image)), controls)
    assertTable(
      Pairing.within[Key].sameOn(participant).differentOn(image).directed.relation,
      controls
    )
    assertTable(
      Pairing
        .between[Key, Key]
        .sameOn(participant, participant)
        .sameOn(image, image)
        .all
        .relation,
      matched
    )
    assertTable(Pairing.matchedOn(image).relation, sameImage)
    assertTable(Pairing.mismatchedWithin(participant).relation, sameParticipant)
    // Nested And is still a conjunction: adding a conjunct already present
    // changes nothing in the table.
    assertTable(
      Relation.And(
        Relation.And(Relation.sameOn(participant), Relation.differentOn(image)),
        Relation.sameOn(participant)
      ),
      controls
    )
  }

  test("All is absorbed by and, structurally") {
    val r = Relation.sameOn(image)
    assertEquals(Relation.all[Key, Key].and(r), r)
    assertEquals(r.and(Relation.all[Key, Key]), r)
    assertEquals(Relation.all[Key, Key].and(Relation.all[Key, Key]), Relation.all[Key, Key])
    // But a literal And(All, r) is a different value with the same table.
    assertNotEquals(Relation.And(Relation.all[Key, Key], r): Relation[Key, Key], r)
    assertTable(Relation.And(Relation.all[Key, Key], r), sameImage)
  }

  // ---------------------------------------------------------------------------
  // Heterogeneous between-collection keys and the phase table
  // ---------------------------------------------------------------------------

  final case class Encoding(subject: String, picture: String) derives CanEqual
  final case class Retrieval(participant: String, image: String) derives CanEqual

  private val encSubject = Projection.named[Encoding, String]("subject")(_.subject)
  private val encPicture = Projection.named[Encoding, String]("picture")(_.picture)
  private val retPerson  = Projection.named[Retrieval, String]("participant")(_.participant)
  private val retImage   = Projection.named[Retrieval, String]("image")(_.image)
  private val encodings  = space.map(k => Encoding(k.participant, k.image))
  private val retrievals = space.map(k => Retrieval(k.participant, k.image))

  test("SameOn and DifferentOn across two different key types") {
    def check(relation: Relation[Encoding, Retrieval], expected: Table): Unit =
      val bad = mismatches(observed(relation, encodings, retrievals), expected)
      assert(bad.isEmpty, clue((relation.render, bad)))
    check(Relation.SameOn(encSubject, retPerson), sameParticipant)
    check(Relation.DifferentOn(encPicture, retImage), diffImage)
    check(
      Pairing
        .between[Encoding, Retrieval]
        .sameOn(encSubject, retPerson)
        .differentOn(encPicture, retImage)
        .all
        .relation,
      controls
    )
    // Crossing the projections compares subject with image: on this space no
    // participant label equals an image label, so nothing matches.
    check(Relation.SameOn(encSubject, retImage), table("0000", "0000", "0000", "0000"))
    check(Relation.DifferentOn(encSubject, retImage), allTable)
    assertEquals(Relation.SameOn(encSubject, retPerson).render, "subject == participant")
  }

  final case class Occasion(participant: String, phase: String) derives CanEqual

  private val occPerson = Projection.named[Occasion, String]("participant")(_.participant)
  private val phase     = Projection.named[Occasion, String]("phase")(_.phase)

  /** `p1e, p1r, p2e, p2r`: the encoding/retrieval occasions of two people. */
  private val occasionLabels = Vector("p1e", "p1r", "p2e", "p2r")
  private val occasions      = Vector(
    Occasion("p1", "enc"),
    Occasion("p1", "ret"),
    Occasion("p2", "enc"),
    Occasion("p2", "ret")
  )

  /** same participant AND different phase: the reinstatement pair. */
  val reinstatement: Table = table("0100", "1000", "0001", "0010")

  /** same participant AND same phase: the same occasion, not reinstatement. */
  val sameOccasion: Table = table("1000", "0100", "0010", "0001")

  test("the reinstatement relation pairs each occasion with its other phase only") {
    def check(relation: Relation[Occasion, Occasion], expected: Table): Unit =
      val bad = mismatches(observed(relation, occasions, occasions), expected, occasionLabels)
      assert(bad.isEmpty, clue((relation.render, bad)))
    check(Relation.sameOn(occPerson).and(Relation.differentOn(phase)), reinstatement)
    check(Relation.sameOn(occPerson).and(Relation.sameOn(phase)), sameOccasion)
    check(Relation.differentOn(phase), table("0101", "1010", "0101", "1010"))
  }

  // ---------------------------------------------------------------------------
  // Every legal PairDesign inhabitant over the same space
  // ---------------------------------------------------------------------------

  private val values = Map("p1a" -> 1.0, "p1b" -> 2.0, "p2a" -> 4.0, "p2b" -> 8.0)
  private def label(k: Key): String = s"${k.participant}${k.image}"
  private val trials                = Trials(space.map(k => Trial(k, (), values(label(k)))))

  private def directed(
      p: DirectedPaired[Key, Unit, Key, Unit, Double, Double]
  ): Vector[String] =
    p.pairs.map { case (l, r) => s"${label(l.key)}>${label(r.key)}" }
  private def undirected(p: UndirectedPaired[Key, Unit, Double]): Vector[String] =
    p.pairs.map { case (l, r) => s"${label(l.key)}-${label(r.key)}" }

  private val within = Relation.sameOn(participant)

  /** Hand-derived pair lists for `participant == participant`, left-major
    * source order, over `p1a, p1b, p2a, p2b`.
    */
  val betweenPairs: Vector[String] =
    Vector(
      "p1a>p1a",
      "p1a>p1b",
      "p1b>p1a",
      "p1b>p1b",
      "p2a>p2a",
      "p2a>p2b",
      "p2b>p2a",
      "p2b>p2b"
    )
  val directedExclude: Vector[String]   = Vector("p1a>p1b", "p1b>p1a", "p2a>p2b", "p2b>p2a")
  val directedInclude: Vector[String]   = betweenPairs
  val undirectedExclude: Vector[String] = Vector("p1a-p1b", "p2a-p2b")
  val undirectedInclude: Vector[String] =
    Vector("p1a-p1a", "p1a-p1b", "p1b-p1b", "p2a-p2a", "p2a-p2b", "p2b-p2b")

  test("BetweenDirected: a collection paired with a copy of itself has no self policy") {
    val paired = pair(trials, trials, PairDesign.BetweenDirected(within, Selection.All))
    assertEquals(directed(paired), betweenPairs)
    assertEquals(paired.eligiblePairCount, 8L)
    assertEquals(paired.storage, PairStorage.BetweenDirected)
    assertEquals(paired.unmatchedLeft, Vector.empty)
    assertEquals(paired.unmatchedRight, Vector.empty)
  }

  test("WithinDirected: self exclusion removes the diagonal; inclusion keeps it") {
    val exclude =
      pair(trials, PairDesign.WithinDirected(within, SelfPolicy.Exclude, Selection.All))
    val include =
      pair(trials, PairDesign.WithinDirected(within, SelfPolicy.Include, Selection.All))
    assertEquals(directed(exclude), directedExclude)
    assertEquals(exclude.eligiblePairCount, 4L)
    assertEquals(directed(include), directedInclude)
    assertEquals(include.eligiblePairCount, 8L)
    assertEquals(exclude.storage, PairStorage.WithinDirected)
    // The directed list is closed under mirroring: every a>b has its b>a.
    val mirrored = directedExclude.map(_.split('>')).map(p => s"${p(1)}>${p(0)}")
    assertEquals(mirrored.sorted, directedExclude.sorted)
  }

  test("WithinUndirected: canonical edge storage keeps one of each mirrored pair") {
    val exclude = pair(trials, PairDesign.WithinUndirected(within, SelfPolicy.Exclude))
    val include = pair(trials, PairDesign.WithinUndirected(within, SelfPolicy.Include))
    assertEquals(undirected(exclude), undirectedExclude)
    assertEquals(exclude.eligiblePairCount, 2L)
    assertEquals(undirected(include), undirectedInclude)
    assertEquals(include.eligiblePairCount, 6L)
    assertEquals(exclude.storage, PairStorage.WithinUndirected)
    // Exactly half the directed edges, with the earlier source row on the left.
    assertEquals(exclude.eligiblePairCount * 2, 4L)
    exclude.pairs.foreach { case (l, r) =>
      assert(space.indexOf(l.key) < space.indexOf(r.key), clue((l.key, r.key)))
    }
  }

  test("the fluent facade reaches exactly the three inhabitants") {
    val w = Pairing.within[Key].sameOn(participant)
    assertEquals(directed(pair(trials, w.directed)), directedExclude)
    assertEquals(directed(pair(trials, w.includingSelf.directed)), directedInclude)
    assertEquals(undirected(pair(trials, w.canonicalUndirected)), undirectedExclude)
    assertEquals(
      undirected(pair(trials, w.includingSelf.canonicalUndirected)),
      undirectedInclude
    )
    assertEquals(
      directed(
        pair(trials, trials, Pairing.between[Key, Key].sameOn(participant, participant).all)
      ),
      betweenPairs
    )
  }

  // ---------------------------------------------------------------------------
  // Reductions: directed left/right versus canonical edge and mirrored endpoint
  // ---------------------------------------------------------------------------

  private val inputs = ContentHash.ofString("relation-truth-tables")
  private val info   = EvaluationInfo("difference", EvaluationScale.Unitless)

  /** Directed scores `left - right` over `directedExclude`:
    * p1a>p1b = -1, p1b>p1a = 1, p2a>p2b = -4, p2b>p2a = 4.
    */
  test("directed left and right reductions are distinct orientations") {
    val paired =
      pair(trials, PairDesign.WithinDirected(within, SelfPolicy.Exclude, Selection.All))
    val evaluated = evaluatePairs(paired, inputs, info)((l: Double, r: Double) =>
      Right(l - r): Either[String, Double]
    )
    assertEquals(
      evaluated.rows.map(_.result),
      Vector(Right(-1.0), Right(1.0), Right(-4.0), Right(4.0))
    )

    val byLeft  = evaluated.meanByLeft(FailurePolicy.RequireAll)
    val byRight = evaluated.meanByRight(FailurePolicy.RequireAll)
    assertEquals(
      byLeft.rows.map { case (k, v) => label(k) -> v },
      Vector(
        "p1a" -> Right(-1.0),
        "p1b" -> Right(1.0),
        "p2a" -> Right(-4.0),
        "p2b" -> Right(4.0)
      )
    )
    // Keyed by the right operand, in order of first appearance on the right.
    assertEquals(
      byRight.rows.map { case (k, v) => label(k) -> v },
      Vector(
        "p1b" -> Right(-1.0),
        "p1a" -> Right(1.0),
        "p2b" -> Right(-4.0),
        "p2a" -> Right(4.0)
      )
    )
    assertEquals(byLeft.diagnostics.orientation, ReductionOrientation.ByLeft)
    assertEquals(byRight.diagnostics.orientation, ReductionOrientation.ByRight)
    assertEquals(byLeft.diagnostics.contributionCount, 4)
    assertEquals(byRight.diagnostics.contributionCount, 4)
  }

  /** Symmetric scores `|left - right|` over `undirectedExclude`:
    * p1a-p1b = 1, p2a-p2b = 4.
    */
  test("canonical edge reduction counts each edge once; mirrored endpoints count it twice") {
    val paired    = pair(trials, PairDesign.WithinUndirected(within, SelfPolicy.Exclude))
    val evaluated = evaluatePairs(paired, inputs, info)(
      SymmetricEvaluator[Double, String, Double]((l, r) => Right(math.abs(l - r)))
    )
    assertEquals(evaluated.rows.map(_.result), Vector(Right(1.0), Right(4.0)))

    val edges     = evaluated.meanEdges(FailurePolicy.RequireAll)
    val endpoints = evaluated.meanByEndpoint(FailurePolicy.RequireAll)
    assertEquals(edges.rows, Vector(() -> Right(2.5)))
    assertEquals(edges.diagnostics.orientation, ReductionOrientation.EdgesOnce)
    assertEquals(edges.diagnostics.contributionCount, 2)
    assertEquals(
      endpoints.rows.map { case (k, v) => label(k) -> v },
      Vector("p1a" -> Right(1.0), "p1b" -> Right(1.0), "p2a" -> Right(4.0), "p2b" -> Right(4.0))
    )
    assertEquals(endpoints.diagnostics.orientation, ReductionOrientation.MirroredEndpoints)
    assertEquals(endpoints.diagnostics.contributionCount, 4)
  }

  test("uniqueness diagnostics and participant scope: duplicates and singletons are explicit") {
    // p1a twice, and p3a with no partner in its participant stratum.
    val rows = Trials(
      Vector(
        Trial(Key("p1", "a"), (), 1.0),
        Trial(Key("p1", "b"), (), 2.0),
        Trial(Key("p2", "a"), (), 4.0),
        Trial(Key("p2", "b"), (), 8.0),
        Trial(Key("p1", "a"), (), 16.0),
        Trial(Key("p3", "a"), (), 32.0)
      )
    )
    val paired =
      pair(rows, PairDesign.WithinDirected(within, SelfPolicy.Exclude, Selection.All))
    assertEquals(directed(paired), Vector("p2a>p2b", "p2b>p2a"))
    assertEquals(paired.eligiblePairCount, 2L)
    assertEquals(paired.unmatchedLeft.map(label), Vector("p1b", "p3a"))
    assertEquals(paired.unmatchedRight.map(label), Vector("p1b", "p3a"))
    assertEquals(
      paired.diagnostics.ambiguous,
      Vector(
        PairingAmbiguity.DuplicateLeft[Key, Key](Key("p1", "a"), Vector(0, 4)),
        PairingAmbiguity.DuplicateRight[Key, Key](Key("p1", "a"), Vector(0, 4))
      )
    )
    // Reduction keeps every one of them as a named row, not a dropped one.
    val reduced = evaluatePairs(paired, inputs, info)((l: Double, r: Double) =>
      Right(l - r): Either[String, Double]
    ).meanByLeft(FailurePolicy.RequireAll)
    assertEquals(
      reduced.rows.map { case (k, v) => label(k) -> v },
      Vector(
        "p2a" -> Right(-4.0),
        "p2b" -> Right(4.0),
        "p1b" -> Left(ReductionError.NoSelectedScores(Key("p1", "b"))),
        "p3a" -> Left(ReductionError.NoSelectedScores(Key("p3", "a"))),
        "p1a" -> Left(ReductionError.AmbiguousKey(Key("p1", "a"), Vector(0, 4)))
      )
    )
    assertEquals(reduced.diagnostics.reducedKeyCount, 2)
    assertEquals(reduced.diagnostics.failedKeys.map(label), Vector("p1b", "p3a", "p1a"))
  }

  // ---------------------------------------------------------------------------
  // Mutants the tables kill
  // ---------------------------------------------------------------------------

  private def disagreesAt(relation: Relation[Key, Key], expected: Table, cell: String): Unit =
    val bad = mismatches(observed(relation, space, space), expected)
    assert(bad.exists(_.startsWith(cell)), clue((relation.render, cell, bad)))

  test("mutant drop-participant-conjunct: killed by the controls table at p1a->p2b") {
    disagreesAt(Relation.differentOn(image), controls, "p1a->p2b")
  }

  test("mutant drop-image-conjunct: killed by the controls table at p1a->p1a") {
    disagreesAt(Relation.sameOn(participant), controls, "p1a->p1a")
  }

  test("mutant invert-phase-equality: killed by the reinstatement table at p1e->p1e") {
    val inverted = Relation.sameOn(occPerson).and(Relation.sameOn(phase))
    val bad      =
      mismatches(observed(inverted, occasions, occasions), reinstatement, occasionLabels)
    assert(bad.exists(_.startsWith("p1e->p1e")), clue(bad))
    assertEquals(bad.size, 8)
  }

  test("mutant include-self-edges: killed by the within-directed pair list") {
    val mutant =
      pair(trials, PairDesign.WithinDirected(within, SelfPolicy.Include, Selection.All))
    assertNotEquals(directed(mutant), directedExclude)
    assert(directed(mutant).contains("p1a>p1a"))
    assert(!directedExclude.contains("p1a>p1a"))
  }

  test("mutant store-edges-twice: killed by the undirected edge list") {
    // A storage that mirrored each edge is exactly the directed list.
    val mirrored =
      pair(trials, PairDesign.WithinDirected(within, SelfPolicy.Exclude, Selection.All))
    val stored = pair(trials, PairDesign.WithinUndirected(within, SelfPolicy.Exclude))
    assertNotEquals(directed(mirrored).size, stored.pairs.size)
    assertEquals(directed(mirrored).size, stored.pairs.size * 2)
  }

  test("mutant asymmetric-under-undirected: killed by endpoint invariance under row reversal") {
    // A symmetric evaluator makes mirrored-endpoint means independent of which
    // orientation canonical storage happened to keep; reversing the source
    // rows flips every stored edge, and the endpoint means must not move.
    def endpointMeans(t: Trials[Key, Unit, Double], f: (Double, Double) => Double) =
      evaluatePairs(
        pair(t, PairDesign.WithinUndirected(within, SelfPolicy.Exclude)),
        inputs,
        info
      )(SymmetricEvaluator[Double, String, Double]((l, r) => Right(f(l, r))))
        .meanByEndpoint(FailurePolicy.RequireAll)
        .rows
        .map { case (k, v) => label(k) -> v }
        .toMap
    val reversed = Trials(trials.rows.reverse)

    val symmetric = (l: Double, r: Double) => math.abs(l - r)
    assertEquals(endpointMeans(trials, symmetric), endpointMeans(reversed, symmetric))

    val asymmetric = (l: Double, r: Double) => l - r
    val forward    = endpointMeans(trials, asymmetric)
    val backward   = endpointMeans(reversed, asymmetric)
    assertNotEquals(forward, backward)
    assertEquals(forward("p1a"), Right(-1.0))
    assertEquals(backward("p1a"), Right(1.0))
  }

  // ---------------------------------------------------------------------------
  // Algebra laws, at the interpreter
  // ---------------------------------------------------------------------------

  private val genLeaf: Gen[Relation[Key, Key]] =
    Gen.oneOf(
      Relation.all[Key, Key],
      Relation.sameOn(participant),
      Relation.sameOn(image),
      Relation.differentOn(participant),
      Relation.differentOn(image)
    )

  private def genRelation(depth: Int): Gen[Relation[Key, Key]] =
    if depth == 0 then genLeaf
    else
      Gen.frequency(
        2 -> genLeaf,
        1 -> Gen.zip(genRelation(depth - 1), genRelation(depth - 1)).map { case (a, b) =>
          Relation.And(a, b)
        },
        1 -> Gen.zip(genRelation(depth - 1), genRelation(depth - 1)).map { case (a, b) =>
          a.and(b)
        }
      )

  private val genRel: Gen[Relation[Key, Key]] = genRelation(3)

  private val genWideKey: Gen[Key] =
    Gen.zip(Gen.oneOf("p1", "p2", "p3"), Gen.oneOf("a", "b", "c")).map(Key.apply.tupled)

  private def same(a: Relation[Key, Key], b: Relation[Key, Key], l: Key, r: Key): Prop =
    Prop(a.accepts(l, r) == b.accepts(l, r)) :| s"${a.render} vs ${b.render} at ($l, $r)"

  property("and is commutative at the interpreter") {
    forAll(genRel, genRel, genWideKey, genWideKey) { (a, b, l, r) =>
      same(a.and(b), b.and(a), l, r)
    }
  }

  property("and is associative at the interpreter") {
    forAll(genRel, genRel, genRel, genWideKey, genWideKey) { (a, b, c, l, r) =>
      same(a.and(b).and(c), a.and(b.and(c)), l, r)
    }
  }

  property("and is idempotent at the interpreter") {
    forAll(genRel, genWideKey, genWideKey) { (a, l, r) =>
      same(a.and(a), a, l, r)
    }
  }

  property("All is a two-sided identity for and, structurally and at the interpreter") {
    forAll(genRel, genWideKey, genWideKey) { (a, l, r) =>
      Prop(Relation.all[Key, Key].and(a) == a) :| "left identity" &&
      Prop(a.and(Relation.all[Key, Key]) == a) :| "right identity" &&
      same(Relation.And(Relation.all[Key, Key], a), a, l, r)
    }
  }

  property("SameOn and DifferentOn on the same projections are complementary") {
    forAll(Gen.oneOf(participant, image), genWideKey, genWideKey) { (p, l, r) =>
      Prop(Relation.sameOn(p).accepts(l, r) != Relation.differentOn(p).accepts(l, r)) :|
        s"${p.name} at ($l, $r)"
    }
  }

  // Symmetry is a property of the generated grammar (sameOn/differentOn with
  // the SAME projection on both sides), not of Relation[K, K] in general:
  // SameOn(participant, image) is constructible for a WithinUndirected design
  // and is asymmetric. Canonical storage relies on the caller supplying a
  // symmetric relation; nothing in the type enforces it.
  property("within-collection relations with identical projections are symmetric") {
    forAll(genRel, genWideKey, genWideKey) { (a, l, r) =>
      Prop(a.accepts(l, r) == a.accepts(r, l)) :| s"${a.render} at ($l, $r)"
    }
  }

  property("and is NOT commutative structurally: render and joinKeys preserve order") {
    forAll(genRel, genRel) { (a, b) =>
      Prop(a.render == b.render || Relation.And(a, b).render != Relation.And(b, a).render) :|
        "render" &&
        Prop(a.and(b).joinKeys == a.joinKeys ++ b.joinKeys) :| "joinKeys"
    }
  }

  /** The hash-join claim of PRD X-2: for a relation whose leaves are all
    * `SameOn`, `accepts` is exactly "every join key pair agrees".
    */
  private def leaves(r: Relation[Key, Key]): Vector[Relation[Key, Key]] = r match
    case Relation.And(a, b) => leaves(a) ++ leaves(b)
    case leaf               => Vector(leaf)

  private val byName: Map[String, Key => String] =
    Map("participant" -> (_.participant), "image" -> (_.image))

  property("joinKeys lists one entry per SameOn leaf and reproduces accepts for pure joins") {
    forAll(genRel, genWideKey, genWideKey) { (a, l, r) =>
      val sameLeaves = leaves(a).collect { case s: Relation.SameOn[?, ?, ?] => s }
      val pureJoin   = leaves(a).forall {
        case Relation.All()        => true
        case Relation.SameOn(_, _) => true
        case _                     => false
      }
      Prop(a.joinKeys.size == sameLeaves.size) :| "one join key per SameOn leaf" &&
      Prop(
        !pureJoin ||
          a.accepts(l, r) == a.joinKeys.forall { case (ln, rn) =>
            byName(ln)(l) == byName(rn)(r)
          }
      ) :| s"hash-join oracle for ${a.render} at ($l, $r)"
    }
  }

end RelationTruthTableSuite
