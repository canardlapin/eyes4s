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

package eyes4s.laws

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Norm, Px}

import org.scalacheck.Test

import scala.annotation.unused

/** The shipped AOI accounting against [[AoiLaws]] on three frames, and a
  * mutation suite for the same laws.
  *
  * Every mutant below is a complete alternative implementation of
  * [[AoiAccounting]]: [[Reference]] re-derives assignment, report and metrics
  * from the published semantics, and each mutant overrides exactly one of its
  * decisions. The shipped implementation and the unmodified reference must
  * pass every law; each mutant must be observed to fail the laws that name
  * it. A law no mutant fails would be decoration, and a mutant no law kills
  * an unguarded regression.
  *
  * ==Mutation execution receipts==
  *
  * Observed on both JVM and Scala.js (`lawsJVM/test`, `lawsJS/test`), over
  * `Generators.genAoiScene[Px]` with the seed fixed below.
  *
  * {{{
  * | mutant              | changed expression                                     | killed by                                   |
  * |---------------------|--------------------------------------------------------|---------------------------------------------|
  * | multiple-reversed   | Multiple lists the containing areas in reverse order   | order                                       |
  * | multiple-first-only | Multiple keeps only the first containing area          | order, dwell, entry, bounded, blocks        |
  * | exclusive-multi     | ExclusiveByPriority assigns every containing area      | single, first, partition, dwell, entry,     |
  * |                     |                                                        | bounded, blocks                             |
  * | exclusive-last      | ExclusiveByPriority assigns the last containing area   | first, dwell, entry, bounded, blocks        |
  * | smallest-largest    | SmallestContaining picks the largest area              | least, dwell, entry, bounded, blocks        |
  * | lower-edge-open     | a box excludes points on its lower (closed) edges      | order, first, least, reject, dwell, entry,  |
  * |                     |                                                        | bounded, blocks                             |
  * | upper-edge-closed   | a box includes points on its upper (open) edges        | order, first, least, reject, dwell, entry,  |
  * |                     |                                                        | bounded, blocks                             |
  * | reject-lower-edge   | RejectOverlap ignores a box the point touches on a     | reject                                      |
  * |                     | closed (lower) edge                                    |                                             |
  * | reject-first-only   | the overlap error names only the first containing area | reject                                      |
  * | lost-as-background  | Lost samples are Background                            | analysable, excluded, partition, multiple,  |
  * |                     |                                                        | dwell                                       |
  * | entry-at-offset     | first entry measured at the sample's end, t + duration | entry                                       |
  * | entry-zero-absent   | a zero latency is reported as no entry                 | defined, entry                              |
  * | runs-per-sample     | the run counter advances on every contained sample     | blocks                                      |
  * | runs-edges          | the run counter advances on entry and on exit          | bounded, blocks                             |
  * | background-dropped  | background support is not accumulated                  | accounting, analysable, partition, multiple |
  * | union-duplicated    | union time counted once per membership, not per sample | accounting, analysable, invariant, multiple |
  * | duplicated-by-count | duplicated time is duration * k rather than * (k - 1)  | partition, multiple                         |
  * }}}
  *
  * The abbreviations are the `private val`s below, each bound to one law name
  * of `AoiLaws.accounting`; the receipt test asserts that the observed set of
  * failing laws is exactly the set listed, so the table cannot drift from the
  * run.
  *
  * Two laws compare the implementation with itself: "union and background
  * time do not depend on the policy" runs the same implementation under
  * three policies, and "first entry is defined exactly when dwell is
  * positive" relates two fields of one metric. Each is anchored by the
  * independent laws beside it -- the dwell, partition and multiple-sum laws
  * fix what union, background and dwell must be, and the first-entry law
  * fixes the latency itself -- so a mutant cannot satisfy them by being
  * consistently wrong.
  *
  * The translation law is metamorphic and is the one law no mutant here
  * fails: every mutant is coordinate-free, so it is as invariant under a
  * translation as the shipped code. It guards a different regression -- a
  * region or point that is not moved with its frame.
  */
class AoiLawsSuite extends munit.DisciplineSuite:

  // ---------------------------------------------------------------------------
  // The shipped implementation against the laws
  // ---------------------------------------------------------------------------

  checkAll(
    "Aoi.accounting[Norm]",
    AoiLaws.accounting(AoiAccounting.shipped, Generators.genAoiScene[Norm])
  )
  checkAll(
    "Aoi.accounting[Px]",
    AoiLaws.accounting(AoiAccounting.shipped, Generators.genAoiScene[Px])
  )

  private val screen = Frame.screen("aoi-screen", 1280, 1024).toOption.get

  checkAll(
    "Aoi.accounting[fixed frame]",
    AoiLaws.accounting(AoiAccounting.shipped, Generators.genAoiSceneIn(screen))
  )

  test("the AOI generator reaches overlap, nesting, edge contact and abutting contained runs") {
    val scenes = LazyList
      .continually(Generators.genAoiScene[Px].sample)
      .flatten
      .take(300)
      .toVector
    def overlapAt(scene: AoiScene[Px]) =
      (0 until scene.n).exists(i => scene.containing(i).length >= 2)
    def nested(scene: AoiScene[Px]) =
      scene.specs.exists { outer =>
        scene.specs.exists { inner =>
          (outer.shape, inner.shape) match
            case (AoiShape.Box(lo, hi), AoiShape.Box(l, h)) =>
              lo.x < l.x && lo.y < l.y && h.x < hi.x && h.y < hi.y
            case (AoiShape.Oval(c, rx, ry), AoiShape.Oval(d, sx, sy)) =>
              c == d && sx < rx && sy < ry
            case _ => false
        }
      }
    def onEdge(scene: AoiScene[Px]) =
      (0 until scene.n).exists { i =>
        val c = scene.scanpath.fixations(i).centre
        scene.containing(i).length >= 2 && scene
          .containing(i)
          .exists(_.region match
            case Region.Rect(lo, _) => lo.x == c.x || lo.y == c.y
            case _                  => false)
      }
    def onUpperEdge(scene: AoiScene[Px]) =
      (0 until scene.n).exists { i =>
        val c = scene.scanpath.fixations(i).centre
        scene.specs.exists(_.shape match
          case AoiShape.Box(lo, hi) =>
            (hi.x == c.x || hi.y == c.y) && lo.x <= c.x && lo.y <= c.y &&
            c.x <= hi.x && c.y <= hi.y
          case _ => false)
      }
    def abutting(scene: AoiScene[Px]) =
      scene.areas.areas.exists { a =>
        val p = scene.contained(a)
        (1 until scene.n).exists(i => p(i) && p(i - 1) && scene.gapBefore(i).isZero)
      }
    def everywhere(scene: AoiScene[Px]) =
      scene.areas.areas.exists(a => scene.contained(a).forall(identity))
    def nowhere(scene: AoiScene[Px]) =
      scene.areas.areas.exists(a => !scene.contained(a).exists(identity))
    assert(scenes.exists(overlapAt), "no observed overlap")
    assert(scenes.exists(nested), "no nested area")
    assert(scenes.exists(onEdge), "no fixation on a closed edge inside two areas")
    assert(scenes.exists(onUpperEdge), "no fixation on an open upper edge")
    assert(scenes.exists(abutting), "no abutting contained fixations")
    assert(scenes.exists(everywhere), "no area containing every fixation")
    assert(scenes.exists(nowhere), "no area containing no fixation")
  }

  test("the published AOI-set generator binds non-degenerate, overlapping sets to the frame") {
    val sets = LazyList
      .continually(Generators.genAoiSetIn(screen).sample)
      .flatten
      .take(200)
      .toVector
    val grid    = Grid.over(screen, 32, 32).toOption.get
    val centres = (0 until grid.size).map(grid.unsafeCellCentre)
    assert(sets.forall(s => s.frame == screen && s.size >= 1 && s.ids.distinct == s.ids))
    // An intersection of two disjoint leaves is empty by construction, and a
    // one-area set may consist of exactly that, so the claim is a proportion.
    val populated = sets.count(_.areas.exists(a => centres.exists(a.region.contains)))
    assert(
      populated * 10 >= sets.length * 9,
      s"only $populated of ${sets.length} sets have an area containing a raster cell centre"
    )
    assert(
      sets.exists(s => centres.exists(c => s.areas.count(_.region.contains(c)) >= 2)),
      "no set with overlapping areas"
    )
  }

  // ---------------------------------------------------------------------------
  // The reference implementation and its mutants
  // ---------------------------------------------------------------------------

  /** A complete alternative to `AoiSet.assign` and `AoiAssignment.measure`,
    * written from the published semantics. Every decision a mutant changes is
    * a method, so that the changed expression is the whole of the mutant.
    */
  class Reference(val name: String) extends AoiAccounting:

    /** Point membership, the one place the edge convention enters. */
    protected def contains[U <: Unit2D](area: Aoi[U], p: Pt[U]): Boolean =
      area.region.contains(p)

    protected def multiple[U <: Unit2D](containing: Vector[Aoi[U]]): Vector[AoiId] =
      containing.map(_.id)

    protected def exclusive[U <: Unit2D](containing: Vector[Aoi[U]]): AoiId =
      containing.head.id

    protected def smallest[U <: Unit2D](containing: Vector[Aoi[U]], grid: Grid[U]): AoiId =
      containing.tail
        .foldLeft(containing.head) { (best, c) =>
          if c.region.area(grid) < best.region.area(grid) then c else best
        }
        .id

    /** Areas that do not count towards an observed overlap. */
    protected def overlapIgnored[U <: Unit2D](area: Aoi[U], p: Pt[U]): Boolean = false

    protected def overlapNames[U <: Unit2D](containing: Vector[Aoi[U]]): Vector[AoiId] =
      containing.map(_.id)

    protected def lost: AoiMembership = AoiMembership.Excluded(ExclusionReason.SignalLoss)

    protected def entryAt(t: Instant, @unused duration: Span): Instant = t

    protected def entryOf(latency: Span): Option[Span] = Some(latency)

    protected def runStarts(wasInside: Boolean, present: Boolean): Boolean =
      present && !wasInside

    protected def unionOf(duration: Long, @unused k: Int): Long = duration
    protected def duplicatedOf(duration: Long, k: Int): Long    = duration * (k - 1L)
    protected def backgroundOf(duration: Long): Long            = duration

    def run[U <: Unit2D](
        areas: AoiSet[U],
        recording: Recording[U],
        policy: MembershipPolicy,
        support: TemporalSupport
    ): Either[AoiError, AoiOutcome] =
      for
        _ <- Agreement
          .frames(areas.frame, recording.frame)
          .left
          .map(AoiError.FrameConflict.apply)
        grid <- policy match
          case MembershipPolicy.SmallestContaining(r) =>
            Grid
              .over(areas.frame, r.nx, r.ny)
              .left
              .map(AoiError.ResolutionGridFailure(areas.frame.id, r.nx, r.ny, _))
              .map(Some(_))
          case _ => Right(None)
        memberships <- assign(areas, recording, policy, grid)
      yield
        val ledger    = recording.representedSupport(support)
        val durations = ledger.toVector.map(_.toMicros)
        AoiOutcome(
          memberships,
          report(memberships, durations, ledger.censoredTime),
          metrics(areas, recording, memberships, durations)
        )

    private def assign[U <: Unit2D](
        areas: AoiSet[U],
        recording: Recording[U],
        policy: MembershipPolicy,
        grid: Option[Grid[U]]
    ): Either[AoiError, Vector[AoiMembership]] =
      val out = Vector.newBuilder[AoiMembership]
      var k   = 0
      var bad = Option.empty[AoiError]
      while k < recording.size && bad.isEmpty do
        recording.samples(k).gaze match
          case Gaze.Blink()      => out += AoiMembership.Excluded(ExclusionReason.Blink)
          case Gaze.Lost()       => out += lost
          case Gaze.OffScreen(_) => out += AoiMembership.Excluded(ExclusionReason.OffSurface)
          case Gaze.Tracked(p, _) if !areas.frame.contains(p) =>
            out += AoiMembership.Excluded(ExclusionReason.OffSurface)
          case Gaze.Tracked(p, _) =>
            val containing = areas.areas.filter(contains(_, p))
            if containing.isEmpty then out += AoiMembership.Background
            else if containing.length == 1 then
              out += AoiMembership.Areas(Vector(containing.head.id))
            else
              policy match
                case MembershipPolicy.Multiple =>
                  out += AoiMembership.Areas(multiple(containing))
                case MembershipPolicy.ExclusiveByPriority =>
                  out += AoiMembership.Areas(Vector(exclusive(containing)))
                case MembershipPolicy.SmallestContaining(_) =>
                  out += AoiMembership.Areas(Vector(smallest(containing, grid.get)))
                case MembershipPolicy.RejectOverlap =>
                  val counted = containing.filterNot(overlapIgnored(_, p))
                  if counted.length >= 2 then
                    bad = Some(
                      AoiError.ObservedOverlap(
                        areas.frame.id,
                        k,
                        p.x,
                        p.y,
                        overlapNames(containing)
                      )
                    )
                  else out += AoiMembership.Areas(Vector(containing.head.id))
        k += 1
      bad.toLeft(out.result())

    private def report(
        memberships: Vector[AoiMembership],
        durations: Vector[Long],
        censored: Span
    ): AoiAssignmentReport =
      var union      = 0L
      var background = 0L
      var excluded   = 0L
      var duplicated = 0L
      memberships.zip(durations).foreach {
        case (AoiMembership.Areas(ids), d) =>
          union += unionOf(d, ids.length)
          duplicated += duplicatedOf(d, ids.length)
        case (AoiMembership.Background, d)  => background += backgroundOf(d)
        case (AoiMembership.Excluded(_), d) => excluded += d
      }
      AoiAssignmentReport(
        Span.micros(union),
        Span.micros(background),
        Span.micros(excluded),
        censored,
        Span.micros(duplicated)
      )

    private def metrics[U <: Unit2D](
        areas: AoiSet[U],
        recording: Recording[U],
        memberships: Vector[AoiMembership],
        durations: Vector[Long]
    ): Vector[AoiMetric] =
      val analysable = memberships
        .zip(durations)
        .collect {
          case (AoiMembership.Areas(_), d)   => d
          case (AoiMembership.Background, d) => d
        }
        .sum
      areas.areas.map { area =>
        var dwell  = 0L
        var runs   = 0
        var inside = false
        var entry  = Option.empty[Span]
        memberships.indices.foreach { k =>
          val present = memberships(k) match
            case AoiMembership.Areas(ids) => ids.contains(area.id)
            case _                        => false
          if present then
            dwell += durations(k)
            if runStarts(inside, present) then runs += 1
            if entry.isEmpty then
              entry = entryOf(
                recording.first.t
                  .until(entryAt(recording.samples(k).t, Span.micros(durations(k))))
              )
          else if runStarts(inside, present) then runs += 1
          inside = present
        }
        AoiMetric(
          area.id,
          area.label,
          Span.micros(dwell),
          Option.when(analysable > 0L)(dwell.toDouble / analysable),
          entry,
          runs
        )
      }

  end Reference

  object MultipleReversed extends Reference("multiple-reversed"):
    override protected def multiple[U <: Unit2D](containing: Vector[Aoi[U]]): Vector[AoiId] =
      containing.reverse.map(_.id)

  object MultipleFirstOnly extends Reference("multiple-first-only"):
    override protected def multiple[U <: Unit2D](containing: Vector[Aoi[U]]): Vector[AoiId] =
      containing.take(1).map(_.id)

  object ExclusiveMulti extends Reference("exclusive-multi"):
    // `exclusive` returns one id, so the whole branch is the mutant.
    override def run[U <: Unit2D](
        areas: AoiSet[U],
        recording: Recording[U],
        policy: MembershipPolicy,
        support: TemporalSupport
    ): Either[AoiError, AoiOutcome] =
      policy match
        case MembershipPolicy.ExclusiveByPriority =>
          super.run(areas, recording, MembershipPolicy.Multiple, support)
        case other => super.run(areas, recording, other, support)

  object ExclusiveLast extends Reference("exclusive-last"):
    override protected def exclusive[U <: Unit2D](containing: Vector[Aoi[U]]): AoiId =
      containing.last.id

  object SmallestLargest extends Reference("smallest-largest"):
    override protected def smallest[U <: Unit2D](
        containing: Vector[Aoi[U]],
        grid: Grid[U]
    ): AoiId =
      containing.tail
        .foldLeft(containing.head) { (best, c) =>
          if c.region.area(grid) > best.region.area(grid) then c else best
        }
        .id

  object LowerEdgeOpen extends Reference("lower-edge-open"):
    override protected def contains[U <: Unit2D](area: Aoi[U], p: Pt[U]): Boolean =
      area.region match
        case Region.Rect(lo, hi) => p.x > lo.x && p.x < hi.x && p.y > lo.y && p.y < hi.y
        case other               => other.contains(p)

  object UpperEdgeClosed extends Reference("upper-edge-closed"):
    override protected def contains[U <: Unit2D](area: Aoi[U], p: Pt[U]): Boolean =
      area.region match
        case Region.Rect(lo, hi) => p.x >= lo.x && p.x <= hi.x && p.y >= lo.y && p.y <= hi.y
        case other               => other.contains(p)

  object RejectLowerEdge extends Reference("reject-lower-edge"):
    override protected def overlapIgnored[U <: Unit2D](area: Aoi[U], p: Pt[U]): Boolean =
      area.region match
        case Region.Rect(lo, _) => p.x == lo.x || p.y == lo.y
        case _                  => false

  object RejectFirstOnly extends Reference("reject-first-only"):
    override protected def overlapNames[U <: Unit2D](
        containing: Vector[Aoi[U]]
    ): Vector[AoiId] =
      containing.take(1).map(_.id)

  object LostAsBackground extends Reference("lost-as-background"):
    override protected def lost: AoiMembership = AoiMembership.Background

  object EntryAtOffset extends Reference("entry-at-offset"):
    override protected def entryAt(t: Instant, duration: Span): Instant = t + duration

  object EntryZeroAbsent extends Reference("entry-zero-absent"):
    override protected def entryOf(latency: Span): Option[Span] =
      Option.unless(latency.isZero)(latency)

  object RunsPerSample extends Reference("runs-per-sample"):
    override protected def runStarts(wasInside: Boolean, present: Boolean): Boolean = present

  object RunsEdges extends Reference("runs-edges"):
    override protected def runStarts(wasInside: Boolean, present: Boolean): Boolean =
      wasInside != present

  object BackgroundDropped extends Reference("background-dropped"):
    override protected def backgroundOf(duration: Long): Long = 0L

  object UnionDuplicated extends Reference("union-duplicated"):
    override protected def unionOf(duration: Long, k: Int): Long = duration * k

  object DuplicatedByCount extends Reference("duplicated-by-count"):
    override protected def duplicatedOf(duration: Long, k: Int): Long = duration * k

  private val accounting = "accounting holds under every policy"
  private val analysable = "analysable time is the fixation dwell and excluded time the rest"
  private val invariant  = "union and background time do not depend on the policy"
  private val partition  = "exclusive dwell partitions the fixation time with the background"
  private val multiple   = "multiple dwell sums to the union plus the duplicated time"
  private val single     = "single-membership policies assign at most one area"
  private val order      = "multiple assigns every containing area in set order"
  private val first      = "exclusive assigns the first containing area"
  private val least      = "smallest assigns the least-area containing area, first on ties"
  private val excluded   = "transitions are excluded support and nothing else is"
  private val reject     = "reject-overlap refuses exactly when a fixation lies in two areas"
  private val dwell      = "dwell is the contained fixation time under each policy"
  private val defined    = "first entry is defined exactly when dwell is positive"
  private val entry      = "first entry latency is the onset of the first contained fixation"
  private val bounded    = "run count is bounded by the contained fixations"
  private val blocks     =
    "run count is the number of maximal abutting blocks of contained fixations"
  private val translated =
    "the accounting is invariant under a translation of scanpath and areas"

  /** Each mutant with the laws the receipt table claims kill it. */
  private val mutants: Vector[(Reference, Set[String])] = Vector(
    MultipleReversed  -> Set(order),
    MultipleFirstOnly -> Set(order, dwell, entry, bounded, blocks),
    ExclusiveMulti    -> Set(single, first, partition, dwell, entry, bounded, blocks),
    ExclusiveLast     -> Set(first, dwell, entry, bounded, blocks),
    SmallestLargest   -> Set(least, dwell, entry, bounded, blocks),
    LowerEdgeOpen     -> Set(order, first, least, reject, dwell, entry, bounded, blocks),
    UpperEdgeClosed   -> Set(order, first, least, reject, dwell, entry, bounded, blocks),
    RejectLowerEdge   -> Set(reject),
    RejectFirstOnly   -> Set(reject),
    LostAsBackground  -> Set(analysable, excluded, partition, multiple, dwell),
    EntryAtOffset     -> Set(entry),
    EntryZeroAbsent   -> Set(defined, entry),
    RunsPerSample     -> Set(blocks),
    RunsEdges         -> Set(bounded, blocks),
    BackgroundDropped -> Set(accounting, analysable, partition, multiple),
    UnionDuplicated   -> Set(accounting, analysable, invariant, multiple),
    DuplicatedByCount -> Set(partition, multiple)
  )

  // ---------------------------------------------------------------------------
  // Receipts
  // ---------------------------------------------------------------------------

  private val killParameters =
    Test.Parameters.default.withMinSuccessfulTests(80).withInitialSeed(0x414f494cL)

  private val ruleSetPrefix = "aoi.accounting."

  private def laws(impl: AoiAccounting): Seq[(String, org.scalacheck.Prop)] =
    AoiLaws
      .accounting(impl, Generators.genAoiScene[Px])
      .all
      .properties
      .map((name, prop) => name.stripPrefix(ruleSetPrefix) -> prop)
      .toVector

  /** The laws of [[AoiLaws.accounting]] an implementation fails, by name. */
  private def failing(impl: AoiAccounting): Set[String] =
    laws(impl).collect {
      case (name, prop) if !Test.check(killParameters, prop).passed => name
    }.toSet

  test("the law names in the receipt table are the published ones") {
    val published = laws(AoiAccounting.shipped).map(_._1).toSet
    val cited     = mutants.flatMap(_._2).toSet + translated
    assertEquals(cited -- published, Set.empty[String], s"published: $published")
    assertEquals(published -- cited, Set.empty[String], s"cited: $cited")
  }

  test("the reference implementation passes every law, so the mutants differ only as stated") {
    assertEquals(failing(new Reference("reference")), Set.empty[String])
  }

  test("every mutant is killed by exactly the laws that name it") {
    val observed = mutants.map((mutant, expected) => (mutant.name, expected, failing(mutant)))
    val wrong    = observed.collect {
      case (name, expected, killed) if killed != expected =>
        s"$name: expected [${expected.mkString("; ")}] observed [${killed.mkString("; ")}]"
    }
    assert(wrong.isEmpty, wrong.mkString("\n"))
  }

end AoiLawsSuite
