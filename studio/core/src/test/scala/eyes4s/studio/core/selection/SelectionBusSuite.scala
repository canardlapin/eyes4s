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

package eyes4s.studio.core.selection

import cats.effect.IO
import eyes4s.studio.core.backend.{
  DatasetRevision,
  PairDesign,
  Phase,
  Response,
  ResultAddress,
  RunId,
  TrialKey
}
import eyes4s.studio.core.document.{FigureId, PanelLetter, ReportingId, SourceRole}
import io.circe.syntax.*
import munit.CatsEffectSuite
import scala.concurrent.duration.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** Generators over a deliberately small ref space, so inputs collide. */
object SelectionGen:
  private def right[E, A](e: Either[E, A]): A =
    e.fold(m => throw new AssertionError(m), identity)

  val view: Gen[ViewId] =
    Gen.oneOf("explore.trial", "compare.ladder", "compare.table").map(v => right(ViewId.of(v)))

  val key: Gen[TrialKey] =
    for
      p <- Gen.oneOf("P11", "P17")
      t <- Gen.oneOf("ret_07", "enc_03")
    yield TrialKey(p, if t.startsWith("ret") then Phase.Retrieval else Phase.Encoding, t, 1)

  private val run   = Gen.oneOf(RunId(7), RunId(8))
  private val scale = Gen.oneOf(0, 2).map(i => right(ScaleIndex.of(i)))
  private val fix   = Gen.choose(1, 3).map(i => right(FixationIndex.of(i)))
  private val rec   = Gen.choose(7213, 7215).map(i => right(RecordNumber.of(i)))
  private val spec  = right(ReportingId.of("by-retrieval-response"))
  private val group = Gen.oneOf(Response.Remembered, Response.Forgotten)

  val observation: Gen[StudioRef] = Gen.oneOf(
    key.map(StudioRef.Trial(_)),
    Gen.zip(key, fix).map(StudioRef.Fixation(_, _)),
    Gen
      .zip(key, fix, rec)
      .map((k, i, r) => StudioRef.SourceRecord(k, Some(i), SourceRole.Fixations, r)),
    Gen
      .zip(run, scale, key, key)
      .map((r, s, f, x) => StudioRef.Pair(r, s, PairDesign.Matched, f, x))
  )

  val aggregate: Gen[StudioRef] = Gen.oneOf(
    Gen.zip(run, scale, key).map(StudioRef.QueryContrast(_, _, _)),
    Gen
      .zip(run, scale, Gen.option(group), Gen.oneOf("P11", "P17"))
      .map((r, s, g, p) => StudioRef.ParticipantSummary(r, spec, s, g, p)),
    Gen.zip(run, scale, group).map((r, s, g) => StudioRef.GroupCell(r, spec, s, g)),
    Gen
      .zip(Gen.choose(1, 2), Gen.oneOf("A", "B"))
      .map((f, l) => StudioRef.FigurePanel(right(FigureId.of(f)), right(PanelLetter.of(l)))),
    Gen
      .zip(Gen.choose(2, 3), Gen.oneOf(TallyRegion.values.toSeq))
      .map((d, r) => StudioRef.WindowTally(DatasetRevision(d), r)),
    Gen
      .zip(
        Gen.choose(2, 3),
        Gen.oneOf(
          InventoryKind.Inventory,
          InventoryKind.Admitted,
          InventoryKind.Quarantined,
          InventoryKind.Cause("quarantine.overlap"),
          InventoryKind.NoFixations,
          InventoryKind.Absent
        )
      )
      .map((d, k) => StudioRef.InventoryCount(DatasetRevision(d), k)),
    Gen
      .zip(
        Gen.choose(2, 3),
        Gen.oneOf(
          TrialGrouping.PhaseOf("P17", Phase.Retrieval),
          TrialGrouping.PhaseOf("P11", Phase.Encoding),
          TrialGrouping.MatchedOn("beach-042")
        )
      )
      .map((d, g) => StudioRef.TrialGroup(DatasetRevision(d), g))
  )

  val ref: Gen[StudioRef] =
    Gen.frequency(
      1 -> Gen.oneOf("P11", "P17").map(StudioRef.Participant(_)),
      4 -> observation,
      4 -> aggregate
    )

  val refs: Gen[Vector[StudioRef]] =
    Gen.choose(0, 4).flatMap(Gen.listOfN(_, ref)).map(_.toVector)

  val mode: Gen[SelectionMode] = Gen.oneOf(SelectionMode.values.toSeq)

  /** A state reached by a few accepted inputs. */
  val state: Gen[SelectionState] =
    Gen.listOf(Gen.zip(view, mode, refs)).map { steps =>
      steps.zipWithIndex.foldLeft(SelectionState.empty) { case (s, ((v, m, rs), i)) =>
        s.submit(SelectionInput(InputStamp(s.context, v, 1000L + i, InputCause.Pointer), m, rs))
          .getOrElse(s)
      }
    }

class SelectionBusSuite extends CatsEffectSuite with munit.ScalaCheckSuite:
  import SelectionGen.*

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(200)

  private def ok[E, A](e: Either[E, A]): A = e.fold(m => fail(s"unexpected: $m"), identity)

  /** The next input from `view`: a sequence above any it has used. */
  private def input(
      s: SelectionState,
      v: ViewId,
      mode: SelectionMode,
      refs: Vector[StudioRef]
  ) =
    val seq = s.delivered.get(v).fold(0L)(_ + 1)
    SelectionInput(InputStamp(s.context, v, seq, InputCause.Pointer), mode, refs)

  private def run(s: SelectionState, v: ViewId, mode: SelectionMode, refs: Vector[StudioRef]) =
    ok(s.submit(input(s, v, mode, refs)))

  // --- Laws -----------------------------------------------------------------

  property("Replace is idempotent") {
    forAll(state, view, view, refs) { (s, a, b, rs) =>
      val once  = run(s, a, SelectionMode.Replace, rs)
      val twice = run(once, b, SelectionMode.Replace, rs)
      assertEquals(once.selected, rs.distinct)
      assertEquals(twice.selected, once.selected)
      assertEquals(twice.revision, once.revision)
    }
  }

  property("Toggle twice is the identity on the selected set") {
    forAll(state, view, refs) { (s, v, rs) =>
      val back = run(run(s, v, SelectionMode.Toggle, rs), v, SelectionMode.Toggle, rs)
      assertEquals(back.selected.toSet, s.selected.toSet)
    }
  }

  property("Add, Subtract and Clear follow set semantics without duplicates") {
    forAll(state, view, refs) { (s, v, rs) =>
      val added = run(s, v, SelectionMode.Add, rs).selected
      assertEquals(added.toSet, s.selected.toSet ++ rs)
      assertEquals(added.distinct, added)
      assertEquals(run(s, v, SelectionMode.Subtract, rs).selected.toSet, s.selected.toSet -- rs)
      assertEquals(run(s, v, SelectionMode.Clear, rs).selected, Vector.empty)
    }
  }

  property("an input from another context is refused and changes nothing") {
    forAll(state, view, mode, refs, Gen.choose(1L, 5L)) { (s, v, m, rs, shift) =>
      val stale = input(s, v, m, rs)
      val moved =
        stale.copy(stamp = stale.stamp.copy(context = ContextRevision(s.context.value + shift)))
      assertEquals(
        s.submit(moved),
        Left(SelectionError.StaleContext(v, s.context, moved.stamp.context))
      )
      // After a rebase, inputs stamped with the old context are refused.
      val rebased = s.rebase(_ => true)
      assertEquals(
        rebased.submit(stale),
        Left(SelectionError.StaleContext(v, rebased.context, s.context))
      )
    }
  }

  property("a duplicate or older sequence from a view is refused") {
    forAll(state, view, mode, refs, mode, refs) { (s, v, m1, rs1, m2, rs2) =>
      val first = input(s, v, m1, rs1)
      val after = ok(s.submit(first))
      val seq   = first.stamp.sequence
      assertEquals(
        after.submit(first.copy(mode = m2, refs = rs2)),
        Left(SelectionError.Duplicate(v, seq))
      )
      if seq > 0 then
        val older = first.copy(stamp = first.stamp.copy(sequence = seq - 1))
        assertEquals(after.submit(older), Left(SelectionError.OutOfOrder(v, seq - 1, seq)))
    }
  }

  property("selecting an aggregate never selects its source observations") {
    forAll(
      state,
      view,
      aggregate,
      observation,
      Gen.oneOf(SelectionMode.Replace, SelectionMode.Add, SelectionMode.Toggle)
    ) { (s0, v, agg, obs, m) =>
      val s    = run(s0, v, SelectionMode.Clear, Vector.empty)
      val next = run(s, v, m, Vector(agg))
      assertEquals(next.selected, Vector(agg))
      assert(next.selected.forall(_.isAggregate))
      assert(!next.isSelected(obs))
      val relation = next.relation(obs)
      assert(relation != SelectionRelation.Selected, relation)
      if Lineage.structural.contains(agg, obs) then
        assertEquals(relation, SelectionRelation.WithinSelection)
    }
  }

  property("modes other than Replace and Clear only touch the refs they name") {
    forAll(
      state,
      view,
      Gen.oneOf(SelectionMode.Add, SelectionMode.Subtract, SelectionMode.Toggle),
      refs
    ) { (s, v, m, rs) =>
      val next      = run(s, v, m, rs)
      val untouched = (s.selected.toSet ++ next.selected.toSet) -- rs
      untouched.foreach(r => assertEquals(next.isSelected(r), s.isSelected(r)))
    }
  }

  // --- Refs, lineage and projection -------------------------------------------

  private val p17ret = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  private val p17enc = TrialKey("P17", Phase.Encoding, "enc_03", 1)
  private val v      = ok(ViewId.of("compare.table"))
  private val s2     = ok(ScaleIndex.of(2))
  private val f6     = ok(FixationIndex.of(6))
  private val r7214  = ok(RecordNumber.of(7214))

  test(
    "the trail chain: participant ⊃ trial ⊃ fixation ⊃ record; contrast ⊃ reduction ⊃ pair"
  ) {
    val record = StudioRef.SourceRecord(p17ret, Some(f6), SourceRole.Fixations, r7214)
    assertEquals(
      Lineage.structural.ancestors(record),
      Vector(
        StudioRef.Fixation(p17ret, f6),
        StudioRef.Trial(p17ret),
        StudioRef.Participant("P17")
      )
    )
    val pair = StudioRef.Pair(RunId(7), s2, PairDesign.Matched, p17ret, p17enc)
    assertEquals(
      Lineage.structural.ancestors(pair),
      Vector(
        ok(
          StudioRef
            .fromAddress(RunId(7), ResultAddress.Reduction(2, PairDesign.Matched, p17ret))
        ),
        StudioRef.QueryContrast(RunId(7), s2, p17ret)
      )
    )
    assertEquals(
      pair.resultAddress.map(_.render),
      Some("matched pair P17 · ret_07 × P17 · enc_03 at scale 2")
    )
    assertEquals(
      StudioRef.QueryContrast(RunId(7), s2, p17ret).resultAddress,
      Some(eyes4s.studio.core.backend.ResultAddress.ContrastRow(2, p17ret))
    )
  }

  test("projection: selected, contains the selection, within the selection, unrelated") {
    val fixation = StudioRef.Fixation(p17ret, f6)
    val s        = run(SelectionState.empty, v, SelectionMode.Replace, Vector(fixation))
    assertEquals(s.relation(fixation), SelectionRelation.Selected)
    assertEquals(s.relation(StudioRef.Trial(p17ret)), SelectionRelation.ContainsSelection)
    assertEquals(s.relation(StudioRef.Participant("P17")), SelectionRelation.ContainsSelection)
    assertEquals(
      s.relation(StudioRef.SourceRecord(p17ret, Some(f6), SourceRole.Fixations, r7214)),
      SelectionRelation.WithinSelection
    )
    assertEquals(s.relation(StudioRef.Trial(p17enc)), SelectionRelation.Unrelated)
  }

  test("a view's lineage links a query contrast to its participant summary") {
    val spec    = ok(ReportingId.of("by-retrieval-response"))
    val summary =
      StudioRef.ParticipantSummary(RunId(7), spec, s2, Some(Response.Remembered), "P17")
    val query   = StudioRef.QueryContrast(RunId(7), s2, p17ret)
    val lineage = Lineage.extended {
      case StudioRef.QueryContrast(RunId(7), `s2`, k) if k.participant == "P17" =>
        Vector(summary)
      case _ => Vector.empty
    }
    val s = run(SelectionState.empty, v, SelectionMode.Replace, Vector(summary))
    assertEquals(s.relation(query, lineage), SelectionRelation.WithinSelection)
    assertEquals(s.relation(query), SelectionRelation.Unrelated)
    assert(!s.isSelected(query))
    assertEquals(
      s.relation(StudioRef.GroupCell(RunId(7), spec, s2, Response.Remembered)),
      SelectionRelation.ContainsSelection
    )
  }

  test("rebase keeps only the refs that still mean something and restarts sequences") {
    val keep = StudioRef.Trial(p17ret)
    val drop = StudioRef.QueryContrast(RunId(7), s2, p17ret)
    val s    = run(SelectionState.empty, v, SelectionMode.Replace, Vector(keep, drop))
    val next = s.rebase {
      case StudioRef.QueryContrast(RunId(7), _, _) => false
      case _                                       => true
    }
    assertEquals(next.selected, Vector(keep))
    assertEquals(next.context, s.context.next)
    assertEquals(next.delivered, Map.empty)
  }

  private val address: Gen[ResultAddress] =
    for
      s   <- Gen.choose(0, 4)
      d   <- Gen.oneOf(PairDesign.Matched, PairDesign.Control)
      k   <- key
      ref <- key
      a   <- Gen.oneOf(
        ResultAddress.Estimation(s, k),
        ResultAddress.PairRow(s, d, k, ref),
        ResultAddress.Reduction(s, d, k),
        ResultAddress.ContrastRow(s, k)
      )
    yield a

  property("fromAddress round-trips every result address, through JSON too") {
    forAll(address, Gen.oneOf(RunId(7), RunId(8))) { (a, r) =>
      val ref = ok(StudioRef.fromAddress(r, a))
      assertEquals(ref.resultAddress, Some(a))
      assertEquals(ref.asJson.as[StudioRef], Right(ref))
    }
  }

  test("Pair and QueryContrast are views of Result") {
    val pair = StudioRef.Pair(RunId(7), s2, PairDesign.Control, p17ret, p17enc)
    assertEquals(
      pair,
      ok(
        StudioRef.fromAddress(
          RunId(7),
          ResultAddress.PairRow(2, PairDesign.Control, p17ret, p17enc)
        )
      )
    )
    pair match
      case StudioRef.Pair(run, scale, design, focal, reference) =>
        assertEquals(
          (run, scale, design, focal, reference),
          (RunId(7), s2, PairDesign.Control, p17ret, p17enc)
        )
      case other => fail(s"not a pair: $other")
    assertEquals(StudioRef.QueryContrast.unapply(pair), None)
    assertEquals(pair.kind, RefKind.Observation)
    assertEquals(StudioRef.QueryContrast(RunId(7), s2, p17ret).kind, RefKind.Aggregate)
  }

  test("negative scales, zero fixation indices and zero records are refused") {
    assertEquals(
      StudioRef.fromAddress(RunId(7), ResultAddress.ContrastRow(-1, p17ret)),
      Left(RefError.NegativeScale(-1))
    )
    assertEquals(ScaleIndex.of(-2), Left(RefError.NegativeScale(-2)))
    assertEquals(FixationIndex.of(0), Left(RefError.FixationIndexNotPositive(0)))
    assertEquals(RecordNumber.of(0), Left(RefError.RecordNotPositive(0)))
    val bad = io.circe.parser.parse(
      """{"Fixation":{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"index":0}}"""
    )
    assert(bad.flatMap(_.as[StudioRef]).isLeft)
  }

  test("refs have a stable JSON form") {
    val ref = StudioRef.Fixation(p17ret, f6)
    assertEquals(ref.asJson.as[StudioRef], Right(ref))
  }

  test("blank view ids are refused") {
    assertEquals(ViewId.of(" "), Left(SelectionError.BlankView))
  }

  test("hover is local: a view's hover reports whether it changed") {
    val trial         = StudioRef.Trial(p17ret)
    val (h1, changed) = Hover.none.set(Some(trial))
    assert(changed)
    assertEquals(h1.set(Some(trial))._2, false)
  }

  // --- The effectful bus ------------------------------------------------------

  test("the bus: every subscriber sees a selection made in any view") {
    val explore = ok(ViewId.of("explore.trial"))
    val compare = ok(ViewId.of("compare.table"))
    val trial   = StudioRef.Trial(p17ret)
    for
      bus <- SelectionBus[IO]()
      seen = bus.changes
        .dropWhile(_.selected.isEmpty)
        .head
        .compile
        .lastOrError
        .timeout(5.seconds)
      first <- seen.start
      other <- seen.start
      _     <- IO.cede
      _     <- bus.submit(
        SelectionInput(
          InputStamp(ContextRevision(0), explore, 0, InputCause.Pointer),
          SelectionMode.Replace,
          Vector(trial)
        )
      )
      a   <- first.joinWithNever
      b   <- other.joinWithNever
      dup <- bus.submit(
        SelectionInput(
          InputStamp(ContextRevision(0), explore, 0, InputCause.Pointer),
          SelectionMode.Clear,
          Vector.empty
        )
      )
      bad <- bus.submit(
        SelectionInput(
          InputStamp(ContextRevision(3), compare, 0, InputCause.Keyboard),
          SelectionMode.Clear,
          Vector.empty
        )
      )
      now <- bus.current
    yield
      assertEquals(a.selected, Vector(trial))
      assertEquals(b.selected, Vector(trial))
      assertEquals(dup, Left(SelectionError.Duplicate(explore, 0)))
      assertEquals(
        bad,
        Left(SelectionError.StaleContext(compare, ContextRevision(0), ContextRevision(3)))
      )
      assertEquals(now.selected, Vector(trial))
  }
