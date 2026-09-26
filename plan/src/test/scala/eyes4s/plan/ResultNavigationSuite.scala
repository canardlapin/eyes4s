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

package eyes4s.plan

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** The chain below a report on a small study: a query contrast's pairs by
  * offset, a pair's maps, a map's fixations and their records, and back up;
  * the used-by index of a reference trial; and every refusal naming its
  * operands.
  */
class ResultNavigationSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private val frame = get(Frame.screen("navigation", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))

  private def trial(key: StudyKey, points: (Double, Double)*) =
    val clock = ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 500L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))

  private def k(p: String, i: String, phase: String) = StudyKey(p, i, phase)
  private val items                                  = Vector("a", "b", "c")
  private val input                                  = StudyInput(
    Trials(
      for
        p     <- Vector("P17", "P18")
        i     <- items
        phase <- Vector("recall", "encode")
      yield trial(k(p, i, phase), (0.5, 0.5), (1.5, if i == "a" then 0.5 else 1.5))
    )
  )
  private val plan = get(
    StudyPlan.cosine(
      input.reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned()),
      FailurePolicy.RequireAll
    )
  )
  private val result  = get(plan.run(input))
  private val records = input.trials.rows
    .flatMap(t => (0 until t.value.n).map(o => t.key -> o))
    .zipWithIndex
    .map { case ((key, o), i) => SourceRecord[StudyKey](i + 2, Disposition.Admitted(key, o)) }
  private val header = Vector("participant", "image", "phase", "fixation")
  private val ledger = get(
    AdmissionLedger.decide(
      SourceRef.of("fixations.csv", header, Vector.empty),
      header,
      records,
      AdmissionDecision.RequireComplete,
      AdmissionPolicy.default[StudyKey],
      Vector.empty
    )
  )
  private val inspection   = get(ResultInspection.study(plan, result, input, Some(ledger)))
  private val provenance   = get(CoordinateProvenance.of(plan, input, Some(ledger)))
  private val scale        = get(inspection.scale(0))
  private val query        = ResultRef.ContrastRow(0, k("P17", "a", "recall"))
  private def size(n: Int) = get(PageSize.of(n))

  test("a query contrast's control pairs by offset: the total first, then each page") {
    val first = get(
      ResultNavigation.pairs(
        inspection,
        query,
        StudyDesign.Control,
        ListingOffset.start,
        size(1)
      )
    )
    assertEquals(first.total, 2)
    assertEquals(first.offset, ListingOffset.start)
    assertEquals(first.next, Some(get(ListingOffset.of(1))))
    val second = get(
      ResultNavigation.pairs(
        inspection,
        query,
        StudyDesign.Control,
        get(first.next.toRight("next")),
        size(1)
      )
    )
    assertEquals(second.next, None)
    assertEquals(
      first.entries ++ second.entries,
      Vector("b", "c").map(i =>
        ResultRef.PairRow(
          0,
          StudyDesign.Control,
          k("P17", "a", "recall"),
          k("P17", i, "encode")
        )
      )
    )
    val matched = get(
      ResultNavigation.pairs(
        inspection,
        query,
        StudyDesign.Matched,
        ListingOffset.start,
        size(10)
      )
    )
    assertEquals(
      matched.entries,
      Vector(
        ResultRef.PairRow(
          0,
          StudyDesign.Matched,
          k("P17", "a", "recall"),
          k("P17", "a", "encode")
        )
      )
    )
    val beyond = get(
      ResultNavigation.pairs(
        inspection,
        query,
        StudyDesign.Control,
        get(ListingOffset.of(7)),
        size(3)
      )
    )
    assertEquals((beyond.entries, beyond.total, beyond.next), (Vector.empty, 2, None))
  }

  test("a pair's maps, a map's fixations and their records, and each step back up") {
    val pair = ResultRef.PairRow(
      0,
      StudyDesign.Matched,
      k("P17", "a", "recall"),
      k("P17", "a", "encode")
    )
    assertEquals(ResultNavigation.queryOf(pair), Right(query))
    val (focal, reference) = get(ResultNavigation.maps(inspection, pair))
    assertEquals(focal, ResultRef.Estimation(0, k("P17", "a", "recall")))
    assertEquals(reference, ResultRef.Estimation(0, k("P17", "a", "encode")))
    val fixations = get(ResultNavigation.fixations(inspection, provenance, reference))
    assertEquals(fixations.map(_.number.value), Vector(1, 2))
    val second = fixations(1)
    assertEquals(second, FixationRef(k("P17", "a", "encode"), get(ScanpathPosition.of(1))))
    val record = get(ResultNavigation.record(provenance, second))
    assertEquals(record, get(DataRecord.of(4)))
    assertEquals(ResultNavigation.fixationOf(provenance, record), Right(second))
  }

  test("the used-by index of a reference trial, and counts that build no pair") {
    val encoding = k("P17", "a", "encode")
    val used     = scale.usedBy(encoding)
    assertEquals(
      used.asMatched,
      Vector(ResultRef.PairRow(0, StudyDesign.Matched, k("P17", "a", "recall"), encoding))
    )
    assertEquals(
      used.asControl,
      Vector("b", "c").map(i =>
        ResultRef.PairRow(0, StudyDesign.Control, k("P17", i, "recall"), encoding)
      )
    )
    assertEquals(used.asQuery, Vector.empty)
    assertEquals(scale.usedByCounts(encoding), UsedByCounts(0, 1, 2))
    assertEquals(used.counts, scale.usedByCounts(encoding))
    assertEquals(scale.usedByCounts(k("P17", "a", "recall")), UsedByCounts(3, 0, 0))
    assertEquals(scale.usedByCounts(k("P99", "a", "recall")), UsedByCounts(0, 0, 0))
  }

  test("listings page by offset and by reference over the same entries") {
    val listing = scale.pairs(StudyDesign.Control)
    assertEquals(listing.total, 12)
    val byOffset = listing.page(get(ListingOffset.of(5)), size(4))
    val byRef    = get(listing.page(listing.references(5), size(4)))
    assertEquals(byOffset.entries, byRef.entries)
    assertEquals(byOffset.next.map(_.value), Some(9))
    assertEquals(byRef.next, Some(listing.references(9)))
  }

  test("refusals name their operands") {
    val map    = ResultRef.Estimation(0, k("P17", "a", "recall"))
    val absent = ResultRef.ContrastRow(0, k("P99", "a", "recall"))
    assertEquals(
      ResultNavigation
        .pairs(inspection, absent, StudyDesign.Matched, ListingOffset.start, size(1)),
      Left(NavigationError.Inspection(InspectionError.UnknownReference(absent)))
    )
    assertEquals(
      ResultNavigation.maps(inspection, map),
      Left(NavigationError.WrongLevel(map, NavigationLevel.Pair))
    )
    assertEquals(
      ResultNavigation.fixations(inspection, provenance, query),
      Left(NavigationError.WrongLevel(query, NavigationLevel.Map))
    )
    val other = get(CoordinateProvenance.of(plan, input, None))
    assertEquals(
      ResultNavigation
        .record(other, FixationRef(k("P17", "a", "recall"), get(ScanpathPosition.of(0)))),
      Left(NavigationError.NoRecord(k("P17", "a", "recall"), get(ScanpathPosition.of(0))))
    )
    assertEquals(
      ResultNavigation.fixationOf(provenance, get(DataRecord.of(99))),
      Left(NavigationError.UnknownRecord(get(DataRecord.of(99))))
    )
    assertEquals(ListingOffset.of(-1), Left(NavigationError.NegativeOffset(-1)))
    val position = get(ScanpathPosition.of(2))
    val messages = Vector(
      NavigationError.NegativeOffset(-1)                        -> "-1 is negative",
      NavigationError.WrongLevel(map, NavigationLevel.Pair)     -> "is not a Pair reference",
      NavigationError.Inspection(InspectionError.NoContrast(0)) -> "no contrast rows",
      NavigationError.NoReduction(query, StudyDesign.Control) -> "no stored control reduction",
      NavigationError.InputMismatch("aa", "bb") -> "input aa; the provenance describes bb",
      NavigationError.Provenance(ProvenanceError.UnknownTrial(k("P9", "a", "recall"))) ->
        "no trial",
      NavigationError.NoRecord(k("P17", "a", "recall"), position) -> "fixation 3 of",
      NavigationError.UnknownRecord(get(DataRecord.of(99)))       -> "no data record 99",
      NavigationError.NotAdmitted(get(DataRecord.of(5))) -> "Data record 5 was rejected"
    )
    messages.foreach((error, text) => assert(error.message.contains(text), error.message))
    assertEquals(NavigationLevel.values.length, 7)
  }
