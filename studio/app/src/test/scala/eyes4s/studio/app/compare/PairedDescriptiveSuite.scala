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

package eyes4s.studio.app.compare

import eyes4s.studio.app.plot.PlotReports
import eyes4s.studio.app.text.{Format, PairedText, PairedTextId}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ReportingContrast, ReportingSpec}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ReportGroup, ScaleIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** The Results headline's paired descriptive (design review ticket 27):
  * "Δ Remembered − Forgotten +0.16", range +0.05 P21 to +0.26 P15, higher in
  * 24 of 24, computed from the unrounded participant means the native report
  * serves and only then rounded. The oracle is fixture.json's
  * `summary.paired_D_diff`, typed in below.
  */
class PairedDescriptiveSuite extends munit.FunSuite:
  private given ExecutionContext           = ExecutionContext.global
  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  /** fixture.json stores the oracle's values at 6 decimals. */
  private val FixtureRounding = 5e-7

  /** The native report's contrast and this mean differ only by summation
    * order.
    */
  private val ArithmeticTolerance = 1e-12

  // fixture.json summary.paired_D_diff (σ 2°, status ok).
  private val oracleMean = 0.155667
  private val oracle     = Vector(
    "P01" -> 0.205,
    "P02" -> 0.15,
    "P03" -> 0.228167,
    "P04" -> 0.181667,
    "P05" -> 0.208846,
    "P06" -> 0.130568,
    "P07" -> 0.218,
    "P08" -> 0.157143,
    "P09" -> 0.096167,
    "P10" -> 0.123125,
    "P11" -> 0.163718,
    "P12" -> 0.154524,
    "P13" -> 0.146818,
    "P14" -> 0.072857,
    "P15" -> 0.261282,
    "P16" -> 0.165,
    "P17" -> 0.062353,
    "P18" -> 0.054583,
    "P19" -> 0.2305,
    "P20" -> 0.147571,
    "P21" -> 0.05,
    "P22" -> 0.213452,
    "P23" -> 0.130429,
    "P24" -> 0.184231
  )

  /** A mean rounded for display first, as the board's 2-decimal tables print it. */
  private def two(v: Double): Double =
    BigDecimal(v).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

  private val forward = ok(ReportingContrast.of("Remembered", "Forgotten"))

  /** "By retrieval response" with its direction declared: grouping alone
    * never declares one (ReportingSpec).
    */
  private val contrasted: ReportingSpec =
    val s = PlotReports.spec
    ok(
      ReportingSpec.of(
        s.id,
        s.name,
        s.groupBy,
        s.filters,
        s.minimumPerGroup,
        s.weighting,
        Some(forward)
      )
    )

  /** Run 7's native report at σ 2° under `spec`. */
  private def native(spec: ReportingSpec): Future[ReportView] =
    HeadlessSession.open(StoryMoment.T3).flatMap { session =>
      session
        .report(PlotReports.run, spec, 2)
        .map(_.fold(e => fail(e.message), identity))
        .transformWith(result => session.close.transform(_ => result))
    }

  test("the fixture's paired difference: 0.15567 → +0.16, P21 to P15, 24 of 24") {
    native(contrasted).map { report =>
      val paired = PairedDescriptive.of(report).getOrElse(fail("no D contrast served"))
      assertEquals(
        (paired.minuend, paired.subtrahend),
        (Response.Remembered, Response.Forgotten)
      )
      assertEquals(paired.differences.map(_.participant), oracle.map(_._1))
      paired.differences.zip(oracle).foreach { (d, o) =>
        assertEqualsDouble(d.d, o._2, FixtureRounding, d.participant)
      }
      val mean = paired.mean.getOrElse(fail("no mean"))
      assertEqualsDouble(mean, oracleMean, FixtureRounding)
      // The served eyes4s contrast agrees: the same pairing, the same mean.
      val served = report.contrast(ReportRole.Difference).getOrElse(fail("no contrast"))
      assertEqualsDouble(
        mean,
        served.estimate.getOrElse(fail("no estimate")),
        ArithmeticTolerance
      )
      assertEquals(paired.pairedN, served.pairedN)
      assertEquals(paired.unpaired, served.unpaired.map(_.participant))
      assertEquals(paired.lowest.map(_.participant), Some("P21"))
      assertEquals(paired.highest.map(_.participant), Some("P15"))
      assertEquals((paired.higher, paired.pairedN), (24, 24))

      val vm = PairedDescriptiveVM.of(paired)
      assertEquals(vm.heading, "Δ Remembered − Forgotten · paired n 24")
      assertEquals(vm.value, Some("+0.16"))
      assertEquals(vm.higher, Some("higher in 24 of 24"))
      assertEquals(
        vm.note,
        "Range +0.05 P21 to +0.26 P15 · computed before rounding participant means · " +
          "Descriptive only, no intervals or tests"
      )
      assert(vm.spoken.contains("mean difference +0.16"), vm.spoken)
      assert(vm.spoken.contains("range +0.05 P21 to +0.26 P15"), vm.spoken)
    }
  }

  test("rounding the fixture's participant means first would read +0.15, not +0.16") {
    native(contrasted).map { report =>
      val paired  = PairedDescriptive.of(report).getOrElse(fail("no D contrast served"))
      val rounded = PairedDescriptive.of(
        report.copy(participants =
          report.participants.map(p => p.copy(value = p.value.map(two)))
        )
      )
      assertEquals(PairedDescriptiveVM.of(paired).value, Some("+0.16"))
      assertEquals(rounded.map(PairedDescriptiveVM.of(_).value), Some(Some("+0.15")))
    }
  }

  test(
    "a grouping without a declared contrast has no paired block: direction is not inferred"
  ) {
    native(PlotReports.spec).map { report =>
      assertEquals(report.contrasts, Vector.empty)
      assertEquals(PairedDescriptive.of(report), None)
    }
  }

  // --- Synthetic reports --------------------------------------------------------------

  private val at = ok(ScaleIndex.of(2))

  private def mean(group: Response, participant: String, value: Option[Double]) =
    ReportParticipantView(
      Some(group),
      ReportRole.Difference,
      participant,
      if value.isDefined then 3 else 0,
      value,
      Option.when(value.isEmpty)(ReportAbsence.EmptyGroup),
      StudioRef.ReportParticipant(
        PlotReports.run,
        PlotReports.reporting,
        at,
        ReportGroup.Level(group),
        ReportRole.Difference,
        participant
      )
    )

  /** A report with a declared Remembered − Forgotten contrast over `rows`
    * (participant, Remembered mean, Forgotten mean).
    */
  private def synthetic(rows: (String, Option[Double], Option[Double])*): ReportView =
    val base = PlotReports.synthetic()
    base.copy(
      participants = rows.toVector.flatMap((p, r, f) =>
        Vector(mean(Response.Remembered, p, r), mean(Response.Forgotten, p, f))
      ),
      contrasts = Vector(
        ReportContrastView(
          ReportRole.Difference,
          Response.Remembered,
          Response.Forgotten,
          None,
          None,
          0,
          Vector.empty,
          StudioRef.ReportContrast(
            PlotReports.run,
            PlotReports.reporting,
            at,
            ReportRole.Difference,
            Response.Remembered,
            Response.Forgotten
          )
        )
      )
    )

  test("each participant's difference is taken before rounding its means") {
    // Remembered 0.304 and Forgotten 0.145 differ by 0.159 → +0.16; their
    // rounded means 0.30 and 0.15 would differ by 0.15.
    val vm =
      PairedDescriptiveVM.of(
        PairedDescriptive.of(synthetic(("A", Some(0.304), Some(0.145)))).get
      )
    assertEquals(vm.value, Some("+0.16"))
    assertEquals(
      Format.signed(
        two(0.304) - two(0.145),
        2
      ),
      "+0.15"
    )
  }

  test("a participant with means in one group only is left out, and the count is said") {
    val paired = PairedDescriptive
      .of(
        synthetic(
          ("A", Some(0.4), Some(0.1)),
          ("B", Some(0.5), None),
          ("C", None, Some(0.2)),
          ("D", Some(0.3), Some(0.2))
        )
      )
      .get
    assertEquals(paired.differences.map(_.participant), Vector("A", "D"))
    assertEquals(paired.unpaired, Vector("B", "C"))
    val vm = PairedDescriptiveVM.of(paired)
    assertEquals(vm.heading, "Δ Remembered − Forgotten · paired n 2")
    assertEquals(vm.value, Some("+0.20"))
    assertEquals(vm.higher, Some("higher in 2 of 2"))
    assert(vm.note.endsWith(" · " + PairedText.unpaired(2)), vm.note)
    assert(vm.spoken.endsWith(PairedText.unpaired(2) + "."), vm.spoken)
    val one = PairedDescriptiveVM.of(
      PairedDescriptive.of(synthetic(("A", Some(0.4), Some(0.1)), ("B", Some(0.5), None))).get
    )
    assert(one.note.endsWith(PairedText(PairedTextId.OneUnpaired)), one.note)
  }

  test("higher counts a strictly higher minuend only: a tie and a drop are not higher") {
    val paired = PairedDescriptive
      .of(
        synthetic(
          ("A", Some(0.25), Some(0.25)),
          ("B", Some(0.10), Some(0.30)),
          ("C", Some(0.40), Some(0.10))
        )
      )
      .get
    assertEquals(paired.higher, 1)
    val vm = PairedDescriptiveVM.of(paired)
    assertEquals(vm.higher, Some("higher in 1 of 3"))
    assert(vm.note.startsWith(s"Range ${Format.Minus}0.20 B to +0.30 C"), vm.note)
  }

  test("no paired participant: no value, and the note says nothing is paired") {
    val paired = PairedDescriptive
      .of(synthetic(("A", Some(0.4), None), ("B", None, Some(0.1))))
      .get
    assertEquals((paired.pairedN, paired.mean), (0, None))
    val vm = PairedDescriptiveVM.of(paired)
    assertEquals(vm.heading, "Δ Remembered − Forgotten · paired n 0")
    assertEquals((vm.value, vm.higher), (None, None))
    assertEquals(
      vm.note,
      "No participant has means in both Remembered and Forgotten; nothing is paired · " +
        PairedText.unpaired(2)
    )
    assertEquals(
      PairedDescriptiveVM.of(PairedDescriptive.of(synthetic()).get).note,
      "No participant has means in both Remembered and Forgotten; nothing is paired"
    )
  }
