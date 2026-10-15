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

package eyes4s.pointfamilyconsumer

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.*

class PointSamplingFamilySuite extends munit.FunSuite:
  private def get[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)
  private val clock                         = ClockId("point-execution")
  private val frame                         = get(Frame.screen("point-execution", 1, 1))
  private val grid                          = get(Grid.over(frame, 1, 1))
  private val layout                        =
    StudyKey.layout(get(DefinitionId.of("example.point-execution-layout", 1)))
  private val path = get(
    Scanpath.of(
      frame,
      clock,
      IArray(
        get(
          Event.Fixation.withoutDispersion(
            get(Interval.of(clock, Instant.micros(0), Instant.micros(10))),
            Pt[Px](0.5, 0.5),
            1
          )
        )
      )
    )
  )
  private val sources = Vector("a", "b").flatMap(stimulus =>
    Vector("one", "two").map(phase => Trial(StudyKey("person", stimulus, phase), (), path))
  )
  private val templates = Trials(Vector("a" -> 2.0, "b" -> 5.0).map { (stimulus, value) =>
    val values = IArray(value)
    Trial(
      StudyKey("person", stimulus, "template"),
      (),
      get(Surface.signed(grid, values, Provenance.raw(ContentHash.of(values))))
    )
  })
  private val spec = get(
    PointSamplingSpec.of(
      clock,
      Vector(0L, 5L, 10L).map(Instant.micros),
      None,
      DensityNormalization.None,
      DensityLookupPolicy.NearestClampedRIndex,
      TrajectoryEndpoint.HoldLastOnset,
      PointControlSelection.Candidates(Selection.All),
      FailurePolicy.RequireAll,
      FailurePolicy.RequireAll
    )
  )
  private def plan(rows: Vector[Trial[StudyKey, Unit, Scanpath[Px]]] = sources) =
    PointSamplingPlan.of(layout, StudyInput(Trials(rows)), templates, spec)

  test("descriptor and typed form cover every choice and rebuild the complete description") {
    val p          = plan(); val form = new PointSamplingForm(clock)
    val inspection = get(p.inspect)
    assertEquals(inspection.description, p.description)
    assertEquals(form.views, PointSamplingViews.fields)
    form.fields.foreach { field =>
      val raw = form.values(p).get(field.view.id)
      assertEquals(field.restore(raw), Right(raw))
      assertEquals(form.validate(field.view.id, raw), Right(()))
    }
    assertEquals(get(get(form.parse(form.values(p))).plan(p)).description, p.description)
    assertEquals(p.diff(p), Vector.empty)
    assertEquals(inspection.execution, ExecutionCapability.SynchronousWholeOperation)
  }

  test("queries, bin endpoints and bottom-k seed retain all signed 64-bit bits") {
    val p    = plan()
    val bins = get(
      PointBins.of(
        clock,
        Vector(Long.MinValue, -9007199254740993L, Long.MaxValue).map(Instant.micros),
        PointBinEndpoint.IncludeFinalEndpoint
      )
    )
    val chosen = get(
      PointSamplingSpec.of(
        clock,
        Vector(Long.MaxValue, Long.MinValue, Long.MaxValue).map(Instant.micros),
        Some(bins),
        DensityNormalization.Maximum,
        DensityLookupPolicy.NearestClampedRIndex,
        TrajectoryEndpoint.OnsetRange,
        PointControlSelection.Candidates(
          Selection.BottomK(get(PairLimit.of(1)), Seed(Long.MinValue), SampleId("λ"))
        ),
        get(FailurePolicy.successfulOnly(1)),
        FailurePolicy.RequireAll
      )
    )
    val altered  = PointSamplingPlan.of(layout, p.source, templates, chosen)
    val form     = new PointSamplingForm(clock)
    val restored = get(get(form.parse(form.values(altered))).plan(p))
    assertEquals(restored.description, altered.description)
    assert(p.diff(altered).map(_.field).contains("queries"))
    assert(p.diff(altered).map(_.field).contains("controlSelection"))
    val empty = get(
      PointSamplingSpec.of(
        clock,
        Vector.empty,
        None,
        spec.normalization,
        spec.lookup,
        spec.endpoint,
        PointControlSelection.Disabled,
        spec.controlPolicy,
        spec.binPolicy
      )
    )
    val emptyPlan = PointSamplingPlan.of(layout, p.source, templates, empty)
    assertEquals(
      get(get(form.parse(form.values(emptyPlan))).plan(p)).description,
      emptyPlan.description
    )
  }

  test("field parsing accumulates independent invalid boundaries and control cap") {
    val p           = plan(); val form = new PointSamplingForm(clock)
    val invalidBins = RawValue.Group(
      Vector(
        get(FieldId.of("boundaries")) -> RawValue.Items(Vector.fill(2)(RawValue.Number("7"))),
        get(FieldId.of("endpoint"))   -> RawValue.Choice("HalfOpen")
      )
    )
    val invalidSelection = RawValue.Variant(
      "bottomK",
      Vector(
        get(FieldId.of("cap"))    -> RawValue.Number("0"),
        get(FieldId.of("seed"))   -> RawValue.Number("-9223372036854775808"),
        get(FieldId.of("sample")) -> RawValue.Text("sample")
      )
    )
    val values = form
      .values(p)
      .updated(form.bins.view.id, invalidBins)
      .updated(form.controlSelection.view.id, invalidSelection)
    assert(form.validate(form.bins.view.id, invalidBins).isLeft)
    assert(form.validate(form.controlSelection.view.id, invalidSelection).isLeft)
    val unknown = get(FieldId.of("unregistered-point-field"))
    assertEquals(
      form.validate(unknown, RawValue.Absent),
      Left(FieldError.UnknownField(unknown))
    )
    assertEquals(form.parse(values).left.toOption.get.toVector.size, 2)
  }

  test("metadata preflight checks artifact identity and typed matching failures") {
    val p = plan()
    assertEquals(p.preflight(Some(p.inputRef)).availability, Availability.Ready)
    assertEquals(p.preflight(None).availability, Availability.Unavailable)
    val duplicate = plan(sources :+ sources.head)
    val report    = duplicate.preflight(Some(duplicate.inputRef))
    assertEquals(report.availability, Availability.Ready)
    assertEquals(report.affectedTrials, Vector(sources.head.key))
    assert(report.diagnostics.exists(_.causes.exists(_.code.name == "at-trial")))
    assertEquals(p.preflight(Some(duplicate.inputRef)).availability, Availability.Unavailable)
    val unknown = Trial(StudyKey("person", "missing", "trial"), (), path)
    val absent  = plan(Vector(unknown))
    val missing = absent.preflight(Some(absent.inputRef))
    assertEquals(missing.affectedTrials, Vector(unknown.key))
    assert(absent.run.rows.head.template.isLeft)
  }

  test(
    "preflight checks clocks and frames without preparing numerically refusing templates"
  ) {
    val p          = plan()
    val otherClock = ClockId("other")
    val mismatched = get(
      PointSamplingSpec.of(
        otherClock,
        spec.queries,
        None,
        spec.normalization,
        spec.lookup,
        spec.endpoint,
        spec.controls,
        spec.controlPolicy,
        spec.binPolicy
      )
    )
    val changed = PointSamplingPlan.of(layout, p.source, templates, mismatched)
    val report  = changed.preflight(Some(changed.inputRef))
    assertEquals(
      report.affectedTrials,
      sources
        .flatMap(row =>
          Vector(row.key, StudyKey(row.key.participant, row.key.stimulus, "template"))
        )
        .distinct
    )
    val degenerate = get(
      PointSamplingSpec.of(
        clock,
        spec.queries,
        None,
        DensityNormalization.Sum,
        spec.lookup,
        spec.endpoint,
        spec.controls,
        spec.controlPolicy,
        spec.binPolicy
      )
    )
    val overflowGrid = get(Grid.over(frame, 2, 1))
    val overflow     = IArray(1e308, 1e308)
    val refs         = Trials(
      templates.rows.map(row =>
        Trial(
          row.key,
          (),
          get(Surface.signed(overflowGrid, overflow, Provenance.raw(ContentHash.of(overflow))))
        )
      )
    )
    val numerical = PointSamplingPlan.of(layout, p.source, refs, degenerate)
    val before    = numerical.preflight(Some(numerical.inputRef))
    assertEquals(before.availability, Availability.Ready)
    assertEquals(before.findings, Vector.empty)
    assert(before.notChecked.contains(UncheckedAspect.PointSampling))
    assert(numerical.run.rows.forall(_.points.forall(_.value.isLeft)))
  }

  test("matched template frame mismatch is reported with source and template keys") {
    val otherFrame = get(Frame.screen("different-point-frame", 1, 1))
    val otherGrid  = get(Grid.over(otherFrame, 1, 1))
    val values     = IArray(2.0)
    val refs       = Trials(
      templates.rows.map(row =>
        Trial(
          row.key,
          (),
          get(Surface.signed(otherGrid, values, Provenance.raw(ContentHash.of(values))))
        )
      )
    )
    val p      = PointSamplingPlan.of(layout, StudyInput(Trials(sources)), refs, spec)
    val report = p.preflight(Some(p.inputRef))
    assertEquals(report.findings.size, sources.size)
    assertEquals(report.availability, Availability.Ready)
    assert(p.run.rows.forall(_.points.forall(_.value.isLeft)))
  }
