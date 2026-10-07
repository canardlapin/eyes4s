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

package eyes4s.studio.app.plot

import eyes4s.studio.core.backend.Response
import eyes4s.studio.core.document.{ScaleSet, Sigma}
import scala.concurrent.ExecutionContext

/** A profile binds report values by explicit run, spec and scale identity. */
class ScaleProfileSuite extends munit.FunSuite:
  private given ExecutionContext           = ExecutionContext.global
  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val columns                      = ok(ProfileColumns.standard)

  test("every native profile point preserves its scale's report value and exact reference") {
    PlotReports.read.map { (summary, grouped, overall) =>
      val profile = ok(ScaleProfile.of(grouped, overall, PlotReports.scales, summary.scales))
      assertEquals(profile.groups.map(_.name), Vector("Remembered", "Forgotten"))
      assertEquals(profile.participants.size, 24)
      for series <- profile.groups; point <- series.points do
        val cell = grouped(point.scale.value)
          .cell(Some(Response(series.name)), eyes4s.studio.core.backend.ReportRole.Difference)
          .get
        assertEquals(point.d, cell.estimate)
        assertEquals(point.ref, cell.ref)
      for series <- profile.participants; point <- series.points do
        val cell = overall(point.scale.value)
          .participant(None, eyes4s.studio.core.backend.ReportRole.Difference, series.name)
          .get
        assertEquals(point.d, cell.value)
        assertEquals(point.ref, cell.ref)
      val reordered = ok(
        ScaleProfile.of(grouped.reverse, overall.reverse, PlotReports.scales, summary.scales)
      )
      assertEquals(reordered.groups, profile.groups)
      assertEquals(reordered.participants, profile.participants)
      assertEquals(ok(ScaleProfile.source(profile, columns)).rows.size, 104)
    }
  }

  private val scales  = ok(ScaleSet.of(Vector(1.0, 2.0).map(d => ok(Sigma.of(d)))))
  private val labels  = scales.values
  private def grouped = Vector.tabulate(2)(i => PlotReports.synthetic(scale = i))
  private def whole   = Vector.tabulate(2)(i =>
    PlotReports.synthetic(scale = i, group = None, id = PlotReports.overallId)
  )

  test(
    "missing estimates remain missing with the served reference, while zero remains a value"
  ) {
    val reports =
      grouped.updated(1, grouped(1).copy(cells = grouped(1).cells.map(_.copy(estimate = None))))
    val profile = ok(ScaleProfile.of(reports, whole, scales, labels))
    assertEquals(profile.groups.head.points.map(_.d), Vector(Some(0.2), None))
    assertEquals(profile.groups.head.points(1).ref, reports(1).cells.head.ref)
    assertEquals(profile.participants.head.points.map(_.d), Vector.fill(2)(Some(0.0)))
    assertEquals(profile.participants(1).points.map(_.d), Vector.fill(2)(None))
  }

  test(
    "foreign run/spec, duplicate scales and missing cells are refused before any profile is made"
  ) {
    val foreign = grouped.updated(1, grouped(1).copy(run = eyes4s.studio.core.backend.RunId(8)))
    assert(ScaleProfile.of(foreign, whole, scales, labels).isLeft)
    val spec = grouped.updated(1, grouped(1).copy(reporting = PlotReports.overallId))
    assert(ScaleProfile.of(spec, whole, scales, labels).isLeft)
    assert(ScaleProfile.of(Vector.fill(2)(grouped.head), whole, scales, labels).isLeft)
    val missing = grouped.updated(1, grouped(1).copy(cells = Vector.empty))
    assertEquals(
      ScaleProfile.of(missing, whole, scales, labels),
      Left(
        ProfileError.MissingSeries(
          PlotReports.run,
          PlotReports.reporting,
          1,
          Some(Response.Remembered),
          None
        )
      )
    )
    val noParticipant =
      whole.updated(1, whole(1).copy(participants = whole(1).participants.drop(1)))
    assertEquals(
      ScaleProfile.of(grouped, noParticipant, scales, labels),
      Left(
        ProfileError.MissingSeries(PlotReports.run, PlotReports.overallId, 1, None, Some("A"))
      )
    )
  }

  test("Results uses each true whole-report estimate, reference and per-scale count") {
    PlotReports.read.map { (summary, grouped, overall) =>
      val profile =
        ok(ScaleProfile.overall(overall.reverse, PlotReports.scales, summary.scales))
      assertEquals(profile.groups.map(_.name), Vector("Grand mean"))
      assertEquals(profile.participants.size, 24)
      val source = ok(ScaleProfile.source(profile, columns))
      assertEquals(source.rows.size, (1 + 24) * PlotReports.scales.values.size)
      for point <- profile.groups.head.points do
        val cell = overall(point.scale.value)
          .cell(None, eyes4s.studio.core.backend.ReportRole.Difference)
          .get
        assertEquals(point.d, cell.estimate)
        assertEquals(point.ref, cell.ref)
        assertEquals(
          source.rows(point.scale.value).values.last,
          PlotValue.Text(eyes4s.studio.app.text.ParticipantText.participants(cell.participants))
        )
      // The figures' explicitly grouped profile remains a different view.
      assertEquals(
        ok(ScaleProfile.of(grouped, overall, PlotReports.scales, summary.scales)).groups
          .map(_.name),
        Vector("Remembered", "Forgotten")
      )
    }
  }

  test("overall profile retains missing and zero means with exact scale-specific counts") {
    // A served estimate is authoritative even when participants' rendered
    // payload alone would yield a different arithmetic reconstruction.
    assertEquals(
      ok(ScaleProfile.overall(whole, scales, labels)).groups.head.points.map(_.d),
      Vector.fill(2)(Some(0.2))
    )
    val reports = whole.zipWithIndex.map { (report, i) =>
      report.copy(
        cells = report.cells.map(
          _.copy(estimate = if i == 0 then Some(0.0) else None, participants = i + 2)
        ),
        participants = report.participants.map(_.copy(queries = i + 3))
      )
    }
    val profile = ok(ScaleProfile.overall(reports, scales, labels))
    assertEquals(profile.groups.head.points.map(_.d), Vector(Some(0.0), None))
    assertEquals(profile.groups.head.points.map(_.ref), reports.map(_.cells.head.ref))
    val source = ok(ScaleProfile.source(profile, columns))
    assertEquals(
      source.rows.take(2).map(_.values.last),
      Vector(PlotValue.Text("2 participants"), PlotValue.Text("3 participants"))
    )
    assertEquals(
      source.rows.slice(2, 4).map(_.values.last),
      Vector(PlotValue.Text("3 queries"), PlotValue.Text("4 queries"))
    )
  }

  test("overall profile refuses absent whole cells, wrong context and incomplete scales") {
    val missing = whole.updated(0, whole.head.copy(cells = Vector.empty))
    assertEquals(
      ScaleProfile.overall(missing, scales, labels),
      Left(ProfileError.MissingSeries(PlotReports.run, PlotReports.overallId, 0, None, None))
    )
    assert(ScaleProfile.overall(grouped, scales, labels).isLeft)
    assert(ScaleProfile.overall(whole.take(1), scales, labels).isLeft)
    assert(ScaleProfile.overall(Vector.fill(2)(whole.head), scales, labels).isLeft)
    assert(
      ScaleProfile
        .overall(
          whole.updated(1, whole(1).copy(run = eyes4s.studio.core.backend.RunId(8))),
          scales,
          labels
        )
        .isLeft
    )
    assert(
      ScaleProfile
        .overall(
          whole.updated(1, whole(1).copy(reporting = PlotReports.reporting)),
          scales,
          labels
        )
        .isLeft
    )
    assert(
      ScaleProfile
        .overall(
          whole.updated(0, whole.head.copy(cells = whole.head.cells ++ whole.head.cells)),
          scales,
          labels
        )
        .isLeft
    )
  }

  test("served scale identities are checked even when numerical reports coincide") {
    val reports = Vector.tabulate(2)(i => PlotReports.synthetic(scale = i))
    val swapped = scales.values.reverse
    val error = ProfileError.ScaleIdentity(PlotReports.run, 0, swapped.head, scales.values.head)
    assertEquals(ScaleProfile.of(reports, whole, scales, swapped), Left(error))
    assertEquals(ScaleProfile.overall(whole, scales, swapped), Left(error))
  }
