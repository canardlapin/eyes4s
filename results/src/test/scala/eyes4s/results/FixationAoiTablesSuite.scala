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

package eyes4s.results

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class FixationAoiTablesSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                             = get(Frame.screen("aoi-tables", 100, 100))
  private val clock                             = ClockId("aoi-tables")
  private def area(id: String, lo: Double, hi: Double): Aoi[Px] =
    get(Aoi.of(id, id, frame, get(Region.rect(Pt[Px](lo, 0), Pt[Px](hi, 60)))))
  private val areas = get(AoiSet.of(Vector(area("left", 0, 50), area("right", 50, 100))))
  private val key   = TableJson.obj(
    "participant" -> TableJson.text("a,\"λ\"\n"),
    "trial"       -> TableJson.integer(9007199254740993L)
  )
  private def path(positions: Vector[Pt[Px]], start: Long = 1000L): Scanpath[Px] =
    val events = positions.zipWithIndex.map { (p, i) =>
      val onset = start + i * 100L
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(onset), Instant.micros(onset + 20))),
          p,
          1
        )
      )
    }
    get(Scanpath.of(frame, clock, IArray.from(events)))
  private def tables(points: Vector[Pt[Px]]): Vector[ResultTable] =
    get(
      FixationAoiTables.tables(get(areas.assign(path(points), MembershipPolicy.Multiple)), key)
    )

  test(
    "metrics retain exact fixation counts, visits and named denominators; transitions retain orientation"
  ) {
    val exported =
      tables(Vector(Pt[Px](10, 10), Pt[Px](20, 10), Pt[Px](70, 10), Pt[Px](10, 10)))
    val metrics     = exported.head
    val transitions = exported(1)
    assertEquals(
      exported.map(_.family),
      Vector(ResultFamily.FixationAoiMetrics, ResultFamily.FixationAoiTransitions)
    )
    assertEquals(
      metrics.column("dwell_us"),
      Some(Vector(ResultCell.Integer(60), ResultCell.Integer(20)))
    )
    assertEquals(
      metrics.column("analysable_fixation_us"),
      Some(Vector.fill(2)(ResultCell.Integer(80)))
    )
    assertEquals(
      metrics.column("fixation_count"),
      Some(Vector(ResultCell.Integer(3), ResultCell.Integer(1)))
    )
    assertEquals(
      metrics.column("entry_count"),
      Some(Vector(ResultCell.Integer(2), ResultCell.Integer(1)))
    )
    assertEquals(metrics.column("visit_count"), metrics.column("entry_count"))
    assertEquals(
      metrics.column("first_entry_latency_us"),
      Some(Vector(ResultCell.Integer(0), ResultCell.Integer(200)))
    )
    assertEquals(
      metrics.column("dwell_proportion"),
      Some(Vector(ResultCell.Number(0.75), ResultCell.Number(0.25)))
    )
    assertEquals(
      transitions.column("from_aoi"),
      Some(Vector(ResultCell.Text("left"), ResultCell.Text("right")))
    )
    assertEquals(
      transitions.column("to_aoi"),
      Some(Vector(ResultCell.Text("right"), ResultCell.Text("left")))
    )
    assertEquals(
      transitions.column("transition_count"),
      Some(Vector.fill(2)(ResultCell.Integer(1)))
    )
    assertEquals(metrics.context, transitions.context)
    assertEquals(
      metrics.context.field("denominator"),
      Some(TableJson.text("on-surface-fixation-duration-including-background"))
    )
    assertEquals(
      metrics.context.field("report").flatMap(_.field("inter_fixation_us")),
      Some(TableJson.integer(240))
    )
    assertEquals(
      metrics.column("key_json"),
      Some(Vector.fill(2)(ResultCell.Text(key.canonical)))
    )
    assertEquals(
      metrics.columns.find(_.name == "dwell_us").map(_.kind),
      Some(ResultColumnType.Int64)
    )
  }

  test("missing proportions differ from zero and empty transition tables retain their schema") {
    val background = tables(Vector(Pt[Px](90, 90))).head
    assertEquals(
      background.column("dwell_proportion"),
      Some(Vector.fill(2)(ResultCell.Number(0)))
    )
    assertEquals(
      background.column("first_entry_latency_us"),
      Some(Vector.fill(2)(ResultCell.Missing))
    )
    assertEquals(
      background.column("first_entry_status"),
      Some(Vector.fill(2)(ResultCell.Text("never-entered")))
    )
    val offSurface = tables(Vector(Pt[Px](110, 10)))
    assertEquals(
      offSurface.head.column("dwell_proportion"),
      Some(Vector.fill(2)(ResultCell.Missing))
    )
    assertEquals(
      offSurface.head.column("dwell_proportion_status"),
      Some(Vector.fill(2)(ResultCell.Text("no-analysable-fixation-time")))
    )
    assert(offSurface(1).rows.isEmpty)
    assertEquals(
      offSurface(1).csvHeader,
      Vector("table_sha256", "key_json", "from_aoi", "to_aoi", "transition_count")
    )
    assert(offSurface.head.csvHeader.contains("dwell_proportion__valid"))
  }

  test("identity binds exact input geometry and absolute timing even when measures agree") {
    val points          = Vector[Pt[Px]](Pt(10, 10), Pt(20, 10))
    val original        = get(areas.assign(path(points), MembershipPolicy.Multiple))
    val enlarged        = get(AoiSet.of(Vector(area("left", 0, 45), area("right", 50, 100))))
    val changedGeometry = get(enlarged.assign(original.scanpath, original.policy))
    val changedTiming   = get(areas.assign(path(points, 2000), original.policy))
    val base            = get(FixationAoiTables.tables(original, key)).head
    Vector(changedGeometry, changedTiming).foreach { changed =>
      val table = get(FixationAoiTables.tables(changed, key)).head
      assertEquals(table.rows, base.rows)
      assertNotEquals(table.identity, base.identity)
      assertNotEquals(table.context.field("input_sha256"), base.context.field("input_sha256"))
    }
    val changedKey = get(FixationAoiTables.tables(original, TableJson.text("other"))).head
    assertNotEquals(changedKey.identity, base.identity)
  }

  test(
    "every region constructor and spatial policy has deterministic structural input identity"
  ) {
    val rect    = get(Region.rect(Pt[Px](0, 0), Pt[Px](50, 60)))
    val ellipse = get(Region.ellipse(Pt[Px](25, 25), 20, 20))
    val polygon = get(Region.polygon(Vector[Pt[Px]](Pt(0, 0), Pt(50, 0), Pt(25, 50))))
    val regions = Vector(
      rect,
      ellipse,
      polygon,
      rect || ellipse,
      rect && ellipse,
      !rect,
      Region.empty[Px],
      Region.everything[Px]
    )
    val policies = Vector(
      MembershipPolicy.Multiple,
      MembershipPolicy.ExclusiveByPriority,
      MembershipPolicy.RejectOverlap,
      MembershipPolicy.SmallestContaining(get(AoiResolution.of(10, 10)))
    )
    val digests = for r <- regions; policy <- policies yield
      val set        = get(AoiSet.of(Vector(get(Aoi.of("area", "Area", frame, r)))))
      val assignment = get(set.assign(path(Vector(Pt[Px](10, 10))), policy))
      val first      = get(FixationAoiTables.tables(assignment, key)).head
      val repeated   = get(FixationAoiTables.tables(assignment, key)).head
      assertEquals(first.identity, repeated.identity)
      first.context.field("input_sha256")
    assertEquals(digests.distinct.size, regions.size * policies.size)
  }
