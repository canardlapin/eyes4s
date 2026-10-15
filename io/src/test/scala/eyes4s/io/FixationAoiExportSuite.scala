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

package eyes4s.io

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.results.{FixationAoiTables, TableJson}

class FixationAoiExportSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame = get(Frame.screen("fixation-aoi-export", 100, 100))
  private val clock = ClockId("fixation-aoi-export")
  private val area  = get(
    Aoi.of("a,\"λ\"", "Area\nlabel", frame, get(Region.rect(Pt[Px](0, 0), Pt[Px](50, 50))))
  )
  private val set                           = get(AoiSet.of(Vector(area)))
  private def path(x: Double): Scanpath[Px] = get(
    Scanpath.of(
      frame,
      clock,
      IArray(
        get(
          Event.Fixation.withoutDispersion(
            get(
              Interval.of(
                clock,
                Instant.micros(9007199254740993L),
                Instant.micros(18014398509481987L)
              )
            ),
            Pt[Px](x, 10),
            1
          )
        )
      )
    )
  )
  private val key = TableJson.obj(
    "participant" -> TableJson.text("p,\"λ\"\n"),
    "trial"       -> TableJson.integer(9007199254740993L)
  )

  test(
    "both fixation AOI families use the shared CSV writer with exact time, quoting and null validity"
  ) {
    val exported =
      get(FixationAoiTables.tables(get(set.assign(path(10), MembershipPolicy.Multiple)), key))
    assertEquals(
      exported.map(_.family).toSet,
      Set(ResultFamily.FixationAoiMetrics, ResultFamily.FixationAoiTransitions)
    )
    exported.foreach { table =>
      val document = table.csv
      val read     = get(Rfc4180.decode(document.encode))
      assertEquals(read.head, document.header)
      assertEquals(read.tail, document.rows)
      assertEquals(read.tail.size, table.rows.size)
      assert(table.columns.forall(c => c.unit.nonEmpty && c.meaning.nonEmpty))
      assertEquals(
        get(table.metadata.circe.hcursor.downField("table_sha256").as[String]),
        table.identity.hex
      )
    }
    val metrics = exported.head
    assertEquals(
      metrics.column("dwell_us"),
      Some(Vector(ResultCell.Integer(9007199254740994L)))
    )
    assertEquals(metrics.column("visit_count"), Some(Vector(ResultCell.Integer(1))))
    assert(metrics.csv.encode.contains("9007199254740994"))
    assertEquals(
      metrics.csv.rows.head(metrics.csv.header.indexOf("dwell_proportion__valid")),
      "true"
    )
    assertEquals(
      metrics.csv.rows.head(metrics.csv.header.indexOf("first_entry_latency_us__valid")),
      "true"
    )
  }

  test("off-surface and background rows retain absent versus zero proportions in CSV") {
    Vector(110.0 -> "false", 90.0 -> "true").foreach { (x, valid) =>
      val metrics = get(
        FixationAoiTables.tables(get(set.assign(path(x), MembershipPolicy.Multiple)), key)
      ).head
      assertEquals(
        metrics.csv.rows.head(metrics.csv.header.indexOf("dwell_proportion__valid")),
        valid
      )
      assertEquals(
        metrics.csv.rows.head(metrics.csv.header.indexOf("first_entry_latency_us__valid")),
        "false"
      )
      assertEquals(get(Rfc4180.decode(metrics.csv.encode)).tail, metrics.csv.rows)
    }
  }
