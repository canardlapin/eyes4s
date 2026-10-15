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

import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

private[io] object ScanpathExportFixtures:
  private def get[E, A](value: Either[E, A]): A = value.toOption.get
  val frame                                     = get(Frame.screen("scanpath-export", 100, 100))
  val clock                                     = ClockId("scanpath-export")
  val layout                                    = StudyKey.layout(DefinitionId.studyLayout)
  val codec                                     =
    new ScanpathStudyCodec[StudyKey, Px](layout, StudyCodecs.key(DefinitionId.studyKey))
  private def path(offset: Double) = get(
    Scanpath.of(
      frame,
      clock,
      IArray.from((0 until 3).map { index =>
        get(
          Event.Fixation.withoutDispersion(
            get(
              Interval
                .of(clock, Instant.micros(index * 1000L), Instant.micros(index * 1000L + 500))
            ),
            Pt[Px](index + 1.0 + offset, if index == 1 then 2.0 else 1.0),
            1
          )
        )
      })
    )
  )
  private val rows = Vector("a", "b", "c").zipWithIndex.flatMap { (item, index) =>
    Vector("focal", "reference").map(phase =>
      Trial(StudyKey("p", item, phase), (), path(index * 20.0))
    )
  } :+
    Trial(StudyKey("p", "excluded", "other"), (), path(0.0))
  val queries = Vector(Long.MinValue, 0L, 1000L, 1000L, Long.MaxValue).map(Instant.micros)
  def overlap(policy: MissingOverlapPolicy): ScanpathMethod[Px] =
    ScanpathMethod.Overlap(
      get(
        FixationOverlapSpec.of(
          clock,
          queries,
          get(OverlapThreshold.of[Px](0.5)),
          FixationGroundDistance.Euclidean,
          TrajectoryEndpoint.OnsetRange,
          policy
        )
      )
    )
  val methods: Vector[ScanpathMethod[Px]] = Vector(
    ScanpathMethods.baselineMultiMatch[Px],
    overlap(MissingOverlapPolicy.CountAsNonOverlap),
    ScanpathMethod.Transport(
      get(
        FixationTransportConfig
          .of[Px](20, 20, Span.micros(1000), 0.25, 0.5, 10000, 1e-10, 100, 1000000L)
      )
    )
  )
  def archive(method: ScanpathMethod[Px]): ScanpathStudyArchive[StudyKey, Px] =
    get(
      ScanpathStudyArchive.run(
        get(
          ScanpathStudyPlan.of(
            layout,
            StudyInput(Trials(rows)),
            frame,
            "focal",
            "reference",
            method
          )
        )
      )
    )
  def exported(method: ScanpathMethod[Px]): Vector[ResultTable] =
    get(ResultExports.scanpaths(archive(method), codec))
  val families: Vector[ResultFamily] = Vector(
    ResultFamily.ScanpathPairs,
    ResultFamily.ScanpathQueries,
    ResultFamily.ScanpathTransportDiagnostics,
    ResultFamily.ScanpathReductions,
    ResultFamily.ScanpathContrasts,
    ResultFamily.ScanpathExclusions
  )
  lazy val tables: Vector[ResultTable] =
    val candidates = methods.flatMap(exported)
    families.map(family =>
      candidates.find(table => table.family == family && table.rows.nonEmpty).get
    )

class ScanpathExportsSuite extends munit.FunSuite:
  import ScanpathExportFixtures.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def cells(table: ResultTable, name: String) =
    table.rows.map(_(table.columns.indexWhere(_.name == name)))
  test(
    "all scanpath table families carry typed schemas, complete archive context and portable CSV"
  ) {
    methods.foreach { method =>
      val tables = exported(method)
      assertEquals(tables.map(_.family), families)
      assert(tables.forall(table => table.context eq tables.head.context))
      tables.foreach { table =>
        assert(table.columns.forall(column => column.unit.nonEmpty && column.meaning.nonEmpty))
        val parsed = get(Rfc4180.decode(table.csv.encode))
        assertEquals(parsed.head, table.csv.header)
        assertEquals(parsed.tail, table.csv.rows)
        assertEquals(
          table.metadata.circe.hcursor.get[String]("family").toOption,
          Some(table.family.toString)
        )
        val stored = get(table.context.circe.hcursor.get[io.circe.Json]("archive"))
        assertEquals(codec.results.encode(get(codec.results.decode(stored))), Right(stored))
      }
    }
    assertEquals(tables.map(_.family).toSet, families.toSet)
    assert(tables.forall(_.rows.nonEmpty))
  }
  test(
    "pair rows retain both keys and all MultiMatch components while exclusions stay separate"
  ) {
    val exported = ScanpathExportFixtures.exported(methods.head).map(t => t.family -> t).toMap
    val pairs    = exported(ResultFamily.ScanpathPairs)
    assertEquals(pairs.rows.size, 9 * 5)
    assertEquals(cells(pairs, "role").count(_ == ResultCell.Text("matched")), 3 * 5)
    assertEquals(cells(pairs, "role").count(_ == ResultCell.Text("control")), 6 * 5)
    assertEquals(
      cells(pairs, "component").distinct,
      methods.head.componentNames.map(ResultCell.Text.apply)
    )
    assertEquals(exported(ResultFamily.ScanpathExclusions).rows.size, 1)
    assertEquals(exported(ResultFamily.ScanpathReductions).rows.size, 3 * 5 * 2)
    assertEquals(exported(ResultFamily.ScanpathContrasts).rows.size, 3 * 5)
  }
  test(
    "exact64 query times, repeats, missing distances and strict-summary failures are distinct"
  ) {
    val exported = ScanpathExportFixtures
      .exported(overlap(MissingOverlapPolicy.RequireComplete))
      .map(t => t.family -> t)
      .toMap
    val query = exported(ResultFamily.ScanpathQueries)
    assertEquals(query.rows.size, 9 * 5)
    assertEquals(
      cells(query, "query_micros").take(5),
      queries.map(t => ResultCell.Integer(t.toMicros))
    )
    assertEquals(cells(query, "distance").count(_ == ResultCell.Missing), 9 * 2)
    assertEquals(cells(query, "status").count(_ == ResultCell.Text("failure")), 9 * 2)
    val pairs = exported(ResultFamily.ScanpathPairs)
    assertEquals(cells(pairs, "status"), Vector.fill(9)(ResultCell.Text("failure")))
    assert(cells(pairs, "error_json").forall {
      case ResultCell.Text(value) => value.contains("missingSupport")
      case _                      => false
    })
    val reductions = exported(ResultFamily.ScanpathReductions)
    assert(cells(reductions, "contributing").forall(_ == ResultCell.Integer(0)))
  }
  test("transport exports original cost, convergence and input-count diagnostics") {
    val table =
      exported(methods(2)).find(_.family == ResultFamily.ScanpathTransportDiagnostics).get
    assertEquals(table.rows.size, 9)
    assertEquals(cells(table, "left_fixations"), Vector.fill(9)(ResultCell.Integer(3)))
    assertEquals(cells(table, "right_fixations"), Vector.fill(9)(ResultCell.Integer(3)))
    assert(cells(table, "marginal_residual").forall {
      case ResultCell.Number(value) => value <= 1e-10
      case _                        => false
    })
    assert(cells(table, "status").forall(_ == ResultCell.Text("success")))
    assert(table.context.compact.contains("no exact EMD or symmetry claim"))
  }
