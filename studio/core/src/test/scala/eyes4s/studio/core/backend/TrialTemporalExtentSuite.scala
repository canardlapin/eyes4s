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

package eyes4s.studio.core.backend

import eyes4s.kernel.{Span, Window}
import eyes4s.studio.core.document.{ColumnName, InventoryDurationColumn, SourcePath, TimeUnit}
import io.circe.Json
import io.circe.syntax.*

class TrialTemporalExtentSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val trial                            = ProtocolSamples.query
  private val dataset                          = DatasetRevision(3)
  private val source                           = ok(SourcePath.of("inputs/trials.csv"))
  private val declaration                      = ok(
    InventoryDurationColumn.of(ok(ColumnName.of("elapsed")), TimeUnit.Microseconds)
  )
  private def extent(until: Long = 5000000L, records: Vector[Int] = Vector(2)) =
    TrialExtentFromStart.of(
      trial,
      dataset,
      ok(Window.lasting(Span.micros(until))),
      source,
      declaration,
      records
    )

  test("declared and all missing states roundtrip; unsafe Long endpoints retain precision") {
    val value = ok(extent(9007199254740993L))
    assertEquals(value.asJson.hcursor.get[String]("untilMicros"), Right("9007199254740993"))
    val evidence = ok(TrialExtentEvidence.of(trial, dataset, source, declaration, Vector(2)))
    Vector[TrialTemporalExtent](
      TrialTemporalExtent.FromTrialStart(value),
      TrialTemporalExtent.Blank(evidence),
      TrialTemporalExtent.Undeclared(trial, dataset),
      TrialTemporalExtent.LegacyMissing(trial)
    ).foreach { e =>
      assertEquals(e.asJson.as[TrialTemporalExtent], Right(e))
    }
  }

  test("header, empty, duplicate and unordered records cannot become evidence or wire values") {
    Vector(Vector.empty[Int], Vector(1), Vector(2, 2), Vector(3, 2)).foreach { records =>
      assertEquals(
        extent(records = records),
        Left(TrialViewError.ExtentRecords(trial, source.value, records))
      )
      val blankValue: TrialTemporalExtent = TrialTemporalExtent.Blank(
        ok(TrialExtentEvidence.of(trial, dataset, source, declaration, Vector(2)))
      )
      val blank        = blankValue.asJson
      val invalidBlank = blank.hcursor
        .downField("Blank")
        .downField("evidence")
        .downField("records")
        .withFocus(_ => records.asJson)
        .top
        .getOrElse(fail("no blank evidence"))
      assert(invalidBlank.as[TrialTemporalExtent].isLeft)
      assert(
        ok(extent()).asJson
          .mapObject(_.add("records", records.asJson))
          .as[TrialExtentFromStart]
          .isLeft
      )
    }
  }

  test("wrong trial and dataset cannot enter the served fixation transport") {
    val original = ProtocolSamples.trialFixations
    val valid    = TrialTemporalExtent.FromTrialStart(ok(extent()))
    assertEquals(
      TrialFixations
        .of(original.revision, dataset, trial, original.fixations, valid)
        .map(_.extent),
      Right(valid)
    )
    val other = trial.copy(trial = "other")
    assertEquals(
      TrialFixations.of(original.revision, dataset, other, Vector.empty, valid),
      Left(TrialViewError.OtherTrial(other, trial))
    )
    val wrong = TrialTemporalExtent.Undeclared(trial, DatasetRevision(99))
    assertEquals(
      TrialFixations.of(original.revision, dataset, trial, original.fixations, wrong),
      Left(TrialViewError.ExtentDataset(trial, dataset, DatasetRevision(99)))
    )
    val wire = original.asJson.mapObject(_.add("extent", wrong.asJson))
    assert(wire.as[TrialFixations].isLeft)
    val wrongDeclared = ok(
      TrialExtentFromStart.of(
        trial,
        DatasetRevision(99),
        ok(Window.lasting(Span.micros(5000000))),
        source,
        declaration,
        Vector(2)
      )
    )
    val wrongDeclaredValue: TrialTemporalExtent =
      TrialTemporalExtent.FromTrialStart(wrongDeclared)
    assert(
      original.asJson
        .mapObject(_.add("extent", wrongDeclaredValue.asJson))
        .as[TrialFixations]
        .isLeft
    )
    val wrongBlankValue: TrialTemporalExtent = TrialTemporalExtent.Blank(
      ok(TrialExtentEvidence.of(trial, DatasetRevision(99), source, declaration, Vector(2)))
    )
    assert(
      original.asJson
        .mapObject(_.add("extent", wrongBlankValue.asJson))
        .as[TrialFixations]
        .isLeft
    )
    val blank = TrialTemporalExtent.Blank(
      ok(TrialExtentEvidence.of(other, dataset, source, declaration, Vector(2)))
    )
    assert(original.asJson.mapObject(_.add("extent", blank.asJson)).as[TrialFixations].isLeft)
  }

  test("zero and nonzero-origin intervals are refused by the trial-start contract") {
    assertEquals(extent(0), Left(TrialViewError.ExtentInvalid(trial, 0, 0)))
    assertEquals(
      TrialExtentFromStart.of(
        trial,
        dataset,
        ok(Window.of(Span.micros(1), Span.micros(10))),
        source,
        declaration,
        Vector(2)
      ),
      Left(TrialViewError.ExtentInvalid(trial, 1, 10))
    )
  }

  test(
    "legacy raw wire omissions are explicit missing; 1.17 frames are refused before bodies"
  ) {
    val response = ok(io.circe.parser.decode[BackendResponse](ProtocolPins.trialFixationsV17))
    response match
      case BackendResponse.TrialFixationsOf(value) =>
        assertEquals(value.extent, TrialTemporalExtent.LegacyMissing(value.trial))
      case _ => fail("wrong legacy response")
    val previous = ProtocolVersion(1, 17)
    val wire     = Envelope(RequestId(1), ServerFrame.Response(response): ServerFrame).asJson
      .deepMerge(
        Json.obj("version" -> previous.asJson, "body" -> Json.fromString("not a body"))
      )
    assertEquals(
      WireFormat.parseCurrent[ServerFrame](wire.noSpaces),
      Left(TransportError.Incompatible(previous, ProtocolVersion.Current))
    )
  }
