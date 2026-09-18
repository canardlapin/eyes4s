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

package eyes4s.codec

import eyes4s.compare.ComparisonWorkError
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg
import eyes4s.plan.*
import io.circe.Json

/** The typed wire forms the recording and temporal archives add: every case
  * of the temporal failure family and the errors it wraps, events of every
  * kind with their support, regions of every kind, and the comparison of
  * archived derived members with their derivation.
  */
class ArchiveWireSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private def samples(name: String): Vector[scala.reflect.Enum] =
    DiagnosticSamples.all
      .find(_.enumName == name)
      .map(_.samples.map(_._1))
      .getOrElse(Vector.empty)

  /** Every sampled case of every family a temporal failure can carry. */
  private val temporal: Vector[TemporalStudyError] =
    samples("TemporalStudyError").collect { case e: TemporalStudyError => e } ++
      samples("PlanError").collect { case e: PlanError => TemporalStudyError.Input(e) } ++
      samples("WindowOccupancyError").collect { case e: WindowOccupancyError =>
        TemporalStudyError.Occupancy(e)
      } ++
      samples("EvaluationSpecError").collect { case e: EvaluationSpecError =>
        TemporalStudyError.Input(PlanError.Specification(e))
      } ++
      samples("PairScheduleError").collect { case e: PairScheduleError =>
        TemporalStudyError.Input(PlanError.Schedule(e))
      } ++
      samples("ComparisonWorkError").collect { case e: ComparisonWorkError =>
        TemporalStudyError.Input(PlanError.ComparisonWork(e))
      }

  test(
    "every temporal failure family case round-trips with its operands, alone and in a study failure"
  ) {
    // Every case of the family and of the plan and occupancy errors it wraps.
    assertEquals(
      temporal.map(_.ordinal).distinct.sorted,
      DiagnosticSamples.labelsOf[TemporalStudyError].indices.toVector
    )
    assertEquals(
      temporal.collect { case TemporalStudyError.Input(e) => e.ordinal }.distinct.sorted,
      DiagnosticSamples.labelsOf[PlanError].indices.toVector
    )
    assertEquals(
      temporal.collect { case TemporalStudyError.Occupancy(e) => e.ordinal }.distinct.sorted,
      DiagnosticSamples.labelsOf[WindowOccupancyError].indices.toVector
    )
    val keys = StudyCodecs.key(DefinitionId.studyKey)
    val key  = DiagnosticSamples.k1
    temporal.foreach { error =>
      assertEquals(
        TemporalWire.readTemporalStudyError(TemporalWire.temporalStudyError(error)),
        Right(error),
        error.toString
      )
      val failure = StudyFailure.Temporal(key, error)
      assertEquals(
        ResultWire.studyFailure(keys)(failure).flatMap(ResultWire.readStudyFailure(keys)),
        Right(failure),
        error.toString
      )
    }
  }

  test("events of every kind round-trip with their support, and fixation spread is derived") {
    val clock   = ClockId("analysis")
    val span    = get(Interval.of(clock, Instant.micros(10), Instant.micros(30)))
    val range   = get(SampleRange.of(2, 5))
    val saccade = get(
      Event.Saccade
        .of[Deg](span, Pt(1.0, 2.0), Pt(3.5, -1.25), Some(get(Velocity.perSecond[Deg](412.5))))
    )
    val unmeasured = get(Event.Saccade.of[Deg](span, Pt(1.0, 2.0), Pt(3.5, -1.25), None))
    val pursuit    =
      get(Event.Pursuit.of[Deg](span, IArray(Pt[Deg](0.0, 0.0), Pt[Deg](0.5, -0.5))))
    val blink    = get(Event.Blink.of[Deg](span))
    val detected = get(Event.Fixation.withoutDispersion[Deg](span, Pt(0.25, 0.75), 3))
    Vector[Event[Deg]](saccade, unmeasured, pursuit, blink, detected).foreach { event =>
      val json            = get(RecordingResultWire.event(event, range))
      val (read, support) = get(RecordingResultWire.readEvent[Deg](json, clock))
      assertEquals(support, range)
      assertEquals(RecordingResultWire.event(read, support), Right(json))
      (event, read) match
        case (a: Event.Pursuit[Deg], b: Event.Pursuit[Deg]) =>
          assertEquals(b.span, a.span)
          assertEquals(b.path.toVector, a.path.toVector)
        case _ => assertEquals(read, event)
    }
    // A detected spread travels as its method; a declared value is not a detection's.
    val declared = get(
      Event.Fixation.of[Deg](span, Pt(0.25, 0.75), 0.5, DispersionMethod.RmsRadius, 3)
    )
    assertEquals(
      RecordingResultWire.event(declared, range).left.toOption.map(_.productPrefix),
      Some("Unsupported")
    )
  }

  test("regions of every kind round-trip") {
    val rect    = get(Region.rect[Deg](Pt(-1.5, -2.0), Pt(3.0, 4.25)))
    val ellipse = get(Region.ellipse[Deg](Pt(0.5, 0.5), 2.0, 1.0))
    val polygon = get(Region.polygon[Deg](Vector(Pt(0.0, 0.0), Pt(4.0, 0.0), Pt(2.0, 3.0))))
    Vector[Region[Deg]](
      rect,
      ellipse,
      polygon,
      rect || ellipse,
      rect && polygon,
      !ellipse,
      Region.empty[Deg],
      Region.everything[Deg]
    ).foreach { region =>
      assertEquals(
        RecordingResultWire.readRegion[Deg](RecordingResultWire.region(region)),
        Right(region)
      )
    }
  }

  test(
    "a derived member is compared by value; unknown members are ignored, missing ones refused"
  ) {
    val derived = Json.obj(
      "a" -> Json.fromInt(1),
      "b" -> Json.arr(Json.fromString("x"), Json.obj("c" -> Json.fromBoolean(true)))
    )
    val noted = Json.obj(
      "a" -> Json.fromInt(1),
      "b" -> Json
        .arr(Json.fromString("x"), Json.obj("c" -> Json.fromBoolean(true), "n" -> Json.Null)),
      "note" -> Json.fromString("an application's")
    )
    assertEquals(Derived.check("m", derived, noted), Right(()))
    // Numbers compare as the doubles they denote, however they are written.
    assertEquals(
      Derived.check(
        "n",
        Json.fromDoubleOrNull(1.0),
        io.circe.parser.parse("1").getOrElse(Json.Null)
      ),
      Right(())
    )
    assertEquals(
      Derived
        .check(
          "n",
          Json.fromDoubleOrNull(0.0),
          io.circe.parser.parse("-0.0").getOrElse(Json.Null)
        )
        .left
        .toOption
        .map(_.productPrefix),
      Some("Derived")
    )
    assertEquals(
      Derived.check("m", derived, noted.mapObject(_.add("a", Json.fromInt(2)))),
      Left(CodecError.Derived("m.a", Json.fromInt(2), Json.fromInt(1)))
    )
    assertEquals(
      Derived.check("m", derived, noted.mapObject(_.add("b", Json.arr(Json.fromString("x"))))),
      Left(CodecError.Derived("m.b.length", Json.fromInt(1), Json.fromInt(2)))
    )
    assertEquals(
      Derived
        .check("m", derived, noted.mapObject(_.remove("a")))
        .left
        .toOption
        .map(_.productPrefix),
      Some("Field")
    )
  }
