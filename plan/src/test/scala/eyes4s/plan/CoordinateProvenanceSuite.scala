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

/** The coordinates of Studio's focus fixation, on a 1920 x 1080 screen with a
  * centred 1024 x 768 image and a declared 35 px/deg: screen (1148, 456),
  * image (700, 300), and (+5.37, +2.4) degrees from the image centre, x right
  * and y up. Every placement, and every refusal naming its operands.
  */
class CoordinateProvenanceSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private val screen = get(Frame.screen("screen", 1920, 1080))
  private val window =
    get(Subframe.of(screen, FrameId("image"), get(Bounds.of[Px](448, 156, 1472, 924))))
  private val scale = get(LinearAngularScale.of(screen, 35.0))

  private val retrieval = StudyKey("P17", "beach-042", "recall")
  private val encoding  = StudyKey("P17", "beach-042", "encode")

  private def path(key: StudyKey, frame: Frame[Px], points: (Double, Double)*): Scanpath[Px] =
    val clock = ClockId(s"${key.participant}/${key.phase}")
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 412L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    get(Scanpath.of(frame, clock, IArray.from(fixes)))

  // Fixation 1 is dropped as the first; fixation 2 is off the screen; 3 is on
  // the screen outside the image; 4 is the focus fixation.
  private val input = StudyInput(
    Trials(
      Vector(
        Trial(
          retrieval,
          (),
          path(retrieval, screen, (960, 540), (2000, 500), (100, 100), (1148, 456))
        ),
        Trial(encoding, (), path(encoding, screen, (900, 500)))
      )
    )
  )

  private def plan(
      geometry: StudyGeometry[Px],
      angular: Option[LinearAngularScale[Px]] = Some(scale),
      source: StudyInput[StudyKey, Px] = input
  ) = get(
    StudyPlan.configure(
      source.reference,
      StudyKey.layout(DefinitionId.studyLayout),
      geometry,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyScale.Native(StudyEstimate.Binned())),
      angular,
      FailurePolicy.RequireAll,
      StudyMethod.cosine[Px](DefinitionId.cosine),
      (),
      initialFixations = InitialFixationPolicy.dropFirst[Px]
    )
  )

  private val windowed =
    get(
      StudyGeometry.windowed(
        window,
        get(Grid.over(window.frame, 64, 48)),
        OffWindowPolicy.Exclude
      )
    )
  private def at(i: Int)    = get(ScanpathPosition.of(i))
  private val provenance    = get(CoordinateProvenance.of(plan(windowed), input, None))
  private def trail(i: Int) = get(provenance.fixation(retrieval, at(i))).trail

  test("the focus fixation: screen (1148, 456), image (700, 300), (+5.37, +2.4) degrees") {
    val focus = get(provenance.fixation(retrieval, at(3)))
    assertEquals(focus.key, retrieval)
    assertEquals(focus.position.number.value, 4)
    assertEquals(focus.span.duration, Span.micros(412L))
    assertEquals(focus.trail.recorded, None)
    assertEquals(focus.trail.correction, None)
    assertEquals(focus.trail.admitted, FramedPosition(FrameId("screen"), Pt[Px](1148, 456)))
    assertEquals(focus.trail.window, Some(FramedPosition(FrameId("image"), Pt[Px](700, 300))))
    assertEquals(focus.trail.placement, MapPlacement.InMap)
    val degrees = get(focus.trail.angular.toRight("no degrees"))
    assertEqualsDouble(degrees.position.x, (700.0 - 512.0) / 35.0, 1e-12)
    assertEqualsDouble(degrees.position.y, (384.0 - 300.0) / 35.0, 1e-12)
    assertEquals(f"${degrees.position.x}%+.1f, ${degrees.position.y}%+.1f", "+5.4, +2.4")
    // Without a ledger no record is known, and it says why.
    assertEquals(focus.record, Left(MissingSource.NoLedger))
    assertEquals(focus.source, None)
  }

  test("the angular frame is explicit: the image centre, x right, y up, at 35 px/deg") {
    val reference = get(provenance.angular.toRight("no degrees"))
    assertEquals(reference.measured, FrameId("image"))
    assertEquals(reference.origin, Pt[Px](512, 384))
    assertEquals(reference.unitsPerDegree, 35.0)
    assertEquals(reference.degrees.id, FrameId("image/degrees"))
    assertEquals(reference.degrees.yAxis, YAxis.Up)
    assertEquals(trail(3).angular.map(_.reference), Some(reference))
    assertEquals(provenance.admission, screen)
  }

  test("every fixation is dropped, off the screen, outside the window or in the map") {
    assertEquals(
      (0 to 3).map(i => trail(i).placement).toVector,
      Vector(
        MapPlacement.DroppedInitial,
        MapPlacement.OutsideScreen,
        MapPlacement.OutsideWindow(OffWindowPolicy.Exclude),
        MapPlacement.InMap
      )
    )
    // Positions outside the image still have image coordinates and degrees.
    assertEquals(trail(2).window, Some(FramedPosition(FrameId("image"), Pt[Px](-348, -56))))
    assertEquals(trail(1).window, Some(FramedPosition(FrameId("image"), Pt[Px](1552, 344))))
    assert(trail(1).angular.exists(_.position.x > 0))
  }

  test("a whole-frame plan measures degrees from the screen's centre and has no window") {
    val whole = get(
      CoordinateProvenance.of(
        plan(StudyGeometry.WholeFrame(get(Grid.over(screen, 64, 36)))),
        input,
        None
      )
    )
    val focus = get(whole.fixation(retrieval, at(3))).trail
    assertEquals(focus.window, None)
    assertEquals(focus.placement, MapPlacement.InMap)
    assertEquals(get(whole.fixation(retrieval, at(2))).trail.placement, MapPlacement.InMap)
    val reference = get(whole.angular.toRight("no degrees"))
    assertEquals((reference.measured, reference.origin), (FrameId("screen"), Pt[Px](960, 540)))
    assertEqualsDouble(get(focus.angular.toRight("none")).position.x, 188.0 / 35.0, 1e-12)
  }

  test("a plan without units per degree has no degrees") {
    val plain = get(CoordinateProvenance.of(plan(windowed, angular = None), input, None))
    assertEquals(plain.angular, None)
    assertEquals(get(plain.fixation(retrieval, at(3))).trail.angular, None)
    assertEquals(plain.records.total, 0)
    assertEquals(plain.records.source, None)
    assertEquals(
      get(plain.records.first(get(PageSize.of(10)))),
      RecordPage[StudyKey, Px](Vector.empty, 0, None)
    )
  }

  test("refusals name their operands") {
    val other = StudyInput(Trials(Vector(Trial(encoding, (), path(encoding, screen, (1, 1))))))
    val mismatch = CoordinateProvenance.of(plan(windowed), other, None)
    assertEquals(
      mismatch.map(_ => ()),
      Left(ProvenanceError.InputMismatch(input.reference.digest, other.reference.digest))
    )
    val unknown = StudyKey("P99", "x", "recall")
    assertEquals(
      provenance.fixation(unknown, at(0)).map(_ => ()),
      Left(ProvenanceError.UnknownTrial(unknown))
    )
    assertEquals(
      provenance.fixation(encoding, at(1)).map(_ => ()),
      Left(ProvenanceError.FixationOutOfRange(encoding, at(1), 1))
    )
    val twice = StudyInput(
      Trials(
        Vector(
          Trial(encoding, (), path(encoding, screen, (1, 1))),
          Trial(encoding, (), path(encoding, screen, (2, 2)))
        )
      )
    )
    val ambiguous = get(CoordinateProvenance.of(plan(windowed, source = twice), twice, None))
    assertEquals(
      ambiguous.fixation(encoding, at(0)).map(_ => ()),
      Left(ProvenanceError.AmbiguousTrial(encoding, 2))
    )
    val elsewhere = get(Frame.screen("other-screen", 1920, 1080))
    val moved     =
      StudyInput(Trials(Vector(Trial(encoding, (), path(encoding, elsewhere, (1, 1))))))
    val framed = get(CoordinateProvenance.of(plan(windowed, source = moved), moved, None))
    assert(
      framed.fixation(encoding, at(0)) match
        case Left(ProvenanceError.TrialFrame(`encoding`, _)) => true
        case _                                               => false
    )
    val messages = Vector(
      ProvenanceError.InputMismatch("aa", "bb")   -> "input aa; the supplied input is bb",
      ProvenanceError.UnknownTrial(unknown)       -> "no trial StudyKey(P99",
      ProvenanceError.AmbiguousTrial(encoding, 2) -> "has 2 trials",
      ProvenanceError
        .FixationOutOfRange(encoding, at(4), 1) -> "there is none at scanpath position 4",
      ProvenanceError.CorrectionConflict(encoding, 0, 1)        -> "rules 0 and 1",
      ProvenanceError.Unmappable(encoding, at(2), FrameId("f")) -> "position 2",
      ProvenanceError.PageStart(get(DataRecord.of(9)), 3) -> "data record 9; 3 are listed",
      ProvenanceError.Identity(RecordIdentityError.HeaderRecord(CsvRecord.header)) ->
        "CSV record 1 is the header",
      ProvenanceError.Angular(GeometryError.FrameMismatch(FrameId("a"), FrameId("b"))) ->
        "degrees frame"
    )
    messages.foreach((error, text) => assert(error.message.contains(text), error.message))
    assert(
      framed
        .fixation(encoding, at(0))
        .left
        .exists(_.message.contains("not in the admission frame"))
    )
  }
