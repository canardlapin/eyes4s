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

package eyes4s.studio.core.fixture

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.studio.core.backend.{AnalysisRevision, Phase, TrialKey as StudioKey}
import eyes4s.studio.core.document.ColumnRole
import munit.CatsEffectSuite

/** The fake backend's fixation placements (protocol 1.6, S6.2) are eyes4s's
  * own: every admitted trial of fixtures/studio-golden, admitted by eyes4s-io
  * under r3's recorded mapping and placed by eyes4s-plan's
  * `CoordinateProvenance` under a plan made as rev 4's recipe states it
  * (the image window `[448, 1472) × [156, 924)`, off-window fixations
  * excluded, every initial fixation kept, 35 px/°), gets exactly the fake's
  * placement and admitted centre for every fixation.
  */
class FakePlacementJvmSuite extends CatsEffectSuite:

  override val munitIOTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(180, "s")

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val rev4 = AnalysisRevision(4)

  private val screen = get(Frame.screen("screen", 1920, 1080))
  private val window =
    get(Subframe.of(screen, FrameId("image"), get(Bounds.of[Px](448, 156, 1472, 924))))

  private val mapping = get(StoryMoments.inventory(occurrence = true))
  private def name(role: ColumnRole): Option[String] = mapping.column(role).map(_.value)

  private val trialColumns = get(
    TrialColumns.of(
      name(ColumnRole.Participant).get,
      name(ColumnRole.Phase).get,
      name(ColumnRole.Trial).get,
      name(ColumnRole.Occurrence)
    )
  )

  private def admitted(fixations: String) = get(
    FixationCsv.admitInventory(
      fixations,
      get(
        FixationTable.of(
          trialColumns,
          "ordinal",
          "x",
          "y",
          TimeColumns("onset_ms", "duration_ms", TimestampUnit.Milliseconds),
          SampleCountRule.PositiveColumn("sample_count")
        )
      ),
      get(
        TrialInventory.read(
          GoldenCsv.trials,
          get(TrialInventoryColumns.of(trialColumns, name(ColumnRole.Item), Vector.empty))
        )
      ),
      screen
    )
  )

  private val input = StudyInput(admitted(GoldenCsv.fixations).fixations.accepted)

  private def planFor(input: StudyInput[TrialKey, Px]) = get(
    StudyPlan.configure(
      input.reference,
      TrialKey.layout(TrialKeyDefinitions.trialLayout),
      get(
        StudyGeometry
          .windowed(window, get(Grid.over(window.frame, 64, 48)), OffWindowPolicy.Exclude)
      ),
      "Retrieval",
      "Encoding",
      Weight.Duration,
      Vector(StudyScale.Native(StudyEstimate.Binned())),
      Some(get(LinearAngularScale.of(screen, 35.0))),
      FailurePolicy.RequireAll,
      StudyMethod.cosine[Px](DefinitionId.cosine),
      (),
      initialFixations = InitialFixationPolicy.keepAll[Px]
    )
  )

  private val provenance = get(CoordinateProvenance.of(planFor(input), input, None))

  private def studioKey(k: TrialKey): StudioKey =
    StudioKey(k.participant, Phase(k.phase), k.trial, k.occurrence.value)

  test("every fixation of every admitted trial: the fake's placement is eyes4s's") {
    val keys = input.trials.rows.map(_.key)
    assertEquals(keys.size, 937)
    FakeStudyBackend.create[IO](StoryMoment.T2).flatMap { fake =>
      keys
        .traverse { k =>
          fake.trialFixations(rev4, studioKey(k)).map { served =>
            val view  = served.fold(e => fail(s"$k: ${e.message}"), identity)
            val pairs = view.fixations.zipWithIndex.map { (f, i) =>
              val eyes4s = get(provenance.fixation(k, get(ScanpathPosition.of(i)))).trail
              (
                (f.placement, f.screenX, f.screenY),
                (eyes4s.placement, eyes4s.admitted.position.x, eyes4s.admitted.position.y)
              )
            }
            // The scanpath has no fixation past the fake's last.
            val ends = provenance
              .fixation(k, get(ScanpathPosition.of(view.fixations.size)))
              .isLeft
            (pairs, ends)
          }
        }
        .map { results =>
          assert(results.forall(_._2), "a scanpath longer than the fake's fixations")
          val all      = results.flatMap(_._1)
          val disagree = all.filter((fake, core) => fake != core)
          val outside  =
            all.count(_._2._1 == MapPlacement.OutsideWindow(OffWindowPolicy.Exclude))
          assertEquals(disagree.take(5), Vector.empty)
          // The golden fixture's admitted trials have records outside the window.
          assert(outside > 0, outside)
        }
    }
  }

  test("off the screen and on the window's far edges, the fake places as eyes4s does") {
    // The golden fixations with P01 enc_01's records moved: off the screen to
    // the left and bottom, onto the screen's and the window's excluded right
    // and bottom edges, and just inside.
    val moved = Vector(
      (-5.0, 100.0),
      (900.0, 1080.0),
      (1920.0, 500.0),
      (1472.0, 500.0),
      (900.0, 924.0),
      (1471.9, 923.9),
      (448.0, 156.0)
    )
    val lines  = GoldenCsv.fixations.linesIterator.toVector
    var next   = 0
    val edited = lines.head +: lines.tail.map { line =>
      val f = line.split(",", -1)
      if f(0) == "P01" && f(2) == "enc_01" && next < moved.size then
        val (x, y) = moved(next)
        next += 1
        f.updated(5, x.toString).updated(6, y.toString).mkString(",")
      else line
    }
    assertEquals(next, moved.size)
    val editedInput = StudyInput(admitted(edited.mkString("", "\n", "\n")).fixations.accepted)
    val editedProv  = get(CoordinateProvenance.of(planFor(editedInput), editedInput, None))
    val key         = editedInput.trials.rows
      .map(_.key)
      .find(k => k.participant == "P01" && k.trial == "enc_01")
      .getOrElse(fail("P01 enc_01 not admitted"))
    val places = Iterator
      .from(0)
      .map(i => editedProv.fixation(key, get(ScanpathPosition.of(i))))
      .takeWhile(_.isRight)
      .map(r => get(r).trail)
      .toVector
    assert(places.size >= moved.size, places.size)
    val fake = places.map(t =>
      FakeTrialViews.placement(
        screen,
        window,
        OffWindowPolicy.Exclude,
        t.admitted.position.x,
        t.admitted.position.y
      )
    )
    assertEquals(fake, places.map(_.placement))
    assert(places.exists(_.placement == MapPlacement.OutsideScreen), places.map(_.placement))
    assert(
      places.exists(_.placement == MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)),
      places.map(_.placement)
    )
  }
