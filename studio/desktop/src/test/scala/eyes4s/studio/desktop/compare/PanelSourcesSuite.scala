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

package eyes4s.studio.desktop.compare

import eyes4s.plan.MapPlacement
import eyes4s.studio.app.compare.{ContentError, TrialContent}
import eyes4s.studio.app.explore.{BackendAnswer, DisplaySource}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import eyes4s.studio.desktop.explore.TrialViewInputs
import eyes4s.studio.desktop.trial.GoldenTrials

class PanelSourcesSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val revision                          = StoryMoments.rev4
  private lazy val spec                         = get(StoryMoments.t2).datasets.last
  private val trial                             = GoldenTrials.key("P17", "ret_07")
  private lazy val native                       = get(
    TrialFixations.of(
      revision,
      spec.id,
      trial,
      Vector(
        get(
          AdmittedFixation.of(
            StudioRef.Fixation(trial, get(FixationIndex.of(1))),
            1,
            601.25,
            302.5,
            3000000000.125,
            0.375,
            MapPlacement.InWindow
          )
        )
      )
    )
  )

  private class Inputs extends TrialViewInputs:
    var displaysAnswer: Either[String, DisplaySource] = Right(
      DisplaySource.Served(GoldenTrials.registry)
    )
    var fixationsAnswer: Either[String, BackendAnswer[TrialFixations]] = Right(
      BackendAnswer.Answered(native)
    )
    var reads   = Vector.empty[(AnalysisRevision, TrialKey)]
    var delayed = false
    var pending = Option.empty[Either[String, BackendAnswer[TrialFixations]] => Unit]
    def displays(
        dataset: DatasetRevisionSpec,
        done: Either[String, DisplaySource] => Unit
    ): Unit =
      assertEquals(dataset, spec)
      done(displaysAnswer)
    def fixations(
        rev: AnalysisRevision,
        key: TrialKey,
        done: Either[String, BackendAnswer[TrialFixations]] => Unit
    ): Unit =
      reads :+= rev -> key
      if delayed then pending = Some(done) else done(fixationsAnswer)
    def preview(
        rev: AnalysisRevision,
        key: TrialKey,
        done: Either[String, BackendAnswer[TrialPreview]] => Unit
    ): Unit =
      fail(s"Unexpected preview read $rev/$key")

  private def source(inputs: Inputs) = PanelSources
    .of(inputs, rev => Option.when(rev == revision)(spec), PanelSources.notServed.stimuli)
    .content

  test(
    "native content waits for backend fixations and retains exact fractional timing and order"
  ) {
    val inputs = new Inputs
    inputs.delayed = true
    var answer = Option.empty[Either[ContentError, TrialContent]]
    source(inputs).content(revision, trial, a => answer = Some(a))
    assertEquals(answer, None)
    assertEquals(inputs.reads, Vector(revision -> trial))
    inputs.pending.get(inputs.fixationsAnswer)
    val content = get(answer.get)
    assertEquals(content.display, GoldenTrials.registry.display(trial).get)
    assertEquals(content.screen, spec.geometry.screen)
    assertEquals(content.fixations.map(_.index), native.fixations.map(_.ref.index))
    val fixation = content.fixations.head
    assertEquals(fixation.onsetMs, 3000000000.125)
    assertEquals(fixation.durationMs, 0.375)
    assertEquals(
      (fixation.screenX, fixation.screenY, fixation.placement),
      (601.25, 302.5, MapPlacement.InWindow)
    )
  }

  test("display and backend refusals remain explicit instead of fabricating a trial") {
    val inputs = new Inputs
    var answer = Option.empty[Either[ContentError, TrialContent]]
    inputs.displaysAnswer = Right(DisplaySource.NotServed)
    source(inputs).content(revision, trial, a => answer = Some(a))
    assertEquals(answer, Some(Left(ContentError.NotServed(trial))))
    assertEquals(inputs.reads, Vector.empty)
    inputs.displaysAnswer = Left("stored inventory failed verification")
    source(inputs).content(revision, trial, a => answer = Some(a))
    assertEquals(
      answer,
      Some(Left(ContentError.Unreadable(trial, "stored inventory failed verification")))
    )
    inputs.displaysAnswer = Right(DisplaySource.Served(GoldenTrials.registry))
    inputs.fixationsAnswer = Right(BackendAnswer.Refused("retained fixations unavailable"))
    source(inputs).content(revision, trial, a => answer = Some(a))
    assertEquals(
      answer,
      Some(Left(ContentError.Unreadable(trial, "retained fixations unavailable")))
    )
  }

  test("another revision's fixation reply and an undeclared revision are refused") {
    val inputs = new Inputs
    val other  = AnalysisRevision(999)
    inputs.fixationsAnswer = Right(
      BackendAnswer.Answered(get(TrialFixations.of(other, spec.id, trial, native.fixations)))
    )
    var answer = Option.empty[Either[ContentError, TrialContent]]
    source(inputs).content(revision, trial, a => answer = Some(a))
    assert(answer.exists(_.left.toOption.exists(_.message.contains(other.label))))
    inputs.reads = Vector.empty
    source(inputs).content(other, trial, a => answer = Some(a))
    assert(answer.exists(_.left.toOption.exists(_.message.contains(other.label))))
    assertEquals(inputs.reads, Vector.empty)
  }
