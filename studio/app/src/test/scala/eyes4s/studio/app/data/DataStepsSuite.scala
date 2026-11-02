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

package eyes4s.studio.app.data

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.geometry.{GeometryPanel, Loading}
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.core.bundle.{InputEntry, InputKind, InputStatus}
import eyes4s.studio.core.document.Perspective
import munit.FunSuite

/** Data's setup steps (overhaul v4): five steps in order, the trail's
  * section current, each opening its section of the shown dataset.
  */
class DataStepsSuite extends FunSuite:
  private val m  = StoryModels.t1Data
  private val vm = SourcesVM.of(SourcesPane.empty, m)

  private def sourceStep(
      model: AppModel,
      read: Loading[DisplaySource] = Loading.Ready(DisplaySource.NotServed)
  ): DataStepVM =
    val pane = SourcesPane.sync(SourcesPane.empty, model)._1.copy(registry = read)
    DataSteps.vm(model, SourcesVM.of(pane, model)).head

  private def present(model: AppModel): Vector[InputStatus] =
    model.document.timedDatasets.flatMap(_.sources.entries).distinct.map { s =>
      val entry = InputEntry
        .of(InputKind.Source(s.role), s.path.value.split('/').last, s.bytes, 1L)
        .fold(e => fail(e.message), identity)
      InputStatus.Present(entry)
    }

  private def checked(model: AppModel, statuses: Vector[InputStatus]): AppModel =
    val asked = AppModel.update(model, Intent.CheckInputs)._1
    AppModel.update(asked, Intent.InputsChecked(asked.checks.asked, statuses))._1

  test("five steps in the golden's order, the trail's Admission current") {
    val steps = DataSteps.vm(m, vm)
    assertEquals(
      steps.map(s => (s.number, s.title)),
      Vector(
        (1, "Sources"),
        (2, "Columns"),
        (3, "Trial identity"),
        (4, "Geometry"),
        (5, "Admission")
      )
    )
    assertEquals(steps.filter(_.current).map(_.section), Vector(DataSection.Admission))
    assertEquals(steps.last.state, StepState.Open)
  }

  test("a step opens its section of the dataset") {
    val d    = m.document.timedDatasets.last.id
    val step = DataSteps.vm(m, vm).find(_.section == DataSection.Geometry).get
    assertEquals(
      step.open,
      Intent.Navigate(
        Location(
          Perspective.Data,
          Vector(Place.Dataset(d), Place.DataView(DataSection.Geometry))
        )
      )
    )
  }

  test("all setup steps retain the Data perspective's selected older revision") {
    val initial = StoryModels.t2Figures
    val older   = initial.document.timedDatasets.head
    assertNotEquals(older.id, initial.document.timedDatasets.last.id)
    val selected = AppModel
      .update(
        initial,
        Intent.Navigate(
          Location(
            Perspective.Data,
            Vector(Place.Dataset(older.id), Place.DataView(DataSection.Sources))
          )
        )
      )
      ._1
    assertEquals(GeometryPanel.selected(selected).map(_.id), Some(older.id))
    val pane  = SourcesPane.sync(SourcesPane.empty, selected)._1
    val steps = DataSteps.vm(selected, SourcesVM.of(pane, selected))
    steps.foreach { step =>
      val target = AppModel.update(selected, step.open)._1
      assertEquals(GeometryPanel.selected(target).map(_.id), Some(older.id))
    }
    assert(steps.last.sub.contains(older.id.label))
    assertEquals(steps.last.state, StepState.Done)
  }

  test("missing and changed CSVs are repair problems even without missing images") {
    val missing = checked(m, Vector.empty)
    assertEquals(sourceStep(missing).state, StepState.Problems(2))
    assert(sourceStep(missing).sub.contains("missing"))
    val source  = m.document.timedDatasets.last.sources.entries.head
    val changed = present(m).map { status =>
      if status.entry.sha256 == source.bytes then
        InputStatus.Changed(status.entry, ByteDigest.sha256(IArray.from(Array[Byte](1, 2, 3))))
      else status
    }
    val damaged = sourceStep(checked(m, changed))
    assertEquals(damaged.state, StepState.Problems(1))
    assert(damaged.sub.contains("changed"))
  }

  test("pending checks stay open, failed checks report a problem, successful checks finish") {
    val pending = AppModel.update(m, Intent.CheckInputs)._1
    assertEquals(sourceStep(pending).state, StepState.Open)
    val failed = AppModel
      .update(
        pending,
        Intent.InputsCheckFailed(pending.checks.asked, "project store unavailable")
      )
      ._1
    assertEquals(sourceStep(failed).state, StepState.Problems(1))
    assert(sourceStep(failed).sub.contains("project store unavailable"))
    val ready = checked(m, present(m))
    assertEquals(sourceStep(ready).state, StepState.Done)
    val rechecking = AppModel.update(ready, Intent.CheckInputs)._1
    assertEquals(sourceStep(rechecking).state, StepState.Open)
  }

  test("pending and failed trial-display reads cannot mark Sources done") {
    assertEquals(sourceStep(m, Loading.Waiting).state, StepState.Open)
    val failed = sourceStep(m, Loading.Failed("trial inventory unreadable"))
    assertEquals(failed.state, StepState.Problems(1))
    assert(failed.sub.contains("trial inventory unreadable"))
  }
