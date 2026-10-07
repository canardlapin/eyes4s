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

package eyes4s.studio.desktop.explore

import cats.effect.IO
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.app.explore.TimelineIntent
import eyes4s.studio.core.document.*
import eyes4s.studio.core.real.{DatasetSources, TrialDurationNativeFixture}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.viz.plot.DataPoint
import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

/** Uses actual CSV through the real backend, with declared trial time after the fixation tail. */
class TrialExtentFxSuite extends ShellFxSuite:
  override val munitTimeout: Duration          = Duration(180, "s")
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  private def model(cell: Option[String]): AppModel =
    val original = ok(StoryMoments.t2)
    val spec     = TrialDurationNativeFixture.spec(cell)
    val document = ok(
      StudioDocument.of(
        original.datasets.map(d => if d.id == spec.id then spec else d),
        original.analyses.map(a =>
          if a.id == TrialDurationNativeFixture.revision then
            a.copy(recipe = a.recipe.copy(input = None))
          else a
        ),
        original.draft,
        original.runs,
        original.reporting,
        original.figures,
        original.presentation,
        original.jobs
      )
    )
    val opened    = AppModel.open(document, None)
    val navigated = AppModel
      .update(
        opened,
        eyes4s.studio.app.Intent.Navigate(
          Location(
            Perspective.Explore,
            Vector(
              Place.At(StudioRef.Participant("P17")),
              Place.At(StudioRef.Trial(TrialDurationNativeFixture.trial)),
              Place.At(
                StudioRef.Fixation(TrialDurationNativeFixture.trial, ok(FixationIndex.of(1)))
              )
            )
          )
        )
      )
      ._1
    val fixation: StudioRef.Fixation =
      StudioRef.Fixation(TrialDurationNativeFixture.trial, ok(FixationIndex.of(1)))
    val selected = AppModel
      .update(navigated, StoryModels.select(navigated, "explore.trial-view", fixation))
      ._1
    assertEquals(eyes4s.studio.app.explore.FixationInspector.focusOf(selected), Some(fixation))
    selected

  private def sources(cell: Option[String]): DatasetSources[IO] = new DatasetSources[IO]:
    def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
      val raw = source.role match
        case SourceRole.Fixations => TrialDurationNativeFixture.fixes
        case SourceRole.Trials    => TrialDurationNativeFixture.trials(cell.getOrElse(""))
      IO.pure(Some(IArray.from(raw.getBytes(UTF_8))))
    def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] = IO.pure(
      Some(
        ok(
          AssetRegistry.of(
            dataset.id,
            dataset.sources.trials.get.bytes,
            dataset.geometry.screen,
            Vector.empty
          )
        )
      )
    )

  for cell <- Vector(Some("5"), None, Some("")) do
    fxStage.test(s"native CSV trial extent $cell reaches inspector and timeline honestly") {
      fx =>
        val w = boot(fx, model(cell), StoryMoment.T2, nativeSources = Some(sources(cell)))
        val expected = if cell.contains("5") then "5.0 s" else "Not declared"
        eventually(
          fx,
          s"native inspector extent and timeline: selected ${w.runtime.model.selection.selected}, inspector ${w.inspector.vm.status}, timeline ${w.timeline.vm.note}"
        ) {
          w.inspector.lines.contains("Extent" -> expected) && w.timeline.vm.enabled
        }
        val timeline = w.timeline.vm
        assertEquals(timeline.source.map(_.rows.size), Some(1))
        assertEquals(
          timeline.source.flatMap(
            _.number(0, ok(eyes4s.studio.app.plot.TimelineColumns.standard).onset)
          ),
          Some(100.0)
        )
        if cell.contains("5") then
          assertEquals(timeline.extentNote, Some("Trial extent 5.00 s"))
          eventually(fx, "declared timeline domain is drawn")(
            w.timeline.twin.input.targets.isDefined
          )
          val inside = runOnFx {
            val targets = w.timeline.twin.input.targets.getOrElse(fail("no timeline transform"))
            val end     = targets.transform.deviceToCanvas(
              ok(targets.transform.dataToDevice(DataPoint(5000, 0)))
            )
            end.x <= targets.transform.surface.logicalWidth
          }
          assert(inside, "served trial end must be visible before playback")
          runOnFx {
            w.timeline.dispatch(TimelineIntent.Play)
            w.timeline.dispatch(TimelineIntent.Tick(1000))
            w.timeline.dispatch(TimelineIntent.Pause)
          }
          assert(runOnFx(w.timeline.timeline.playheadMs >= 1000))
          runOnFx {
            w.timeline.dispatch(TimelineIntent.Play)
            w.timeline.dispatch(TimelineIntent.Tick(10000))
          }
          assertEquals(runOnFx(w.timeline.timeline.playheadMs), 5000.0)
          assert(!runOnFx(w.timeline.timeline.playing))
          assert(runOnFx(w.timeline.note.getText.contains("Trial extent 5.00 s")))
        else
          assert(timeline.extentNote.exists(_.contains("not declared")))
          runOnFx {
            w.timeline.dispatch(TimelineIntent.Play)
            w.timeline.dispatch(TimelineIntent.Tick(10000))
          }
          assertEquals(runOnFx(w.timeline.timeline.playheadMs), 400.0)
    }
