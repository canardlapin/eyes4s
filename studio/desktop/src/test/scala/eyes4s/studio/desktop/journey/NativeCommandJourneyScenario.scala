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

package eyes4s.studio.desktop.journey

import cats.effect.{IO, Resource}
import eyes4s.studio.app.{AppEffect, AppModel, Intent}
import eyes4s.studio.app.driver.DriverRecord
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.execution.{ExecutionEffect, JobPhase}
import eyes4s.studio.core.preset.InitialRecipe
import eyes4s.studio.core.preview.PreviewEvent
import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}

/** A separate owned command/effect run, reusable as the FX scientific oracle. */
object NativeCommandJourneyScenario:
  import NativeCommandJourneyFixture.{run as requestedRun, *}
  private given ExecutionContext                         = ExecutionContext.global
  private def future[A](effect: => Future[A]): IO[A]     = IO.fromFuture(IO(effect))
  private def checkEqual[A](found: A, expected: A): Unit =
    assert(found == expected, s"Native command path: expected $expected, found $found")

  def run(inputs: Inputs, path: Path): IO[NativeCommandJourneyReadback.Output] =
    val empty = get(AppModel.newProject)
    NativeJourneyProject.open(path, empty.document, inputs).use { port =>
      Resource
        .make(future(NativeCommandJourney.open(empty, port)))(j => future(j.close()))
        .use { journey =>
          for
            _ <- future(journey.intent(Intent.CheckInputs))
            _ <- future(journey.command(inputs.importCommand))
            _ <- future(journey.command(Command.VerifyDataset(dataset)))
            verifying = journey.model.document.dataset(dataset).get.decision match
              case AdmissionDecision.Verifying(content) => content
              case other                                =>
                throw new AssertionError(s"Dataset did not enter native verification: $other")
            _ = checkEqual(
              journey.records.collect { case DriverRecord.Admission(summary) => summary }.size,
              1
            )
            _ <- future(
              journey.command(
                Command.Admit(
                  dataset,
                  verifying,
                  Some(eyes4s.plan.AdmissionDecision.ReviewExclusions),
                  CoreBinding.unbound,
                  CoreBinding.unbound
                )
              )
            )
            seed = get(
              InitialRecipe
                .of(journey.model.document.dataset(dataset).get, Preset.EncodingRetrieval)
            )
            _ = checkEqual(seed._1, recipe)
            _ <- future(journey.intent(Intent.ChoosePreset(Preset.EncodingRetrieval)))
            _ = checkEqual(journey.model.document.analyses, Vector.empty)
            _ = checkEqual(journey.model.document.draft.flatMap(_.savedBase), None)
            ready <- future(journey.prepare())
            _ = assert(ready.recipe.contains(recipe))
            _ = checkEqual(journey.resolved.rows.size, 480)
            _ = assert(journey.counted.exists(_.isInstanceOf[PreviewEvent.CountingWork]))
            _ <- future(journey.command(Command.SaveAndRun(None)))
            _ <- future(journey.awaitRun(requestedRun))
            _ <- future(journey.awaitArtifacts(requestedRun))
            _ = checkEqual(
              journey.model.document.run(requestedRun).map(_.state),
              Some(RunLifecycle.Completed)
            )
            _ = assert(journey.records.exists {
              case DriverRecord.Performed(
                    AppEffect.Execution(ExecutionEffect.SubmitPreview(receipt, Some(requested)))
                  ) =>
                receipt == ready && requested == requestedRun
              case _ => false
            })
            _ = assert(journey.events.exists {
              case eyes4s.studio.core.execution.ExecutionEvent.Changed(job) =>
                job.run == requestedRun && job.phase.isInstanceOf[JobPhase.Succeeded]
              case _ => false
            })
            _ <- future(journey.command(Command.ShowRun(Some(requestedRun))))
            _ <- future(journey.command(Command.PutReporting(reporting)))
            _ <- future(
              journey.command(Command.CreateFigure(requestedRun, reporting.id, panels))
            )
            _      <- future(journey.command(Command.SetPerspective(Perspective.Figures)))
            output <- NativeCommandJourneyReadback.capture(
              journey.model.document,
              NativeCommandJourneyReadback.Port.from(journey.session)
            )
            direct <- inputs.direct(output.document)
            _ = checkEqual(
              ready.stamp.plan,
              CoreBinding.Bound(
                get(
                  eyes4s.codec.CanonicalDigest.parse[StudyPlanArtifact](
                    get(direct.plans.codec.digest(direct.plan)).sha256.hex
                  )
                )
              )
            )
            _ = checkEqual(
              get(journey.model.document.latestAnalysis.toRight("no native analysis")).plan,
              ready.stamp.plan
            )
            _ = checkEqual(
              get(
                journey.model.document.latestAnalysis.toRight("no native analysis")
              ).recipe.input,
              Some(SemanticIdentity.fromCore(direct.admitted.evidence.source.records))
            )
            _ = checkEqual(
              get(journey.model.document.run(requestedRun).toRight("no native run")).archive
                .isInstanceOf[CoreBinding.Bound[?]],
              true
            )
            _ = checkEqual(output.source.rows.head.ref, output.sourceRef)
            _ = assert(output.source.rows.head.screen.nonEmpty)
            storedDocument <- port.session.document
            _ = checkEqual(storedDocument, output.document)
            _ = assert(!journey.records.exists(_.isInstanceOf[DriverRecord.Stubbed]))
          yield output
        }
    }
