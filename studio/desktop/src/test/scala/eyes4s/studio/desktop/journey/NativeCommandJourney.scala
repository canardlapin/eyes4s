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

import cats.instances.future.*
import cats.syntax.all.*
import eyes4s.studio.app.{AppEffect, AppModel, ClockTime, Intent}
import eyes4s.studio.app.analysis.{DesignEffect, DesignIntent, ResolvedDesign}
import eyes4s.studio.app.driver.{StudioDriver, DriverRecord}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.execution.{ExecutionEffect, ExecutionEvent}
import eyes4s.studio.core.headless.{NativeHeadlessSession, StudioServices}
import eyes4s.studio.core.preview.{PreviewEvent, PreviewReady}
import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, ProjectPort}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{ExecutionContext, Future, Promise}

/** The pure command driver with actual native service effects and bounded
  * resolved-design effects. Every published document is an immutable snapshot.
  */
final class NativeCommandJourney private (
    val session: NativeHeadlessSession,
    initial: AppModel,
    project: ProjectPort
)(using ExecutionContext):
  import NativeCommandJourneyFixture.get
  private val snapshot        = new AtomicReference(initial.document)
  private var driver          = StudioDriver.open(initial)
  private var design          = ResolvedDesign.empty
  private var executionEvents = Vector.empty[ExecutionEvent]
  private var previewEvents   = Vector.empty[PreviewEvent]
  private var performed       = 0
  session.bindDocument(() => snapshot.get())

  def model: AppModel                = driver.model
  def records: Vector[DriverRecord]  = driver.records
  def events: Vector[ExecutionEvent] = executionEvents
  def counted: Vector[PreviewEvent]  = previewEvents
  def resolved: ResolvedDesign       = design

  private val services: StudioServices[Future] = new StudioServices[Future]:
    def admission(dataset: DatasetRevision) = session.admission(dataset)
    def verify(
        dataset: DatasetRevision,
        content: eyes4s.codec.CanonicalDigest[eyes4s.studio.core.document.DatasetRevisionSpec]
    ) =
      session.verify(dataset, content)
    def execute(effect: ExecutionEffect) = session.execute(effect)
    def events                           = session.events.map { found =>
      executionEvents = executionEvents ++ found
      found
    }
    def navigator = session.navigator

  private def publish(next: StudioDriver): Unit =
    driver = next
    snapshot.set(next.model.document)

  private def callback[A](request: (Either[String, A] => Unit) => Unit): Future[A] =
    val result = Promise[A]()
    request { answer =>
      answer match
        case Left(reason) => result.tryFailure(new AssertionError(reason))
        case Right(value) => result.trySuccess(value)
      ()
    }
    result.future

  def settle(): Future[Unit] =
    snapshot.set(driver.model.document)
    StudioDriver.settle(driver, services).flatMap { next =>
      publish(next)
      val fresh = next.records.drop(performed)
      performed = next.records.size
      fresh
        .foldLeft(Future.successful(())) { (previous, record) =>
          previous.flatMap { _ =>
            record match
              case DriverRecord.Performed(AppEffect.Journal(entry)) =>
                project.journal(entry)
                Future.successful(())
              case DriverRecord.Performed(AppEffect.Persist(mark)) =>
                callback(project.save).map { _ =>
                  publish(driver.dispatch(Intent.Saved(get(ClockTime.of(12, 0)), mark)))
                }
              case DriverRecord.Performed(AppEffect.CheckInputs(round)) =>
                callback(project.checkInputs).map(statuses =>
                  publish(driver.dispatch(Intent.InputsChecked(round, statuses)))
                )
              case DriverRecord.Refused(effect, error) =>
                Future.failed(new AssertionError(s"Native effect $effect was refused: $error"))
              case _ => Future.successful(())
          }
        }
        .flatMap(_ => if driver.queued.nonEmpty then settle() else Future.successful(()))
    }
  def command(command: Command): Future[Unit] =
    publish(get(driver.command(command)))
    settle()
  def intent(intent: Intent): Future[Unit] =
    publish(get(driver.perform("native journey", intent)))
    settle()

  /** Feed backend-owned events through the same pure design update as FX. */
  private def designIntent(intent: DesignIntent): Future[Unit] =
    val (next, effects) = ResolvedDesign.update(design, intent)
    design = next
    designEffects(effects)
  private def designEffects(effects: Vector[DesignEffect]): Future[Unit] =
    effects.foldLeft(Future.successful(())) { (previous, effect) =>
      previous.flatMap { _ =>
        effect match
          case DesignEffect.App(action)                       => intent(action)
          case DesignEffect.StartPreview(g, revision, budget) =>
            session.previewCounting(revision, budget).flatMap(receive(g, _))
          case DesignEffect.ContinuePreview(g, id, budget) =>
            session.continuePreview(id, budget).flatMap(receive(g, _))
          case DesignEffect.ReadRows(g, revision, page) =>
            session
              .previewRows(revision, page)
              .flatMap(result =>
                designIntent(DesignIntent.RowsRead(g, result.leftMap(_.message)))
              )
      }
    }
  private def receive(
      generation: Long,
      events: Vector[Either[BackendError, PreviewEvent]]
  ): Future[Unit] =
    events
      .foldLeft(Future.successful(())) { (previous, event) =>
        previous.flatMap { _ =>
          event match
            case Left(error) =>
              designIntent(DesignIntent.PreviewRefused(generation, error.message))
            case Right(event) =>
              previewEvents = previewEvents :+ event
              designIntent(DesignIntent.Previewed(generation, event))
        }
      }
      .flatMap(_ => designIntent(DesignIntent.PageEnded(generation)))

  def prepare(): Future[PreviewReady] =
    intent(Intent.SwitchPerspective(Perspective.Analysis)).flatMap { _ =>
      val (next, effects) = ResolvedDesign.sync(design, model)
      design = next
      designEffects(effects).map(_ =>
        design.preview.receipt.getOrElse(
          throw new AssertionError(s"Native design was not ready: ${design.preview}")
        )
      )
    }

  def awaitRun(run: RunId): Future[Unit] =
    if model.jobs.ready.exists(_.run == run) then Future.successful(())
    else
      session
        .awaitEvent {
          case ExecutionEvent.Ready(ready) => ready.run == run
          case _                           => false
        }
        .flatMap { answer =>
          val found = get(answer)
          executionEvents = executionEvents ++ found
          publish(driver.feed(found))
          settle()
        }

  def close(): Future[Unit] = session.close

object NativeCommandJourney:
  def open(initial: AppModel, project: ProjectPort)(using
      ExecutionContext
  ): Future[NativeCommandJourney] =
    NativeHeadlessSession
      .open(initial.document, DatasetSourceHosts.stored(project))
      .map(new NativeCommandJourney(_, initial, project))
