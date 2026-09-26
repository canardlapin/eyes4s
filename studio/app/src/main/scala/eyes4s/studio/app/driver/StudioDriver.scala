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

package eyes4s.studio.app.driver

import cats.{Applicative, Monad}
import cats.syntax.all.*
import eyes4s.studio.app.{AppEffect, AppModel, Intent, Notice, PlatformDialog}
import eyes4s.studio.app.keys.{CommandId, CommandRegistry}
import eyes4s.studio.app.text.{MessageId, Messages}
import eyes4s.studio.app.vm.{AppBarVM, ContextStripVM, Shell, ShellVM, StatusBarVM}
import eyes4s.studio.core.backend.AdmissionSummary
import eyes4s.studio.core.bundle.{
  BundleError,
  InputEntry,
  ProjectBundle,
  SharingOptions,
  StoreError
}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.backend.BackendError
import eyes4s.studio.core.execution.{ExecutionError, ExecutionEvent}
import eyes4s.studio.core.headless.{HeadlessError, StudioServices}
import eyes4s.studio.core.navigation.NavigationError

/** What the driver did outside the model, kept in order as a record. */
enum DriverRecord derives CanEqual:
  /** An effect performed on the services. */
  case Performed(effect: AppEffect)

  /** The admission summary a `RequestAdmission` returned. */
  case Admission(summary: AdmissionSummary)

  /** A dialog only a platform can show; headless, none is shown. */
  case Dialog(dialog: PlatformDialog)

  /** A service refused an effect; the model was told nothing. */
  case Refused(effect: AppEffect, error: ServiceError)

  /** A scripted step that a later ticket implements. */
  case Stubbed(step: String, reason: String)

/** A studio service's refusal, by the service that refused. */
enum ServiceError derives CanEqual:
  case Backend(error: BackendError)
  case Execution(error: ExecutionError)
  case Navigation(error: NavigationError)
  case Headless(error: HeadlessError)

  def message: String = this match
    case Backend(e)    => e.message
    case Execution(e)  => e.message
    case Navigation(e) => e.message
    case Headless(e)   => e.message

/** Why a driver step failed. Every case names its operands. */
enum DriverError derives CanEqual:
  /** The model refused the step's intent and raised `notice`. */
  case Refused(step: String, notice: Notice)

  /** No registered command has this id text. */
  case UnknownCommand(name: String)

  /** A service refused what the step asked of it. */
  case Service(step: String, error: ServiceError)
  case Bundle(error: BundleError)

  /** A scenario's check did not hold. */
  case Expectation(step: String, expected: String, actual: String)

  def message: String = this match
    case Refused(step, notice)               => s"$step was refused: ${notice.message}"
    case UnknownCommand(name)                => s"No registered command is called '$name'."
    case Service(step, error)                => s"$step: ${error.message}"
    case Bundle(error)                       => error.message
    case Expectation(step, expected, actual) =>
      s"$step: expected $expected, found $actual."

/** A headless driver over the app (ticket S3.6): it dispatches intents,
  * document commands and registered commands to [[AppModel.update]], reads
  * the view-models a shell would render, and queues the effects the update
  * asks for. Performing them is [[StudioDriver.settle]]'s, on a
  * [[StudioServices]]; the driver itself is a pure value, so the same
  * scenario can validate any future shell.
  */
final case class StudioDriver private (
    model: AppModel,
    queued: Vector[AppEffect],
    records: Vector[DriverRecord],
    messages: Messages
) derives CanEqual:

  /** Apply one intent; its effects are queued. */
  def dispatch(intent: Intent): StudioDriver =
    val (next, effects) = AppModel.update(model, intent)
    copy(model = next, queued = queued ++ effects)

  def dispatchAll(intents: Iterable[Intent]): StudioDriver =
    intents.foldLeft(this)(_.dispatch(_))

  /** Apply one intent that must not be refused: a new notice fails the step. */
  def perform(step: String, intent: Intent): Either[DriverError, StudioDriver] =
    val next = dispatch(intent)
    next.model.notice match
      case Some(notice) if !model.notice.contains(notice) =>
        Left(DriverError.Refused(step, notice))
      case _ => Right(next)

  /** Dispatch a document command (S2.2). */
  def command(command: Command): Either[DriverError, StudioDriver] =
    perform(command.name, Intent.Dispatch(command))

  /** Invoke a registered command (S1.0) through the same path as the menu
    * and keymap; a disabled command is refused with its notice.
    */
  def invoke(id: CommandId): Either[DriverError, StudioDriver] =
    perform(id.value, Intent.Invoke(id))

  /** Invoke a registered command by its id text ("navigate.back"). */
  def invoke(name: String): Either[DriverError, StudioDriver] =
    CommandId.parse(name).toRight(DriverError.UnknownCommand(name)).flatMap(invoke)

  /** Whether a registered command is enabled now. */
  def enabled(id: CommandId): Boolean = CommandRegistry.find(id).exists(_.enabled(model))

  def record(entry: DriverRecord): StudioDriver = copy(records = records :+ entry)

  /** Open another project in the same session: the model is replaced, the
    * queue dropped and the record kept.
    */
  def openProject(model: AppModel): StudioDriver = copy(model = model, queued = Vector.empty)

  /** A scripted step a later ticket implements, kept visible in the record. */
  def stub(step: String, reason: String): StudioDriver =
    record(DriverRecord.Stubbed(step, reason))

  /** Feed execution events, in order, as the shell does: each as an intent
    * (the update records a settled job's run outcome itself).
    */
  def feed(events: Iterable[ExecutionEvent]): StudioDriver =
    dispatchAll(events.map(Intent.Execution(_)))

  // --- View-models ------------------------------------------------------------

  def shell: ShellVM                       = Shell.project(model, messages)
  def appBar: AppBarVM                     = Shell.appBar(model, messages)
  def context: ContextStripVM              = Shell.context(model, messages)
  def status: StatusBarVM                  = Shell.status(model, messages)
  def crumbs: Vector[String]               = context.trail.map(_.label)
  def admissions: Vector[AdmissionSummary] =
    records.collect { case DriverRecord.Admission(s) => s }

  /** The trail as one line, crumbs joined by the path separator. */
  def trailText: String = crumbs.mkString(messages(MessageId.PathSeparator))

  /** Close the project and reopen it from its bundle: the document is
    * encoded as bundle parts and rebuilt from them (S2.3), and the app opens
    * it afresh with the same project name and item labels. Every source must
    * be one of `inputs`. Effects still queued are dropped with the session.
    */
  def reopen(inputs: Vector[InputEntry]): Either[DriverError, StudioDriver] =
    (for
      encoded <- ProjectBundle.encode(model.document, SharingOptions.complete, inputs)
      parts = encoded.parts.toMap
      assembled <- ProjectBundle.assemble(
        encoded.manifest,
        path =>
          parts
            .get(path)
            .toRight(
              BundleError.Store(
                StoreError.Missing(path)
              )
            )
      )
    yield StudioDriver(
      AppModel
        .update(
          AppModel.open(assembled.document, model.project),
          Intent.ItemsLoaded(model.items)
        )
        ._1,
      Vector.empty,
      records,
      messages
    )).leftMap(DriverError.Bundle(_))

object StudioDriver:

  /** A driver on `model`, with nothing queued. */
  def open(model: AppModel, messages: Messages = Messages.english): StudioDriver =
    StudioDriver(model, Vector.empty, Vector.empty, messages)

  /** Perform every queued effect on `services`, in order, and feed back the
    * events the services published meanwhile, until nothing is queued.
    * Service refusals are recorded, not raised: the app's own model decides
    * what a refusal means.
    */
  def settle[F[_]: Monad](driver: StudioDriver, services: StudioServices[F]): F[StudioDriver] =
    Monad[F].tailRecM(driver) { d =>
      if d.queued.isEmpty then
        services.events.map { events =>
          if events.isEmpty then Right(d) else Left(d.feed(events))
        }
      else
        val effect = d.queued.head
        val rest   = d.copy(queued = d.queued.tail)
        perform(rest, services, effect).map(Left(_))
    }

  private def perform[F[_]: Monad](
      d: StudioDriver,
      services: StudioServices[F],
      effect: AppEffect
  ): F[StudioDriver] =
    val done = d.record(DriverRecord.Performed(effect))
    effect match
      case AppEffect.Execution(e) =>
        services.execute(e).map {
          case Left(error) =>
            d.record(DriverRecord.Refused(effect, ServiceError.Execution(error)))
          case Right(()) => done
        }
      case AppEffect.RequestAdmission(dataset, _) =>
        services.admission(dataset).map {
          case Left(error) =>
            d.record(DriverRecord.Refused(effect, ServiceError.Backend(error)))
          case Right(summary) => done.record(DriverRecord.Admission(summary))
        }
      case AppEffect.OpenDialog(dialog) =>
        Applicative[F].pure(done.record(DriverRecord.Dialog(dialog)))
      // Saving belongs to S2.4, and the rest act on a shell's own window and
      // dock; headless, each is only recorded.
      case AppEffect.Persist | AppEffect.Journal(_) | AppEffect.RevealProject |
          AppEffect.ResetLayouts(_) | AppEffect.Dock(_) =>
        Applicative[F].pure(done)

/** One scripted step: a name and what it does to the driver. */
final case class Step[F[_]](
    name: String,
    run: StudioDriver => F[Either[DriverError, StudioDriver]]
)

object Step:
  /** A step that needs no service. */
  def pure[F[_]: Applicative](name: String)(
      f: StudioDriver => Either[DriverError, StudioDriver]
  ): Step[F] = Step(name, d => Applicative[F].pure(f(d)))

  def intent[F[_]: Applicative](name: String, intent: Intent): Step[F] =
    pure(name)(_.perform(name, intent))

  def command[F[_]: Applicative](command: Command): Step[F] =
    pure(command.name)(_.command(command))

  def invoke[F[_]: Applicative](id: CommandId): Step[F] = pure(id.value)(_.invoke(id))

  /** Perform what is queued on `services` (see [[StudioDriver.settle]]). */
  def settle[F[_]: Monad](services: StudioServices[F]): Step[F] =
    Step("settle", d => StudioDriver.settle(d, services).map(Right(_)))

  /** A check on the driver that changes nothing. */
  def check[F[_]: Applicative](name: String)(
      p: StudioDriver => Either[DriverError, Unit]
  ): Step[F] = pure(name)(d => p(d).as(d))

  /** A step a later ticket implements, recorded as such. */
  def stub[F[_]: Applicative](name: String, reason: String): Step[F] =
    pure(name)(d => Right(d.stub(name, reason)))

/** A scenario failed at `step` (its position and name). */
final case class ScenarioFailure(index: Int, step: String, error: DriverError) derives CanEqual:
  def message: String = s"Step ${index + 1} ($step): ${error.message}"

/** Steps composed in order (ticket S3.6); a scenario stops at its first
  * failure.
  */
final case class Scenario[F[_]](steps: Vector[Step[F]]):
  def ++(other: Scenario[F]): Scenario[F] = Scenario(steps ++ other.steps)
  def :+(step: Step[F]): Scenario[F]      = Scenario(steps :+ step)

  def run(driver: StudioDriver)(using F: Monad[F]): F[Either[ScenarioFailure, StudioDriver]] =
    steps.zipWithIndex.foldLeft(F.pure(driver.asRight[ScenarioFailure])) {
      case (acc, (step, i)) =>
        acc.flatMap {
          case Left(failure) => F.pure(Left(failure))
          case Right(d)      => step.run(d).map(_.leftMap(ScenarioFailure(i, step.name, _)))
        }
    }

object Scenario:
  def of[F[_]](steps: Step[F]*): Scenario[F] = Scenario(steps.toVector)
