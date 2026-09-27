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

import eyes4s.studio.app.{AppEffect, AppGen, AppModel, Intent, Notice, StoryModels}
import eyes4s.studio.app.keys.CommandRegistry
import eyes4s.studio.app.vm.JobsChipState
import eyes4s.studio.core.bundle.BundleSamples
import eyes4s.studio.core.command.{Command, CommandSamples, JournalEntry}
import eyes4s.studio.core.document.RunLifecycle
import eyes4s.studio.core.execution.{ExecutionEffect, ExecutionEvent, JobPhase}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import org.scalacheck.Prop.forAll
import org.scalacheck.Test

import scala.concurrent.{ExecutionContext, Future}

/** The headless driver (S3.6): every registered command and every document
  * command is callable from it, it adds nothing to the update, it performs
  * effects on the services, and scripted steps compose into scenarios.
  */
class StudioDriverSuite extends munit.ScalaCheckSuite:
  import StoryModels.*

  private given ExecutionContext = ExecutionContext.global

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(40)

  private def models: Vector[(String, AppModel)] = Vector(
    "first run"   -> firstRun,
    "t1 Data"     -> t1Data,
    "t2 Compare"  -> t2Compare,
    "t2 Explore"  -> t2Explore,
    "t2 Analysis" -> t2Analysis,
    "t3 summary"  -> t3Summary
  )

  test("every registered command is callable from the driver, by id and by name") {
    for
      (name, model) <- models
      command       <- CommandRegistry.all
    do
      val driver = StudioDriver.open(model)
      val byId   = driver.invoke(command.id)
      assertEquals(driver.invoke(command.id.value), byId, s"${command.id.value} on $name")
      byId match
        case Right(d) =>
          assert(command.enabled(model), s"${command.id.value} ran while disabled on $name")
          assertEquals(d.model, AppModel.update(model, Intent.Invoke(command.id))._1)
        case Left(DriverError.Refused(step, Notice.Unavailable(id, _))) =>
          assert(!command.enabled(model), s"${command.id.value} refused while enabled on $name")
          assertEquals((step, id), (command.id.value, command.id))
        case Left(other) => fail(s"${command.id.value} on $name: $other")
  }

  test("an unregistered command name is refused by name") {
    assertEquals(
      StudioDriver.open(t2Compare).invoke("perspective.nowhere"),
      Left(DriverError.UnknownCommand("perspective.nowhere"))
    )
  }

  test("every S2.2 document command reaches the reducer from the driver") {
    val accepted = for
      (label, command) <- CommandSamples.commands
      model            <- Vector(opened(t1), opened(t2))
    yield
      val driver   = StudioDriver.open(model)
      val expected = model.history.apply(command)
      driver.command(command) match
        case Right(d) =>
          assert(expected.isRight, s"$label accepted against the history")
          assertEquals((d.model, d.queued), AppModel.update(model, Intent.Dispatch(command)))
          label
        case Left(DriverError.Refused(step, Notice.Refused(entry, error))) =>
          assertEquals((step, entry), (command.name, JournalEntry.Apply(command)))
          assertEquals(expected.map(_ => ()), Left(error), label)
          ""
        case Left(other) => fail(s"$label: $other")
    // Each case is accepted on some story document, bar the removals' exact
    // inverses and the reporting commands the samples aim at other states.
    assert(accepted.count(_.nonEmpty) >= CommandSamples.commands.size / 2, accepted)
  }

  property("the driver adds nothing to the update: dispatching a session is AppModel.run") {
    forAll(AppGen.session(20)) { traces =>
      traces.headOption.foreach { first =>
        val driver = StudioDriver.open(first.before).dispatchAll(traces.map(_.intent))
        assertEquals(driver.model, traces.last.after)
        assertEquals(driver.queued, traces.flatMap(_.effects))
      }
    }
  }

  test("settle performs Save & run on the services and feeds the job back") {
    HeadlessSession.open(StoryMoment.T2).flatMap { session =>
      val start = StudioDriver.open(play(opened(t2), _ => Intent.ReviewDraft))
      val run   = for
        saved <- Future.fromTry(
          start
            .command(Command.SaveAndRun(None))
            .left
            .map(e => new AssertionError(e.message))
            .toTry
        )
        settled <- StudioDriver.settle(saved, session)
        events  <- session.awaitEvent {
          case ExecutionEvent.Changed(j) => j.run == StoryMoments.run8
          case _                         => false
        }
      yield
        val performed = settled.records.collect { case DriverRecord.Performed(e) => e }
        assert(
          performed.exists {
            case AppEffect.Execution(ExecutionEffect.Submit(stamp)) =>
              stamp.revision == StoryMoments.rev5
            case _ => false
          },
          performed
        )
        assert(
          performed.exists { case AppEffect.Persist(_) => true; case _ => false },
          performed
        )
        assert(performed.exists(_.isInstanceOf[AppEffect.Journal]), performed)
        assertEquals(settled.queued, Vector.empty)
        val fed = settled.feed(events.getOrElse(Vector.empty))
        assert(fed.model.jobs.jobs.exists(_.run == StoryMoments.run8), fed.model.jobs)
        assertNotEquals(fed.appBar.jobs.state, JobsChipState.Idle)
      run.transformWith(r => session.close.transform(_ => r))
    }
  }

  test("the driver feeds a job's end as an intent; the update records the outcome") {
    val m     = opened(t3)
    val stamp = run8Stamp
    val done  = eyes4s.studio.core.execution.ExecutionJob(
      StoryMoments.run8Job,
      StoryMoments.run8,
      stamp,
      JobPhase.Cancelled(None)
    )
    val fed = StudioDriver.open(m).feed(Vector(ExecutionEvent.Changed(done)))
    assertEquals(
      fed.model.document.run(StoryMoments.run8).map(_.state),
      Some(RunLifecycle.Cancelled(None))
    )
    // Fed again, the run is no longer running: nothing more is recorded.
    val again = fed.feed(Vector(ExecutionEvent.Changed(done)))
    assertEquals(again.model.document, fed.model.document)
    assertEquals(AppModel.outcomeOf(fed.model.document, ExecutionEvent.Changed(done)), None)
  }

  test("scenarios compose in order and stop at the first failure, naming it") {
    val back     = Step.intent[Future]("back", Intent.Back)
    val scenario = Scenario.of[Future](
      Step.intent[Future]("to the summary", Intent.OpenCrumb(0)),
      Step.check[Future]("on the summary")(d =>
        Either.cond(
          d.crumbs.size == 1,
          (),
          DriverError.Expectation("crumbs", "1", d.crumbs.toString)
        )
      ),
      Step.invoke[Future](CommandRegistry.showRun.id),
      back
    )
    scenario.run(StudioDriver.open(t2Compare)).map {
      case Left(failure) =>
        assertEquals((failure.index, failure.step), (2, "run.show"))
        assert(failure.message.startsWith("Step 3 (run.show)"), failure.message)
      case Right(d) =>
        fail(s"run.show has no ready run on t2, yet the scenario ended at ${d.crumbs}")
    }
  }

  test("close and reopen through the bundle keeps the document and drops the session") {
    val d = StudioDriver.open(t2Compare).dispatch(Intent.Dispatch(Command.SetUnderlay(true)))
    assert(d.queued.nonEmpty)
    val reopened = d.reopen(BundleSamples.inputsFor(d.model.document))
    reopened match
      case Left(e)  => fail(e.message)
      case Right(r) =>
        assertEquals(r.model.document, d.model.document)
        assertEquals(r.queued, Vector.empty)
        assertEquals(r.context.freshness, d.context.freshness)
        assertEquals(r.model.items, d.model.items)
  }

  private def opened(document: eyes4s.studio.core.document.StudioDocument): AppModel =
    play(AppModel.open(document, Some(project)), _ => Intent.ItemsLoaded(items))
