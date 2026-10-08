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

package eyes4s.studio.desktop.data

import cats.effect.IO
import eyes4s.codec.ByteDigest
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.analysis.{DesignEffect, ResolvedDesign}
import eyes4s.studio.app.data.{SourcesEffect, SourcesIntent, SourcesPane}
import eyes4s.studio.core.assets.{
  DisplayKind,
  DisplayState,
  InputCheck,
  SourceCheck,
  SourceState
}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.bundle.{BundleError, InputKind}
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.real.RealStudyBackend
import eyes4s.studio.core.session.SessionError
import eyes4s.studio.desktop.platform.TempDirs
import eyes4s.studio.desktop.runtime.DatasetSourceHosts
import java.nio.file.Files
import scala.concurrent.duration.*

/** Actual filesystem tampering and SessionPort repairs, including native read boundaries and reopen. */
class SourceRepairFileSuite extends munit.CatsEffectSuite:
  import SourceRepairFileFixture.*
  override val munitIOTimeout: Duration = 90.seconds

  private def callback[A](start: (Either[String, A] => Unit) => Unit): IO[A] =
    IO.async_[Either[String, A]](done => start(a => done(Right(a)))).map(get)
  private def modelState(model: AppModel, id: DatasetRevision) =
    model.sources.of(id).find(_.source.role == SourceRole.Fixations).get.state

  private def assertBlocked(model: AppModel): Unit =
    assert(model.runBlock.isDefined)
    val (design, effects) =
      ResolvedDesign.sync(ResolvedDesign.empty.copy(started = true), model)
    assert(design.blocked.isDefined)
    assert(!effects.exists(_.isInstanceOf[DesignEffect.StartPreview]))
    val requested = AppModel.update(model, Intent.Dispatch(Command.SaveAndRun(None)))
    val answered  = AppModel.update(
      requested._1,
      Intent.InputsChecked(
        requested._1.checks.asked,
        model.inputs match
          case InputCheck.Checked(statuses) => statuses
          case other                        => fail(s"not checked: $other")
      )
    )
    assertEquals(answered._1.document.runs, model.document.runs)
    assert(!answered._2.exists {
      case eyes4s.studio.app.AppEffect.Execution(_) => true; case _ => false
    })

  test(
    "same-path changes block native previews/runs; wrong restore is atomic, exact repair survives reopen"
  ) {
    TempDirs.resource("eyes4s-source-file-").use { directory =>
      val root = directory.resolve("repair.eyes")
      for
        saved <- open(root).use { port =>
          for
            ready <- working(port, dataset)
            clean <- checked(port, ready)
            _    = assertEquals(clean.runBlock, None)
            fix  = source(ready)
            path = address(root, fix)
            _       <- IO.blocking(Files.write(path, Array.from(replacement)))
            changed <- checked(port, ready)
            _ = assertEquals(
              modelState(changed, dataset),
              SourceState.Changed(ByteDigest.sha256(replacement))
            )
            _ = assertBlocked(changed)
            native <- RealStudyBackend
              .resource[IO](ready, DatasetSourceHosts.stored(port))
              .use(_.preview(ready.draft.get.id))
            _ = assert(
              native.left.toOption.exists(_.message.contains(fix.path.value)),
              "native read refusal must name the tampered source"
            )
            wrong <- port.session.restoreInput(fix, replacement)
            _ = assert(wrong.left.toOption.exists {
              case SessionError.Bundle(_, _: BundleError.RestoreMismatch) => true;
              case _                                                      => false
            })
            unchanged <- port.session.document
            _ = assertEquals(unchanged, ready)
            stillWrong <- IO.blocking(Files.readAllBytes(path).toVector)
            _ = assertEquals(stillWrong, Array.from(replacement).toVector)
            _        <- callback[Unit](done => port.restoreInput(fix, fixations, done))
            repaired <- checked(port, ready)
            _ = assertEquals(modelState(repaired, dataset), SourceState.Present)
            _ = assertEquals(repaired.document.science, changed.document.science)
            _ = assertEquals(repaired.runBlock, None)
            preview <- RealStudyBackend
              .resource[IO](ready, DatasetSourceHosts.stored(port))
              .use(_.preview(ready.draft.get.id))
            _ = assert(preview.isRight)
            _ <- port.session.save.map(get)
          yield ready
        }
        _ <- reopen(root).use { port =>
          for
            actual  <- port.session.document
            checked <- SourceRepairFileFixture.checked(port, actual)
            _ = assertEquals(actual, saved)
            _ = assertEquals(checked.runBlock, None)
            raw <- port.session.readInput(source(actual)).map(get)
          yield assertEquals(Array.from(raw).toVector, Array.from(fixations).toVector)
        }
      yield ()
    }
  }

  test("replacement bytes create a pending immutable revision, re-admit natively and reopen") {
    TempDirs.resource("eyes4s-source-replace-").use { directory =>
      val root = directory.resolve("replacement.eyes")
      for
        saved <- open(root).use { port =>
          for
            ready <- working(port, dataset)
            _ <- IO.blocking(Files.write(address(root, source(ready)), Array.from(replacement)))
            damaged <- checked(port, ready)
            pane      = SourcesPane.sync(SourcesPane.empty, damaged)._1
            cancelled = SourcesPane.update(
              pane,
              damaged,
              SourcesIntent.SourceNotChosen(SourceRole.Fixations, None)
            )
            _      = assertEquals(cancelled._2, Vector.empty)
            chosen = SourcesPane
              .update(
                pane,
                damaged,
                SourcesIntent.SourceChosen(
                  dataset,
                  SourceRole.Fixations,
                  replacement,
                  ByteDigest.sha256(replacement)
                )
              )
              ._2
            store = chosen.collectFirst { case value: SourcesEffect.Store => value }.get
            _ <- callback[Unit](done =>
              port.importInput(
                InputKind.Source(store.replacement.role),
                "fixations.csv",
                store.bytes,
                done
              )
            )
            finished = SourcesPane
              .update(
                pane,
                damaged,
                SourcesIntent.SourceStored(dataset, store.replacement, Right(()))
              )
              ._2
            command = finished.collectFirst { case SourcesEffect.App(Intent.Dispatch(value)) =>
              value
            }.get
            _        <- port.session.perform(JournalEntry.Apply(command)).map(get)
            replaced <- port.session.document
            next = replaced.datasets.last
            _    = assertEquals(replaced.datasets.head, ready.datasets.head)
            _    = assertEquals(next.parent, Some(dataset))
            _    = assertEquals(next.decision, AdmissionDecision.Pending)
            _    = assertEquals(
              next.sources.fixations.map(_.bytes),
              Some(ByteDigest.sha256(replacement))
            )
            _ = assertNotEquals(
              address(root, source(ready)),
              address(root, next.sources.fixations.get)
            )
            statuses <- port.session.checkInputs
            _ = assertEquals(
              SourceCheck.of(replaced, InputCheck.Checked(statuses)).block(next.id),
              None
            )
            verified <- admit(port, next.id)
            _ = assert(verified.datasets.last.decision.isAdmitted)
            _ <- port.session.perform(JournalEntry.Apply(Command.RebaseDraft(next.id))).map(get)
            rebased  <- port.session.document
            runnable <- checked(port, rebased)
            _ = assertEquals(runnable.runBlock, None)
            _ <- callback[Unit](done => port.restoreInput(source(ready), fixations, done))
            _ <- port.session.save.map(get)
          yield rebased
        }
        _ <- reopen(root).use { port =>
          for
            restored <- port.session.document
            _ = assertEquals(restored, saved)
            raw <- port.session.readInput(source(restored, restored.datasets.last.id)).map(get)
          yield assertEquals(Array.from(raw).toVector, Array.from(replacement).toVector)
        }
      yield ()
    }
  }

  test("real stored missing images stay missing while blank trials and science remain intact") {
    TempDirs.resource("eyes4s-source-assets-").use { directory =>
      val root = directory.resolve("assets.eyes")
      open(root).use { port =>
        for
          document <- port.session.document
          spec = document.datasets.head
          registry <- DatasetSourceHosts
            .stored(port)
            .assets(spec)
            .map(_.getOrElse(fail("no stored inventory")))
          _ = assert(
            registry
              .display(trial)
              .exists(_.state match
                case DisplayState.MissingAsset(DisplayKind.Image, file) =>
                  file.value == "missing.png"
                case _ => false)
          )
          _ = assertEquals(registry.display(blank).map(_.state), Some(DisplayState.Blank))
          _ = assertEquals(registry.count(DisplayKind.Image), 1)
          _ = assertEquals(registry.count(DisplayKind.Blank), 1)
          _ = assertEquals(registry.missing.map(_.file.value), Vector("missing.png"))
          after <- port.session.document
        yield assertEquals(after.science, document.science)
      }
    }
  }
