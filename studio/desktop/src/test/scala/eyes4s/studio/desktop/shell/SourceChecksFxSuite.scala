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

package eyes4s.studio.desktop.shell

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.StoryModels
import eyes4s.studio.core.bundle.{InputEntry, InputKind, InputStatus}
import eyes4s.studio.core.command.JournalEntry
import eyes4s.studio.core.document.Source
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.session.SaveReceipt
import eyes4s.studio.desktop.runtime.ProjectPort
import com.sun.javafx.stage.WindowHelper
import scala.collection.mutable.ArrayBuffer

class SourceChecksFxSuite extends ShellFxSuite:
  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private def entry(s: Source): InputEntry    =
    right(InputEntry.of(InputKind.Source(s.role), s.path.value.split('/').last, s.bytes, 1L))

  private final class Port extends ProjectPort:
    val checks = ArrayBuffer.empty[Either[String, Vector[InputStatus]] => Unit]
    def journal(e: JournalEntry): Unit                        = ()
    def save(done: Either[String, SaveReceipt] => Unit): Unit = done(Left("unused"))
    def close(): Unit                                         = ()
    override def checkInputs(done: Either[String, Vector[InputStatus]] => Unit): Unit =
      checks += done: Unit

  fxStage.test(
    "focus return rechecks changed, missing and restored sources/assets; old answers stay ignored"
  ) { fx =>
    val port    = Port()
    val model   = StoryModels.t2Analysis
    val sources = model.document.dataset(StoryMoments.r3).get.sources.entries.map(entry)
    val image   = right(
      InputEntry.of(
        InputKind.StimulusImage,
        "image.png",
        ByteDigest.sha256(IArray.from(Array(1.toByte))),
        1
      )
    )
    val present        = sources.map(InputStatus.Present(_)) :+ InputStatus.Present(image)
    val w              = boot(fx, model, project = Some(port))
    def regain(): Unit =
      val before = runOnFx(port.checks.size)
      // Exercise the same observable-property update the toolkit delivers.
      // Monocle's virtual stages do not reliably transfer OS focus.
      runOnFx {
        WindowHelper.setFocused(fx.stage, false)
        WindowHelper.setFocused(fx.stage, true)
      }
      fx.awaitLayout()
      eventually(fx, "a focus-triggered verification")(port.checks.size == before + 1)
    def answer(statuses: Vector[InputStatus]): Unit =
      runOnFx(port.checks.last(Right(statuses)))
      fx.awaitLayout()
      eventually(fx, "latest verification answer")(
        w.runtime.model.checks.answered == w.runtime.model.checks.asked
      )
    answer(present)
    assertEquals(runOnFx(w.runtime.model.runBlock), None)
    regain()
    val stale = runOnFx(port.checks.last)
    regain()
    answer(
      Vector(
        InputStatus.Changed(sources.head, ByteDigest.sha256(IArray.from(Array(2.toByte))))
      ) ++
        sources.tail.map(InputStatus.Present(_)) :+ InputStatus.Missing(image)
    )
    assert(runOnFx(w.runtime.model.runBlock.isDefined))
    assert(runOnFx(w.resolvedDesign.state.blocked.isDefined))
    assert(!runOnFx(w.preflight.runButton._2))
    assertEquals(runOnFx(w.runtime.model.sources.imagesUnstored(Set(image.sha256)).size), 1)
    assert(
      runOnFx(
        w.sources.view.sourceLines.flatten.exists(_.startsWith("changed since it was stored"))
      )
    )
    runOnFx(stale(Right(present)))
    fx.awaitLayout()
    assert(runOnFx(w.runtime.model.runBlock.isDefined))
    assert(runOnFx(w.resolvedDesign.state.blocked.isDefined))
    assert(!runOnFx(w.preflight.runButton._2))
    regain()
    answer(
      Vector(InputStatus.Missing(sources.head)) ++ sources.tail.map(InputStatus.Present(_))
    )
    assert(runOnFx(w.runtime.model.runBlock.isDefined))
    assert(runOnFx(w.resolvedDesign.state.blocked.isDefined))
    assert(!runOnFx(w.preflight.runButton._2))
    regain()
    answer(present)
    assertEquals(runOnFx(w.runtime.model.runBlock), None)
    assertEquals(runOnFx(w.resolvedDesign.state.blocked), None)
    assertEquals(runOnFx(w.runtime.model.sources.imagesUnstored(Set(image.sha256)).size), 0)
    val count = runOnFx(port.checks.size)
    runOnFx(w.close())
    opened -= w
    runOnFx {
      WindowHelper.setFocused(fx.stage, false)
      WindowHelper.setFocused(fx.stage, true)
    }
    fx.awaitLayout()
    assertEquals(runOnFx(port.checks.size), count)
  }
