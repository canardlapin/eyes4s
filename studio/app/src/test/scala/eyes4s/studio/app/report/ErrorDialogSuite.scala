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

package eyes4s.studio.app.report

import eyes4s.studio.core.backend.ProtocolVersion
import eyes4s.studio.core.report.{BuildFacts, ErrorBundle, FailureOrigin, FailureTrace}

/** The error report dialog's view-model (ticket S1.12). */
class ErrorDialogSuite extends munit.FunSuite:

  private def bundle(origin: FailureOrigin) = ErrorBundle(
    "2026-10-04T12:00:00Z",
    BuildFacts("0.1.0", None, false, ProtocolVersion(1, 8), Vector.empty),
    origin,
    FailureTrace.of(RuntimeException("P17 ret_07")),
    None
  )

  test("the dialog shows exactly the bundle, where it happened and where it was logged") {
    val b  = bundle(FailureOrigin.UiThread("JavaFX Application Thread"))
    val vm = ErrorDialog.vm(b, LogState.Written("~/Library/Logs/Eyes Studio/eyes-studio.log"))
    assertEquals(vm.report, b.render)
    assertEquals(vm.title, "Eyes Studio error")
    assertEquals(vm.heading, "Something went wrong in Eyes Studio")
    assert(vm.summary.startsWith("An unexpected failure on the user interface."), vm.summary)
    assertEquals(
      vm.log,
      "The same report is in the log: ~/Library/Logs/Eyes Studio/eyes-studio.log"
    )
    assertEquals((vm.copy, vm.copied, vm.close), ("Copy report", "Copied", "Close"))
    assert(!vm.summary.contains("P17") && !vm.report.contains("P17"))
  }

  test("a job failure and an unwritable log say so") {
    val vm =
      ErrorDialog.vm(bundle(FailureOrigin.Job("Submit")), LogState.Unavailable("disk full"))
    assert(vm.summary.startsWith("An unexpected failure on the job (Submit)."), vm.summary)
    assertEquals(vm.log, "The log could not be written: disk full")
    val bg = ErrorDialog.vm(bundle(FailureOrigin.Background("io-1")), LogState.Unavailable("x"))
    assert(bg.summary.contains("background thread io-1"), bg.summary)
  }
