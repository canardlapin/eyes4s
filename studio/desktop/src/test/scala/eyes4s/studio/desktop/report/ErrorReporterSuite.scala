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

package eyes4s.studio.desktop.report

import eyes4s.studio.app.report.LogState
import eyes4s.studio.core.report.FailureOrigin

/** The reporter never throws (ticket S1.12): a failure while building the
  * report is said on standard error and reported as no bundle. No JavaFX:
  * the failure comes before any dialog.
  */
class ErrorReporterSuite extends munit.FunSuite:

  test("a report that cannot be built is no bundle, never an exception") {
    val reporter = ErrorReporter(
      ErrorReporter.facts,
      () => throw IllegalStateException("project"),
      None,
      LogState.Unavailable("test"),
      (_, _) => fail("no dialog without a bundle"),
      clock = () => throw IllegalStateException("clock")
    )
    assertEquals(reporter.report(FailureOrigin.Job("Submit"), RuntimeException("P17")), None)
    reporter.jobFailed("Submit", RuntimeException("P17"))
    reporter.handler.uncaughtException(Thread.currentThread, RuntimeException("P17"))
  }
