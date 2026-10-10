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

package eyes4s.studio.desktop.harness

/** Suites that run the native golden study end to end hold several GB of heap
  * (bead bd-01M4GMYBSRG806CPZR724X33RR). Where a runner cannot spare that
  * beside sbt, the environment variable [[Variable]] set to `skip` makes them
  * skip with [[reason]]; the Linux job runs them.
  */
object NativeArchiveRuns:
  val Variable = "EYES4S_STUDIO_NATIVE_ARCHIVE"

  /** Whether this run asked the native golden-study suites to skip. */
  val skipped: Boolean = sys.env.get(Variable).contains("skip")

  val reason: String =
    s"$Variable=skip: the native golden-study run needs more heap than this runner has"
