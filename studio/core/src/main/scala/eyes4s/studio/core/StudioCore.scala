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

package eyes4s.studio.core

/** Eyes Studio's application core: document, commands, revisions, services and
  * the `StudyBackend` protocol (DESIGN_SPEC section 13).
  *
  * Cross-built for the JVM and Scala.js. It may use Cats Effect and FS2, but it
  * names no JavaFX type and no JVM file-system API; platform services are
  * interfaces implemented in studio-desktop.
  */
object StudioCore:

  /** The artifact name of this layer. */
  val moduleName: String = "eyes4s-studio-core"
