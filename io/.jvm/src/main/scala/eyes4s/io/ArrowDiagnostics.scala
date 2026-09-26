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

package eyes4s.io

import eyes4s.plan.*

/** The code table and instance of the JVM-only Arrow export. The family is
  * kept apart from [[IoDiagnosticCatalog]] so that table stays identical on
  * the JVM and Scala.js. Import `ArrowDiagnostics.given` for `Diagnostic.of`.
  */
object ArrowDiagnostics:
  import DiagnosticFamily.error

  val arrowExport: DiagnosticFamily = error("arrow-export")(
    "Limits",
    "Write"
  )

  given arrowExportError: Diagnose[ArrowExportError, Nothing] =
    Diagnose.derived[ArrowExportError, Nothing](arrowExport)(_.message)
