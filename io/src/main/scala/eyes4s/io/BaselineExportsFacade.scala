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

/** The baseline export adapters under their former name. Every table they
  * build is a table of the one result-table layer, with the same identity;
  * new code calls [[ResultExports]], and reports use
  * [[eyes4s.results.ReportTables]].
  */
@deprecated(
  "BaselineExports is a compatibility facade; use ResultExports, which builds the same tables",
  "0.1"
)
object BaselineExports:
  export ResultExports.*
