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

/** The decimal spelling of result-table and CSV numbers. It is
  * [[eyes4s.results.DecimalText]]'s, so a report table rendered by the
  * results layer and a contrast CSV spell every number the same way.
  */
private[io] object CsvNumber:
  def render(value: Double): String = eyes4s.results.DecimalText.render(value)
