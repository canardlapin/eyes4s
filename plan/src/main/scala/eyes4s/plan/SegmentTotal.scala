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

package eyes4s.plan

/** What is known about a segment's total units before it runs. `Exact` is
  * the count the segment finishes at; `AtMost` is a bound the segment cannot
  * exceed but may finish under; `Unknown` is stated rather than estimated.
  *
  * Stated here, in the pure plan vocabulary, so the published execution laws
  * in `eyes4s-laws` can check every family's claims without an effect
  * system; `eyes4s-fs2` re-exports the name for its runner.
  */
enum SegmentTotal derives CanEqual:
  case Exact(units: Long)
  case AtMost(units: Long)

  /** An automatic traversal is still determining the exact total. */
  case Counting
  case Unknown
