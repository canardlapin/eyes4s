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

import sbt._

/** Scala 3.7 Scaladoc discards compiler warning-policy flags. Enforce the
  * link policy on the doc task's own output, including cached-task output.
  */
object StrictScaladoc {
  private val BrokenLink =
    "(?i).*(couldn't resolve a member|unable to parse query|ambiguous.*link|link.*ambiguous).*".r

  def verify(module: String, output: File): Unit = {
    if (!output.isFile)
      sys.error(s"$module: missing Scaladoc task output at $output; cannot verify links")
    val failures =
      IO.readLines(output).filter(line => BrokenLink.pattern.matcher(line).matches())
    if (failures.nonEmpty)
      sys.error(
        s"$module: ${failures.size} unresolved or ambiguous Scaladoc links in $output:\n" + failures
          .mkString("\n")
      )
  }
}
