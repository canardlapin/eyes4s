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

package eyes4s.results

import eyes4s.plan.*
import eyes4s.plan.DiagnosticSamples.generated

/** One generated value of every case of every results error and finding
  * family, projected through the public `Diagnose` instances, in catalog
  * order. Shared by the results, codec and io catalog suites.
  */
object ResultsDiagnosticSamples:
  import ResultsDiagnostics.given

  given DiagnosticExample[GroupKey] =
    DiagnosticExample.of(seed => GroupKey(Vector(s"term-$seed" -> s"level-$seed")))
  given DiagnosticExample[DescriptorError] =
    DiagnosticExample.of(seed =>
      DescriptorError.ComponentMismatch(Vector(s"id-$seed"), Vector(s"component-$seed"))
    )

  /** How the results operands project structured fields, for the
    * alignment checks of every catalog suite that samples these families.
    */
  val structured: PartialFunction[Any, Operand[Any]] = {
    case g: GroupKey =>
      Operand.Items(
        g.levels.map((t, l) =>
          Operand.Fields(Vector("term" -> Operand.Name(t), "level" -> Operand.Name(l)))
        )
      )
    case m: WindowMeasure   => Operand.Token(Term.window(m))
    case ResultCell.Text(s) =>
      Operand.Fields(Vector("kind" -> Operand.Token("Text"), "value" -> Operand.Text(s)))
  }

  val all: Vector[FamilySamples] = Vector(
    generated[ReportFinding[StudyKey]]("ReportFinding"),
    generated[ReportError[StudyKey]]("ReportError"),
    generated[SpecError]("SpecError"),
    generated[CovariateError[StudyKey]]("CovariateError"),
    generated[ResultTableError]("ResultTableError")
  )
