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

private[eyes4s] object ProjectPrerequisiteDiagnostics:
  import DiagnosticSupport.*

  private def selector[M](value: MarkSelector[M]): Operand[Nothing] =
    fields(
      "kind"       -> text(value.kind.toString),
      "occurrence" -> summon[DiagnosticOperand[Occurrence, Nothing]].apply(value.occurrence)
    )

  def project[K, M](value: ProjectFinding[K, M]): Diagnostic[K] =
    import ProjectFinding.*
    val family = DiagnosticCatalog.projectPrerequisite
    value match
      case MissingRecording(key) =>
        diagnostic[K](family, value, value.message, Vector(Locus.Entry(key.value)))(
          name(key.value)
        )
      case MissingViewing() =>
        diagnostic[K](family, value, value.message, Vector(Locus.Field("viewing")))()
      case MissingAreas(key) =>
        diagnostic[K](family, value, value.message, Vector(Locus.Entry(key.value)))(
          name(key.value)
        )
      case MissingGrid(key) =>
        diagnostic[K](family, value, value.message, Vector(Locus.Entry(key.value)))(
          name(key.value)
        )
      case MissingObservedTimeline(trial, selected, expected) =>
        diagnostic(family, value, value.message, Vector(Locus.Trial(trial)))(
          key(trial),
          selector(selected),
          clock(expected)
        )
      case Anchor(underlying) =>
        val inner = Diagnostic.of(underlying)
        diagnostic(family, value, value.message, inner.subject)(cause(inner))
      case MissingSynchronization(trial, selected, source, target, maximum, available) =>
        val pairs = Operand.Items(
          available.map((from, to) => fields("_1" -> clock(from), "_2" -> clock(to)))
        )
        diagnostic(family, value, value.message, Vector(Locus.Trial(trial)))(
          key(trial),
          selector(selected),
          clock(source),
          clock(target),
          Operand.Micros(maximum.toMicros),
          pairs
        )
      case ImpreciseSynchronization(trial, selected, source, target, maximum, best) =>
        diagnostic(family, value, value.message, Vector(Locus.Trial(trial)))(
          key(trial),
          selector(selected),
          clock(source),
          clock(target),
          Operand.Micros(maximum.toMicros),
          Operand.Micros(best.toMicros)
        )
      case MissingSchema(id, availableVersions) =>
        diagnostic[K](family, value, value.message, Vector(Locus.Definition(id)))(
          definition(id),
          ints(availableVersions)
        )
