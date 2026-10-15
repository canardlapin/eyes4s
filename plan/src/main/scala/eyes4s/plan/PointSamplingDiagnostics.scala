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

/** Keyed point failures retain their source/template operands around the original reason. */
private[plan] object PointSamplingDiagnostics:
  import DiagnosticSupport.*

  private def op[A](value: A)(using
      projection: DiagnosticOperand[A, Nothing]
  ): Operand[Nothing] =
    projection(value)

  private def projected[K](error: PointSamplingError[K], values: Operand[K]*): Diagnostic[K] =
    val inherited = values.toVector.flatMap {
      case Operand.Cause(inner)  => inner.subject
      case Operand.Causes(inner) => inner.flatMap(_.subject)
      case _                     => Vector.empty
    }.distinct
    diagnostic(DiagnosticCatalog.pointSampling, error, error.message, inherited)(values*)

  def apply[K](error: PointSamplingError[K]): Diagnostic[K] = error match
    case PointSamplingError.AtTrial(source, template, underlying) =>
      val inner = apply[Nothing](underlying)
      val locus = template.fold[Vector[Locus[K]]](Vector(Locus.Trial(source)))(key =>
        Vector(Locus.Pair(source, key))
      )
      diagnostic(DiagnosticCatalog.pointSampling, error, error.message, locus ++ inner.subject)(
        key(source),
        optional(template.map(key(_))),
        cause(inner)
      )
    case PointSamplingError.Boundaries(values) => projected(error, op(values))
    case PointSamplingError.Clock(underlying)  => projected(error, op(underlying))
    case PointSamplingError.Frames(underlying) => projected(error, op(underlying))
    case PointSamplingError.MissingTemplate(participant, stimulus) =>
      projected(error, op(participant), op(stimulus))
    case PointSamplingError.AmbiguousTemplate(participant, stimulus, indices) =>
      projected(error, op(participant), op(stimulus), op(indices))
    case PointSamplingError.DuplicateSource(indices)   => projected(error, op(indices))
    case PointSamplingError.Density(index, underlying) =>
      projected(error, op(index), op(underlying))
    case PointSamplingError.Point(underlying) => projected(error, op(underlying))
    case PointSamplingError.Insufficient(level, requested, successful, minimum) =>
      projected(error, op(level), op(requested), op(successful), op(minimum))
    case PointSamplingError.NonFinite(level, index, value) =>
      projected(error, op(level), op(index), op(value))
