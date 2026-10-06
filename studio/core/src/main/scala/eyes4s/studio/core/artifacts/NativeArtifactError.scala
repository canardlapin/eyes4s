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

package eyes4s.studio.core.artifacts

import eyes4s.studio.core.backend.*

/** Artifact delivery failures name the run, operation, and failed operand. */
enum NativeArtifactError derives CanEqual:
  case Layout(operation: String, reason: String)
  case UnscopedBudget(resource: String, found: BigInt, maximum: BigInt)
  case InvalidBudget(field: String, value: BigInt)
  case Budget(run: RunId, resource: String, found: BigInt, maximum: BigInt)
  case NotRetained(run: RunId, known: Vector[RunId])
  case InvalidFacts(run: RunId, field: String, reason: String)
  case Package(run: RunId, operation: String, reason: String)
  case Defect(run: RunId, operation: String, failureClass: String)
  case WrongFacts(expectedRun: RunId, found: NativeBindingFacts)
  case Persistence(run: RunId, operation: String, cause: StudioDiagnostic)

  def message: String = this match
    case Layout(operation, reason) => s"Native artifact package $operation: $reason"
    case UnscopedBudget(resource, found, maximum) =>
      s"Native artifact package needs $found $resource; maximum is $maximum."
    case InvalidBudget(field, value) =>
      s"Native artifact budget $field must be positive, received $value."
    case Budget(run, resource, found, maximum) =>
      s"${run.label} native artifacts need $found $resource; maximum is $maximum."
    case NotRetained(run, known) =>
      s"${run.label} has no retained completed native result; retained runs: ${known.map(_.label).mkString(", ")}."
    case InvalidFacts(run, field, reason)     => s"${run.label} artifact fact $field: $reason"
    case Package(run, operation, reason)      => s"${run.label} artifact $operation: $reason"
    case Defect(run, operation, failureClass) =>
      s"${run.label} artifact $operation raised $failureClass."
    case WrongFacts(expected, found) =>
      s"Artifact completion for ${expected.label} returned facts for ${found.run.label}, ${found.stamp.label}."
    case Persistence(run, operation, cause) =>
      s"${run.label} artifact $operation: ${cause.message}"

  def code: String = this match
    case Layout(_, _)            => "native-artifacts.layout"
    case UnscopedBudget(_, _, _) => "native-artifacts.budget"
    case InvalidBudget(_, _)     => "native-artifacts.invalid-budget"
    case Budget(_, _, _, _)      => "native-artifacts.budget"
    case NotRetained(_, _)       => "native-artifacts.not-retained"
    case InvalidFacts(_, _, _)   => "native-artifacts.invalid-facts"
    case Package(_, _, _)        => "native-artifacts.package"
    case Defect(_, _, _)         => "native-artifacts.defect"
    case WrongFacts(_, _)        => "native-artifacts.wrong-facts"
    case Persistence(_, _, _)    => "native-artifacts.persistence"

  def diagnostic: StudioDiagnostic =
    val subjects = this match
      case Layout(_, _) | UnscopedBudget(_, _, _) =>
        Vector(DiagnosticLocus.Artifact("native artifact package"))
      case InvalidBudget(field, _)     => Vector(DiagnosticLocus.Field(field))
      case Budget(run, _, _, _)        => Vector(DiagnosticLocus.Run(run))
      case NotRetained(run, _)         => Vector(DiagnosticLocus.Run(run))
      case InvalidFacts(run, field, _) =>
        Vector(DiagnosticLocus.Run(run), DiagnosticLocus.Field(field))
      case Package(run, _, _)          => Vector(DiagnosticLocus.Run(run))
      case Defect(run, _, _)           => Vector(DiagnosticLocus.Run(run))
      case WrongFacts(expected, found) =>
        Vector(DiagnosticLocus.Run(expected), DiagnosticLocus.Run(found.run))
      case Persistence(run, _, cause) => Vector(DiagnosticLocus.Run(run)) ++ cause.subject
    StudioDiagnostic(
      code,
      DiagnosticLevel.Error,
      DiagnosticOrigin.Host,
      subjects.distinct,
      message
    )

object NativeArtifactError:
  def persistence(run: RunId, operation: String, reason: String): NativeArtifactError =
    NativeArtifactError.Persistence(
      run,
      operation,
      StudioDiagnostic(
        "native-artifacts.storage",
        DiagnosticLevel.Error,
        DiagnosticOrigin.Host,
        Vector(DiagnosticLocus.Run(run)),
        reason
      )
    )
