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

/** The projections behind the initial-fixation and plan-revision
  * [[Diagnose]] instances, which live in the error companions so
  * `Diagnostic.of` finds them through the errors' implicit scope.
  */
private[plan] object RevisionDiagnostics:
  import DiagnosticSupport.*
  import DiagnosticCatalog as C

  def initialFixation(e: InitialFixationError): Diagnostic[Nothing] =
    import InitialFixationError.*
    val field = Vector(Locus.Field("initialFixations"))
    e match
      case NonPositiveRadius(radius) =>
        diagnostic(C.initialFixation, e, e.message, field)(real(radius))
      case NonFiniteCross(x, y) =>
        diagnostic(C.initialFixation, e, e.message, field)(real(x), real(y))
      case CrossOffFrame(x, y, frame) =>
        diagnostic(C.initialFixation, e, e.message, field)(real(x), real(y), name(frame.name))
      case MissingAngularScale(radius) =>
        diagnostic(C.initialFixation, e, e.message, Vector(Locus.Field("angularScale")))(
          real(radius)
        )
      case InvalidTally(dropped, total, droppedMicros, totalMicros) =>
        diagnostic[Nothing](C.initialFixation, e, e.message)(
          int(dropped),
          int(total),
          Operand.Micros(droppedMicros),
          Operand.Micros(totalMicros)
        )
      case NoFixationKept(dropped, micros) =>
        diagnostic[Nothing](C.initialFixation, e, e.message)(
          int(dropped),
          Operand.Micros(micros)
        )

  def studyRevision(e: StudyRevisionError): Diagnostic[Nothing] =
    import StudyRevisionError.*
    def at(field: StudyField) = Vector(Locus.Field(field.label))
    e match
      case DuplicateField(field) =>
        diagnostic(C.studyRevision, e, e.message, at(field))(token(field.toString))
      case Stale(field, stated, current) =>
        diagnostic(C.studyRevision, e, e.message, at(field))(
          token(field.toString),
          text(stated),
          text(current)
        )
      case IncompleteWindow(window, offWindow) =>
        diagnostic(C.studyRevision, e, e.message, at(StudyField.Window))(
          optional(window.map(name)),
          optional(offWindow.map(p => token(p.toString)))
        )
      case Plan(underlying) =>
        val inner = Projections.plan(underlying)
        diagnostic(C.studyRevision, e, e.message, inner.subject)(cause(inner))
