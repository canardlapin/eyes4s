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

import eyes4s.design.SampleQuantum
import eyes4s.kernel.Unit2D
import eyes4s.plan.*

private[io] enum LedgerDescriptionError derives CanEqual:
  case Unsupported(evidence: LedgerExecutionEvidence.Unsupported)
  case Resource(error: LedgerResourceError)

private[io] enum LedgerDescriptionVisit:
  case Columns(count: Long)
  case Texts(values: Vector[String], index: Int, dimension: LedgerResource)
  case Attributes(values: Vector[AttributeColumn], index: Int)
  case StudyRules(values: Vector[AppliedCorrection[StudyKey]], index: Int)
  case TrialRules(values: Vector[AppliedCorrection[TrialKey]], index: Int)

/** Metadata preflight counts one fixed-shape cell, attribute or correction-rule
  * visit per unit. Collection roots retain references, never mapped copies.
  * Names are sized before any trim, equality, digest or correction traversal.
  */
private[io] final case class LedgerDescriptionCursor private (
    limits: LedgerExecutionLimits,
    pending: List[LedgerDescriptionVisit],
    retained: Long
):
  def advance(quantum: SampleQuantum): Either[LedgerResourceError, LedgerDescriptionStep] =
    import LedgerDescriptionVisit.*
    val at    = LedgerResourceLocation(LedgerResourceSource.ImportDescription)
    var todo  = pending
    var kept  = retained
    var units = 0
    var failure: Option[LedgerResourceError]                      = None
    def check(dimension: LedgerResource, observed: Long): Boolean =
      limits.check(dimension, observed, at) match
        case Left(error) => failure = Some(error); false
        case Right(())   => true
    def keep(amount: Long): Boolean =
      limits.add(LedgerResource.RetainedEvidenceUnits, kept, amount, at) match
        case Left(error)  => failure = Some(error); false
        case Right(value) => kept = value; true
    def text(value: String, dimension: LedgerResource): Unit =
      if check(LedgerResource.FieldCodeUnits, value.length.toLong) &&
        (dimension == LedgerResource.FieldCodeUnits || check(dimension, value.length.toLong))
      then
        val _ = keep(value.length.toLong + 1)
    def ruleTexts(values: Vector[String], next: LedgerDescriptionVisit): Unit =
      todo = Texts(values, 0, LedgerResource.CorrectionOperandCodeUnits) :: next :: todo

    while todo.nonEmpty && units < quantum.value && failure.isEmpty do
      units += 1
      val current = todo.head
      todo = todo.tail
      current match
        case Columns(count) =>
          val _ = check(LedgerResource.Columns, count)
        case Texts(values, index, dimension) =>
          if index < values.size then
            text(values(index), dimension)
            if index + 1 < values.size then todo = Texts(values, index + 1, dimension) :: todo
        case Attributes(values, index) =>
          if (index != 0 || check(
              LedgerResource.DeclaredAttributes,
              values.size.toLong
            )) && index < values.size
          then
            text(values(index).name, LedgerResource.FieldCodeUnits)
            if index + 1 < values.size then todo = Attributes(values, index + 1) :: todo
        case StudyRules(values, index) =>
          if (index != 0 || check(
              LedgerResource.CorrectionRules,
              values.size.toLong
            )) && index < values.size && keep(1)
          then
            val fields = values(index).scope match
              case CorrectionScope.AllTrials()    => Vector.empty
              case CorrectionScope.Participant(p) => Vector(p)
              case CorrectionScope.Trial(k)       => Vector(k.participant, k.stimulus, k.phase)
            ruleTexts(fields, StudyRules(values, index + 1))
        case TrialRules(values, index) =>
          if (index != 0 || check(
              LedgerResource.CorrectionRules,
              values.size.toLong
            )) && index < values.size && keep(1)
          then
            val fields = values(index).scope match
              case CorrectionScope.AllTrials()    => Vector.empty
              case CorrectionScope.Participant(p) => Vector(p)
              case CorrectionScope.Trial(k) => Vector(k.participant, k.phase, k.trial, k.item)
            ruleTexts(fields, TrialRules(values, index + 1))
    failure.toLeft(
      LedgerDescriptionStep(
        units,
        kept,
        Option.when(todo.nonEmpty)(new LedgerDescriptionCursor(limits, todo, kept))
      )
    )

private[io] object LedgerDescriptionCursor:
  def start[K, U <: Unit2D](
      spec: ImportSpec[K, U],
      limits: LedgerExecutionLimits
  ): Either[LedgerDescriptionError, LedgerDescriptionCursor] =
    import LedgerDescriptionVisit.*
    LedgerExecutionEvidence
      .qualify(spec)
      .left
      .map(LedgerDescriptionError.Unsupported.apply)
      .flatMap { _ =>
        val rules: Either[LedgerDescriptionError, LedgerDescriptionVisit] = spec.keys match
          case SourceKeyColumns.Study(_, _, _) => Right(StudyRules(spec.policy.corrections, 0))
          case SourceKeyColumns.Trial(_, _, _, _, _) =>
            Right(TrialRules(spec.policy.corrections, 0))
          case SourceKeyColumns.Custom(_, _, _) =>
            Left(
              LedgerDescriptionError.Unsupported(LedgerExecutionEvidence.Unsupported.KeyColumns)
            )
        rules.map { policy =>
          val c         = spec.columns
          val keyNames  = spec.keys.names
          val countName = c.samples.countColumn.toVector
          val names     = Vector(
            spec.frame.id.name,
            c.ordinal,
            c.x,
            c.y,
            c.onset,
            c.duration
          ) ++ countName ++ keyNames
          val base: List[LedgerDescriptionVisit] = List(
            Columns(keyNames.size.toLong + 5 + countName.size + c.attributes.size),
            Texts(names, 0, LedgerResource.FieldCodeUnits),
            Attributes(c.attributes, 0),
            policy
          )
          val inventory = spec.inventory.toList.flatMap { binding =>
            val i      = binding.spec
            val fields = Vector(
              i.participant,
              i.phase,
              i.trial
            ) ++ i.occurrence.toVector ++ i.item.toVector
            List(
              Columns(fields.size.toLong + i.attributes.size),
              Texts(fields, 0, LedgerResource.FieldCodeUnits),
              Attributes(i.attributes, 0)
            )
          }
          new LedgerDescriptionCursor(limits, base ++ inventory, 0L)
        }
      }

private[io] final case class LedgerDescriptionStep(
    workUnits: Int,
    retainedUnits: Long,
    next: Option[LedgerDescriptionCursor]
)
