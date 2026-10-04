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

import eyes4s.core.*
import eyes4s.design.{SampleQuantum, Trial}
import eyes4s.kernel.Unit2D
import eyes4s.plan.*

private[io] enum LedgerInputVisit[K, U <: Unit2D]:
  case Trials(values: Vector[Trial[K, Unit, Scanpath[U]]], index: Int)
  case Texts(values: Vector[String], index: Int)
  case Path(value: Scanpath[U])
  case Fixations(values: IArray[Event.Fixation[U]], index: Int)
  case Summary(value: SummaryEvidence)
  case Transform(value: SpatialTransform)
  case Recording(value: eyes4s.core.Recording[U])
  case Samples(values: IArray[Sample[U]], index: Int)
  case Support(values: Vector[SampleRange])

/** Expected-input resource preflight only. Nodes contain closed domain values;
  * no callback, structural equality, content hash, extent computation or value
  * rendering runs here. Transform composition uses the explicit frontier, so
  * even deeply nested `Then` evidence yields between nodes without recursion.
  *
  * Work counts one fixed-shape node, fixation, sample or text per visit. Sample
  * ranges and lineage consist only of primitive closed values, so their sizes
  * suffice for resource accounting without traversal. Full evidence comparison
  * must subsequently count its own element comparisons.
  */
private[io] final case class LedgerInputCursor[K, U <: Unit2D](
    limits: LedgerExecutionLimits,
    keys: LedgerEvidenceKey[K],
    pending: List[LedgerInputVisit[K, U]],
    retained: Long,
    fixations: Long
):
  def advance(quantum: SampleQuantum): Either[LedgerResourceError, LedgerInputStep[K, U]] =
    import LedgerInputVisit.*
    val at    = LedgerResourceLocation(LedgerResourceSource.ExpectedInput)
    var todo  = pending
    var kept  = retained
    var count = fixations
    var units = 0
    var failure: Option[LedgerResourceError] = None
    def keep(amount: Long): Boolean          =
      limits.add(LedgerResource.RetainedEvidenceUnits, kept, amount, at) match
        case Left(error)  => failure = Some(error); false
        case Right(value) => kept = value; true
    def text(value: String): Unit =
      limits.check(LedgerResource.FieldCodeUnits, value.length.toLong, at) match
        case Left(error) => failure = Some(error)
        case Right(())   => val _ = keep(value.length.toLong)
    def push(value: LedgerInputVisit[K, U]): Unit = todo = value :: todo
    def texts(values: Vector[String]): Unit       =
      if values.nonEmpty then push(Texts(values, 0))
    while todo.nonEmpty && units < quantum.value && failure.isEmpty do
      val current = todo.head
      todo = todo.tail
      units += 1
      if keep(1) then
        current match
          case Trials(values, index) =>
            if index < values.size then
              if index + 1 < values.size then push(Trials(values, index + 1))
              val row = values(index)
              push(Path(row.value))
              texts(keys.fields(row.key))
          case Texts(values, index) =>
            text(values(index))
            if index + 1 < values.size then push(Texts(values, index + 1))
          case Path(value) =>
            value.sourceRecording.foreach(r => push(Recording(r)))
            value.sampleSupport.foreach(ranges => push(Support(ranges)))
            value.source.foreach(source => texts(Vector(source.value)))
            push(Fixations(value.fixations, 0))
            texts(Vector(value.frame.id.name, value.clock.name))
          case Fixations(values, index) =>
            if index < values.length then
              limits.add(LedgerResource.LogicalRecords, count, 1, at) match
                case Left(error) => failure = Some(error)
                case Right(next) =>
                  count = next
                  if index + 1 < values.length then push(Fixations(values, index + 1))
                  val value = values(index)
                  value.dispersionStatus match
                    case DispersionStatus.Available(_, evidence) => push(Summary(evidence))
                    case DispersionStatus.Unavailable(DispersionUnavailable.NotReported) => ()
                    case DispersionStatus.Unavailable(
                          DispersionUnavailable.SourceSupportUnavailable(from, to)
                        ) =>
                      texts(Vector(from.name, to.name))
                  texts(Vector(value.span.clock.name))
          case Summary(value) =>
            value match
              case SummaryEvidence.Declared                   => ()
              case SummaryEvidence.SourceSupported(source, _) => texts(Vector(source.value))
              case SummaryEvidence.Recomputed(source, _, from, to, transform) =>
                push(Transform(transform))
                texts(Vector(source.value, from.name, to.name))
          case Transform(value) =>
            value match
              case SpatialTransform.Identity | SpatialTransform.Affine(_) |
                  SpatialTransform.Homography(_) | SpatialTransform.Tangent(_, _) =>
                ()
              case SpatialTransform.Then(first, second) =>
                push(Transform(second))
                push(Transform(first))
          case Recording(value) =>
            push(Samples(value.samples, 0))
            texts(Vector(value.frame.id.name, value.clock.name))
          case Samples(values, index) =>
            if index < values.length then
              val _ = keep(values(index).lineage.toVector.size.toLong)
              if index + 1 < values.length then push(Samples(values, index + 1))
          case Support(values) =>
            val _ = keep(values.size.toLong)
    failure.toLeft(
      LedgerInputStep(
        units,
        kept,
        count,
        Option.when(todo.nonEmpty)(LedgerInputCursor(limits, keys, todo, kept, count))
      )
    )

private[io] object LedgerInputCursor:
  def start[K, U <: Unit2D](
      spec: ImportSpec[K, U],
      input: StudyInput[K, U],
      limits: LedgerExecutionLimits
  ): Either[LedgerExecutionEvidence.Unsupported, LedgerInputCursor[K, U]] =
    LedgerExecutionEvidence.qualify(spec).flatMap { _ =>
      val keys: Either[LedgerExecutionEvidence.Unsupported, LedgerEvidenceKey[K]] =
        spec.keys match
          case SourceKeyColumns.Study(_, _, _)       => Right(LedgerEvidenceKey.Study)
          case SourceKeyColumns.Trial(_, _, _, _, _) => Right(LedgerEvidenceKey.Trial)
          case SourceKeyColumns.Custom(_, _, _)      =>
            Left(LedgerExecutionEvidence.Unsupported.KeyColumns)
      keys.map(shape =>
        LedgerInputCursor(
          limits,
          shape,
          List(LedgerInputVisit.Trials(input.trials.rows, 0)),
          0L,
          0L
        )
      )
    }

private[io] final case class LedgerInputStep[K, U <: Unit2D](
    workUnits: Int,
    retainedUnits: Long,
    fixationCount: Long,
    next: Option[LedgerInputCursor[K, U]]
)
