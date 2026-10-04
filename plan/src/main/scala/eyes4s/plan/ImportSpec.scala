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

import eyes4s.design.KeyDigest
import eyes4s.kernel.*

/** Source timestamp units are declared; no numeric heuristic selects them. */
enum SourceTimeUnit derives CanEqual:
  case Microseconds, Milliseconds, Seconds

/** How a decimal source timestamp becomes integral microseconds. The one policy,
  * `NearestMicrosecond`, rounds halves toward positive infinity (floor of value plus one
  * half, so -1.5 µs becomes -1); it is named so a saved import states it.
  */
enum SourceRounding derives CanEqual:
  case NearestMicrosecond

/** A declarative key reader and its per-trial clock convention. A custom
  * definition can persist even when the current host cannot replay it.
  */
enum SourceKeyColumns[K] derives CanEqual:
  case Study(participant: String, stimulus: String, phase: String)
      extends SourceKeyColumns[StudyKey]
  case Trial(
      participant: String,
      phase: String,
      trial: String,
      item: Option[String],
      occurrence: Option[String]
  ) extends SourceKeyColumns[TrialKey]
  case Custom(reader: DefinitionId, clock: DefinitionId, columns: Vector[String])

  def names: Vector[String] = this match
    case Study(p, s, f)        => Vector(p, s, f)
    case Trial(p, f, t, i, o)  => Vector(p, f, t) ++ i.toVector ++ o.toVector
    case Custom(_, _, columns) => columns

/** Pure fixation-column declarations shared by Studio and the importer. */
final case class SourceFixationColumns private (
    ordinal: String,
    x: String,
    y: String,
    onset: String,
    duration: String,
    samples: SampleCountRule,
    attributes: Vector[AttributeColumn]
) derives CanEqual:
  def names: Vector[String] = Vector(ordinal, x, y, onset, duration) ++
    samples.countColumn.toVector ++ attributes.map(_.name)

object SourceFixationColumns:
  def of(
      ordinal: String,
      x: String,
      y: String,
      onset: String,
      duration: String,
      samples: SampleCountRule,
      attributes: Vector[AttributeColumn] = Vector.empty
  ): Either[ImportSpecError, SourceFixationColumns] =
    val value = new SourceFixationColumns(ordinal, x, y, onset, duration, samples, attributes)
    ImportSpec.checkColumns(value.names).map(_ => value)

/** The independent trial-inventory source interpretation. */
final case class InventoryImportSpec private (
    participant: String,
    phase: String,
    trial: String,
    occurrence: Option[String],
    item: Option[String],
    attributes: Vector[AttributeColumn]
) derives CanEqual:
  def names: Vector[String] = Vector(participant, phase, trial) ++ occurrence.toVector ++
    item.toVector ++ attributes.map(_.name)
  def digest: ContentHash = SourceHash.sequence(
    Vector(
      SourceHash.text("eyes4s.inventory-import/1"),
      SourceHash.text(participant),
      SourceHash.text(phase),
      SourceHash.text(trial),
      SourceHash.optional(occurrence.map(SourceHash.text)),
      SourceHash.optional(item.map(SourceHash.text)),
      SourceHash.attributes(attributes)
    )
  )
  def source(label: String, header: Vector[String], rows: Vector[Vector[String]]): SourceRef =
    SourceRef(
      label,
      ArtifactRef.of(SourceRef.digest(header, rows)),
      SourceInterpretation.inventory(this)
    )

object InventoryImportSpec:
  def of(
      participant: String,
      phase: String,
      trial: String,
      occurrence: Option[String] = None,
      item: Option[String] = None,
      attributes: Vector[AttributeColumn] = Vector.empty
  ): Either[ImportSpecError, InventoryImportSpec] =
    val value = new InventoryImportSpec(participant, phase, trial, occurrence, item, attributes)
    ImportSpec.checkColumns(value.names).map(_ => value)

/** An inventory admission binds both the declaration and its whole-source identity. */
final case class SourceInventory(spec: InventoryImportSpec, identity: SourceIdentity)
    derives CanEqual

/** Complete pure description for Phase 1 fixation admission. K is the same
  * key type as the correction policy and its registered key codec.
  */
final class ImportSpec[K, U <: Unit2D] private (
    val keys: SourceKeyColumns[K],
    val columns: SourceFixationColumns,
    val frame: Frame[U],
    val timeUnit: SourceTimeUnit,
    val rounding: SourceRounding,
    val policy: AdmissionPolicy[K],
    val decision: AdmissionDecision,
    val inventory: Option[SourceInventory]
)(using val keyDigest: KeyDigest[K], val unit: UnitLabel[U]):
  lazy val digest: ContentHash =
    val key = keys match
      case SourceKeyColumns.Study(p, s, f)       => SourceHash.strings(Vector("study", p, s, f))
      case SourceKeyColumns.Trial(p, f, t, i, o) =>
        SourceHash.sequence(
          Vector(
            SourceHash.strings(Vector("trial", p, f, t)),
            SourceHash.optional(i.map(SourceHash.text)),
            SourceHash.optional(o.map(SourceHash.text))
          )
        )
      case SourceKeyColumns.Custom(reader, clock, columns) =>
        SourceHash.sequence(
          Vector(
            SourceHash.text("custom"),
            SourceHash.definition(reader),
            SourceHash.definition(clock),
            SourceHash.strings(columns)
          )
        )
    val counts = columns.samples match
      case SampleCountRule.PositiveColumn(column) =>
        SourceHash.strings(Vector("positiveColumn", column))
      case SampleCountRule.DerivedFromDuration(rate) =>
        SourceHash.sequence(
          Vector(SourceHash.text("derivedFromDuration"), ContentHash.of(IArray(rate.value)))
        )
    val corrections = policy.corrections.map { rule =>
      val scope = rule.scope match
        case CorrectionScope.AllTrials()    => SourceHash.text("allTrials")
        case CorrectionScope.Participant(p) => SourceHash.strings(Vector("participant", p))
        case CorrectionScope.Trial(k)       =>
          SourceHash.sequence(Vector(SourceHash.text("trial"), keyDigest.digest(k)))
      val correction = rule.correction match
        case Correction.FlipX             => SourceHash.text("flipX")
        case Correction.FlipY             => SourceHash.text("flipY")
        case Correction.Translate(dx, dy) =>
          SourceHash.sequence(
            Vector(SourceHash.text("translate"), ContentHash.of(IArray(dx, dy)))
          )
      SourceHash.sequence(Vector(scope, correction))
    }
    SourceHash.sequence(
      Vector(
        SourceHash.text("eyes4s.import-spec/1"),
        key,
        SourceHash.strings(
          Vector(columns.ordinal, columns.x, columns.y, columns.onset, columns.duration)
        ),
        counts,
        SourceHash.attributes(columns.attributes),
        SourceHash.strings(Vector(frame.id.name, unit.symbol, frame.yAxis.toString)),
        ContentHash.of(
          IArray(frame.spec.xMin, frame.spec.yMin, frame.spec.xMax, frame.spec.yMax)
        ),
        SourceHash.strings(
          Vector(
            timeUnit.toString,
            rounding.toString,
            policy.offScreen.toString,
            decision.toString
          )
        ),
        SourceHash.sequence(corrections),
        SourceHash.optional(
          inventory.map(i => SourceHash.sequence(Vector(i.spec.digest, i.identity.hash)))
        )
      )
    )

  def source(label: String, header: Vector[String], rows: Vector[Vector[String]]): SourceRef =
    SourceRef(
      label,
      ArtifactRef.of(SourceRef.digest(header, rows)),
      SourceInterpretation.fixation(this)
    )

object ImportSpec:
  def of[K: KeyDigest, U <: Unit2D: UnitLabel](
      keys: SourceKeyColumns[K],
      columns: SourceFixationColumns,
      frame: Frame[U],
      timeUnit: SourceTimeUnit,
      policy: AdmissionPolicy[K],
      decision: AdmissionDecision,
      rounding: SourceRounding = SourceRounding.NearestMicrosecond,
      inventory: Option[SourceInventory] = None
  ): Either[ImportSpecError, ImportSpec[K, U]] =
    for
      _ <- checkColumns(keys.names)
      _ <- checkColumns(keys.names ++ columns.names)
      _ <- Either.cond(frame.id.name.trim.nonEmpty, (), ImportSpecError.BlankFrame(frame.id))
      _ <- Either.cond(
        inventory.isEmpty || (keys match
          case SourceKeyColumns.Trial(_, _, _, _, _) => true
          case _                                     => false),
        (),
        ImportSpecError.InventoryKeys(keys)
      )
      _ <- Either.cond(
        inventory.nonEmpty || (keys match
          case SourceKeyColumns.Trial(_, _, _, item, _) => item.nonEmpty
          case _                                        => true),
        (),
        ImportSpecError.MissingItem(keys)
      )
    yield new ImportSpec(keys, columns, frame, timeUnit, rounding, policy, decision, inventory)

  private[plan] def checkColumns(names: Vector[String]): Either[ImportSpecError, Unit] =
    Either.cond(
      names.nonEmpty && names.forall(_.trim.nonEmpty) && names.distinct == names,
      (),
      ImportSpecError.Columns(names)
    )

/** The versioned identities of the built-in fixation and trial-inventory CSV
  * parsers that a saved [[ImportSpec]] names.
  */
object SourceImportDefinitions:
  val fixationParser: DefinitionId  = DefinitionId.builtIn("eyes4s.fixation-csv-parser", 1)
  val inventoryParser: DefinitionId =
    DefinitionId.builtIn("eyes4s.trial-inventory-csv-parser", 1)

/** Why an import declaration was refused, naming the columns, frame or key
  * reader at fault: blank, empty or duplicate column names; a blank frame
  * identity; inventory admission without trial key columns; or fixation-only
  * admission whose key columns lack an item column.
  */
enum ImportSpecError derives CanEqual:
  case Columns(names: Vector[String])
  case BlankFrame(frame: FrameId)
  case InventoryKeys(keys: SourceKeyColumns[?])
  case MissingItem(keys: SourceKeyColumns[?])
  def message: String = this match
    case Columns(names) => s"Import columns $names must be non-empty, non-blank and distinct."
    case BlankFrame(frame)   => s"Import frame '${frame.name}' must have a non-blank identity."
    case InventoryKeys(keys) => s"Inventory admission requires trial columns, found $keys."
    case MissingItem(keys)   =>
      s"Fixation admission without an inventory requires an item column: $keys."

private[plan] object SourceHash:
  def text(value: String): ContentHash                   = ContentHash.ofString(value)
  def sequence(values: Vector[ContentHash]): ContentHash =
    ContentHash.combineAll(text(values.size.toString) +: values)
  def strings(values: Vector[String]): ContentHash      = sequence(values.map(text))
  def optional(value: Option[ContentHash]): ContentHash = sequence(value.toVector)
  def definition(value: DefinitionId): ContentHash      = strings(
    Vector(value.name, value.version.toString)
  )
  def attributes(values: Vector[AttributeColumn]): ContentHash =
    sequence(values.map(v => strings(Vector(v.name, v.kind.toString))))
