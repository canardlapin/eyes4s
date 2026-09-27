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

package eyes4s.studio.core.importing

import cats.data.NonEmptyVector
import eyes4s.plan.AttributeColumn
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.document.{
  AttributeBinding,
  InventoryMapping,
  AttributeKindChoice,
  ColumnBinding,
  ColumnMapping,
  ColumnName,
  ColumnRole,
  DeclaredAttributes,
  DeclaredUnits,
  DocumentError,
  TimeUnit
}

/** What one source column is to the import: a role eyes4s reads, or an
  * attribute passed through unchanged (UI-H). There is no "ignore": a column
  * without a role is kept as an attribute, never dropped.
  */
enum ColumnChoice derives CanEqual:
  case Role(role: ColumnRole)
  case Attribute

  def roleOption: Option[ColumnRole] = this match
    case Role(r)   => Some(r)
    case Attribute => None

object ColumnChoice:
  /** Every choice a column offers, in the board's order. */
  val all: Vector[ColumnChoice] = ColumnRole.values.toVector.map(Role(_)) :+ Attribute

/** What kind of value a role's column holds; a sampled value of another kind
  * suggests the role is on the wrong column.
  */
enum ValueKind derives CanEqual:
  /** Any non-blank text. */
  case Label

  /** A non-negative decimal integer. */
  case Count

  /** A finite decimal number. */
  case Number

  def requirement: String = this match
    case Label  => "a non-blank label"
    case Count  => "a whole number"
    case Number => "a finite decimal number"

  /** Whether a non-blank `value` is of this kind. Blank cells are data (a
    * rejected record), not a sign of a wrong column, and are not checked.
    */
  def admits(value: String): Boolean = this match
    case Label  => value.trim.nonEmpty
    case Count  => value.trim.matches("[0-9]+")
    case Number =>
      value.trim.matches("[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?") &&
      value.trim.toDoubleOption.exists(_.isFinite)

object ValueKind:
  def of(role: ColumnRole): ValueKind = role match
    case ColumnRole.Participant | ColumnRole.Phase | ColumnRole.Trial | ColumnRole.Item |
        ColumnRole.Response =>
      Label
    case ColumnRole.Occurrence | ColumnRole.Ordinal | ColumnRole.SampleCount  => Count
    case ColumnRole.X | ColumnRole.Y | ColumnRole.Onset | ColumnRole.Duration => Number

/** Where a mapping re-applied to a file came from. */
enum MappingOrigin derives CanEqual:
  case Preset(name: PresetName)
  case Dataset(revision: DatasetRevision)

  def label: String = this match
    case Preset(name)      => s"preset ${name.value}"
    case Dataset(revision) => s"dataset ${revision.label}"

/** Why a column mapping cannot be imported. Every case names the column (or,
  * for a role no column plays, the columns that could play it) and the file.
  */
enum MappingError derives CanEqual:
  /** No column plays required `role`; `unassigned` are the attribute columns. */
  case MissingRole(file: String, role: ColumnRole, unassigned: Vector[ColumnName])

  /** More than one column plays `role`. */
  case RepeatedRole(file: String, role: ColumnRole, columns: Vector[ColumnName])

  /** `column`'s sampled `value` in `record` is not of the kind `role` reads. */
  case ValueMismatch(
      file: String,
      column: ColumnName,
      role: ColumnRole,
      record: Int,
      value: String,
      kind: ValueKind
  )

  /** Onset and duration are mapped but their time unit is not declared. */
  case TimeUnitUndeclared(file: String, onset: ColumnName, duration: ColumnName)

  /** A mapping re-applied from `origin` binds `role` to `column`, which the
    * file's header does not have.
    */
  case ColumnAbsent(file: String, origin: MappingOrigin, column: ColumnName, role: ColumnRole)

  /** A choice names `column`, which the file's header does not have. */
  case UnknownColumn(file: String, column: ColumnName)

  /** `role` is not read from this kind of file (a position in trials.csv). */
  case RoleNotRead(file: String, column: ColumnName, role: ColumnRole)

  /** The file has a header and no records: there is nothing to import. */
  case NoRecords(file: String)

  /** The document refused the mapping (a defect in the checks above). */
  case Refused(file: String, error: DocumentError)

  def message: String = this match
    case MissingRole(file, role, unassigned) =>
      val candidates =
        if unassigned.isEmpty then "every column already has a role"
        else s"unassigned columns: ${unassigned.map(_.value).mkString(", ")}"
      s"$file: no column has the required ${role.label} role ($candidates)."
    case RepeatedRole(file, role, columns) =>
      s"$file: columns ${columns.map(_.value).mkString(", ")} all have the ${role.label} " +
        "role; choose one."
    case ValueMismatch(file, column, role, record, value, kind) =>
      s"$file column ${column.value} (${role.label}): record $record holds '$value', " +
        s"not ${kind.requirement}."
    case TimeUnitUndeclared(file, onset, duration) =>
      s"$file columns ${onset.value} and ${duration.value}: declare their time unit; " +
        "it is never inferred."
    case ColumnAbsent(file, origin, column, role) =>
      s"$file has no column ${column.value}, which ${origin.label} maps to ${role.label}."
    case UnknownColumn(file, column)     => s"$file has no column ${column.value}."
    case RoleNotRead(file, column, role) =>
      s"$file column ${column.value}: ${role.label} is not read from this file."
    case Refused(file, error) => s"$file: ${error.message}"
    case NoRecords(file) => s"$file has a header but no records; there is nothing to import."

  /** The column the error points at, when it names one. */
  def pointsAt: Option[ColumnName] = this match
    case ValueMismatch(_, c, _, _, _, _)                     => Some(c)
    case TimeUnitUndeclared(_, onset, _)                     => Some(onset)
    case ColumnAbsent(_, _, c, _)                            => Some(c)
    case UnknownColumn(_, c)                                 => Some(c)
    case RoleNotRead(_, c, _)                                => Some(c)
    case RepeatedRole(_, _, cs)                              => cs.headOption
    case MissingRole(_, _, _) | Refused(_, _) | NoRecords(_) => None

/** A mapping ready to import: the document's column mapping and declared
  * units, and the eyes4s attribute declarations of every other column
  * (UI-H: only declared columns become attributes, so each one is declared,
  * as text kept as written).
  */
final case class ResolvedMapping(
    mapping: ColumnMapping,
    units: DeclaredUnits,
    attributes: DeclaredAttributes
) derives CanEqual

/** Suggested roles from header names (ticket S5.2). A suggestion only: the
  * analyst confirms or changes every role, and no unit is ever suggested.
  */
object RoleProposals:

  private def norm(name: String): String = name.toLowerCase.filter(_.isLetterOrDigit)

  private val names: Map[String, ColumnRole] =
    import ColumnRole.*
    Vector(
      Participant -> Vector("participant", "subject", "subj", "sub", "pid", "participantid"),
      Phase       -> Vector("phase", "stage"),
      Trial       -> Vector("trial", "trialid", "triallabel", "trialname"),
      Occurrence  -> Vector("occurrence", "block", "repeat", "repetition"),
      Item        -> Vector("item", "image", "stimulus", "stim", "picture"),
      Response    -> Vector("response", "resp"),
      Ordinal     -> Vector("ordinal", "fixnum", "fixationnumber", "fixindex", "fixation"),
      SampleCount -> Vector("samplecount", "nsamples", "samples", "numsamples"),
      X           -> Vector("x", "fixx", "gazex", "posx", "xpos"),
      Y           -> Vector("y", "fixy", "gazey", "posy", "ypos"),
      Onset       -> Vector("onset", "onsetms", "fixstart", "start", "starttime", "onsetus"),
      Duration    -> Vector("duration", "durationms", "fixdur", "dur", "durationus")
    ).flatMap((role, ns) => ns.map(_ -> role)).toMap

  /** The suggested choice for each column, in header order. A role goes to
    * the first column whose name suggests it; any other column is an
    * attribute.
    */
  def propose(header: Vector[ColumnName]): Vector[ColumnChoice] =
    header
      .foldLeft((Vector.empty[ColumnChoice], Set.empty[ColumnRole])) {
        case ((acc, taken), column) =>
          names.get(norm(column.value)).filterNot(taken) match
            case Some(role) => (acc :+ ColumnChoice.Role(role), taken + role)
            case None       => (acc :+ ColumnChoice.Attribute, taken)
      }
      ._1

/** A fixation file's column mapping while it is edited: the file's preview,
  * each column's choice in header order, the declared time unit, and the
  * declared kind of attribute columns that are not text (from a re-mapped
  * revision; a new attribute is text, kept as written).
  */
final case class MappingDraft private (
    preview: CsvPreview,
    choices: Vector[ColumnChoice],
    time: Option[TimeUnit],
    kinds: Map[ColumnName, AttributeKindChoice] = Map.empty
) derives CanEqual:

  def file: String = preview.file

  def columns: Vector[(PreviewColumn, ColumnChoice)] = preview.columns.zip(choices)

  def choice(column: ColumnName): Option[ColumnChoice] =
    columns.collectFirst { case (c, choice) if c.name == column => choice }

  /** The columns that play `role`, in header order. */
  def columnsFor(role: ColumnRole): Vector[ColumnName] =
    columns.collect { case (c, ColumnChoice.Role(r)) if r == role => c.name }

  def choose(column: ColumnName, choice: ColumnChoice): Either[MappingError, MappingDraft] =
    val at = preview.header.indexOf(column)
    Either.cond(
      at >= 0,
      copy(choices = choices.updated(at, choice)),
      MappingError.UnknownColumn(file, column)
    )

  def declare(unit: Option[TimeUnit]): MappingDraft = copy(time = unit)

  /** Every reason the mapping cannot be imported yet, in a stable order:
    * a file with no records, missing and repeated roles in role order, then
    * the time unit, then sampled values in header order.
    */
  def issues: Vector[MappingError] =
    val undeclared =
      (
        columnsFor(ColumnRole.Onset).headOption,
        columnsFor(ColumnRole.Duration).headOption
      ) match
        case (Some(o), Some(d)) if time.isEmpty =>
          Vector(MappingError.TimeUnitUndeclared(file, o, d))
        case _ => Vector.empty
    val (roles, values) =
      RoleChecks.issues(file, columns, ColumnRole.required, ColumnRole.values.toVector)
    val empty = Option.when(preview.records == 0)(MappingError.NoRecords(file)).toVector
    empty ++ roles ++ undeclared ++ values

  /** The mapping, its declared units and the attribute declarations, or every
    * issue.
    */
  def resolve: Either[NonEmptyVector[MappingError], ResolvedMapping] =
    NonEmptyVector.fromVector(issues) match
      case Some(errors) => Left(errors)
      case None         =>
        val bindings = columns.collect { case (c, ColumnChoice.Role(r)) =>
          ColumnBinding(r, c.name)
        }
        val attributes = columns.collect { case (c, ColumnChoice.Attribute) =>
          AttributeBinding(c.name, kinds.getOrElse(c.name, AttributeKindChoice.Text))
        }
        // The checks above are ColumnMapping.of's and DeclaredAttributes.of's
        // (header names are distinct); a refusal here would be a defect in
        // them, reported as the document's own error.
        (for
          mapping  <- ColumnMapping.of(bindings)
          declared <- DeclaredAttributes.of(attributes)
        yield ResolvedMapping(mapping, DeclaredUnits(time), declared)).left
          .map(e => NonEmptyVector.one(MappingError.Refused(file, e)))

  /** Bind this draft's roles and time unit as a preset named `name`. */
  def preset(name: PresetName): Either[PresetError, ImportPreset] =
    ImportPreset.of(
      name,
      columns.collect { case (c, ColumnChoice.Role(r)) => ColumnBinding(r, c.name) },
      time
    )

object MappingDraft:

  /** A new file's draft: suggested roles, no time unit declared. */
  def proposed(preview: CsvPreview): MappingDraft =
    MappingDraft(preview, RoleProposals.propose(preview.header), None)

  /** `bindings` and `time` re-applied to `preview` by column name: every
    * bound column must be in the header; every other column is an attribute.
    */
  def reapply(
      preview: CsvPreview,
      origin: MappingOrigin,
      bindings: Vector[ColumnBinding],
      time: Option[TimeUnit]
  ): Either[NonEmptyVector[MappingError], MappingDraft] =
    val absent = bindings.filterNot(b => preview.header.contains(b.column))
    NonEmptyVector.fromVector(
      absent.map(b => MappingError.ColumnAbsent(preview.file, origin, b.column, b.role))
    ) match
      case Some(errors) => Left(errors)
      case None         =>
        val choices = preview.header.map(name =>
          bindings
            .find(_.column == name)
            .fold(ColumnChoice.Attribute)(b => ColumnChoice.Role(b.role))
        )
        Right(MappingDraft(preview, choices, time))

  /** A dataset revision's mapping and units re-applied to its file. */
  def ofDataset(
      preview: CsvPreview,
      revision: DatasetRevision,
      mapping: ColumnMapping,
      units: DeclaredUnits,
      attributes: DeclaredAttributes
  ): Either[NonEmptyVector[MappingError], MappingDraft] =
    reapply(preview, MappingOrigin.Dataset(revision), mapping.bindings, units.time)
      .map(_.copy(kinds = attributes.bindings.map(a => a.column -> a.kind).toMap))

/** The role checks fixation and trial mappings share. */
private[importing] object RoleChecks:

  /** Role issues (missing required roles, then roles on more than one
    * column, then roles this file does not offer, all in role order) and
    * value issues (the first sampled value of the wrong kind per column, in
    * header order).
    */
  def issues(
      file: String,
      columns: Vector[(PreviewColumn, ColumnChoice)],
      required: Vector[ColumnRole],
      offered: Vector[ColumnRole]
  ): (Vector[MappingError], Vector[MappingError]) =
    def columnsFor(role: ColumnRole) =
      columns.collect { case (c, ColumnChoice.Role(r)) if r == role => c.name }
    val unassigned = columns.collect { case (c, ColumnChoice.Attribute) => c.name }
    val missing    =
      required.filter(columnsFor(_).isEmpty).map(MappingError.MissingRole(file, _, unassigned))
    val repeated = ColumnRole.values.toVector.flatMap { role =>
      val cs = columnsFor(role)
      Option.when(cs.size > 1)(MappingError.RepeatedRole(file, role, cs))
    }
    val unread = columns.collect {
      case (c, ColumnChoice.Role(r)) if !offered.contains(r) =>
        MappingError.RoleNotRead(file, c.name, r)
    }
    val values = columns.flatMap {
      case (c, ColumnChoice.Role(role)) =>
        val kind = ValueKind.of(role)
        c.samples.zipWithIndex.collectFirst {
          case (v, i) if v.trim.nonEmpty && !kind.admits(v) =>
            MappingError.ValueMismatch(file, c.name, role, i + 1, v, kind)
        }
      case _ => None
    }
    (missing ++ repeated ++ unread, values)

/** A trials.csv inventory's column mapping while it is edited ("Trial
  * metadata" tab): the trial identity (participant, phase, trial and,
  * optionally, occurrence, as eyes4s `TrialColumns` requires), an optional
  * item and response, and every other column as an attribute, text kept as
  * written unless a re-mapped revision declared its kind. It resolves to the
  * document's [[InventoryMapping]] (S5.4), which eyes4s joins fixation
  * records to.
  */
final case class TrialMetadataDraft private (
    preview: CsvPreview,
    choices: Vector[ColumnChoice],
    kinds: Map[ColumnName, AttributeKindChoice] = Map.empty
) derives CanEqual:

  def file: String = preview.file

  def columns: Vector[(PreviewColumn, ColumnChoice)] = preview.columns.zip(choices)

  def choose(
      column: ColumnName,
      choice: ColumnChoice
  ): Either[MappingError, TrialMetadataDraft] =
    val at = preview.header.indexOf(column)
    if at < 0 then Left(MappingError.UnknownColumn(file, column))
    else
      choice.roleOption.filterNot(TrialMetadataDraft.offered.contains) match
        case Some(role) => Left(MappingError.RoleNotRead(file, column, role))
        case None       => Right(copy(choices = choices.updated(at, choice)))

  def issues: Vector[MappingError] =
    val (roles, values) =
      RoleChecks.issues(file, columns, TrialMetadataDraft.required, TrialMetadataDraft.offered)
    roles ++ values

  /** The columns passed through as trial attributes (UI-H). */
  def attributes: Vector[AttributeColumn] =
    columns.collect { case (c, ColumnChoice.Attribute) =>
      AttributeColumn(c.name.value, kinds.getOrElse(c.name, AttributeKindChoice.Text).core)
    }

  /** The inventory mapping, or every issue. */
  def resolve: Either[NonEmptyVector[MappingError], InventoryMapping] =
    NonEmptyVector.fromVector(issues) match
      case Some(errors) => Left(errors)
      case None         =>
        val bindings = columns.collect { case (c, ColumnChoice.Role(r)) =>
          ColumnBinding(r, c.name)
        }
        val declared = columns.collect { case (c, ColumnChoice.Attribute) =>
          AttributeBinding(c.name, kinds.getOrElse(c.name, AttributeKindChoice.Text))
        }
        // The checks above are InventoryMapping.of's; a refusal here would be
        // a defect in them, reported as the document's own error.
        DeclaredAttributes
          .of(declared)
          .flatMap(InventoryMapping.of(bindings, _))
          .left
          .map(e => NonEmptyVector.one(MappingError.Refused(file, e)))

object TrialMetadataDraft:
  val required: Vector[ColumnRole] = InventoryMapping.required

  val offered: Vector[ColumnRole] = InventoryMapping.offered

  /** A dataset revision's inventory mapping re-applied to its trials file by
    * column name: every bound column must be in the header; every other
    * column is an attribute, of the kind the revision declared.
    */
  def ofDataset(
      preview: CsvPreview,
      revision: DatasetRevision,
      mapping: InventoryMapping
  ): Either[NonEmptyVector[MappingError], TrialMetadataDraft] =
    val origin = MappingOrigin.Dataset(revision)
    val absent = mapping.bindings.filterNot(b => preview.header.contains(b.column))
    NonEmptyVector.fromVector(
      absent.map(b => MappingError.ColumnAbsent(preview.file, origin, b.column, b.role))
    ) match
      case Some(errors) => Left(errors)
      case None         =>
        val choices = preview.header.map(name =>
          mapping.bindings
            .find(_.column == name)
            .fold(ColumnChoice.Attribute)(b => ColumnChoice.Role(b.role))
        )
        Right(
          TrialMetadataDraft(
            preview,
            choices,
            mapping.attributes.bindings.map(a => a.column -> a.kind).toMap
          )
        )

  /** Suggested roles, restricted to those a trials table offers. */
  def proposed(preview: CsvPreview): TrialMetadataDraft =
    TrialMetadataDraft(
      preview,
      RoleProposals
        .propose(preview.header)
        .map(c => if c.roleOption.forall(offered.contains) then c else ColumnChoice.Attribute)
    )
