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

package eyes4s.studio.core.document

import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, JobId, RunId}
import io.circe.{Codec, Decoder, Encoder}

/** Why a studio document value was refused (ticket S2.1). Every case names
  * the operands that failed, so an application can point at them; `message`
  * is a default English rendering, never an identity.
  */
enum DocumentError derives CanEqual:
  case ExhaustedAnalysisRevision(last: AnalysisRevision)
  case FamilyDraftRevision(draft: AnalysisRevision, expected: AnalysisRevision)
  case FamilyDraftIdentity(found: AnalysisFamilyId, expected: AnalysisFamilyId)
  case FamilyOwnership(error: AnalysisFamilyError)
  case InitialDraftWithFamilies(draft: AnalysisRevision, families: Vector[AnalysisFamilyId])
  case Blank(field: String)
  case BadPath(path: String, reason: String)
  case BadDigest(field: String, value: String, reason: String)
  case BadSemanticIdentity(value: String)
  case MissingSource(role: SourceRole, present: Vector[SourceRole])
  case DuplicateSource(role: SourceRole, paths: Vector[String])
  case SharedPath(path: String, roles: Vector[SourceRole])
  case MissingColumnRoles(missing: Vector[ColumnRole])
  case DuplicateColumnRole(role: ColumnRole, columns: Vector[String])
  case SharedColumn(column: String, roles: Vector[ColumnRole])
  case NotPositive(field: String, value: Double)
  case NotFinite(field: String, value: Double)
  case OutOfRange(field: String, value: Double, lower: Double, upper: Double)
  case PlacementOffScreen(placement: ImagePlacement, screen: ScreenSize)
  case EmptyScales
  case DuplicateScales(degrees: Vector[Double])
  case EmptyFilter(attribute: String)
  case ContrastOperands(minuend: String, subtrahend: String)
  case ContrastWithoutGrouping(
      reporting: String,
      minuend: Option[String],
      subtrahend: Option[String]
  )
  case BadPanelLetter(value: String)
  case NoPanels(figure: FigureId)
  case DuplicatePanels(figure: FigureId, letters: Vector[String])
  case DuplicateLayouts(perspectives: Vector[Perspective])
  case BadDefinition(name: String, version: Int)
  case EmptyWindow(xMin: Double, yMin: Double, xMax: Double, yMax: Double)
  case BadSchemaId(name: String, version: Int)
  case RepeatedAttribute(column: String)
  case AttributeIsMapped(dataset: DatasetRevision, column: String, role: ColumnRole)

  // A trial inventory's mapping (S5.4).
  case MissingInventoryRoles(missing: Vector[ColumnRole])
  case InventoryRoleNotRead(column: String, role: ColumnRole)
  case InventoryAttributeIsMapped(column: String, role: ColumnRole)
  case DurationColumnShared(column: String, mappedAs: String)
  case DurationColumnKind(column: String, kind: AttributeKindChoice)

  /** `dataset` maps a trial inventory but has no trials source. */
  case InventoryWithoutTrials(dataset: DatasetRevision)

  /** Rules `first` and `second` (0-based) of `dataset`'s corrections both
    * cover some trial; eyes4s refuses a policy in which they do (S5.5).
    */
  case CorrectionsOverlap(dataset: DatasetRevision, first: Int, second: Int)

  /** `dataset` has a trials source whose columns are not mapped. */
  case InventoryUnmapped(dataset: DatasetRevision, path: String)

  /** `dataset`'s fixations and trial inventory name a different trial key:
    * `role` is mapped in `mappedIn` only (S5.4 follow-up).
    */
  case InventoryKeyDisagrees(
      dataset: DatasetRevision,
      role: ColumnRole,
      mappedIn: String,
      notIn: String
  )

  /** An inventory's display column is also a role's or an attribute's. */
  case DisplayColumnMapped(column: String, mappedAs: String)

  /** The display kind and image file are read from one column. */
  case DisplayColumnsShared(column: String)

  /** A repaired asset of a dataset revision the document does not hold. */
  case RelinkUnknownDataset(dataset: DatasetRevision, file: String)

  /** One inventory file of one dataset revision repaired twice. */
  case DuplicateRelink(dataset: DatasetRevision, file: String)

  // A draft against its base revision.
  case NoChanges(draft: AnalysisRevision, base: AnalysisRevision)
  case RepeatedField(draft: AnalysisRevision, field: RecipeField)
  case IdentityChange(draft: AnalysisRevision, field: RecipeField, value: String)
  case DraftBefore(
      draft: AnalysisRevision,
      base: AnalysisRevision,
      field: RecipeField,
      expected: String,
      found: String
  )
  case DraftSeedBefore(
      draft: AnalysisRevision,
      field: RecipeField,
      expected: String,
      found: String
  )
  case InitialPresetNotHeld(draft: AnalysisRevision, preset: Preset)
  case InitialDraftWithAnalyses(draft: AnalysisRevision, saved: Vector[AnalysisRevision])
  case InitialDraftId(draft: AnalysisRevision, expected: AnalysisRevision)
  case DraftNotLatest(draft: AnalysisRevision, latest: AnalysisRevision)
  case RebaseToSame(draft: AnalysisRevision, dataset: DatasetRevision)
  case RebaseNotAdmitted(draft: AnalysisRevision, dataset: DatasetRevision)

  // Cross-references inside a document.
  case UnorderedIds(kind: String, ids: Vector[String])
  case UnknownDataset(referrer: String, dataset: DatasetRevision)
  case UnknownAnalysis(referrer: String, revision: AnalysisRevision)
  case UnknownRun(referrer: String, run: RunId)
  case UnknownReporting(referrer: String, reporting: ReportingId)
  case JobNotRunning(run: RunId, job: JobId)
  case DuplicateJobs(run: RunId, jobs: Vector[JobId])
  case ParentNotEarlier(dataset: DatasetRevision, parent: DatasetRevision)
  case PanelScaleNotInRun(
      figure: FigureId,
      panel: PanelLetter,
      scale: Sigma,
      run: RunId,
      scales: Vector[Sigma]
  )

  case PresetWithoutDefaults(preset: Preset, dataset: DatasetRevision)

  def message: String = this match
    case ExhaustedAnalysisRevision(last) =>
      s"No positive analysis revision follows ${last.label}."
    case FamilyDraftRevision(draft, expected) =>
      s"New-family draft ${draft.label} must use next global identity ${expected.label}."
    case FamilyDraftIdentity(found, expected) =>
      s"Provisional ${found.label} must use next family identity ${expected.label}."
    case FamilyOwnership(error)                    => error.message
    case InitialDraftWithFamilies(draft, families) =>
      s"Initial ${draft.label} requires implicit legacy ownership; explicit families are $families."
    case PresetWithoutDefaults(preset, dataset) =>
      s"Preset $preset declares no initial recipe for ${dataset.label}."
    case Blank(field)           => s"$field is blank."
    case BadPath(path, reason)  => s"Source path '$path' is refused: $reason."
    case BadDigest(f, v, r)     => s"$f '$v' is not a SHA-256 digest: $r"
    case BadSemanticIdentity(v) =>
      s"Semantic identity '$v' is not an eyes4s artifact reference (16 lowercase hex digits)."
    case MissingSource(role, present) =>
      s"No ${role.label} source; the dataset has ${present.map(_.label).mkString(", ")}."
    case DuplicateSource(role, paths) =>
      s"More than one ${role.label} source: ${paths.mkString(", ")}."
    case SharedPath(path, roles) =>
      s"Source path $path serves more than one role: ${roles.map(_.label).mkString(", ")}."
    case MissingColumnRoles(missing) =>
      s"The column mapping has no ${missing.map(_.label).mkString(", ")} column."
    case DuplicateColumnRole(role, columns) =>
      s"The ${role.label} role is mapped to more than one column: ${columns.mkString(", ")}."
    case SharedColumn(column, roles) =>
      s"Column $column is mapped to more than one role: ${roles.map(_.label).mkString(", ")}."
    case NotPositive(field, value)        => s"$field is $value; it must be positive."
    case NotFinite(field, value)          => s"$field is $value; it must be finite."
    case OutOfRange(field, value, lo, hi) => s"$field is $value; it must lie in [$lo, $hi]."
    case PlacementOffScreen(p, screen)    =>
      s"Image placement ${p.render} does not lie inside the ${screen.render} screen."
    case EmptyScales              => "A recipe needs at least one scale."
    case DuplicateScales(degrees) => s"Scales repeat: ${degrees.mkString(", ")} degrees."
    case EmptyFilter(attribute)   => s"The filter on $attribute keeps no value."
    case ContrastOperands(minuend, subtrahend) =>
      s"Contrast operands '$minuend' and '$subtrahend' must be distinct."
    case ContrastWithoutGrouping(reporting, minuend, subtrahend) =>
      s"Reporting spec $reporting contrasts $minuend minus $subtrahend but has no grouping covariate."
    case BadPanelLetter(value)           => s"Panel letter '$value' is not one of A to Z."
    case NoPanels(figure)                => s"${figure.label} has no panels."
    case DuplicatePanels(figure, labels) =>
      s"${figure.label} repeats panels ${labels.mkString(", ")}."
    case DuplicateLayouts(perspectives) =>
      s"More than one saved layout for ${perspectives.map(_.label).mkString(", ")}."
    case BadDefinition(name, version) =>
      s"Definition $name@$version is not a valid eyes4s definition identity."
    case EmptyWindow(x0, y0, x1, y1) =>
      s"Analysis window [$x0, $x1) × [$y0, $y1) is empty."
    case RepeatedAttribute(column) => s"Attribute column $column is declared more than once."
    case AttributeIsMapped(dataset, column, role) =>
      s"Dataset ${dataset.label}: column $column is both an attribute and the ${role.label} column."
    case MissingInventoryRoles(missing) =>
      s"The trial inventory mapping has no ${missing.map(_.label).mkString(", ")} column."
    case InventoryRoleNotRead(column, role) =>
      s"Trial inventory column $column: the ${role.label} role is not read from a trial inventory."
    case DurationColumnShared(column, mappedAs) =>
      s"Trial duration column '$column' is also mapped as $mappedAs."
    case DurationColumnKind(column, kind) =>
      s"Trial duration column '$column' is a $kind attribute; retain its exact source text as Text."
    case InventoryAttributeIsMapped(column, role) =>
      s"Trial inventory column $column is both an attribute and the ${role.label} column."
    case InventoryWithoutTrials(dataset) =>
      s"Dataset ${dataset.label} maps a trial inventory but has no trial inventory source."
    case CorrectionsOverlap(dataset, first, second) =>
      s"Dataset ${dataset.label}: correction rules ${first + 1} and ${second + 1} both cover " +
        "a trial; at most one rule may cover a trial."
    case InventoryUnmapped(dataset, path) =>
      s"Dataset ${dataset.label}: the columns of trial inventory $path are not mapped."
    case InventoryKeyDisagrees(dataset, role, mappedIn, notIn) =>
      s"Dataset ${dataset.label}: ${role.label} is mapped in $mappedIn but not in $notIn; " +
        "both files must name the trial by the same key."
    case BadSchemaId(name, version) =>
      s"Studio schema identity $name@$version is not a valid definition identity."
    case RebaseToSame(draft, dataset) =>
      s"Draft ${draft.label} rebases onto ${dataset.label}, which its base already uses."
    case RebaseNotAdmitted(draft, dataset) =>
      s"Draft ${draft.label} rebases onto ${dataset.label}, which is not admitted."
    case JobNotRunning(run, job) =>
      s"Job ${job.number} is recorded for ${run.label}, which is not running."
    case DuplicateJobs(run, jobs) =>
      s"${run.label} has more than one job: ${jobs.map(_.number).mkString(", ")}."
    case NoChanges(draft, base) => s"Draft ${draft.label} changes nothing in ${base.label}."
    case RepeatedField(draft, field) =>
      s"Draft ${draft.label} changes the ${field.label} more than once."
    case IdentityChange(draft, field, value) =>
      s"Draft ${draft.label} changes the ${field.label} from $value to itself."
    case DraftBefore(draft, base, field, expected, found) =>
      s"Draft ${draft.label} changes the ${field.label} from $found, but ${base.label} " +
        s"has $expected."
    case DraftSeedBefore(draft, field, expected, found) =>
      s"Initial ${draft.label} ${field.label} starts at $found, but its seed holds $expected."
    case InitialPresetNotHeld(draft, preset) =>
      s"Initial ${draft.label}'s seed recipe does not hold preset $preset."
    case InitialDraftWithAnalyses(draft, saved) =>
      s"Initial ${draft.label} cannot coexist with saved analyses ${saved.map(_.label).mkString(", ")}."
    case InitialDraftId(draft, expected) =>
      s"Initial ${draft.label} must use first identity ${expected.label}."
    case DraftNotLatest(draft, latest) =>
      s"Draft ${draft.label} must come after the latest analysis revision, ${latest.label}."
    case UnorderedIds(kind, ids) =>
      s"The ${kind}s are not in strictly ascending order: ${ids.mkString(", ")}."
    case UnknownDataset(referrer, dataset) =>
      s"$referrer names dataset ${dataset.label}, which is not in the document."
    case UnknownAnalysis(referrer, revision) =>
      s"$referrer names analysis ${revision.label}, which is not in the document."
    case UnknownRun(referrer, run) =>
      s"$referrer names ${run.label}, which is not in the document."
    case UnknownReporting(referrer, reporting) =>
      s"$referrer names reporting spec ${reporting.value}, which is not in the document."
    case ParentNotEarlier(dataset, parent) =>
      s"Dataset ${dataset.label} names ${parent.label} as its parent, which is not earlier."
    case DisplayColumnMapped(column, role) =>
      s"Inventory column '$column' is a display column and also the $role."
    case DisplayColumnsShared(column) =>
      s"Inventory column '$column' is both the display kind and the image file."
    case RelinkUnknownDataset(dataset, file) =>
      s"Asset $file is repaired for dataset ${dataset.label}, which the document does not hold."
    case DuplicateRelink(dataset, file) =>
      s"Asset $file of dataset ${dataset.label} is repaired more than once."
    case PanelScaleNotInRun(figure, panel, scale, run, scales) =>
      s"${figure.label} panel ${panel.value} shows ${scale.render}, but ${run.label} has " +
        s"${scales.map(_.render).mkString(", ")}."

/** Codec helpers shared by the document types. */
private[document] object DocumentCodecs:
  /** A validated single-field wrapper encoded as its field; decoding runs the
    * smart constructor again, so a stored document cannot hold a value the
    * constructor refuses.
    */
  def validated[A, B: Encoder: Decoder](
      of: B => Either[DocumentError, A],
      unwrap: A => B
  ): Codec[A] =
    Codec.from(
      Decoder[B].emap(b => of(b).left.map(_.message)),
      Encoder[B].contramap(unwrap)
    )

  /** Ids that must be strictly ascending, rendered for an error. */
  def ascending[A](kind: String, values: Vector[A])(key: A => Int)(
      render: A => String
  ): Either[DocumentError, Unit] =
    val keys = values.map(key)
    Either.cond(
      keys.zip(keys.drop(1)).forall(_ < _),
      (),
      DocumentError.UnorderedIds(kind, values.map(render))
    )
