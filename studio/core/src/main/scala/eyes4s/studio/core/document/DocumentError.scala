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

import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import io.circe.{Codec, Decoder, Encoder}

/** Why a studio document value was refused (ticket S2.1). Every case names
  * the operands that failed, so an application can point at them; `message`
  * is a default English rendering, never an identity.
  */
enum DocumentError derives CanEqual:
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
  case BadPanelLetter(value: String)
  case NoPanels(figure: FigureId)
  case DuplicatePanels(figure: FigureId, letters: Vector[String])
  case DuplicateLayouts(perspectives: Vector[Perspective])

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
  case DraftNotLatest(draft: AnalysisRevision, latest: AnalysisRevision)

  // Cross-references inside a document.
  case UnorderedIds(kind: String, ids: Vector[String])
  case UnknownDataset(referrer: String, dataset: DatasetRevision)
  case UnknownAnalysis(referrer: String, revision: AnalysisRevision)
  case UnknownRun(referrer: String, run: RunId)
  case UnknownReporting(referrer: String, reporting: ReportingId)
  case ParentNotEarlier(dataset: DatasetRevision, parent: DatasetRevision)
  case PanelScaleNotInRun(
      figure: FigureId,
      panel: PanelLetter,
      scale: Sigma,
      run: RunId,
      scales: Vector[Sigma]
  )

  def message: String = this match
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
    case EmptyScales                     => "A recipe needs at least one scale."
    case DuplicateScales(degrees)        => s"Scales repeat: ${degrees.mkString(", ")} degrees."
    case EmptyFilter(attribute)          => s"The filter on $attribute keeps no value."
    case BadPanelLetter(value)           => s"Panel letter '$value' is not one of A to Z."
    case NoPanels(figure)                => s"${figure.label} has no panels."
    case DuplicatePanels(figure, labels) =>
      s"${figure.label} repeats panels ${labels.mkString(", ")}."
    case DuplicateLayouts(perspectives) =>
      s"More than one saved layout for ${perspectives.map(_.label).mkString(", ")}."
    case NoChanges(draft, base) => s"Draft ${draft.label} changes nothing in ${base.label}."
    case RepeatedField(draft, field) =>
      s"Draft ${draft.label} changes the ${field.label} more than once."
    case IdentityChange(draft, field, value) =>
      s"Draft ${draft.label} changes the ${field.label} from $value to itself."
    case DraftBefore(draft, base, field, expected, found) =>
      s"Draft ${draft.label} changes the ${field.label} from $found, but ${base.label} " +
        s"has $expected."
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
