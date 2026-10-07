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

import cats.syntax.all.*
import eyes4s.studio.core.backend.AnalysisRevision
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*

/** Why a family identity or ownership registry could not be parsed. */
enum AnalysisFamilyError derives CanEqual:
  case NonPositiveId(value: Int)
  case MissingName(family: AnalysisFamilyId)
  case BlankName(family: AnalysisFamilyId, value: String)
  case NonPositiveRevision(value: AnalysisRevision)
  case DuplicateFamilies(values: Vector[AnalysisFamilyId])
  case DuplicateRevisions(values: Vector[AnalysisRevision])
  case DuplicateExpected(values: Vector[AnalysisRevision])
  case UnknownFamily(
      revision: AnalysisRevision,
      family: AnalysisFamilyId,
      known: Vector[AnalysisFamilyId]
  )
  case UnexpectedRevisions(found: Vector[AnalysisRevision], expected: Vector[AnalysisRevision])
  case MissingRevisions(missing: Vector[AnalysisRevision], expected: Vector[AnalysisRevision])
  case UnownedFamilies(values: Vector[AnalysisFamilyId])
  case ScopeMismatch(held: Vector[AnalysisRevision], requested: Vector[AnalysisRevision])
  case Exhausted(last: AnalysisFamilyId)
  case Wire(field: String, reason: String)

  def message: String = this match
    case NonPositiveId(value)       => s"Analysis family id $value must be positive."
    case MissingName(id)            => s"${id.label} has no name."
    case BlankName(id, value)       => s"${id.label} name '$value' is blank."
    case NonPositiveRevision(value) =>
      s"Family ownership revision ${value.number} must be positive."
    case DuplicateFamilies(values)  => s"Analysis family ids $values are repeated."
    case DuplicateRevisions(values) => s"Family ownership repeats revisions $values."
    case DuplicateExpected(values)  =>
      s"The requested ownership scope repeats revisions $values."
    case UnknownFamily(revision, family, known) =>
      s"${revision.label} names ${family.label}; known families are $known."
    case UnexpectedRevisions(found, expected) =>
      s"Family ownership has unexpected revisions $found; its scope is $expected."
    case MissingRevisions(missing, expected) =>
      s"Family ownership omits revisions $missing from scope $expected."
    case UnownedFamilies(values)        => s"Analysis families $values own no saved revision."
    case ScopeMismatch(held, requested) =>
      s"Family ownership covers $held, not the requested revisions $requested."
    case Exhausted(last)     => s"No positive family id follows ${last.label}."
    case Wire(field, reason) => s"Analysis ownership field $field: $reason"

/** Nominal identity within a project. Names and presets never define identity.
  * Existing global revision and run ids remain unchanged (bead q-analysis-family-identity).
  */
final class AnalysisFamilyId private (val value: Int) derives CanEqual:
  def label: String                        = s"analysis $value"
  override def equals(other: Any): Boolean = other match
    case that: AnalysisFamilyId => value == that.value
    case _                      => false
  override def hashCode(): Int  = value.hashCode
  override def toString: String = s"AnalysisFamilyId($value)"

object AnalysisFamilyId:
  /** The real single family implicit in legacy projects; an empty project has none. */
  val Legacy: AnalysisFamilyId                                      = new AnalysisFamilyId(1)
  def of(value: Int): Either[AnalysisFamilyError, AnalysisFamilyId] =
    Either.cond(
      value > 0,
      new AnalysisFamilyId(value),
      AnalysisFamilyError.NonPositiveId(value)
    )
  given Encoder[AnalysisFamilyId] = Encoder[Int].contramap(_.value)
  given Decoder[AnalysisFamilyId] = Decoder.instance { cursor =>
    cursor.value.asNumber
      .flatMap(_.toInt)
      .toRight(
        DecodingFailure(
          s"Expected a numeric analysis family id, found ${cursor.value.noSpaces}.",
          cursor.history
        )
      )
      .flatMap(value =>
        of(value).leftMap(error => DecodingFailure(error.message, cursor.history))
      )
  }

/** A display name belongs to a nominal family; different families may share it. */
final class AnalysisFamily private (val id: AnalysisFamilyId, val name: String)
    derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case that: AnalysisFamily => id == that.id && name == that.name
    case _                    => false
  override def hashCode(): Int  = (id, name).hashCode
  override def toString: String = s"AnalysisFamily($id,$name)"

object AnalysisFamily:
  def of(id: AnalysisFamilyId, name: String): Either[AnalysisFamilyError, AnalysisFamily] =
    Option(name)
      .toRight(AnalysisFamilyError.MissingName(id))
      .flatMap(value =>
        Either.cond(
          value.trim.nonEmpty,
          new AnalysisFamily(id, value),
          AnalysisFamilyError.BlankName(id, value)
        )
      )
  given Encoder.AsObject[AnalysisFamily] =
    Encoder.forProduct2("id", "name")(f => (f.id, f.name))
  given Decoder[AnalysisFamily] =
    Decoder.forProduct2("id", "name")(of).emap(_.left.map(_.message))

/** One saved revision's owner; its global revision identity is not renumbered. */
final class AnalysisFamilyOwner private (
    val revision: AnalysisRevision,
    val family: AnalysisFamilyId
) derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case that: AnalysisFamilyOwner => revision == that.revision && family == that.family
    case _                         => false
  override def hashCode(): Int  = (revision, family).hashCode
  override def toString: String = s"AnalysisFamilyOwner($revision,$family)"

object AnalysisFamilyOwner:
  def of(
      revision: AnalysisRevision,
      family: AnalysisFamilyId
  ): Either[AnalysisFamilyError, AnalysisFamilyOwner] =
    Either.cond(
      revision.number > 0,
      new AnalysisFamilyOwner(revision, family),
      AnalysisFamilyError.NonPositiveRevision(revision)
    )
  given Encoder.AsObject[AnalysisFamilyOwner] =
    Encoder.forProduct2("revision", "family")(o => (o.revision, o.family))
  given Decoder[AnalysisFamilyOwner] =
    Decoder.forProduct2("revision", "family")(of).emap(_.left.map(_.message))

/** Parsed ownership for an exact set of saved revisions. The universe comes from
  * the document; the wire does not duplicate it as another mutable authority.
  * This foundation does not make explicit registries readable as StudioDocument.
  */
final class AnalysisFamilyRegistry private (
    val families: Vector[AnalysisFamily],
    val owners: Vector[AnalysisFamilyOwner],
    val revisions: Vector[AnalysisRevision]
) derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case that: AnalysisFamilyRegistry =>
      families == that.families && owners == that.owners && revisions == that.revisions
    case _ => false
  override def hashCode(): Int  = (families, owners, revisions).hashCode
  override def toString: String = s"AnalysisFamilyRegistry($families,$owners,$revisions)"
  private val byRevision        = owners.map(o => o.revision -> o.family).toMap
  def familyOf(revision: AnalysisRevision): Option[AnalysisFamilyId] = byRevision.get(revision)
  def family(id: AnalysisFamilyId): Option[AnalysisFamily]           = families.find(_.id == id)
  def checkScope(requested: Vector[AnalysisRevision]): Either[AnalysisFamilyError, Unit] =
    Either.cond(
      requested.sortBy(_.number) == revisions,
      (),
      AnalysisFamilyError.ScopeMismatch(revisions, requested)
    )
  def nextId: Either[AnalysisFamilyError, AnalysisFamilyId] = families.lastOption match
    case None                                        => Right(AnalysisFamilyId.Legacy)
    case Some(last) if last.id.value == Int.MaxValue =>
      Left(AnalysisFamilyError.Exhausted(last.id))
    case Some(last) => AnalysisFamilyId.of(last.id.value + 1)

object AnalysisFamilyRegistry:
  def of(
      families: Vector[AnalysisFamily],
      owners: Vector[AnalysisFamilyOwner],
      revisions: Vector[AnalysisRevision]
  ): Either[AnalysisFamilyError, AnalysisFamilyRegistry] =
    val ids              = families.map(_.id)
    val assigned         = owners.map(_.revision)
    val expected         = revisions.sortBy(_.number)
    val repeatedFamilies = ids.diff(ids.distinct)
    val repeatedOwners   = assigned.diff(assigned.distinct)
    val repeatedExpected = revisions.diff(revisions.distinct)
    val unknownFamily    = owners.find(o => !ids.contains(o.family))
    val unexpected       = assigned.filterNot(expected.contains)
    val missing          = expected.filterNot(assigned.contains)
    val unowned          = ids.filterNot(id => owners.exists(_.family == id))
    for
      _ <- revisions
        .find(_.number <= 0)
        .map(AnalysisFamilyError.NonPositiveRevision(_))
        .toLeft(())
      _ <- Either.cond(
        repeatedFamilies.isEmpty,
        (),
        AnalysisFamilyError.DuplicateFamilies(repeatedFamilies)
      )
      _ <- Either.cond(
        repeatedOwners.isEmpty,
        (),
        AnalysisFamilyError.DuplicateRevisions(repeatedOwners)
      )
      _ <- Either.cond(
        repeatedExpected.isEmpty,
        (),
        AnalysisFamilyError.DuplicateExpected(repeatedExpected)
      )
      _ <- unknownFamily
        .map(o => AnalysisFamilyError.UnknownFamily(o.revision, o.family, ids))
        .toLeft(())
      _ <- Either.cond(
        unexpected.isEmpty,
        (),
        AnalysisFamilyError.UnexpectedRevisions(unexpected, expected)
      )
      _ <- Either.cond(
        missing.isEmpty,
        (),
        AnalysisFamilyError.MissingRevisions(missing, expected)
      )
      _ <- Either.cond(unowned.isEmpty, (), AnalysisFamilyError.UnownedFamilies(unowned))
    yield new AnalysisFamilyRegistry(
      families.sortBy(_.id.value),
      owners.sortBy(_.revision.number),
      expected
    )

  def encode(registry: AnalysisFamilyRegistry): Json = Json.obj(
    "families" -> registry.families.asJson,
    "owners"   -> registry.owners.asJson
  )

  /** Decode vectors before maps, so repeated assignments cannot be silently lost. */
  def decode(
      json: Json,
      revisions: Vector[AnalysisRevision]
  ): Either[AnalysisFamilyError, AnalysisFamilyRegistry] =
    for
      families <- json.hcursor
        .get[Vector[AnalysisFamily]]("families")
        .leftMap(e => AnalysisFamilyError.Wire("families", e.message))
      owners <- json.hcursor
        .get[Vector[AnalysisFamilyOwner]]("owners")
        .leftMap(e => AnalysisFamilyError.Wire("owners", e.message))
      registry <- of(families, owners, revisions)
    yield registry

/** Legacy ownership is a projection, never an emitted member, synthetic saved
  * revision or named-family registry. Rename and preset changes preserve identity.
  */
object LegacyAnalysisFamily:
  def familyOf(document: StudioDocument, revision: AnalysisRevision): Option[AnalysisFamilyId] =
    document.analysisFamilies match
      case Some(_) => document.familyOf(revision)
      case None    =>
        Option.when(
          document.analysis(revision).isDefined || document.draft.exists(_.id == revision)
        )(AnalysisFamilyId.Legacy)
