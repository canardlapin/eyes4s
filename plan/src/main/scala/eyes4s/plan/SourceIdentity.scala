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

import eyes4s.kernel.{ContentHash, Unit2D}

/** Interpretation of the complete decoded source, separate from its byte checksum. */
enum SourceFormat derives CanEqual:
  case FixationCsv, TrialInventoryCsv

/** The schema whose typed options produced a declaration's options digest:
  * `eyes4s.import-spec@1` or `eyes4s.inventory-import-spec@1`.
  */
enum SourceOptionsSchema derives CanEqual:
  case FixationCsvV1, TrialInventoryCsvV1

/** Earlier ledgers did not record enough information to replay their importer. */
sealed trait SourceInterpretation derives CanEqual

object SourceInterpretation:
  case object LegacyUnspecified extends SourceInterpretation

  /** Construction checks the format, supported parser and options schema together. */
  final class Declared private[SourceInterpretation] (
      val format: SourceFormat,
      val parser: DefinitionId,
      val options: ContentHash
  ) extends SourceInterpretation:
    def optionsSchema: SourceOptionsSchema   = schemaFor(format)
    override def equals(other: Any): Boolean = other match
      case that: Declared =>
        format == that.format && parser == that.parser && options == that.options
      case _ => false
    override def hashCode: Int    = (format, parser, options).hashCode
    override def toString: String = s"Declared($format,$parser,$optionsSchema,$options)"

  object Declared:
    def unapply(value: Declared): (SourceFormat, DefinitionId, ContentHash) =
      (value.format, value.parser, value.options)

  private def parserFor(format: SourceFormat): DefinitionId = format match
    case SourceFormat.FixationCsv       => SourceImportDefinitions.fixationParser
    case SourceFormat.TrialInventoryCsv => SourceImportDefinitions.inventoryParser

  private def schemaFor(format: SourceFormat): SourceOptionsSchema = format match
    case SourceFormat.FixationCsv       => SourceOptionsSchema.FixationCsvV1
    case SourceFormat.TrialInventoryCsv => SourceOptionsSchema.TrialInventoryCsvV1

  def declared(
      format: SourceFormat,
      parser: DefinitionId,
      optionsSchema: SourceOptionsSchema,
      options: ContentHash
  ): Either[SourceIdentityError, Declared] =
    for
      _ <- Either.cond(
        parser == parserFor(format),
        (),
        SourceIdentityError.Parser(format, parserFor(format), parser)
      )
      _ <- Either.cond(
        optionsSchema == schemaFor(format),
        (),
        SourceIdentityError.OptionsSchema(format, schemaFor(format), optionsSchema)
      )
    yield new Declared(format, parser, options)

  /** A typed fixation description supplies compatible schema evidence directly. */
  def fixation[K, U <: Unit2D](spec: ImportSpec[K, U]): Declared =
    new Declared(SourceFormat.FixationCsv, SourceImportDefinitions.fixationParser, spec.digest)

  /** A typed inventory description supplies compatible schema evidence directly. */
  def inventory(spec: InventoryImportSpec): Declared =
    new Declared(
      SourceFormat.TrialInventoryCsv,
      SourceImportDefinitions.inventoryParser,
      spec.digest
    )

/** Versioned semantic identity. Labels, paths and byte checksums are not operands. */
final case class SourceIdentity private (hash: ContentHash) derives CanEqual:
  def digest: String = hash.render

object SourceIdentity:
  val versionTag: String = "eyes4s.source-identity/1"

  def of(
      records: ArtifactRef[Vector[Vector[String]]],
      interpretation: SourceInterpretation.Declared
  ): SourceIdentity =
    new SourceIdentity(
      ContentHash.combineAll(
        Vector(
          ContentHash.ofString(versionTag),
          ContentHash.ofString(interpretation.format.toString),
          ContentHash.ofString(interpretation.parser.name),
          ContentHash.ofString(interpretation.parser.version.toString),
          ContentHash.ofString(records.digest),
          interpretation.options
        )
      )
    )

  def parse(value: String): Either[SourceIdentityError, SourceIdentity] =
    ContentHash
      .parse(value)
      .map(new SourceIdentity(_))
      .toRight(SourceIdentityError.InvalidDigest(value))

enum SourceIdentityError derives CanEqual:
  case InvalidDigest(value: String)
  case Parser(format: SourceFormat, expected: DefinitionId, actual: DefinitionId)
  case OptionsSchema(
      format: SourceFormat,
      expected: SourceOptionsSchema,
      actual: SourceOptionsSchema
  )
  def message: String = this match
    case InvalidDigest(value) =>
      s"Source identity '$value' must be 16 lowercase hexadecimal digits."
    case Parser(format, expected, actual) =>
      s"Source format $format requires parser $expected; found $actual."
    case OptionsSchema(format, expected, actual) =>
      s"Source format $format requires options schema $expected; found $actual."

/** The actual identity components that differ, or missing legacy evidence. */
enum IdentityChange derives CanEqual:
  case Format, Parser, Options, Records, Undeclared

/** A non-empty set: construction always requires at least one cause. */
final case class IdentityChanges private (head: IdentityChange, rest: Set[IdentityChange])
    derives CanEqual:
  def values: Set[IdentityChange] = rest + head

object IdentityChanges:
  def of(head: IdentityChange, rest: Set[IdentityChange] = Set.empty): IdentityChanges =
    val values = rest + head
    val first  = values.minBy(_.ordinal)
    new IdentityChanges(first, values - first)

/** Identity is checked before bytes. Equal bytes under changed options are stale. */
enum SourceComparison derives CanEqual:
  /** Both declared identity components and byte checksums agree. */
  case SameBytes

  /** All declared identity components agree, but byte checksums differ. */
  case SameIdentity

  /** The non-empty, exact set of differences; bytes cannot override these. */
  case ChangedIdentity(causes: IdentityChanges)

object SourceComparison:
  def of(sameBytes: Boolean, expected: SourceRef, actual: SourceRef): SourceComparison =
    val records =
      Option.when(expected.records != actual.records)(IdentityChange.Records).toVector
    val interpretation = (expected.interpretation, actual.interpretation) match
      case (
            SourceInterpretation.Declared(fa, pa, oa),
            SourceInterpretation.Declared(fb, pb, ob)
          ) =>
        Vector(
          Option.when(fa != fb)(IdentityChange.Format),
          Option.when(pa != pb)(IdentityChange.Parser),
          Option.when(oa != ob)(IdentityChange.Options)
        ).flatten
      case _ => Vector(IdentityChange.Undeclared)
    (records ++ interpretation) match
      case head +: tail =>
        SourceComparison.ChangedIdentity(IdentityChanges.of(head, tail.toSet))
      case _ if sameBytes => SourceComparison.SameBytes
      case _              => SourceComparison.SameIdentity
