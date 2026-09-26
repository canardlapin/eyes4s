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

import eyes4s.kernel.ContentHash

/** Interpretation of the complete decoded source, separate from its byte checksum. */
enum SourceFormat derives CanEqual:
  case FixationCsv, TrialInventoryCsv

/** Earlier ledgers did not record enough information to replay their importer. */
enum SourceInterpretation derives CanEqual:
  case LegacyUnspecified
  case Declared(format: SourceFormat, parser: DefinitionId, options: ContentHash)

/** Versioned semantic identity. Labels, paths and byte checksums are not operands. */
final case class SourceIdentity private (hash: ContentHash) derives CanEqual:
  def digest: String = hash.render

object SourceIdentity:
  val versionTag: String = "eyes4s.source-identity/1"

  def of(
      records: ArtifactRef[Vector[Vector[String]]],
      format: SourceFormat,
      parser: DefinitionId,
      options: ContentHash
  ): SourceIdentity =
    new SourceIdentity(
      ContentHash.combineAll(
        Vector(
          ContentHash.ofString(versionTag),
          ContentHash.ofString(format.toString),
          ContentHash.ofString(parser.name),
          ContentHash.ofString(parser.version.toString),
          ContentHash.ofString(records.digest),
          options
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
  def message: String = this match
    case InvalidDigest(value) =>
      s"Source identity '$value' must be 16 lowercase hexadecimal digits."

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
