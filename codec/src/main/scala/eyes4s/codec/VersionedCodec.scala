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

package eyes4s.codec

import cats.syntax.all.*
import eyes4s.core.{DetectionSupportError, RecordingError}
import eyes4s.design.{ReconstructionError, Trial, Trials}
import eyes4s.kernel.SyncEvidenceError
import eyes4s.plan.*
import io.circe.{Json, Decoder}

/** Decoding never loads artifacts or resolves behavior implicitly. */
enum CodecError derives CanEqual:
  case InvalidJson(input: String, reason: String)
  case Field(path: String, input: Json, reason: String)
  case Schema(expected: DefinitionId, found: DefinitionId)
  case Definition(underlying: PlanError)
  case DuplicateKeys(schema: DefinitionId, indices: Vector[Int])
  case MissingIdentity(kind: String, id: String)
  case IdentityConflict(kind: String, id: String, existing: Json, incoming: Json)
  case MissingMethod(method: DefinitionId)
  case DuplicateMethod(method: DefinitionId)
  case MissingKeySchema(schema: DefinitionId)
  case DuplicateKeySchema(schema: DefinitionId)
  case Entry(path: String, underlying: CodecError)
  case Unsupported(path: String, reason: String)
  case InputIdentity(declared: String, reconstructed: String)
  case Admission(underlying: AdmissionError)
  // UI-S3: recording and temporal input payloads.
  case Recording(path: String, underlying: RecordingError)
  case Support(path: String, underlying: DetectionSupportError)
  case Synchronization(path: String, underlying: SyncEvidenceError)
  case Input(underlying: RecordingInputError)
  case Temporal(underlying: TemporalStudyError)
  case SampleBound(path: String, samples: Int, maximum: Int)
  case SynchronizationFit(
      path: String,
      declaredOffsetMicros: Long,
      declaredDrift: Double,
      refitOffsetMicros: Long,
      refitDrift: Double
  )
  // UI-S4: result archives.
  case Reconstruction(underlying: ReconstructionError[?])
  case Result(underlying: StudyResultError[?])
  case ScoreComponents(expected: Vector[String], found: Vector[String])
  case MissingResultCodec(method: DefinitionId)
  case DuplicateResultCodec(method: DefinitionId)
  // UI-S5: artifact manifests and typed payload references.
  case Payload(path: String, underlying: PayloadError)
  case MissingPayload(path: String, reference: PayloadRef)
  case Manifest(underlying: ManifestError)
  case Text(offset: Int, reason: String)
  case UnsupportedSchema(role: String, found: DefinitionId, supported: Vector[DefinitionId])
  // Recording and temporal result archives.
  /** An archived member the archive's own evidence re-derives differs. */
  case Derived(path: String, declared: Json, derived: Json)
  case RecordingResult(underlying: RecordingResultError)
  case TemporalResult(underlying: TemporalResultError[?])

  /** A second spelling of a value the writer spells one way: `found` at
    * `path` decodes, but the writer writes `canonical`, as `rule` says.
    */
  case NonCanonical(path: String, found: Json, canonical: Json, rule: String)
  // UI-C: report documents.
  case Report(underlying: eyes4s.results.ReportError[?])
  case ReportSpec(underlying: eyes4s.results.SpecError)
  case Covariates(underlying: eyes4s.results.CovariateError[?])
  // CR4 S3: repetition result archives.
  /** The archive's run stamp is not the stamp of the plan it carries. */
  case Stamp(underlying: RunStampError[?, ?])

  /** The archive's analyses are not the ones its repetition plan computes. */
  case RepetitionResult(underlying: RepetitionPlanError)

  def message: String = this match
    case InvalidJson(_, reason)     => s"Invalid project JSON: $reason"
    case Field(path, input, reason) => s"Cannot decode $path from ${input.noSpaces}: $reason"
    case Schema(expected, found)    =>
      s"Expected schema ${expected.name}@${expected.version}, found ${found.name}@${found.version}."
    case Definition(e)             => e.message
    case DuplicateKeys(s, indices) =>
      s"Schema ${s.name}@${s.version} has duplicate keys at entries $indices."
    case MissingIdentity(kind, id) => s"No $kind with nominal ID '$id' in this document."
    case IdentityConflict(kind, id, old, incoming) =>
      s"Conflicting $kind ID '$id': ${old.noSpaces} versus ${incoming.noSpaces}."
    case MissingMethod(id)      => s"No registered method ${id.name}@${id.version}."
    case DuplicateMethod(id)    => s"Method ${id.name}@${id.version} is already registered."
    case MissingKeySchema(id)   => s"No registered key schema ${id.name}@${id.version}."
    case DuplicateKeySchema(id) =>
      s"Key schema ${id.name}@${id.version} is already registered."
    case Entry(path, underlying)                => s"At $path: ${underlying.message}"
    case Unsupported(path, reason)              => s"Cannot encode $path: $reason"
    case InputIdentity(declared, reconstructed) =>
      s"Payload declares input $declared but its trials reconstruct $reconstructed."
    case Admission(e) => e.message
    // UI-S3
    case Recording(path, e)                  => s"Cannot decode $path: ${e.message}"
    case Support(path, e)                    => s"Cannot decode $path: ${e.message}"
    case Synchronization(path, e)            => s"Cannot decode $path: ${e.message}"
    case Input(e)                            => e.message
    case Temporal(e)                         => e.message
    case SampleBound(path, samples, maximum) =>
      s"Cannot carry $path inline: $samples samples exceed the payload bound of $maximum."
    case SynchronizationFit(path, declaredOffset, declaredDrift, refitOffset, refitDrift) =>
      s"$path declares offsetMicros=$declaredOffset drift=$declaredDrift but the observed marks " +
        s"refit to offsetMicros=$refitOffset drift=$refitDrift."
    // UI-S4
    case Reconstruction(e)                => e.message
    case Result(e)                        => e.message
    case ScoreComponents(expected, found) =>
      s"Archived score components $found differ from the method's components $expected."
    case MissingResultCodec(id) =>
      s"No registered result codec for method ${id.name}@${id.version}."
    case DuplicateResultCodec(id) =>
      s"Result codec for method ${id.name}@${id.version} is already registered."
    // UI-S5
    case Payload(path, e)          => s"At $path: ${e.message}"
    case MissingPayload(path, ref) =>
      s"$path references payload ${ref.sha256.hex} (${ref.layout.element.wire}" +
        s"${ref.layout.shape.mkString("[", ",", "]")}), which is not available."
    case Manifest(e)          => e.message
    case Text(offset, reason) => s"Artifact text is not strict UTF-8 at offset $offset: $reason"
    case UnsupportedSchema(role, found, supported) =>
      if supported.isEmpty then
        s"No $role decoder is registered, so schema ${found.name}@${found.version} is refused."
      else
        s"No $role decoder for schema ${found.name}@${found.version}; supported: " +
          supported.map(id => s"${id.name}@${id.version}").mkString(", ") + "."
    case Derived(path, declared, derived) =>
      s"Archived $path is ${declared.noSpaces}, but the archive's own evidence derives " +
        s"${derived.noSpaces}."
    case RecordingResult(e)                         => e.message
    case TemporalResult(e)                          => e.message
    case NonCanonical(path, found, canonical, rule) =>
      s"$path is not in canonical form ($rule): found ${found.noSpaces}, " +
        s"written as ${canonical.noSpaces}."
    case Report(e)           => e.message
    case ReportSpec(e)       => e.message
    case Covariates(e)       => e.message
    case Stamp(e)            => e.message
    case RepetitionResult(e) => e.message

/** A typed, explicitly versioned codec. Unsupported old versions fail precisely.
  * The wire envelope separates schema identity from any method identity in its payload.
  *
  * `schema` is the latest version the codec writes. A codec built from a
  * [[SchemaLadder]] (its `ladder`) also reads the ladder's earlier versions,
  * each with its own original meaning, and writes each value under the
  * earliest version that expresses it; `schemas` lists every version it reads.
  */
final class VersionedCodec[A] private (
    val schema: DefinitionId,
    val schemas: Vector[DefinitionId],
    write: A => Either[CodecError, (DefinitionId, Json)],
    read: (DefinitionId, Json) => Either[CodecError, A],
    role: Option[String],
    val ladder: Option[SchemaLadder[A]]
):
  def encode(value: A): Either[CodecError, Json] =
    write(value).map { case (id, payload) =>
      Json.obj("schema" -> Wire.id(id), "value" -> payload)
    }
  def decode(json: Json): Either[CodecError, A] = for
    found <- Wire.definition(json, "schema")
    _     <- Either.cond(
      schemas.contains(found),
      (),
      role.fold(CodecError.Schema(schema, found))(
        CodecError.UnsupportedSchema(_, found, schemas)
      )
    )
    payload <- Wire.field[Json](json, "value")
    result  <- read(found, payload)
  yield result

  /** The collision-resistant identity of `value`: the SHA-256 of its
    * canonical document (see [[CanonicalDigest]]).
    */
  def digest(value: A): Either[CodecError, CanonicalDigest[A]] =
    encode(value).flatMap(CanonicalDigest.document)
  def parse(input: String): Either[CodecError, A] =
    io.circe.parser
      .parse(input)
      .left
      .map(e => CodecError.InvalidJson(input, e.message))
      .flatMap(decode)

object VersionedCodec:
  def of[A](schema: DefinitionId)(write: A => Json)(
      read: Json => Either[CodecError, A]
  ): VersionedCodec[A] =
    new VersionedCodec(
      schema,
      Vector(schema),
      value => Right(schema -> write(value)),
      (_, json) => read(json),
      None,
      None
    )

  def string(schema: DefinitionId): VersionedCodec[String] = of(schema)(Json.fromString)(json =>
    json.asString.toRight(CodecError.Field("string", json, "expected a string"))
  )

  def unit(schema: DefinitionId): VersionedCodec[Unit] =
    of[Unit](schema)(_ => Json.obj())(json =>
      Either.cond(
        json.asObject.exists(_.isEmpty),
        (),
        CodecError.Field("unit", json, "expected an empty object")
      )
    )

  def checked[A](schema: DefinitionId)(write: A => Either[CodecError, Json])(
      read: Json => Either[CodecError, A]
  ): VersionedCodec[A] =
    new VersionedCodec(
      schema,
      Vector(schema),
      value => write(value).map(schema -> _),
      (_, json) => read(json),
      None,
      None
    )

  /** The codec of a [[SchemaLadder]]: `write` chooses each value's version
    * and payload, `read` decodes a payload of any of the ladder's versions.
    */
  private[codec] def laddered[A](ladder: SchemaLadder[A])(
      write: A => Either[CodecError, (DefinitionId, Json)]
  )(
      read: (DefinitionId, Json) => Either[CodecError, A]
  ): VersionedCodec[A] =
    new VersionedCodec(
      ladder.latest,
      ladder.versions,
      write,
      read,
      Some(ladder.role),
      Some(ladder)
    )

  /** Entry-array encoding preserves arbitrary typed keys; it is written in
    * ascending key order and refuses duplicates and any other order.
    */
  def entries[K: Ordering, V](
      schema: DefinitionId,
      key: VersionedCodec[K],
      value: VersionedCodec[V]
  ): VersionedCodec[Map[K, V]] =
    checked(schema)((entries: Map[K, V]) =>
      entries.toVector
        .sortBy(_._1)
        .traverse { case (k, v) =>
          for
            encodedKey   <- key.encode(k)
            encodedValue <- value.encode(v)
          yield Json.obj("key" -> encodedKey, "value" -> encodedValue)
        }
        .map(rows => Json.arr(rows*))
    ) { json =>
      for
        rows   <- json.asArray.toRight(CodecError.Field("entries", json, "expected an array"))
        result <- rows.toVector.traverse { row =>
          for
            k <- Wire.field[Json](row, "key").flatMap(key.decode)
            v <- Wire.field[Json](row, "value").flatMap(value.decode)
          yield k -> v
        }
        duplicates = result.zipWithIndex.collect {
          case ((k, _), i) if result.count(_._1 == k) > 1 => i
        }
        _ <- Either.cond(duplicates.isEmpty, (), CodecError.DuplicateKeys(schema, duplicates))
        _ <- Wire.ascending("entries", result.map(_._1).zip(rows.toVector))
      yield result.toMap
    }

  /** Row-array encoding of typed trials: input order and repeated keys are
    * preserved, so identity stays in `K` and never in a JSON field name.
    */
  def trials[K, M, A](
      schema: DefinitionId,
      key: VersionedCodec[K],
      meta: VersionedCodec[M],
      value: VersionedCodec[A]
  ): VersionedCodec[Trials[K, M, A]] =
    checked(schema)((trials: Trials[K, M, A]) =>
      trials.rows.zipWithIndex
        .traverse { case (row, index) =>
          (for
            k <- key.encode(row.key)
            m <- meta.encode(row.meta)
            v <- value.encode(row.value)
          yield Json.obj("key" -> k, "meta" -> m, "value" -> v)).left
            .map(Wire.at(s"rows[$index]"))
        }
        .map(rows => Json.arr(rows*))
    ) { json =>
      for
        rows   <- json.asArray.toRight(CodecError.Field("rows", json, "expected an array"))
        result <- rows.toVector.zipWithIndex.traverse { case (row, index) =>
          (for
            k <- Wire.field[Json](row, "key").flatMap(key.decode)
            m <- Wire.field[Json](row, "meta").flatMap(meta.decode)
            v <- Wire.field[Json](row, "value").flatMap(value.decode)
          yield Trial(k, m, v)).left.map(Wire.at(s"rows[$index]"))
        }
      yield Trials(result)
    }

/** Small decoding primitives; errors retain the path and offending JSON value. */
private[codec] object Wire:
  /** Read member `name` in its one canonical spelling (see [[Member]]). */
  def field[A](json: Json, name: String)(using member: Member[A]): Either[CodecError, A] =
    json.hcursor
      .downField(name)
      .focus
      .toRight("missing member")
      .flatMap(member.read)
      .left
      .map(reason => CodecError.Field(name, json, reason))

  /** Read a member its writer omits when there is no value: absence is
    * `None`, and a `null` is refused as a second spelling of absence.
    */
  def omittable[A](json: Json, name: String)(using
      member: Member[A]
  ): Either[CodecError, Option[A]] =
    json.hcursor.downField(name).focus match
      case None                => Right(None)
      case Some(v) if v.isNull =>
        Left(
          CodecError.NonCanonical(
            name,
            json,
            json.mapObject(_.remove(name)),
            "an absent value is omitted, not null"
          )
        )
      case Some(v) => member.read(v).map(Some(_)).left.map(r => CodecError.Field(name, json, r))

  /** Refuse `entries` unless their keys are in the writer's (ascending)
    * order; the canonical spelling carries the same members sorted.
    */
  def ascending[A](path: String, entries: Vector[(A, Json)])(using
      order: Ordering[A]
  ): Either[CodecError, Unit] =
    val ordered = entries.zip(entries.drop(1)).forall((a, b) => order.lteq(a._1, b._1))
    Either.cond(
      ordered,
      (),
      CodecError.NonCanonical(
        path,
        Json.arr(entries.map(_._2)*),
        Json.arr(entries.sortBy(_._1).map(_._2)*),
        "members are written in ascending order"
      )
    )

  /** How a member value is read. Members have one spelling each: a number
    * is a JSON number, never a numeric string or `null`, and an integer is
    * spelled as an integer (`1`, not `1.0` or `1e0`); an `Option` member is
    * present, with `null` meaning no value. Other types read through circe.
    */
  trait Member[A]:
    def read(value: Json): Either[String, A]

  object Member extends LowPriorityMember:
    private def number(value: Json): Either[String, io.circe.JsonNumber] =
      value.asNumber.toRight(s"expected a JSON number, got ${value.noSpaces}")

    given Member[Double] = value => number(value).map(_.toDouble)

    given Member[Int] = value =>
      number(value).flatMap(n =>
        n.toInt
          .filter(_.toString == n.toString)
          .toRight(s"expected an integer spelled as one, got ${value.noSpaces}")
      )

    /** Beyond 2^53 a JSON number is refused on every platform: Scala.js
      * parses numbers into doubles, so it cannot tell such an integer from
      * its neighbours. Integers that large are written as decimal strings.
      */
    given Member[Long] = value =>
      number(value).flatMap(n =>
        n.toLong
          .filter(_.toString == n.toString)
          .toRight(s"expected an integer spelled as one, got ${value.noSpaces}")
          .filterOrElse(
            l => l <= (1L << 53) && l >= -(1L << 53),
            s"an integer beyond 2^53 is written as a decimal string, got ${value.noSpaces}"
          )
      )

    given [A](using inner: Member[A]): Member[Vector[A]] = value =>
      value.asArray
        .toRight(s"expected an array, got ${value.noSpaces}")
        .flatMap(_.zipWithIndex.traverse((v, i) => inner.read(v).left.map(r => s"[$i]: $r")))

    given [A](using inner: Member[A]): Member[Option[A]] = value =>
      if value.isNull then Right(None) else inner.read(value).map(Some(_))

  /** The fallback to circe's decoder for member types without a single-spelling rule
    * above; lower priority so those rules always win.
    */
  trait LowPriorityMember:
    given [A](using decoder: Decoder[A]): Member[A] = value =>
      decoder.decodeJson(value).left.map(_.message)

  /** The members of `base` followed by those of `later`, in that order. */
  def append(base: Json, later: Json): Json =
    Json.fromFields(
      base.asObject.toVector.flatMap(_.toVector) ++ later.asObject.toVector.flatMap(_.toVector)
    )

  /** Locate an error at a containing entry; nested locations join into one path. */
  def at(path: String)(error: CodecError): CodecError = error match
    case CodecError.Entry(inner, underlying) => CodecError.Entry(s"$path.$inner", underlying)
    case other                               => CodecError.Entry(path, other)
  def requireId(json: Json, name: String, expected: DefinitionId): Either[CodecError, Unit] =
    definition(json, name).flatMap(found =>
      Either.cond(found == expected, (), CodecError.Schema(expected, found))
    )
  def id(value: DefinitionId): Json =
    Json.obj("name" -> Json.fromString(value.name), "version" -> Json.fromInt(value.version))
  def definition(json: Json, name: String): Either[CodecError, DefinitionId] = for
    value   <- field[Json](json, name)
    id      <- field[String](value, "name")
    version <- field[Int](value, "version")
    result  <- DefinitionId.of(id, version).left.map(CodecError.Definition.apply)
  yield result
