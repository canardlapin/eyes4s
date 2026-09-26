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

import eyes4s.plan.DefinitionId
import io.circe.Json

/** The one mechanism by which a schema has more than one version.
  *
  * A ladder lists consecutive versions `first`, `first + 1`, ... of one schema
  * name. Every version has a writer and a reader with that version's own
  * meaning, and every version after the first has a total '''upcast''': a
  * function from a payload of the previous version to a payload of this
  * version that means the same thing. A version can only be added through
  * [[next]], which requires its upcast and the vocabulary of the version it
  * extends, so no version exists without its lift.
  *
  * The vocabularies are nested: a value the earlier version expresses is also
  * expressed by every later version. [[codec]] writes each value under the
  * '''earliest''' version that expresses it, so a document written by an older
  * release re-encodes to its own bytes and an older release can read every
  * document whose value it can express; it reads every listed version with
  * that version's reader, and refuses any other version as
  * `CodecError.UnsupportedSchema(role, found, versions)`. Decoding never
  * migrates; [[lift]] is the explicit function that rewrites a stored document
  * as the latest version.
  *
  * The published `eyes4s.laws.SchemaLadderLaws` state the contract: the
  * earliest version is the lowest whose own round trip preserves the value,
  * each version reads what it writes, a lifted document is exactly what the
  * later writer writes, and a lifted document decodes to the same value and
  * re-encodes to the earliest document.
  */
final class SchemaLadder[A] private (
    val role: String,
    private val rungs: Vector[SchemaLadder.Rung[A]]
):
  /** Every version, earliest first. */
  val versions: Vector[DefinitionId] = rungs.map(_.id)

  /** The latest version. */
  def latest: DefinitionId = rungs.last.id

  /** The earliest version that expresses `value`. */
  def earliest(value: A): DefinitionId =
    rungs.find(_.expresses(value)).getOrElse(rungs.last).id

  private def rung(version: DefinitionId): Either[CodecError, SchemaLadder.Rung[A]] =
    rungs
      .find(_.id == version)
      .toRight(CodecError.UnsupportedSchema(role, version, versions))

  /** The payload `version`'s own writer writes for `value`, whether or not
    * that version is the one [[codec]] would choose. A version earlier than
    * [[earliest]] cannot express the value, so its payload means something
    * else.
    */
  def writeAt(version: DefinitionId, value: A): Either[CodecError, Json] =
    rung(version).flatMap(_.write(value))

  /** Read a payload with `version`'s own reader and meaning. */
  def readAt(version: DefinitionId, payload: Json): Either[CodecError, A] =
    rung(version).flatMap(_.read(payload))

  /** Lift a payload of `from` to the next version, with the same meaning.
    * The latest version has no next version and is refused.
    */
  def upcast(from: DefinitionId, payload: Json): Either[CodecError, (DefinitionId, Json)] =
    val index = rungs.indexWhere(_.id == from)
    if index < 0 || index == rungs.size - 1 then
      Left(CodecError.UnsupportedSchema(s"$role upcast", from, versions.init))
    else
      val next = rungs(index + 1)
      Right(next.id -> next.upcast(payload))

  /** Lift a payload of `from` through every later version up to `to`. */
  def upcastTo(
      from: DefinitionId,
      to: DefinitionId,
      payload: Json
  ): Either[CodecError, Json] =
    val start = rungs.indexWhere(_.id == from)
    val end   = rungs.indexWhere(_.id == to)
    if start < 0 then Left(CodecError.UnsupportedSchema(role, from, versions))
    else if end < start then Left(CodecError.UnsupportedSchema(role, to, versions.drop(start)))
    else Right(rungs.slice(start + 1, end + 1).foldLeft(payload)((json, r) => r.upcast(json)))

  /** Rewrite a stored document of any listed version as a document of the
    * latest version with the same meaning. The document is first read with
    * its own version's reader, so a document that version refuses is refused
    * here with the same error: lifting never makes an invalid document valid
    * (a later reader's wider vocabulary would otherwise admit it). Decoding
    * the result gives the same value as decoding the original, and [[codec]]
    * re-encodes it as the original earliest document.
    */
  def lift(document: Json): Either[CodecError, Json] = for
    found   <- Wire.definition(document, "schema")
    payload <- Wire.field[Json](document, "value")
    _       <- readAt(found, payload)
    lifted  <- upcastTo(found, latest, payload)
  yield Json.obj("schema" -> Wire.id(latest), "value" -> lifted)

  /** The ladder an older release had: the versions up to `version`. Its
    * codec reads exactly those versions, as that release did.
    */
  def upTo(version: DefinitionId): Either[CodecError, SchemaLadder[A]] =
    val end = rungs.indexWhere(_.id == version)
    if end < 0 then Left(CodecError.UnsupportedSchema(role, version, versions))
    else
      val kept = rungs.take(end + 1)
      Right(new SchemaLadder(role, kept.init :+ kept.last.expressingAll))

  /** Add the next version. `previousExpresses` is the vocabulary of the
    * version being extended (until now the latest, which expressed every
    * value); `upcast` lifts its payloads to the new version; `write` and
    * `read` are the new version's own writer and reader.
    */
  def next(previousExpresses: A => Boolean, upcast: Json => Json)(
      write: A => Either[CodecError, Json]
  )(read: Json => Either[CodecError, A]): SchemaLadder[A] =
    val id = DefinitionId.builtIn(latest.name, latest.version + 1)
    new SchemaLadder(
      role,
      rungs.init :+ rungs.last.copy(expresses = previousExpresses) :+
        SchemaLadder.Rung(id, _ => true, upcast, write, read)
    )

  /** Write each value under its earliest version; read every listed version
    * with its own reader.
    */
  lazy val codec: VersionedCodec[A] =
    VersionedCodec.laddered(this)(value =>
      val chosen = rungs.find(_.expresses(value)).getOrElse(rungs.last)
      chosen.write(value).map(chosen.id -> _)
    )((version, payload) => readAt(version, payload))

object SchemaLadder:
  /** One version: its identity, the values it expresses, the upcast from the
    * previous version (unused for the first) and its own writer and reader.
    */
  private final case class Rung[A](
      id: DefinitionId,
      expresses: A => Boolean,
      upcast: Json => Json,
      write: A => Either[CodecError, Json],
      read: Json => Either[CodecError, A]
  ):
    def expressingAll: Rung[A] = copy(expresses = _ => true)

  /** A ladder with one version, `first`, which expresses every value. */
  def of[A](role: String, first: DefinitionId)(
      write: A => Either[CodecError, Json]
  )(read: Json => Either[CodecError, A]): SchemaLadder[A] =
    new SchemaLadder(role, Vector(Rung(first, _ => true, identity, write, read)))
