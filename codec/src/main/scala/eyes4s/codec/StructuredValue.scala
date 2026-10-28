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

import io.circe.Json

/** Explicit access to a described value. Large arrays go through `rows`; only
  * the requested member or row is materialized by `json`.
  */
private[codec] final class StructuredValue private (val document: CanonicalDoc):
  def json: Either[CodecError, Json] = document match
    case CanonicalDoc.Leaf(value) => Right(value)
    case other                    => other.boundedJson(65536, "structured row or member")

  def isNull: Boolean = document match
    case CanonicalDoc.Deferred(make) => StructuredValue(make()).isNull
    case CanonicalDoc.Leaf(value)    => value.isNull
    case _                           => false

  def member(name: String): Either[CodecError, StructuredValue] = document match
    case CanonicalDoc.Deferred(make) => StructuredValue(make()).member(name)
    case CanonicalDoc.Obj(members)   =>
      members
        .get(name)
        .map(StructuredValue(_))
        .toRight(
          CodecError.Unsupported(
            name,
            s"missing member; present members: ${members.keys.mkString(", ")}"
          )
        )
    case CanonicalDoc.Leaf(value) =>
      Wire.field[Json](value, name).map(j => StructuredValue(CanonicalDoc.Leaf(j)))
    case _: CanonicalDoc.Arr =>
      Left(CodecError.Unsupported(name, "expected an object, found an array"))

  def definition(name: String): Either[CodecError, eyes4s.plan.DefinitionId] =
    member(name)
      .flatMap(_.json)
      .flatMap(value => Wire.definition(Json.obj(name -> value), name))

  def optional(name: String): Either[CodecError, Option[StructuredValue]] = document match
    case CanonicalDoc.Deferred(make) => StructuredValue(make()).optional(name)
    case CanonicalDoc.Obj(members)   => Right(members.get(name).map(StructuredValue(_)))
    case CanonicalDoc.Leaf(value)    =>
      value.asObject
        .map(o => o(name).map(j => StructuredValue(CanonicalDoc.Leaf(j))))
        .toRight(CodecError.Field(name, value, "expected an object"))
    case _: CanonicalDoc.Arr =>
      Left(CodecError.Unsupported(name, "expected an object, found an array"))

  def field[A](name: String)(using memberReader: Wire.Member[A]): Either[CodecError, A] =
    member(name)
      .flatMap(_.json)
      .flatMap(j => memberReader.read(j).left.map(reason => CodecError.Field(name, j, reason)))

  /** Materialize an object's small header while its large members stay described. */
  def header(names: Set[String]): Either[CodecError, Json] = document match
    case CanonicalDoc.Deferred(make) => StructuredValue(make()).header(names)
    case CanonicalDoc.Obj(members)   =>
      CanonicalDoc
        .Obj(members.filterNot((k, _) => names(k)))
        .boundedJson(524288, "structured object header")
    case CanonicalDoc.Leaf(value) =>
      Either.cond(
        value.isObject,
        value,
        CodecError.Field("object", value, "expected an object")
      )
    case _: CanonicalDoc.Arr =>
      Left(CodecError.Unsupported("object header", "expected an object, found an array"))

  def rows[A](path: String)(
      read: (StructuredValue, Int) => Either[CodecError, A]
  ): Either[CodecError, Vector[A]] =
    StructuredRows.readDocument(document, path)((doc, i) => read(StructuredValue(doc), i))

  def size(path: String): Either[CodecError, Int] = StructuredRows.size(document, path)

  def mapRows(path: String)(
      transform: (StructuredValue, Int) => Either[CodecError, CanonicalDoc]
  ): Either[CodecError, CanonicalDoc] =
    StructuredRows.describeMap(document, path)((value, i) =>
      transform(StructuredValue(value), i)
    )

  def replace(name: String, replacement: CanonicalDoc): Either[CodecError, CanonicalDoc] =
    document match
      case CanonicalDoc.Deferred(make) => StructuredValue(make()).replace(name, replacement)
      case CanonicalDoc.Obj(members)   =>
        Either.cond(
          members.contains(name),
          CanonicalDoc.obj(
            members.toVector.map((key, value) =>
              key -> (if key == name then replacement else value)
            )*
          ),
          CodecError.Unsupported(name, "cannot replace a missing member")
        )
      case CanonicalDoc.Leaf(value) =>
        value.asObject
          .filter(_.contains(name))
          .map { members =>
            CanonicalDoc.obj(
              members.toVector.map((key, v) =>
                key -> (if key == name then replacement else CanonicalDoc.Leaf(v))
              )*
            )
          }
          .toRight(CodecError.Field(name, value, "cannot replace a missing member"))
      case _: CanonicalDoc.Arr =>
        Left(CodecError.Unsupported(name, "cannot replace an object member in an array"))

private[codec] object StructuredValue:
  private[codec] def apply(document: CanonicalDoc): StructuredValue = new StructuredValue(
    document
  )
