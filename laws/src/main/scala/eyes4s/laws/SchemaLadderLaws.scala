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

package eyes4s.laws

import eyes4s.codec.{CodecError, SchemaLadder}
import eyes4s.plan.DefinitionId
import io.circe.Json
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Published conformance for a multi-version schema: the version policy of
  * `eyes4s.codec.SchemaLadder`, stated over generated values.
  *
  * Write the vN payload of a value `x` as `write_N(x)`, read it as `read_N`,
  * and lift a vN payload to vN+1 with `upcast_N`. For every value `x` whose
  * ladder codec writes it as version `E`:
  *
  *   - '''earliest''': `read_N(write_N(x))` is `x` exactly for `N >= E`, and
  *     is not `x` for every `N < E`, so `E` is the lowest version whose
  *     vocabulary expresses `x` and the vocabularies are nested;
  *   - '''old readers''': the ladder cut at `E`, which is what the release
  *     that introduced `E` could read, decodes the written document to `x`;
  *   - '''upcast''': for every later version `M`, `upcast^(M-E)(write_E(x))`
  *     is exactly `write_M(x)`, so `read_M(upcast^(M-E)(write_E(x)))` is `x`;
  *   - '''canonical''': the document lifted to the latest version decodes to
  *     `x` and re-encodes to the earliest document, so writing is idempotent;
  *   - '''refusal''': a document the codec refuses stays refused after
  *     lifting, so `lift` never turns an invalid document into a valid one.
  *     `edits` supplies the invalid documents: each edit is applied to a
  *     written document, and [[SchemaLadderLaws.payloadEdits]] are generic
  *     ones; pass edits that reach vocabulary a version does not have.
  */
trait SchemaLadderLaws extends Laws:
  def ladder[A](
      ladder: SchemaLadder[A],
      gen: Gen[A],
      equivalent: (A, A) => Boolean,
      edits: Gen[Json => Json] = SchemaLadderLaws.payloadEdits
  ): RuleSet =
    val codec                                                       = ladder.codec
    def written(value: A): Either[CodecError, (DefinitionId, Json)] = for
      document <- codec.encode(value)
      schema   <- document.hcursor
        .get[Json]("schema")
        .left
        .map(e => CodecError.Field("schema", document, e.message))
      name <- schema.hcursor
        .get[String]("name")
        .left
        .map(e => CodecError.Field("name", schema, e.message))
      version <- schema.hcursor
        .get[Int]("version")
        .left
        .map(e => CodecError.Field("version", schema, e.message))
      id <- ladder.versions
        .find(v => v.name == name && v.version == version)
        .toRight(CodecError.UnsupportedSchema(ladder.role, ladder.latest, ladder.versions))
    yield id -> document
    def expressedAt(version: DefinitionId, value: A): Boolean =
      ladder
        .writeAt(version, value)
        .flatMap(ladder.readAt(version, _))
        .exists(equivalent(value, _))
    new SimpleRuleSet(
      "schemaLadder",
      "a value is written under the earliest version that expresses it" -> forAll(gen) {
        value =>
          written(value).exists { (chosen, _) =>
            ladder.versions.forall(v => expressedAt(v, value) == (v.version >= chosen.version))
          }
      },
      "a document written as vN decodes with a vN-only reader" -> forAll(gen) { value =>
        written(value).exists { (chosen, document) =>
          ladder
            .upTo(chosen)
            .flatMap(_.codec.decode(document))
            .exists(equivalent(value, _))
        }
      },
      "upcasting a vN payload writes exactly what each later version writes" -> forAll(gen) {
        value =>
          written(value).exists { (chosen, _) =>
            ladder.writeAt(chosen, value).exists { payload =>
              ladder.versions.filter(_.version >= chosen.version).forall { later =>
                val lifted = ladder.upcastTo(chosen, later, payload)
                lifted.map(_.noSpaces) == ladder.writeAt(later, value).map(_.noSpaces) &&
                lifted.flatMap(ladder.readAt(later, _)).exists(equivalent(value, _))
              }
            }
          }
      },
      "a lifted document decodes to the value and re-encodes to the earliest document" ->
        forAll(gen) { value =>
          written(value).exists { (_, document) =>
            val decoded = ladder.lift(document).flatMap(codec.decode)
            decoded.exists(equivalent(value, _)) &&
            decoded.flatMap(codec.encode).map(_.noSpaces) == Right(document.noSpaces)
          }
        },
      "a refused document stays refused after lifting" -> forAll(gen, edits) { (value, edit) =>
        codec.encode(value).exists { document =>
          val edited = edit(document)
          codec.decode(edited).isRight || ladder.lift(edited).flatMap(codec.decode).isLeft
        }
      }
    )
object SchemaLadderLaws extends SchemaLadderLaws:
  /** Generic damage to a written document's payload: one top-level member
    * deleted, set to `null`, to a string or to an empty array.
    */
  val payloadEdits: Gen[Json => Json] =
    for
      pick        <- Gen.choose(0, 63)
      replacement <- Gen.oneOf(
        Option.empty[Json],
        Some(Json.Null),
        Some(Json.fromString("x")),
        Some(Json.arr())
      )
    yield (document: Json) =>
      document.hcursor
        .downField("value")
        .withFocus(_.mapObject { members =>
          members.keys.toVector.lift(pick % math.max(1, members.size)) match
            case None      => members
            case Some(key) => replacement.fold(members.remove(key))(members.add(key, _))
        })
        .top
        .getOrElse(document)
