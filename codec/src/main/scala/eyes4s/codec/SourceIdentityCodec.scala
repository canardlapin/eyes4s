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

import eyes4s.kernel.ContentHash
import eyes4s.plan.*
import io.circe.Json

object SourceCodecDefinitions:
  val admissionLedgerV4: DefinitionId = DefinitionId.builtIn("eyes4s.admission-ledger", 4)
  val sourceRef: DefinitionId         = DefinitionId.builtIn("eyes4s.source-ref", 1)
  val importSpec: DefinitionId        = DefinitionId.builtIn("eyes4s.import-spec", 1)
  val inventorySpec: DefinitionId     = DefinitionId.builtIn("eyes4s.inventory-import-spec", 1)

/** Declared evidence round-trips without asserting that replay took place. */
object SourceIdentityCodec:
  val source: VersionedCodec[SourceRef] =
    VersionedCodec.checked[SourceRef](SourceCodecDefinitions.sourceRef)(s => Right(write(s)))(
      read
    )

  private[codec] def ledgerSource(source: SourceRef): Json =
    if source.interpretation == SourceInterpretation.LegacyUnspecified then
      Json.obj(
        "label"   -> Json.fromString(source.label),
        "records" -> Json.fromString(source.records.digest)
      )
    else write(source)

  private[codec] def liftLegacy(json: Json): Json =
    json.mapObject(
      _.add("interpretation", Json.obj("kind" -> Json.fromString("legacyUnspecified")))
    )

  private[codec] def write(source: SourceRef): Json =
    val evidence = source.interpretation match
      case SourceInterpretation.LegacyUnspecified =>
        Json.obj("kind" -> Json.fromString("legacyUnspecified"))
      case SourceInterpretation.Declared(format, parser, options) =>
        Json.obj(
          "kind"            -> Json.fromString("declared"),
          "format"          -> Json.fromString(format.toString),
          "parser"          -> Wire.id(parser),
          "options"         -> Json.fromString(options.render),
          "identityVersion" -> Json.fromString(SourceIdentity.versionTag),
          "identity"        -> Json.fromString(
            SourceIdentity.of(source.records, format, parser, options).digest
          )
        )
    Json.obj(
      "label"          -> Json.fromString(source.label),
      "records"        -> Json.fromString(source.records.digest),
      "interpretation" -> evidence
    )

  private[codec] def read(json: Json): Either[CodecError, SourceRef] = for
    label   <- Wire.field[String](json, "label")
    records <- Wire
      .field[String](json, "records")
      .flatMap(
        ArtifactRef.parse[Vector[Vector[String]]](_).left.map(CodecError.Definition.apply)
      )
    evidence <- Wire.field[Json](json, "interpretation")
    kind     <- Wire.field[String](evidence, "kind")
    source   <- kind match
      case "legacyUnspecified" => Right(SourceRef(label, records))
      case "declared"          =>
        for
          name   <- Wire.field[String](evidence, "format")
          format <- SourceFormat.values
            .find(_.toString == name)
            .toRight(CodecError.Field("format", evidence, s"unknown source format $name"))
          parser      <- Wire.definition(evidence, "parser")
          optionsText <- Wire.field[String](evidence, "options")
          options     <- ContentHash
            .parse(optionsText)
            .toRight(CodecError.Field("options", evidence, "expected a content digest"))
          tag <- Wire.field[String](evidence, "identityVersion")
          _   <- Either.cond(
            tag == SourceIdentity.versionTag,
            (),
            CodecError.Field(
              "identityVersion",
              evidence,
              s"expected ${SourceIdentity.versionTag}, found $tag"
            )
          )
          identity <- Wire.field[String](evidence, "identity")
          found = SourceIdentity.of(records, format, parser, options)
          _ <- Either.cond(
            identity == found.digest,
            (),
            CodecError
              .Field("identity", evidence, s"declared $identity, computed ${found.digest}")
          )
        yield SourceRef(label, records, SourceInterpretation.Declared(format, parser, options))
      case other =>
        Left(CodecError.Field("kind", evidence, s"unknown source interpretation $other"))
  yield source
