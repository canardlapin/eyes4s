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
import eyes4s.design.KeyDigest
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** Pure codecs for the descriptions Studio builds and io replays. */
object ImportSpecCodec:
  private def str(value: String): Json                              = Json.fromString(value)
  private def opt(value: Option[String]): Json                      = value.fold(Json.Null)(str)
  private def error(json: Json, value: ImportSpecError): CodecError =
    CodecError.Field("importSpec", json, value.message)
  private def attributes(values: Vector[AttributeColumn]): Json =
    Json.arr(values.map(v => Json.obj("name" -> str(v.name), "kind" -> str(v.kind.toString)))*)
  private def readAttributes(
      json: Json,
      field: String
  ): Either[CodecError, Vector[AttributeColumn]] =
    Wire
      .field[Vector[Json]](json, field)
      .flatMap(_.traverse { value =>
        for
          name <- Wire.field[String](value, "name")
          kind <- enumValue(value, "kind", AttributeKind.values)
        yield AttributeColumn(name, kind)
      })
  private def enumValue[A](json: Json, field: String, values: Array[A]): Either[CodecError, A] =
    Wire
      .field[String](json, field)
      .flatMap(name =>
        values
          .find(_.toString == name)
          .toRight(CodecError.Field(field, json, s"unknown value $name"))
      )

  val inventory: VersionedCodec[InventoryImportSpec] =
    VersionedCodec.checked[InventoryImportSpec](SourceCodecDefinitions.inventorySpec)(v =>
      Right(
        Json.obj(
          "participant" -> str(v.participant),
          "phase"       -> str(v.phase),
          "trial"       -> str(v.trial),
          "occurrence"  -> opt(v.occurrence),
          "item"        -> opt(v.item),
          "attributes"  -> attributes(v.attributes)
        )
      )
    )(json =>
      for
        p      <- Wire.field[String](json, "participant")
        f      <- Wire.field[String](json, "phase")
        t      <- Wire.field[String](json, "trial")
        o      <- Wire.field[Option[String]](json, "occurrence")
        i      <- Wire.field[Option[String]](json, "item")
        a      <- readAttributes(json, "attributes")
        result <- InventoryImportSpec.of(p, f, t, o, i, a).left.map(error(json, _))
      yield result
    )

  def study[U <: Unit2D: UnitLabel]: VersionedCodec[ImportSpec[StudyKey, U]] =
    make(
      StudyCodecs.key(DefinitionId.studyKey),
      json =>
        for
          p <- Wire.field[String](json, "participant")
          s <- Wire.field[String](json, "stimulus")
          f <- Wire.field[String](json, "phase")
        yield SourceKeyColumns.Study(p, s, f),
      "study"
    )

  def trial[U <: Unit2D: UnitLabel]: VersionedCodec[ImportSpec[TrialKey, U]] =
    make(
      StudyCodecs.trialKey(TrialKeyDefinitions.trialKey),
      json =>
        for
          p <- Wire.field[String](json, "participant")
          f <- Wire.field[String](json, "phase")
          t <- Wire.field[String](json, "trial")
          i <- Wire.field[Option[String]](json, "item")
          o <- Wire.field[Option[String]](json, "occurrence")
        yield SourceKeyColumns.Trial(p, f, t, i, o),
      "trial"
    )

  /** A custom description remains readable; absent replay registration is
    * refused by io rather than silently replaced with a built-in reader.
    */
  def custom[K: KeyDigest, U <: Unit2D: UnitLabel](
      keys: VersionedCodec[K]
  ): VersionedCodec[ImportSpec[K, U]] =
    make(
      keys,
      json => Left(CodecError.Field("keys", json, "expected custom key description")),
      "custom"
    )

  private def writeKeys[K](value: SourceKeyColumns[K]): Json = value match
    case SourceKeyColumns.Study(p, s, f) =>
      Json.obj(
        "kind"        -> str("study"),
        "participant" -> str(p),
        "stimulus"    -> str(s),
        "phase"       -> str(f)
      )
    case SourceKeyColumns.Trial(p, f, t, i, o) =>
      Json.obj(
        "kind"        -> str("trial"),
        "participant" -> str(p),
        "phase"       -> str(f),
        "trial"       -> str(t),
        "item"        -> opt(i),
        "occurrence"  -> opt(o)
      )
    case SourceKeyColumns.Custom(reader, clock, columns) =>
      Json.obj(
        "kind"    -> str("custom"),
        "reader"  -> Wire.id(reader),
        "clock"   -> Wire.id(clock),
        "columns" -> Json.arr(columns.map(str)*)
      )

  private def writePolicy[K](
      policy: AdmissionPolicy[K],
      keys: VersionedCodec[K]
  ): Either[CodecError, Json] =
    policy.corrections
      .traverse { rule =>
        val scope = rule.scope match
          case CorrectionScope.AllTrials()    => Right(Json.obj("kind" -> str("allTrials")))
          case CorrectionScope.Participant(p) =>
            Right(Json.obj("kind" -> str("participant"), "participant" -> str(p)))
          case CorrectionScope.Trial(k) =>
            keys.encode(k).map(v => Json.obj("kind" -> str("trial"), "key" -> v))
        scope.map(s =>
          Json.obj("scope" -> s, "correction" -> StudyInputCodec.correction(rule.correction))
        )
      }
      .map(rules =>
        Json
          .obj("offScreen" -> str(policy.offScreen.toString), "corrections" -> Json.arr(rules*))
      )

  private def readPolicy[K](
      json: Json,
      keys: VersionedCodec[K]
  ): Either[CodecError, AdmissionPolicy[K]] = for
    off         <- enumValue(json, "offScreen", OffScreenPolicy.values)
    rules       <- Wire.field[Vector[Json]](json, "corrections")
    corrections <- rules.traverse { rule =>
      for
        scope <- Wire.field[Json](rule, "scope")
        kind  <- Wire.field[String](scope, "kind")
        s     <- kind match
          case "allTrials"   => Right(CorrectionScope.AllTrials[K]())
          case "participant" =>
            Wire.field[String](scope, "participant").map(CorrectionScope.Participant[K](_))
          case "trial" =>
            Wire.field[Json](scope, "key").flatMap(keys.decode).map(CorrectionScope.Trial(_))
          case other => Left(CodecError.Field("kind", scope, s"unknown scope $other"))
        c <- Wire.field[Json](rule, "correction").flatMap(StudyInputCodec.readCorrection)
      yield AppliedCorrection(s, c)
    }
  yield AdmissionPolicy(off, corrections)

  private def counts(value: SampleCountRule): Json = value match
    case SampleCountRule.PositiveColumn(column) =>
      Json.obj("kind" -> str("positiveColumn"), "column" -> str(column))
    case SampleCountRule.DerivedFromDuration(rate) =>
      Json.obj(
        "kind"   -> str("derivedFromDuration"),
        "rateHz" -> Json.fromDoubleOrNull(rate.value)
      )
  private def readCounts(json: Json): Either[CodecError, SampleCountRule] =
    Wire.field[String](json, "kind").flatMap {
      case "positiveColumn" =>
        Wire.field[String](json, "column").map(SampleCountRule.PositiveColumn(_))
      case "derivedFromDuration" =>
        DomainWire
          .finite(json, "rateHz")
          .flatMap(v => Hz(v).left.map(e => CodecError.Field("rateHz", json, e.toString)))
          .map(SampleCountRule.DerivedFromDuration(_))
      case other => Left(CodecError.Field("kind", json, s"unknown sample count rule $other"))
    }

  private def make[K: KeyDigest, U <: Unit2D: UnitLabel](
      keys: VersionedCodec[K],
      builtIn: Json => Either[CodecError, SourceKeyColumns[K]],
      kind: String
  ): VersionedCodec[ImportSpec[K, U]] =
    VersionedCodec.checked[ImportSpec[K, U]](SourceCodecDefinitions.importSpec)(value =>
      for
        keyJson <- Right(writeKeys(value.keys))
        keyKind <- Wire.field[String](keyJson, "kind")
        _       <- Either.cond(
          keyKind == "custom" || keyKind == kind,
          (),
          CodecError.Field("kind", keyJson, s"expected $kind or custom, found $keyKind")
        )
        policy <- writePolicy(value.policy, keys)
        inv    <- value.inventory.traverse(i =>
          inventory
            .encode(i.spec)
            .map(s => Json.obj("spec" -> s, "identity" -> str(i.identity.digest)))
        )
      yield Json.obj(
        "keySchema" -> Wire.id(keys.schema),
        "keys"      -> keyJson,
        "columns"   -> Json.obj(
          "ordinal"    -> str(value.columns.ordinal),
          "x"          -> str(value.columns.x),
          "y"          -> str(value.columns.y),
          "onset"      -> str(value.columns.onset),
          "duration"   -> str(value.columns.duration),
          "samples"    -> counts(value.columns.samples),
          "attributes" -> attributes(value.columns.attributes)
        ),
        "frame"     -> DomainWire.frame(value.frame),
        "timeUnit"  -> str(value.timeUnit.toString),
        "rounding"  -> str(value.rounding.toString),
        "policy"    -> policy,
        "decision"  -> str(value.decision.toString),
        "inventory" -> inv.getOrElse(Json.Null),
        "digest"    -> str(value.digest.render)
      )
    )(json =>
      for
        _          <- Wire.requireId(json, "keySchema", keys.schema)
        keyJson    <- Wire.field[Json](json, "keys")
        keyKind    <- Wire.field[String](keyJson, "kind")
        keyColumns <-
          if keyKind == "custom" then
            for
              reader  <- Wire.definition(keyJson, "reader")
              clock   <- Wire.definition(keyJson, "clock")
              columns <- Wire.field[Vector[String]](keyJson, "columns")
            yield SourceKeyColumns.Custom[K](reader, clock, columns)
          else if keyKind == kind then builtIn(keyJson)
          else Left(CodecError.Field("kind", keyJson, s"expected $kind, found $keyKind"))
        c            <- Wire.field[Json](json, "columns")
        ordinal      <- Wire.field[String](c, "ordinal")
        x            <- Wire.field[String](c, "x")
        y            <- Wire.field[String](c, "y")
        onset        <- Wire.field[String](c, "onset")
        duration     <- Wire.field[String](c, "duration")
        sampleCounts <- Wire.field[Json](c, "samples").flatMap(readCounts)
        attrs        <- readAttributes(c, "attributes")
        columns      <- SourceFixationColumns
          .of(ordinal, x, y, onset, duration, sampleCounts, attrs)
          .left
          .map(error(c, _))
        frame    <- Wire.field[Json](json, "frame").flatMap(DomainWire.readFrame[U])
        unit     <- enumValue(json, "timeUnit", SourceTimeUnit.values)
        rounding <- enumValue(json, "rounding", SourceRounding.values)
        policy   <- Wire.field[Json](json, "policy").flatMap(readPolicy(_, keys))
        decision <- enumValue(json, "decision", AdmissionDecision.values)
        invJson  <- Wire.field[Option[Json]](json, "inventory")
        inv      <- invJson.traverse(j =>
          for
            s  <- Wire.field[Json](j, "spec").flatMap(inventory.decode)
            d  <- Wire.field[String](j, "identity")
            id <- SourceIdentity
              .parse(d)
              .left
              .map(e => CodecError.Field("identity", j, e.message))
          yield SourceInventory(s, id)
        )
        result <- ImportSpec
          .of(keyColumns, columns, frame, unit, policy, decision, rounding, inv)
          .left
          .map(error(json, _))
        declared <- Wire.field[String](json, "digest")
        _        <- Either.cond(
          declared == result.digest.render,
          (),
          CodecError.Field(
            "digest",
            json,
            s"declared $declared, computed ${result.digest.render}"
          )
        )
      yield result
    )
