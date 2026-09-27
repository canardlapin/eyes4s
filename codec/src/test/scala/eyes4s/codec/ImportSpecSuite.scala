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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class ImportSpecSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("source-frame", 100, 100))
  private val columns                           = get(
    SourceFixationColumns.of(
      "n",
      "x",
      "y",
      "onset",
      "duration",
      SampleCountRule.PositiveColumn("samples")
    )
  )
  private val spec = get(
    ImportSpec.of(
      SourceKeyColumns.Study("participant", "item", "phase"),
      columns,
      frame,
      SourceTimeUnit.Milliseconds,
      AdmissionPolicy.default[StudyKey],
      AdmissionDecision.ReviewExclusions
    )
  )
  private val codec = ImportSpecCodec.study[Px]
  test("declared import descriptions round-trip with their identity") {
    val json = get(codec.encode(spec))
    val read = get(codec.decode(json))
    assertEquals(read.digest, spec.digest)
    assertEquals(get(codec.encode(read)), json)
    val source = spec.source("test.csv", Vector("x"), Vector(Vector("1")))
    assertEquals(
      get(SourceIdentityCodec.source.decode(get(SourceIdentityCodec.source.encode(source)))),
      source
    )
  }
  test("altering an option without updating the declared digest is refused") {
    val json = get(codec.encode(spec)).hcursor
      .downField("value")
      .downField("timeUnit")
      .withFocus(_ => io.circe.Json.fromString("Seconds"))
      .top
      .get
    assert(codec.decode(json).isLeft)
  }
  test("custom key and clock identities persist without inventing a replay implementation") {
    val reader = get(DefinitionId.of("custom.reader", 2))
    val clock  = get(DefinitionId.of("custom.clock", 1))
    val custom = get(
      ImportSpec.of[StudyKey, Px](
        SourceKeyColumns.Custom(reader, clock, Vector("p")),
        columns,
        frame,
        SourceTimeUnit.Seconds,
        AdmissionPolicy.default[StudyKey],
        AdmissionDecision.RequireComplete
      )
    )
    val restored = get(codec.decode(get(codec.encode(custom))))
    assertEquals(restored.keys, custom.keys)
    assertEquals(restored.digest, custom.digest)
    val customCodec =
      ImportSpecCodec.custom[StudyKey, Px](StudyCodecs.key(DefinitionId.studyKey))
    assertEquals(get(customCodec.decode(get(customCodec.encode(custom)))).digest, custom.digest)
  }

  test("frozen identity is identical on JVM and Scala.js") {
    assertEquals(spec.digest.render, "36995201760330b5")
    assertEquals(
      spec.source("arbitrary label", Vector("x"), Vector(Vector("1"))).identity.map(_.digest),
      Some("068718ec24fd232f")
    )
  }
  test("pinned version one descriptions and version four ledger remain readable") {
    val json0 = get(
      io.circe.parser.parse(
        SourceIdentityMirrors.importSpec
      )
    )
    assertEquals(get(codec.encode(get(codec.decode(json0)))), json0)
    val json1 = get(
      io.circe.parser.parse(
        SourceIdentityMirrors.inventorySpec
      )
    )
    assertEquals(
      get(ImportSpecCodec.inventory.encode(get(ImportSpecCodec.inventory.decode(json1)))),
      json1
    )
    val json2 = get(
      io.circe.parser.parse(
        SourceIdentityMirrors.sourceRef
      )
    )
    assertEquals(
      get(SourceIdentityCodec.source.encode(get(SourceIdentityCodec.source.decode(json2)))),
      json2
    )
    val json3 = get(
      io.circe.parser.parse(
        SourceIdentityMirrors.inventoryRef
      )
    )
    assertEquals(
      get(SourceIdentityCodec.source.encode(get(SourceIdentityCodec.source.decode(json3)))),
      json3
    )
    val json4 = get(
      io.circe.parser.parse(
        SourceIdentityMirrors.ledgerV4
      )
    )
    assertEquals(
      get(
        StudyInputCodecs
          .study[Px]
          .ledger
          .encode(get(StudyInputCodecs.study[Px].ledger.decode(json4)))
      ),
      json4
    )
  }

  test("source wire rejects incompatible parser and options schema at their fields") {
    val source = spec.source("checked.csv", Vector("x"), Vector(Vector("1")))
    val json   = get(SourceIdentityCodec.source.encode(source))
    def replace(field: String, value: io.circe.Json) = json.hcursor
      .downField("value")
      .downField("interpretation")
      .downField(field)
      .withFocus(_ => value)
      .top
      .get
    // Recompute the inconsistent declaration's digest independently: checking
    // only the combined hash would accept this original review counterexample.
    val parser         = SourceImportDefinitions.inventoryParser
    val forgedIdentity = ContentHash
      .combineAll(
        Vector(
          ContentHash.ofString(SourceIdentity.versionTag),
          ContentHash.ofString(SourceFormat.FixationCsv.toString),
          ContentHash.ofString(parser.name),
          ContentHash.ofString(parser.version.toString),
          ContentHash.ofString(source.records.digest),
          spec.digest
        )
      )
      .render
    val wrongParser = replace("parser", Wire.id(parser)).hcursor
      .downField("value")
      .downField("interpretation")
      .downField("identity")
      .withFocus(_ => io.circe.Json.fromString(forgedIdentity))
      .top
      .get
    val wrongSchema = replace("optionsSchema", io.circe.Json.fromString("TrialInventoryCsvV1"))
    val unknownSchema = replace("optionsSchema", io.circe.Json.fromString("future"))
    Vector(
      "parser"        -> wrongParser,
      "optionsSchema" -> wrongSchema,
      "optionsSchema" -> unknownSchema
    ).foreach { (field, document) =>
      assert(SourceIdentityCodec.source.decode(document).left.toOption.exists {
        case CodecError.Field(actual, _, _) => actual == field
        case _                              => false
      })
    }
    val missing = json.hcursor
      .downField("value")
      .downField("interpretation")
      .withFocus(_.mapObject(_.remove("optionsSchema")))
      .top
      .get
    assert(SourceIdentityCodec.source.decode(missing).isLeft)
  }

  test("custom codecs refuse built-in key descriptions before successful encoding") {
    val customStudy =
      ImportSpecCodec.custom[StudyKey, Px](StudyCodecs.key(DefinitionId.studyKey))
    assert(customStudy.encode(spec).left.toOption.exists {
      case CodecError.Field("kind", _, _) => true
      case _                              => false
    })
    val trial = get(
      ImportSpec.of(
        SourceKeyColumns.Trial("p", "phase", "trial", Some("item"), None),
        columns,
        frame,
        SourceTimeUnit.Milliseconds,
        AdmissionPolicy.default[TrialKey],
        AdmissionDecision.RequireComplete
      )
    )
    val customTrial =
      ImportSpecCodec.custom[TrialKey, Px](StudyCodecs.trialKey(TrialKeyDefinitions.trialKey))
    assert(customTrial.encode(trial).isLeft)
    val builtIn = ImportSpecCodec.trial[Px]
    assertEquals(get(builtIn.decode(get(builtIn.encode(trial)))).digest, trial.digest)
  }
