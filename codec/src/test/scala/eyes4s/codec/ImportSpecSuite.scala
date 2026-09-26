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
        """{"schema":{"name":"eyes4s.import-spec","version":1},"value":{"keySchema":{"name":"eyes4s.study-key","version":1},"keys":{"kind":"study","participant":"participant","stimulus":"item","phase":"phase"},"columns":{"ordinal":"n","x":"x","y":"y","onset":"onset","duration":"duration","samples":{"kind":"positiveColumn","column":"samples"},"attributes":[]},"frame":{"id":"source-frame","unit":"px","xMin":0.0,"yMin":0.0,"xMax":100.0,"yMax":100.0,"yAxis":"Down"},"timeUnit":"Milliseconds","rounding":"NearestMicrosecond","policy":{"offScreen":"ExcludeRecord","corrections":[]},"decision":"ReviewExclusions","inventory":null,"digest":"36995201760330b5"}}"""
      )
    )
    assertEquals(get(codec.encode(get(codec.decode(json0)))), json0)
    val json1 = get(
      io.circe.parser.parse(
        """{"schema":{"name":"eyes4s.inventory-import-spec","version":1},"value":{"participant":"participant","phase":"phase","trial":"trial","occurrence":null,"item":"item","attributes":[]}}"""
      )
    )
    assertEquals(
      get(ImportSpecCodec.inventory.encode(get(ImportSpecCodec.inventory.decode(json1)))),
      json1
    )
    val json2 = get(
      io.circe.parser.parse(
        """{"schema":{"name":"eyes4s.source-ref","version":1},"value":{"label":"fixations.csv","records":"e5740769eab149c6","interpretation":{"kind":"declared","format":"FixationCsv","parser":{"name":"eyes4s.fixation-csv-parser","version":1},"options":"36995201760330b5","identityVersion":"eyes4s.source-identity/1","identity":"068718ec24fd232f"}}}"""
      )
    )
    assertEquals(
      get(SourceIdentityCodec.source.encode(get(SourceIdentityCodec.source.decode(json2)))),
      json2
    )
    val json3 = get(
      io.circe.parser.parse(
        """{"schema":{"name":"eyes4s.source-ref","version":1},"value":{"label":"trials.csv","records":"e1f5405aea80983e","interpretation":{"kind":"declared","format":"TrialInventoryCsv","parser":{"name":"eyes4s.trial-inventory-csv-parser","version":1},"options":"3a6a492490fede29","identityVersion":"eyes4s.source-identity/1","identity":"e14cf26d5ad7c128"}}}"""
      )
    )
    assertEquals(
      get(SourceIdentityCodec.source.encode(get(SourceIdentityCodec.source.decode(json3)))),
      json3
    )
    val json4 = get(
      io.circe.parser.parse(
        """{"schema":{"name":"eyes4s.admission-ledger","version":4},"value":{"keySchema":{"name":"eyes4s.study-key","version":1},"source":{"label":"empty.csv","records":"df0f52b97b77fd3b","interpretation":{"kind":"declared","format":"FixationCsv","parser":{"name":"eyes4s.fixation-csv-parser","version":1},"options":"36995201760330b5","identityVersion":"eyes4s.source-identity/1","identity":"8f6e78db3722138f"}},"header":["x"],"outcome":"complete","records":[],"offScreen":"excludeRecord","corrections":[],"outsideFrame":[],"inventory":null}}"""
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
