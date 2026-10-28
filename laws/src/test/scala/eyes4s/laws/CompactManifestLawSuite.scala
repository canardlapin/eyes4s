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

import eyes4s.codec.*
import io.circe.Json
import org.scalacheck.{Gen, Test}

class CompactManifestLawSuite extends munit.DisciplineSuite:
  import ArtifactCodecLawSuite.sure
  private val compact = ArtifactCodecLawSuite.manifests.flatMap { original =>
    Gen.listOfN(original.entries.size, Gen.oneOf(true, false)).map { mask =>
      val first   = original.entries.find(_.role == ArtifactRole.StudyInput).get.name
      val entries = original.entries.zip(mask).map { (entry, choose) =>
        val eligible =
          Set(ArtifactRole.StudyInput, ArtifactRole.AdmissionLedger, ArtifactRole.StudyResult)(
            entry.role
          )
        val media = if eligible && (choose || entry.name == first) then MediaKind.ColumnarJson
        else entry.media
        sure(
          ManifestEntry.of(
            entry.name,
            entry.role,
            entry.schema,
            media,
            entry.length,
            entry.sha256,
            entry.identity,
            entry.layout
          )
        )
      }
      sure(ScientificManifest.of(entries, original.relations))
    }
  }
  private val all = Gen.oneOf(ArtifactCodecLawSuite.manifests, compact)
  checkAll(
    "compact manifest",
    CodecLaws.roundTrip(ScientificManifest.codec, compact, (a, b) => a == b)
  )
  checkAll(
    "manifest storage ladder",
    SchemaLadderLaws.ladder(ScientificManifest.ladder, all, (a, b) => a == b)
  )

  test("the manifest ladder laws kill a dropped-entry upcast") {
    val shipped = ScientificManifest.ladder
    val broken  = SchemaLadder
      .of[ScientificManifest]("scientific manifest", ScientificManifest.schema)(m =>
        shipped.writeAt(ScientificManifest.schema, m)
      )(json => shipped.readAt(ScientificManifest.schema, json))
      .next(
        m => !m.entries.exists(_.media == MediaKind.ColumnarJson),
        json => json.mapObject(_.add("entries", Json.arr()))
      )(m => shipped.writeAt(CompactArchiveDefinitions.manifestV2, m))(json =>
        shipped.readAt(CompactArchiveDefinitions.manifestV2, json)
      )
    val rules =
      SchemaLadderLaws.ladder(broken, ArtifactCodecLawSuite.manifests, (a, b) => a == b)
    val parameters =
      Test.Parameters.default.withMinSuccessfulTests(30).withInitialSeed(0x434f4d50414354L)
    assert(rules.all.properties.exists { (_, property) =>
      Test.check(parameters, property).status match
        case Test.Failed(_, _) | Test.PropException(_, _, _) => true
        case _                                               => false
    })
  }

  test("the compact manifest law kills replacing its physical media with JSON") {
    val version = CompactArchiveDefinitions.manifestV2
    val mutant  = VersionedCodec.checked[ScientificManifest](version)(m =>
      ScientificManifest.ladder.writeAt(version, m)
    ) { json =>
      ScientificManifest.ladder.readAt(version, json).flatMap { value =>
        val entries = value.entries.map { entry =>
          val media =
            if entry.media == MediaKind.ColumnarJson then MediaKind.JsonText else entry.media
          sure(
            ManifestEntry.of(
              entry.name,
              entry.role,
              entry.schema,
              media,
              entry.length,
              entry.sha256,
              entry.identity,
              entry.layout
            )
          )
        }
        ScientificManifest.of(entries, value.relations).left.map(CodecError.Manifest(_))
      }
    }
    val parameters =
      Test.Parameters.default.withMinSuccessfulTests(30).withInitialSeed(0x434f4d50414354L)
    val rules = CodecLaws.roundTrip(mutant, compact, (a, b) => a == b)
    assert(rules.all.properties.exists { (_, property) =>
      Test.check(parameters, property).status match
        case Test.Failed(_, _) | Test.PropException(_, _, _) => true
        case _                                               => false
    })
  }
