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

import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

class SourceManifestSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def name(s: String): ArtifactName     = get(ArtifactName.of(s))
  private def bytes(s: String): IArray[Byte]    = get(Utf8.encode(s))
  private val spec   = get(ImportSpecCodec.study[Px].parse(SourceIdentityMirrors.importSpec))
  private val ledger = get(
    StudyInputCodecs.study[Px].ledger.parse(SourceIdentityMirrors.ledgerV4)
  )
  private val artifacts = Vector(
    get(StoredArtifact.ledger("ledger", StudyInputCodecs.study[Px], ledger)),
    get(
      StoredArtifact.sourceFile("sources/source.csv", SourceFormat.FixationCsv, bytes("x\n"))
    ),
    get(StoredArtifact.importSpec("import.json", ImportSpecCodec.study[Px], spec))
  )
  private val relation: ManifestRelation.LedgerSource = ManifestRelation.LedgerSource(
    name("ledger"),
    name("sources/source.csv"),
    name("import.json"),
    LedgerSourceRole.Primary
  )
  private val saved                         = get(SavedManifest.of(artifacts, Vector(relation)))
  private val decoders                      = get(ArtifactDecoders.study[Px])
  private def resolve(value: SavedManifest) =
    ArtifactResolver.resolve(value.address, value.source, decoders)

  test("additive manifest@1 stores raw bytes and resolves declared sources without replay") {
    val json = get(ScientificManifest.codec.encode(saved.manifest))
    assertEquals(ScientificManifest.schema, DefinitionId.manifest)
    assert(relation.render.contains("role=primary"))
    assertEquals(get(ScientificManifest.codec.decode(json)), saved.manifest)
    assertEquals(
      get(ScientificManifest.bytes(get(ScientificManifest.codec.decode(json)))).toVector,
      saved.bytes.toVector
    )
    assertEquals(
      json.hcursor.downField("value").downField("relations").focus,
      Some(
        Json.arr(
          Json.obj(
            "kind"       -> Json.fromString("ledger-source"),
            "ledger"     -> Json.fromString("ledger"),
            "sourceFile" -> Json.fromString("sources/source.csv"),
            "importSpec" -> Json.fromString("import.json"),
            "role"       -> Json.fromString("primary")
          )
        )
      )
    )
    val result = get(resolve(saved))
    assertEquals(result.sourceFile(name("sources/source.csv")), Some("x\n"))
    assertEquals(result.importSpec(name("import.json")).map(_.digest), Some(spec.digest))
    assertEquals(result.inventorySpec(name("import.json")), None)
    assertEquals(artifacts(1).bytes.toVector, bytes("x\n").toVector)
    assertEquals(artifacts(1).entry.media, MediaKind.Binary)
    assertEquals(artifacts(1).entry.layout, None)
    assertEquals(artifacts(1).entry.identity, None)
  }

  test("source bytes are copied and all byte integrity failures precede decoding") {
    val mutable = Array[Byte](120, 10)
    val source  = get(
      StoredArtifact.sourceFile(
        "sources/source.csv",
        SourceFormat.FixationCsv,
        IArray.unsafeFromArray(mutable)
      )
    )
    mutable(0) = 121
    assertEquals(source.bytes.toVector, Vector[Byte](120, 10))
    val changed = ByteSource.inMemory(
      Map.empty,
      artifacts.map(a => a.name -> a.bytes).toMap.updated(source.name, bytes("y\n"))
    )
    var calls     = 0
    val observing = new ArtifactDecoders.Delegating[StudyKey, Px](decoders):
      override def importSpec(document: Json) =
        calls += 1
        super.importSpec(document)
    val refused = ArtifactResolver.resolveManifest(saved.manifest, changed, observing)
    assertEquals(calls, 0)
    assert(refused.left.toOption.exists(_.toVector.exists {
      case ResolveError.Digest(n, _, _) => n == source.name
      case _                            => false
    }))
  }

  test("invalid UTF-8 source is a located typed refusal after byte verification") {
    val bad = get(
      StoredArtifact.sourceFile(
        "sources/source.csv",
        SourceFormat.FixationCsv,
        IArray[Byte](0xc3.toByte, 0x28.toByte)
      )
    )
    val value = get(SavedManifest.of(artifacts.updated(1, bad), Vector(relation)))
    assertEquals(
      resolve(value).left.map(_.toVector),
      Left(Vector(ResolveError.Text(bad.name, 0)))
    )
  }

  test("unknown artifact and source roles are typed wire refusals") {
    val json = get(ScientificManifest.codec.encode(saved.manifest))
    val role = json.hcursor
      .downField("value")
      .downField("entries")
      .downN(0)
      .downField("role")
      .withFocus(_ => Json.fromString("future-role"))
      .top
      .get
    val sourceRole = json.hcursor
      .downField("value")
      .downField("relations")
      .downN(0)
      .downField("role")
      .withFocus(_ => Json.fromString("future-source"))
      .top
      .get
    Vector(role, sourceRole).foreach(j =>
      assert(ScientificManifest.codec.decode(j).left.toOption.exists {
        case CodecError.Entry(path, CodecError.Field("role", _, reason)) =>
          Set("entries[0]", "relations[0]").contains(path) && reason.contains("unknown")
        case _ => false
      })
    )
  }

  test("source links reject missing endpoints, wrong roles and competing primary sources") {
    assert(
      ScientificManifest
        .of(saved.manifest.entries, Vector(relation.copy(sourceFile = name("absent"))))
        .left
        .toOption
        .exists {
          case ManifestError.UnknownEntry(_, n) => n == name("absent")
          case _                                => false
        }
    )
    assert(
      ScientificManifest
        .of(saved.manifest.entries, Vector(relation.copy(sourceFile = name("import.json"))))
        .left
        .toOption
        .exists {
          case ManifestError
                .RoleMismatch(_, _, ArtifactRole.SourceFile, ArtifactRole.ImportSpec) =>
            true
          case _ => false
        }
    )
    val extra =
      get(StoredArtifact.sourceFile("second.csv", SourceFormat.FixationCsv, bytes("x\n")))
    assert(
      ScientificManifest
        .of(
          saved.manifest.entries :+ extra.entry,
          Vector(relation, relation.copy(sourceFile = extra.name))
        )
        .left
        .toOption
        .exists {
          case ManifestError.RelationCount(_, "ledger-source:primary", 2, _) => true
          case _                                                             => false
        }
    )
    assert(
      ScientificManifest
        .of(
          saved.manifest.entries,
          Vector(relation.copy(role = LedgerSourceRole.TrialInventory))
        )
        .isLeft
    )
  }

  test("ledger declarations bind the parser and import options before replay") {
    val wrongParser = get(
      StoredArtifact.sourceFile(
        "sources/source.csv",
        SourceFormat.TrialInventoryCsv,
        bytes("x\n")
      )
    )
    val changedSpec = get(
      ImportSpec.of(
        spec.keys,
        spec.columns,
        spec.frame,
        SourceTimeUnit.Seconds,
        spec.policy,
        spec.decision
      )
    )
    val wrongSpec =
      get(StoredArtifact.importSpec("import.json", ImportSpecCodec.study[Px], changedSpec))
    Vector(artifacts.updated(1, wrongParser), artifacts.updated(2, wrongSpec)).foreach { as =>
      val value = get(SavedManifest.of(as, Vector(relation)))
      assert(resolve(value).left.toOption.exists(_.toVector.exists {
        case ResolveError.Relation(_, RelationMismatch.SourceBinding(_, _, _)) => true
        case _                                                                 => false
      }))
    }
  }

  test("registration decorators forward the import decoder and missing registration refuses") {
    val forwarding = new ArtifactDecoders.Delegating[StudyKey, Px](decoders) {}
    assert(ArtifactResolver.resolve(saved.address, saved.source, forwarding).isRight)
    val missing = ArtifactDecoders.of(
      StudyRegistry.empty[StudyKey, Px],
      get(StudyInputRegistry.empty[StudyKey, Px].register(StudyInputCodecs.study[Px])),
      StudyResultRegistry.empty[StudyKey, Px]
    )
    assert(ArtifactResolver.resolve(saved.address, saved.source, missing).isLeft)
    assert(
      ArtifactResolver
        .resolve(
          saved.address,
          saved.source,
          missing.withImportSpecs(ImportSpecCodec.study[Px])
        )
        .isRight
    )
  }
