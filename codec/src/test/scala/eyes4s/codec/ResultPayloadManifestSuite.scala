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
import io.circe.Json

class ResultPayloadManifestSuite extends munit.FunSuite:
  import ManifestFixtures.{planArtifact, inputArtifact, results, result, decoders}
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private lazy val packed    = get(ResultManifest.packed("result", results, result))
  private lazy val artifacts =
    Vector(planArtifact, inputArtifact, packed.result) ++ packed.payloads
  private lazy val relations = Vector(
    ManifestRelation.PlanInput(planArtifact.name, inputArtifact.name),
    ManifestRelation.ResultOf(packed.result.name, planArtifact.name, inputArtifact.name)
  ) ++ packed.relations
  private def save(
      as: Vector[StoredArtifact] = artifacts,
      rs: Vector[ManifestRelation] = relations
  ) =
    get(SavedManifest.of(as, rs))
  private def resolve(saved: SavedManifest) =
    ArtifactResolver.resolve(saved.address, saved.source, decoders).left.map(_.toVector)
  private def checkDensityRefusal(saved: SavedManifest): Unit =
    assert(resolve(saved).left.toOption.exists(_.exists {
      case ResolveError.Decode(n, CodecError.Field("density", _, _)) => n == packed.result.name
      case _                                                         => false
    }))

  test("packed densities resolve through additive manifest@1 to the exact completed result") {
    val saved = save()
    assertEquals(ScientificManifest.schema, DefinitionId.manifest)
    assertEquals(packed.result.entry.schema, DensityArchiveDefinitions.studyResultV2)
    assert(packed.payloads.nonEmpty)
    assertEquals(ArtifactRole.ResultPayload.wire, "result-payload")
    assert(packed.relations.forall(_.kind == "result-payload-of"))
    assert(packed.payloads.forall(_.entry.role == ArtifactRole.ResultPayload))
    assert(
      packed.payloads.forall(p => p.entry.media == MediaKind.Binary && p.entry.layout.nonEmpty)
    )
    assertEquals(
      get(
        ScientificManifest.codec.decode(get(ScientificManifest.codec.encode(saved.manifest)))
      ),
      saved.manifest
    )
    val loaded = get(resolve(saved))
    assertEquals(loaded.results.map(_._2.encode), Vector(results.codec.encode(result)))
    assertEquals(loaded.payloads.map(_._1), packed.payloads.map(_.name))
  }

  test("unknown role and relation remain typed refusals in manifest@1") {
    val document = get(ScientificManifest.codec.encode(save().manifest))
    val body     = get(document.hcursor.get[Json]("value"))
    val entries  = get(body.hcursor.get[Vector[Json]]("entries"))
    val changed  = entries.head.mapObject(_.add("role", Json.fromString("future-role")))
    val unknown  = document.mapObject(
      _.add("value", body.mapObject(_.add("entries", Json.arr(entries.updated(0, changed)*))))
    )
    assert(ScientificManifest.codec.decode(unknown).left.toOption.exists {
      case CodecError.Entry(_, CodecError.Field("role", _, _)) => true
      case _                                                   => false
    })
    val rs              = get(body.hcursor.get[Vector[Json]]("relations"))
    val bad             = rs.head.mapObject(_.add("kind", Json.fromString("future-relation")))
    val unknownRelation = document.mapObject(
      _.add("value", body.mapObject(_.add("relations", Json.arr(rs.updated(0, bad)*))))
    )
    assert(ScientificManifest.codec.decode(unknownRelation).left.toOption.exists {
      case CodecError.Entry(_, CodecError.Field("kind", _, _)) => true
      case _                                                   => false
    })
  }

  test("result payloads require layout and a version-two result owner") {
    val p = packed.payloads.head.entry
    assert(
      ManifestEntry
        .of(p.name, p.role, p.schema, p.media, p.length, p.sha256, None, None)
        .left
        .toOption
        .exists {
          case ManifestError.PayloadDeclaration(_, _, _, _) => true
          case _                                            => false
        }
    )
    val inline = get(StoredArtifact.result("result", results, result))
    assert(
      ScientificManifest
        .of(
          artifacts.map(a => if a.name == inline.name then inline.entry else a.entry),
          relations
        )
        .left
        .toOption
        .exists {
          case ManifestError.PayloadOwner(_, schema) => schema == DefinitionId.studyResult
          case _                                     => false
        }
    )
    assert(
      ScientificManifest
        .of(artifacts.map(_.entry), relations.filterNot(packed.relations.contains))
        .left
        .toOption
        .exists {
          case ManifestError.RelationCount(_, "result-payload-of", 0, _) => true
          case _                                                         => false
        }
    )
    assert(
      ScientificManifest
        .of(artifacts.map(_.entry), relations :+ packed.relations.head)
        .left
        .toOption
        .exists {
          case ManifestError.DuplicateRelation(_) => true
          case _                                  => false
        }
    )
  }

  test("recording payload roles cannot substitute for result payloads") {
    val p       = packed.payloads.head
    val ref     = PayloadRef(p.entry.sha256, p.entry.layout.get)
    val changed =
      get(StoredArtifact.payload(p.name.value, get(VerifiedPayload.verify(ref, p.bytes))))
    assert(
      ScientificManifest
        .of(artifacts.map(a => if a.name == p.name then changed.entry else a.entry), relations)
        .left
        .toOption
        .exists {
          case ManifestError
                .RoleMismatch(_, n, ArtifactRole.ResultPayload, ArtifactRole.Payload) =>
            n == p.name
          case _ => false
        }
    )
  }

  test("a missing owned chunk refuses materialization even with a valid remaining graph") {
    val p = packed.payloads.head
    checkDensityRefusal(
      save(
        artifacts.filterNot(_.name == p.name),
        relations.filterNot(_.endpoints.exists(_._2 == p.name))
      )
    )
  }

  test("unreferenced extra chunks are refused by semantic relation checking") {
    val layout =
      get(PayloadLayout.of(ElementKind.Float64, Vector(1, 1, 1), ArrayOrder.RowMajor))
    val chunk = get(PackedArrays.pack(layout, IArray(0.5)))
    val extra = get(StoredArtifact.resultPayload("extra", chunk))
    assert(
      resolve(
        save(
          artifacts :+ extra,
          relations :+ ManifestRelation.ResultPayloadOf(packed.result.name, extra.name)
        )
      ).left.toOption.exists(_.exists {
        case ResolveError.Relation(
              ManifestRelation.ResultPayloadOf(_, n),
              RelationMismatch.UnreferencedPayload(ref)
            ) =>
          n == extra.name && ref == chunk.ref
        case _ => false
      })
    )
  }

  test("same bytes with a different layout cannot satisfy an archive chunk reference") {
    val p      = packed.payloads.head
    val old    = p.entry.layout.get
    val layout = get(PayloadLayout.of(old.element, Vector(old.byteLength / 8, 1, 1), old.order))
    assertNotEquals(layout, old)
    val changed = get(
      StoredArtifact.resultPayload(
        p.name.value,
        get(VerifiedPayload.verify(PayloadRef(p.entry.sha256, layout), p.bytes))
      )
    )
    checkDensityRefusal(save(artifacts.map(a => if a.name == p.name then changed else a)))
  }

  test("changed bytes fail before any scientific decoder runs") {
    val saved = save()
    val p     = packed.payloads.head
    val bad   = IArray.tabulate(p.bytes.length)(i =>
      if i == 0 then (p.bytes(i) ^ 1).toByte else p.bytes(i)
    )
    var calls    = 0
    val counting = new ArtifactDecoders.Delegating(decoders):
      override def resultWithPayloads(
          document: Json,
          payloads: PayloadRef => Option[VerifiedPayload]
      ) =
        calls += 1
        super.resultWithPayloads(document, payloads)
    val source = ByteSource.inMemory(
      Map(saved.address -> saved.bytes),
      artifacts.map(a => a.name -> (if a.name == p.name then bad else a.bytes)).toMap
    )
    val resolved = ArtifactResolver.resolve(saved.address, source, counting)
    assertEquals(calls, 0)
    assert(resolved.left.toOption.exists(_.toVector.exists {
      case ResolveError.Digest(n, _, _) => n == p.name
      case _                            => false
    }))
  }

  test("redeclaring chunk bytes and references still cannot change the stored density digest") {
    val p     = packed.payloads.head
    val bytes = IArray.tabulate(p.bytes.length)(i =>
      if i == 0 then (p.bytes(i) ^ 1).toByte else p.bytes(i)
    )
    val ref     = PayloadRef(ByteDigest.sha256(bytes), p.entry.layout.get)
    val changed =
      get(StoredArtifact.resultPayload(p.name.value, get(VerifiedPayload.verify(ref, bytes))))
    val document                  = get(Documents.parse(packed.result.bytes))
    def replace(json: Json): Json = json.arrayOrObject(
      json,
      values => Json.arr(values.map(replace)*),
      obj => Json.fromJsonObject(obj.mapValues(replace))
    ) match
      case j if j.asString.contains(p.entry.sha256.hex) => Json.fromString(ref.sha256.hex)
      case j                                            => j
    val resultArtifact =
      get(StoredArtifact.document("result", ArtifactRole.StudyResult, replace(document), None))
    val changedArtifacts = artifacts.map(a =>
      if a.name == p.name then changed
      else if a.name == resultArtifact.name then resultArtifact
      else a
    )
    assert(resolve(save(changedArtifacts)).left.toOption.exists(_.exists {
      case ResolveError.Decode(_, CodecError.Field("density", _, reason)) =>
        reason.contains("recomputed")
      case _ => false
    }))
  }

  test("legacy custom decoders retain inline success and exact packed refusal") {
    var documents = Vector.empty[Json]
    val legacy    = new ArtifactDecoders[StudyKey, Px]:
      def plan(d: Json)   = decoders.plan(d)
      def input(d: Json)  = decoders.input(d)
      def ledger(d: Json) = decoders.ledger(d)
      def result(d: Json) =
        documents :+= d
        decoders.result(d)
      def recording(d: Json, p: PayloadRef => Option[VerifiedPayload]) =
        decoders.recording(d, p)
      def recordingInput(d: Json) = decoders.recordingInput(d)
      def temporalInput(
          d: Json,
          b: ArtifactRef[StudyInput[StudyKey, Px]] => Option[StudyInput[StudyKey, Px]]
      ) = decoders.temporalInput(d, b)

    val inline = get(results.codec.encode(result))
    val loaded = get(legacy.resultWithPayloads(inline, _ => fail("unexpected payload lookup")))
    assertEquals(loaded.encode, Right(inline))
    assertEquals(documents, Vector(inline))

    val document = get(Documents.parse(packed.result.bytes))
    val refusal  = decoders.result(document).left.toOption.getOrElse(fail("expected refusal"))
    assertEquals(
      legacy.resultWithPayloads(document, _ => fail("unexpected payload lookup")).left.toOption,
      Some(refusal)
    )
    assertEquals(documents, Vector(inline, document))
    val saved    = save()
    val resolved = ArtifactResolver.resolve(saved.address, saved.source, legacy)
    assert(
      resolved.left.toOption.exists(
        _.toVector.contains(
          ResolveError.Decode(packed.result.name, refusal)
        )
      )
    )
    assertEquals(documents, Vector(inline, document, document))
  }

  test("delegating decorators preserve packed result registration") {
    val saved   = save()
    val wrapped = new ArtifactDecoders.Delegating(decoders) {}
    val loaded  =
      get(ArtifactResolver.resolve(saved.address, saved.source, wrapped).left.map(_.toVector))
    assertEquals(loaded.results.map(_._2.encode), Vector(results.codec.encode(result)))
  }

  test(
    "chunks owned only by another result are unavailable even when present in the same manifest"
  ) {
    val other = get(
      StoredArtifact.document(
        "other-result",
        ArtifactRole.StudyResult,
        get(Documents.parse(packed.result.bytes)),
        None
      )
    )
    val redirected = relations.filterNot(packed.relations.contains) ++
      packed.payloads.map(p => ManifestRelation.ResultPayloadOf(other.name, p.name)) :+
      ManifestRelation.ResultOf(other.name, planArtifact.name, inputArtifact.name)
    checkDensityRefusal(save(artifacts :+ other, redirected))
  }

  test("recomputable archives are explicitly refused rather than silently executed") {
    val codec  = new DensityArchiveCodec(results)
    val bundle = get(codec.encode(result))
    assertEquals(bundle.chunks.size, 0)
    val stored = get(
      StoredArtifact.document(
        "result",
        ArtifactRole.StudyResult,
        get(codec.codec.encode(bundle.archive)),
        None
      )
    )
    checkDensityRefusal(
      save(
        Vector(planArtifact, inputArtifact, stored),
        relations.filterNot(packed.relations.contains)
      )
    )
  }
