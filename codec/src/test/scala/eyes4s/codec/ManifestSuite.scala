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

/** The verifying resolver over an in-memory source: end-to-end
  * reconstruction, the corruption matrix, ownership of admitted bytes, read
  * counts, and the manifest's own structural checks.
  */
class ManifestSuite extends munit.FunSuite:
  import ManifestFixtures.*

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)

  private def resolveWith(
      source: ByteSource,
      manifest: ScientificManifest = saved.manifest
  ): Either[Vector[ResolveError], ResolvedManifest[StudyKey, Px]] =
    ArtifactResolver.resolveManifest(manifest, source, decoders).left.map(_.toVector)

  private def entries(changes: Map[ArtifactName, IArray[Byte]]): ByteSource =
    ByteSource.inMemory(Map(saved.address -> saved.bytes), blobs ++ changes)

  private def entryOf(n: String): ManifestEntry = get(saved.manifest.entry(name(n)).toRight(n))

  private def rebuilt(
      entries: Vector[ManifestEntry] = saved.manifest.entries,
      relations: Vector[ManifestRelation] = saved.manifest.relations
  ): ScientificManifest = get(ScientificManifest.of(entries, relations))

  private def replaced(n: String, entry: ManifestEntry): Vector[ManifestEntry] =
    saved.manifest.entries.map(e => if e.name == name(n) then entry else e)

  private def flip(bytes: IArray[Byte], at: Int): IArray[Byte] =
    IArray.tabulate(bytes.length)(i => if i == at then (bytes(i) ^ 0x01).toByte else bytes(i))

  test("a saved graph resolves by address to the values it was written from") {
    val resolved =
      get(ArtifactResolver.resolve(saved.address, saved.source, decoders).left.map(_.toVector))
    assertEquals(resolved.manifest, saved.manifest)
    assertEquals(resolved.plans.map(_._1), Vector(name("plan")))
    assertEquals(get(resolved.plan(name("plan")).toRight("plan")).description, plan.description)
    assertEquals(
      resolved.inputs.map(_._2.reference),
      Vector(input.reference, temporal.study.reference)
    )
    assertEquals(resolved.ledger(name("ledger")), Some(ledger))
    assertEquals(
      get(resolved.result(name("result")).toRight("result")).encode,
      results.codec.encode(result)
    )
    assertEquals(
      resolved.temporalInput(name("temporal")).map(_.reference),
      Some(temporal.reference)
    )
    assertEquals(
      resolved.recordingInput(name("recording-input")).map(_.reference),
      Some(recordingInput.reference)
    )
    assertEquals(
      resolved.recording(name("recording")).map(_.contentHash),
      Some(InputPayloadFixtures.monocular.contentHash)
    )
    assertEquals(resolved.payloads.map(_._1.value), packed.payloads.map(_.name.value))
    // The admitted input is runnable and reproduces the archived result.
    val rerun = get(plan.run(get(resolved.input(name("input")).toRight("input"))))
    assertEquals(results.codec.encode(rerun), results.codec.encode(result))
  }

  test("two sources holding identical content reconstruct equal scientific values") {
    val byName   = get(resolveWith(saved.source))
    val byDigest = get(
      resolveWith(ByteSource.contentAddressed(saved.bytes +: saved.artifacts.map(_.bytes)))
    )
    assertEquals(byDigest.inputs.map(_._2.reference), byName.inputs.map(_._2.reference))
    assertEquals(byDigest.ledgers, byName.ledgers)
    assertEquals(byDigest.results.map(_._2.encode), byName.results.map(_._2.encode))
    assertEquals(
      byDigest.recordings.map(_._2.contentHash),
      byName.recordings.map(_._2.contentHash)
    )
    assertEquals(
      byDigest.temporalInputs.map(_._2.reference),
      byName.temporalInputs.map(_._2.reference)
    )
  }

  test("reformatting an artifact changes only its byte digest, never its semantic identity") {
    val compact  = get(inputs.input.encode(input)).noSpaces
    val artifact = get(
      StoredArtifact.bytes(
        "input",
        ArtifactRole.StudyInput,
        IArray.from(compact.getBytes(java.nio.charset.StandardCharsets.UTF_8)),
        Some(input.hash)
      )
    )
    assertNotEquals(artifact.entry.sha256, inputArtifact.entry.sha256)
    assertNotEquals(artifact.entry.length, inputArtifact.entry.length)
    assertEquals(artifact.entry.identity, inputArtifact.entry.identity)
    val reformatted = get(
      SavedManifest.of(
        artifacts.map(a => if a.name == name("input") then artifact else a),
        relations
      )
    )
    val resolved = get(resolveWith(reformatted.source, reformatted.manifest))
    assertEquals(resolved.input(name("input")).map(_.reference), Some(input.reference))
    assertNotEquals(reformatted.address, saved.address)
    // The old bytes do not satisfy the new declaration.
    assertEquals(
      resolveWith(saved.source, reformatted.manifest).left.map(_.map(_.entryName)),
      Left(Vector(Some(name("input"))))
    )
  }

  test(
    "corruption matrix: missing, changed, truncated and swapped bytes are refused before decode"
  ) {
    val ledgerBytes = blobs(name("ledger"))
    val resultBytes = blobs(name("result"))
    assertEquals(
      resolveWith(ByteSource.inMemory(Map.empty, blobs - name("input"))),
      Left(Vector(ResolveError.Missing(name("input"))))
    )
    val changed = flip(ledgerBytes, 100)
    assertEquals(
      resolveWith(entries(Map(name("ledger") -> changed))),
      Left(
        Vector(
          ResolveError.Digest(
            name("ledger"),
            entryOf("ledger").sha256,
            ByteDigest.sha256(changed)
          )
        )
      )
    )
    assertEquals(
      resolveWith(entries(Map(name("result") -> resultBytes.take(resultBytes.length - 1)))),
      Left(
        Vector(
          ResolveError.Length(name("result"), resultBytes.length.toLong, resultBytes.length - 1)
        )
      )
    )
    // Swapping two stored artifacts is refused for both, in manifest order.
    val swapped = entries(
      Map(name("plan") -> blobs(name("input")), name("input") -> blobs(name("plan")))
    )
    assertEquals(
      resolveWith(swapped).left.map(_.map(_.entryName)),
      Left(Vector(Some(name("plan")), Some(name("input"))))
    )
    // Every failing entry is reported, not only the first.
    val several = ByteSource.inMemory(
      Map.empty,
      blobs - name("plan") + (name("recording.values") -> flip(
        blobs(name("recording.values")),
        0
      ))
    )
    assertEquals(
      resolveWith(several).left.map(_.map(_.entryName)),
      Left(Vector(Some(name("plan")), Some(name("recording.values"))))
    )
  }

  test(
    "corruption matrix: wrong schema, wrong layout, unsupported versions and conflicting identities"
  ) {
    // The input entry claims to be a ledger document.
    val input       = entryOf("input")
    val wrongSchema = get(
      ManifestEntry.of(
        input.name,
        input.role,
        DefinitionId.admissionLedger,
        input.media,
        input.length,
        input.sha256,
        input.identity,
        None
      )
    )
    assertEquals(
      resolveWith(saved.source, rebuilt(replaced("input", wrongSchema))),
      Left(
        Vector(
          ResolveError.Schema(
            name("input"),
            DefinitionId.admissionLedger,
            DefinitionId.studyInput
          )
        )
      )
    )
    // The values payload declared flat instead of [n, 3] column-major: same bytes, wrong shape.
    val values     = entryOf("recording.values")
    val flat       = get(PayloadLayout.column(ElementKind.Float64, 24))
    val wrongShape = get(
      ManifestEntry.of(
        values.name,
        values.role,
        values.schema,
        values.media,
        values.length,
        values.sha256,
        None,
        Some(flat)
      )
    )
    val shapeErrors =
      get(resolveWith(saved.source, rebuilt(replaced("recording.values", wrongShape))).swap)
    assertEquals(shapeErrors.map(_.entryName), Vector(Some(name("recording"))))
    assert(shapeErrors.head match
      case ResolveError.Decode(
            _,
            CodecError.Entry("recording", CodecError.MissingPayload("samples.values", _))
          ) =>
        true
      case _ => false)
    // An artifact written under an unsupported schema version is refused by its decoder.
    val v2 = get(
      StoredArtifact.document(
        "input",
        ArtifactRole.StudyInput,
        get(inputs.input.encode(ManifestFixtures.input)).mapObject(
          _.add(
            "schema",
            Json.obj(
              "name"    -> Json.fromString("eyes4s.study-input"),
              "version" -> Json.fromInt(2)
            )
          )
        ),
        Some(ManifestFixtures.input.hash)
      )
    )
    val unsupported = get(
      SavedManifest.of(artifacts.map(a => if a.name == name("input") then v2 else a), relations)
    )
    val versionErrors = get(resolveWith(unsupported.source, unsupported.manifest).swap)
    assertEquals(
      versionErrors.head,
      ResolveError.Decode(
        name("input"),
        CodecError.Schema(
          DefinitionId.studyInput,
          get(DefinitionId.of("eyes4s.study-input", 2))
        )
      )
    )
    // The manifest declares the base study's identity for the pinned input.
    val conflicting = get(
      ManifestEntry.of(
        input.name,
        input.role,
        input.schema,
        input.media,
        input.length,
        input.sha256,
        Some(temporal.study.hash),
        None
      )
    )
    assertEquals(
      resolveWith(saved.source, rebuilt(replaced("input", conflicting))),
      Left(
        Vector(
          ResolveError.Identity(
            name("input"),
            temporal.study.hash.render,
            ManifestFixtures.input.hash.render
          )
        )
      )
    )
  }

  test(
    "corruption matrix: relations pointing at the wrong artifact are refused with typed mismatches"
  ) {
    def relation(r: ManifestRelation, keep: ManifestRelation => Boolean) =
      rebuilt(relations = saved.manifest.relations.filter(keep) :+ r)
    val wrongResult = ManifestRelation.ResultOf(name("result"), name("plan"), name("base"))
    assertEquals(
      resolveWith(saved.source, relation(wrongResult, _.kind != "result-of")),
      Left(
        Vector(
          ResolveError.Relation(
            wrongResult,
            RelationMismatch.ResultInput(
              temporal.study.reference.digest,
              input.reference.digest
            )
          )
        )
      )
    )
    val wrongPlan = ManifestRelation.PlanInput(name("plan"), name("base"))
    assertEquals(
      resolveWith(saved.source, relation(wrongPlan, _.kind != "plan-input")),
      Left(
        Vector(
          ResolveError.Relation(
            wrongPlan,
            RelationMismatch.Prerequisites(
              Vector(
                PlanError.ArtifactMismatch(
                  input.reference.digest,
                  temporal.study.reference.digest
                )
              )
            )
          )
        )
      )
    )
    val wrongBase = ManifestRelation.TemporalBase(name("temporal"), name("input"))
    // The temporal input embeds its base by reference, so the wrong base is
    // already refused when it is decoded.
    assert(
      resolveWith(saved.source, relation(wrongBase, _.kind != "temporal-base")).left.exists {
        case Vector(ResolveError.Decode(n, _)) => n == name("temporal")
        case _                                 => false
      }
    )
    val wrongLedger = ManifestRelation.LedgerOf(name("ledger"), name("base"))
    assert(resolveWith(saved.source, relation(wrongLedger, _.kind != "ledger-of")).left.exists {
      case Vector(ResolveError.Relation(`wrongLedger`, RelationMismatch.Admission(_))) => true
      case _                                                                           => false
    })
  }

  test(
    "a refused import cannot be related to an input, and a recording relation names both recordings"
  ) {
    val refusedArtifact = get(StoredArtifact.ledger("refused", inputs, refused))
    val other           =
      get(StoredArtifact.recording("other-recording", InputPayloadFixtures.sourceRecording))
    val withRefused = get(
      SavedManifest.of(
        artifacts ++ Vector(refusedArtifact, other),
        relations.filter(_.kind != "recording-of") ++ Vector(
          ManifestRelation.LedgerOf(name("refused"), name("input")),
          ManifestRelation.RecordingOf(name("recording-input"), name("other-recording"))
        )
      )
    )
    assertEquals(
      resolveWith(withRefused.source, withRefused.manifest),
      Left(
        Vector(
          ResolveError.Relation(
            ManifestRelation.LedgerOf(name("refused"), name("input")),
            RelationMismatch.RefusedAdmission
          ),
          ResolveError.Relation(
            ManifestRelation.RecordingOf(name("recording-input"), name("other-recording")),
            RelationMismatch.RecordingIdentity(
              InputPayloadFixtures.sourceRecording.contentHash.render,
              InputPayloadFixtures.monocular.contentHash.render
            )
          )
        )
      )
    )
    // The refused ledger is carried as evidence when no relation claims it admitted the input.
    val carried = get(SavedManifest.of(artifacts :+ refusedArtifact, relations))
    assertEquals(
      get(resolveWith(carried.source, carried.manifest)).ledger(name("refused")),
      Some(refused)
    )
  }

  test("caller mutation cannot change bytes that were verified or values that were admitted") {
    val arrays = blobs.map((n, bytes) => n -> bytes.toVector.toArray)
    val source =
      ByteSource.inMemory(Map.empty, arrays.map((n, a) => n -> IArray.unsafeFromArray(a)))
    val admitted = get(resolveWith(source))
    arrays.values.foreach(a => java.util.Arrays.fill(a, 0x20.toByte))
    assertEquals(admitted.input(name("input")).map(_.reference), Some(input.reference))
    assertEquals(
      admitted.payloads.map(_._2.bytes.toVector),
      packed.payloads.map(_.bytes.toVector)
    )
    // The same storage, now mutated, is refused.
    assert(resolveWith(source).isLeft)

    // A source that rewrites the previous buffer on every read cannot change
    // what was verified: the resolver decodes its own copy.
    var previous = Option.empty[Array[Byte]]
    val hostile  = ByteSource {
      case ByteRequest.Entry(entry) =>
        previous.foreach(a => java.util.Arrays.fill(a, 0x20.toByte))
        val fresh = blobs(entry.name).toVector.toArray
        previous = Some(fresh)
        Right(IArray.unsafeFromArray(fresh))
      case ByteRequest.Manifest(_) => Left(SourceFailure.Missing)
    }
    assertEquals(
      get(resolveWith(hostile)).inputs.map(_._2.reference),
      admitted.inputs.map(_._2.reference)
    )
  }

  test("each entry is read once; a failed integrity check decodes nothing") {
    var reads    = Vector.empty[ByteRequest]
    val counting = ByteSource { request =>
      reads = reads :+ request
      saved.source.read(request)
    }
    get(ArtifactResolver.resolve(saved.address, counting, decoders).left.map(_.toVector))
    assertEquals(reads.size, saved.manifest.entries.size + 1)
    assertEquals(reads.head, ByteRequest.Manifest(saved.address))
    assertEquals(reads.tail, saved.manifest.entries.map(ByteRequest.Entry(_)))

    // Decoding a manifest document performs no reads at all.
    reads = Vector.empty
    get(ScientificManifest.codec.decode(get(ScientificManifest.codec.encode(saved.manifest))))
    assertEquals(reads, Vector.empty)

    var decoded = 0
    val counted = new ArtifactDecoders[StudyKey, Px]:
      def plan(d: Json)   = { decoded += 1; decoders.plan(d) }
      def input(d: Json)  = { decoded += 1; decoders.input(d) }
      def ledger(d: Json) = { decoded += 1; decoders.ledger(d) }
      def result(d: Json) = { decoded += 1; decoders.result(d) }
      def recording(d: Json, p: PayloadRef => Option[VerifiedPayload]) =
        decoded += 1
        decoders.recording(d, p)
      def recordingInput(d: Json) = { decoded += 1; decoders.recordingInput(d) }
      def temporalInput(
          d: Json,
          b: ArtifactRef[StudyInput[StudyKey, Px]] => Option[StudyInput[StudyKey, Px]]
      ) =
        decoded += 1
        decoders.temporalInput(d, b)
    get(
      ArtifactResolver
        .resolveManifest(saved.manifest, saved.source, counted)
        .left
        .map(_.toVector)
    )
    assertEquals(decoded, saved.manifest.entries.count(_.role != ArtifactRole.Payload))
    decoded = 0
    val corrupted = entries(Map(name("result") -> flip(blobs(name("result")), 7)))
    assert(ArtifactResolver.resolveManifest(saved.manifest, corrupted, counted).isLeft)
    assertEquals(decoded, 0)
  }

  test("storage failures name the entry; a throwing source is reported, not propagated") {
    val failing = ByteSource {
      case ByteRequest.Entry(entry) if entry.name == name("ledger") =>
        Left(SourceFailure.Unreadable("permission denied"))
      case ByteRequest.Entry(entry) if entry.name == name("result") =>
        throw new IllegalStateException("disk vanished")
      case request => saved.source.read(request)
    }
    assertEquals(
      resolveWith(failing),
      Left(
        Vector(
          ResolveError.Unreadable(name("ledger"), "permission denied"),
          ResolveError.Unreadable(
            name("result"),
            "java.lang.IllegalStateException: disk vanished"
          )
        )
      )
    )
    val missing = ByteDigest.sha256(IArray.empty[Byte])
    assertEquals(
      ArtifactResolver.resolve(missing, saved.source, decoders).left.map(_.toVector),
      Left(Vector(ResolveError.MissingManifest(missing)))
    )
  }

  test("the manifest is digest-addressed: corrupted or re-versioned manifests are refused") {
    assertEquals(ScientificManifest.address(saved.manifest), Right(saved.address))
    val corrupted = flip(saved.bytes, 30)
    val source    = ByteSource.inMemory(Map(saved.address -> corrupted), blobs)
    assertEquals(
      ArtifactResolver.manifest(saved.address, source),
      Left(ResolveError.ManifestDigest(saved.address, ByteDigest.sha256(corrupted)))
    )
    val v2 = get(ScientificManifest.codec.encode(saved.manifest)).mapObject(
      _.add(
        "schema",
        Json.obj("name" -> Json.fromString("eyes4s.manifest"), "version" -> Json.fromInt(2))
      )
    )
    val bytes   = IArray.from(v2.spaces2.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val address = ByteDigest.sha256(bytes)
    assertEquals(
      ArtifactResolver.manifest(address, ByteSource.inMemory(Map(address -> bytes), blobs)),
      Left(
        ResolveError.ManifestDecode(
          address,
          CodecError.Schema(DefinitionId.manifest, get(DefinitionId.of("eyes4s.manifest", 2)))
        )
      )
    )
  }

  test("manifest structure: names, roles, identities, layouts and relation multiplicities") {
    val plan   = entryOf("plan")
    val input  = entryOf("input")
    val values = entryOf("recording.values")
    val base   = saved.manifest.entries
    val rels   = saved.manifest.relations
    def structural(entries: Vector[ManifestEntry], relations: Vector[ManifestRelation]) =
      ScientificManifest.of(entries, relations).swap.toOption
    assertEquals(
      structural(base :+ plan, rels),
      Some(ManifestError.DuplicateName(name("plan")))
    )
    val ghost = ManifestRelation.PlanInput(name("plan"), name("ghost"))
    assertEquals(
      structural(base, rels :+ ghost),
      Some(ManifestError.UnknownEntry(ghost, name("ghost")))
    )
    val backwards = ManifestRelation.LedgerOf(name("input"), name("ledger"))
    assertEquals(
      structural(base, rels :+ backwards),
      Some(
        ManifestError.RoleMismatch(
          backwards,
          name("input"),
          ArtifactRole.AdmissionLedger,
          ArtifactRole.StudyInput
        )
      )
    )
    val plainOwner = get(StoredArtifact.recording("plain", InputPayloadFixtures.monocular))
    val notPacked  = ManifestRelation.PayloadOf(name("plain"), name("recording.values"))
    assertEquals(
      structural(base :+ plainOwner.entry, rels :+ notPacked),
      Some(ManifestError.PayloadOwner(notPacked, DefinitionId.recording))
    )
    assertEquals(
      structural(base, rels :+ rels.head),
      Some(ManifestError.DuplicateRelation(rels.head))
    )
    assertEquals(
      structural(base, rels.filter(_.kind != "plan-input")),
      Some(ManifestError.RelationCount(name("plan"), "plan-input", 0, "exactly one"))
    )
    assertEquals(
      structural(
        base,
        rels.filter(r =>
          !(r.kind == "payload-of" && r.endpoints(1)._2 == name("recording.values"))
        )
      ),
      Some(
        ManifestError.RelationCount(name("recording.values"), "payload-of", 0, "at least one")
      )
    )
    assertEquals(
      ManifestEntry.of(
        input.name,
        input.role,
        input.schema,
        MediaKind.Binary,
        input.length,
        input.sha256,
        input.identity,
        None
      ),
      Left(
        ManifestError.MediaMismatch(name("input"), ArtifactRole.StudyInput, MediaKind.Binary)
      )
    )
    assertEquals(
      ManifestEntry.of(
        input.name,
        input.role,
        input.schema,
        input.media,
        input.length,
        input.sha256,
        None,
        None
      ),
      Left(ManifestError.IdentityPresence(name("input"), ArtifactRole.StudyInput, None))
    )
    assertEquals(
      ManifestEntry.of(
        values.name,
        values.role,
        values.schema,
        values.media,
        values.length + 8,
        values.sha256,
        None,
        values.layout
      ),
      Left(
        ManifestError.LayoutLength(
          name("recording.values"),
          values.length + 8,
          values.length.toInt
        )
      )
    )
    assertEquals(
      ManifestEntry.of(
        values.name,
        values.role,
        values.schema,
        values.media,
        values.length,
        values.sha256,
        None,
        None
      ),
      Left(
        ManifestError.PayloadDeclaration(
          name("recording.values"),
          ArtifactRole.Payload,
          values.schema,
          None
        )
      )
    )
    assertEquals(ArtifactName.of(" padded"), Left(ManifestError.InvalidName(" padded")))
    assertEquals(ArtifactName.of(""), Left(ManifestError.InvalidName("")))
    assertEquals(ArtifactName.of("x\ud800"), Left(ManifestError.InvalidName("x\ud800")))
  }

  test("the manifest codec round-trips canonically and locates malformed entries") {
    val json = get(ScientificManifest.codec.encode(saved.manifest))
    assertEquals(ScientificManifest.codec.decode(json), Right(saved.manifest))
    def editFirstEntry(field: String, value: Json): Json =
      json.hcursor
        .downField("value")
        .downField("entries")
        .downArray
        .downField(field)
        .withFocus(_ => value)
        .top
        .get
    assert(
      ScientificManifest.codec
        .decode(editFirstEntry("length", Json.fromString("0984")))
        .left
        .exists {
          case CodecError.Entry("entries[0]", CodecError.Field("length", _, _)) => true
          case _                                                                => false
        }
    )
    assert(
      ScientificManifest.codec
        .decode(
          editFirstEntry("sha256", Json.fromString(entryOf("plan").sha256.hex.toUpperCase))
        )
        .left
        .exists {
          case CodecError.Entry("entries[0]", CodecError.Field("sha256", _, _)) => true
          case _                                                                => false
        }
    )
    assert(
      ScientificManifest.codec
        .decode(editFirstEntry("role", Json.fromString("script")))
        .left
        .exists {
          case CodecError.Entry("entries[0]", CodecError.Field("role", _, reason)) =>
            reason.contains("script")
          case _ => false
        }
    )
    assertEquals(
      ScientificManifest.codec.decode(
        editFirstEntry("identity", Json.fromString(input.hash.render))
      ),
      Left(
        CodecError.Entry(
          "entries[0]",
          CodecError.Manifest(
            ManifestError.IdentityPresence(
              name("plan"),
              ArtifactRole.StudyPlan,
              Some(input.hash.render)
            )
          )
        )
      )
    )
  }
