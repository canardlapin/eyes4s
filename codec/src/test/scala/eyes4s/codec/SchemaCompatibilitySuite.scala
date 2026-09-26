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

import cats.data.NonEmptyVector
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.{Json, JsonObject}

/** The schema compatibility policy of docs/DOMAIN_CODECS.md, enforced on
  * every pinned v1 document on the JVM and Scala.js:
  *
  *   - every pinned v1 document decodes on the current code and re-encodes to
  *     its pinned JSON value, so a v1 writer cannot change shape unnoticed;
  *   - a version the reader does not know is refused with a typed error
  *     naming the expected and found identities, at the envelope, at every
  *     nested identity a codec or registry dispatches on, and through the
  *     manifest resolver; there is no version before 1 and no migration;
  *   - unknown object members are ignored at every level and dropped on
  *     re-encoding, and cannot pass verification unnoticed, because the byte
  *     digest covers them and semantic identities are re-derived;
  *   - unknown enumerated values, and members whose v1 meaning belongs to
  *     another position, are refused with a located error.
  */
class SchemaCompatibilitySuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A     = e.fold(error => fail(s"$error"), identity)
  private def parse(text: String): Json         = get(io.circe.parser.parse(text))
  private def id(name: String, version: Int)    = get(DefinitionId.of(name, version))
  private def name(value: String): ArtifactName = get(ArtifactName.of(value))
  private def utf8(text: String): IArray[Byte]  = get(Utf8.encode(text).left.map(i => s"at $i"))

  private val studies   = StudyCodecs.cosine[Px]
  private val inputs    = StudyInputCodecs.study[Px]
  private val results   = StudyResultCodecs.cosine[Px]
  private val temporals = TemporalInputCodecs.study[Px]()

  /** A pinned v1 document and the codec of its envelope schema. */
  private final class Pinned[A](
      val label: String,
      val text: String,
      val codec: VersionedCodec[A]
  ):
    def json: Json = parse(text)

    /** Decode and re-encode: the pinned JSON value round-trips exactly. */
    def reencoded(document: Json): Either[CodecError, Json] =
      codec.decode(document).flatMap(codec.encode)

  private val pinned: Vector[Pinned[?]] = Vector(
    Pinned("study-v1", SavedStudyFixtures.versionOne, studies.codec),
    Pinned("study-v2", StudyV2Mirrors.studyVersionTwo, studies.codec),
    Pinned(
      "study-trial-v2",
      StudyV2Mirrors.trialStudyVersionTwo,
      StudyCodecs.trialCosine[Px].codec
    ),
    Pinned("admission-ledger-v2", StudyV2Mirrors.ledgerVersionTwo, inputs.ledger),
    Pinned("study-input-v1", StudyInputFixtures.inputVersionOne, inputs.input),
    Pinned("admission-ledger-v1", StudyInputFixtures.ledgerVersionOne, inputs.ledger),
    Pinned("study-result-v1", StudyResultFixtures.resultVersionOne, results.codec),
    Pinned(
      "recording-input-v1",
      InputPayloadFixtures.recordingInputVersionOne,
      RecordingInputCodecs.input[Px]
    ),
    Pinned(
      "binocular-recording-input-v1",
      InputPayloadFixtures.binocularInputVersionOne,
      RecordingInputCodecs.input[Px]
    ),
    Pinned(
      "study-input-source-supported-v1",
      InputPayloadFixtures.sourceSupportedVersionOne,
      inputs.input
    ),
    Pinned(
      "temporal-study-input-v1",
      InputPayloadFixtures.temporalInputVersionOne,
      temporals.input
    ),
    Pinned(
      "recording-standalone-v1",
      InputsManifestV1Fixtures.standaloneRecordingVersionOne,
      RecordingInputCodecs.recording[Px]
    ),
    Pinned(
      "binocular-recording-v1",
      InputsManifestV1Fixtures.binocularRecordingVersionOne,
      RecordingInputCodecs.binocular[Px]
    ),
    Pinned(
      "timeline-v1",
      InputsManifestV1Fixtures.timelineVersionOne,
      InputsManifestV1Fixtures.timelineCodec
    ),
    Pinned("manifest-v1", ManifestV1Fixtures.manifestVersionOne, ScientificManifest.codec),
    Pinned(
      "manifest-inputs-v1",
      InputsManifestV1Fixtures.manifestInputsVersionOne,
      ScientificManifest.codec
    ),
    Pinned(
      "recording-v1",
      ConventionalPlanFixtures.recordingPlanVersionOne,
      ConventionalPlanFixtures.recordingCodec.codec
    ),
    Pinned(
      "temporal-study-v1",
      ConventionalPlanFixtures.temporalStudyVersionOne,
      ConventionalPlanFixtures.temporalCodec.codec
    ),
    Pinned(
      "recording-result-v1",
      ResultArchiveMirrors.recordingResultVersionOne,
      ArchiveFixtures.recordingResults.codec
    ),
    Pinned(
      "temporal-result-v1",
      ResultArchiveMirrors.temporalResultVersionOne,
      ArchiveFixtures.temporalResults.codec
    )
  ) ++ scoreEnvelopes

  /** The four score and difference envelopes of score-codecs-v1, each a
    * document of its own schema.
    */
  private def scoreEnvelopes: Vector[Pinned[?]] =
    val envelopes = get(
      parse(InputsManifestV1Fixtures.scoreCodecsVersionOne).asArray.toRight("array")
    )
    val codecs = StudyResultCodecs
    Vector[(String, VersionedCodec[?])](
      "similarity"        -> codecs.similarity(),
      "measure-distance"  -> codecs.measureDistance(),
      "scalar"            -> codecs.scalar(),
      "signed-difference" -> codecs.signedDifference()
    ).zip(envelopes).map { case ((label, codec), envelope) =>
      Pinned(s"score-codecs-v1 $label", envelope.noSpaces, codec)
    }

  test("the pinned documents cover every version of every shipped document schema") {
    assertEquals(
      pinned.flatMap(_.codec.schemas).distinct.map(id => s"${id.name}@${id.version}").sorted,
      pinned
        .map(p => get(Wire.definition(p.json, "schema")))
        .distinct
        .map(id => s"${id.name}@${id.version}")
        .sorted
    )
    assertEquals(
      pinned
        .map(p => get(Wire.definition(p.json, "schema")))
        .distinct
        .map(id => s"${id.name}@${id.version}")
        .sorted,
      Vector(
        "eyes4s.admission-ledger@1",
        "eyes4s.admission-ledger@2",
        "eyes4s.binocular-recording@1",
        "eyes4s.manifest@1",
        "eyes4s.measure-distance@1",
        "eyes4s.recording-input@1",
        "eyes4s.recording-plan@1",
        "eyes4s.recording-result@1",
        "eyes4s.recording@1",
        "eyes4s.scalar@1",
        "eyes4s.signed-difference@1",
        "eyes4s.similarity@1",
        "eyes4s.study-input@1",
        "eyes4s.study-result@1",
        "eyes4s.study@1",
        "eyes4s.study@2",
        "eyes4s.temporal-result@1",
        "eyes4s.temporal-study-input@1",
        "eyes4s.temporal-study@1",
        "eyes4s.timeline@1"
      )
    )
  }

  private def withSchema(document: Json, name: String, version: Json): Json =
    document.mapObject(
      _.add("schema", Json.obj("name" -> Json.fromString(name), "version" -> version))
    )

  /** The document with `member` added to every non-empty object. The empty
    * parameter and metadata payloads of the unit schema stay exact; see the
    * refusal test below.
    */
  private def annotated(document: Json, member: (String, Json)): Json =
    document.fold(
      Json.Null,
      Json.fromBoolean,
      Json.fromJsonNumber,
      Json.fromString,
      values => Json.arr(values.map(annotated(_, member))*),
      fields =>
        if fields.isEmpty then Json.fromJsonObject(fields)
        else
          Json.fromJsonObject(
            JsonObject.fromIterable(
              fields.toVector.map((k, v) => k -> annotated(v, member)) :+ member
            )
          )
    )

  private def members(document: Json): Vector[String] =
    document.fold(
      Vector.empty,
      _ => Vector.empty,
      _ => Vector.empty,
      _ => Vector.empty,
      _.flatMap(members),
      fields => fields.toVector.flatMap((k, v) => k +: members(v))
    )

  private val unknown = "x-application-note" -> Json.obj(
    "author" -> Json.fromString("an application"),
    "text"   -> Json.fromString("not an eyes4s member")
  )

  test("every pinned v1 document decodes and re-encodes to its pinned JSON value") {
    pinned.foreach { p =>
      assertEquals(p.reencoded(p.json), Right(p.json), p.label)
    }
  }

  test("an unknown major version of every pinned document is refused, naming both versions") {
    pinned.foreach { p =>
      val next   = p.codec.schema.version + 1
      val bumped = withSchema(p.json, p.codec.schema.name, Json.fromInt(next))
      assertEquals(
        p.codec.decode(bumped).left.toOption,
        Some(CodecError.Schema(p.codec.schema, id(p.codec.schema.name, next))),
        p.label
      )
    }
    // Nor does any codec read a document of another schema.
    assertEquals(
      studies.codec.parse(StudyInputFixtures.inputVersionOne).left.toOption,
      Some(CodecError.Schema(StudyCodecDefinitions.studyV2, DefinitionId.studyInput))
    )
  }

  test(
    "there is no version before 1: version 0, a negative or a fractional version is refused"
  ) {
    pinned.foreach { p =>
      Vector(0, -1).foreach { v =>
        assertEquals(
          p.codec
            .decode(withSchema(p.json, p.codec.schema.name, Json.fromInt(v)))
            .left
            .toOption,
          Some(CodecError.Definition(PlanError.InvalidDefinition(p.codec.schema.name, v))),
          s"${p.label} at version $v"
        )
      }
      p.codec.decode(withSchema(p.json, p.codec.schema.name, Json.fromDoubleOrNull(1.5))) match
        case Left(CodecError.Field("version", _, _)) => ()
        case other                                   => fail(s"${p.label}: unexpected $other")
    }
  }

  test("registries and dispatchers refuse unknown versions of the identities they select on") {
    val plans    = get(StudyRegistry.empty[StudyKey, Px].register(studies.registration))
    val keyed    = get(StudyInputRegistry.empty[StudyKey, Px].register(inputs))
    val archived = get(StudyResultRegistry.empty[StudyKey, Px].register(results.registration))
    def nested(text: String, field: String, version: Int): Json =
      val json = parse(text)
      json.mapObject(
        _.add(
          "value",
          get(json.hcursor.get[Json]("value")).mapObject(inner =>
            inner.add(
              field,
              get(inner(field).toRight(field))
                .mapObject(_.add("version", Json.fromInt(version)))
            )
          )
        )
      )
    val cosine2 = id(DefinitionId.cosine.name, 2)
    assertEquals(
      plans.decode(nested(SavedStudyFixtures.versionOne, "method", 2)).left.toOption,
      Some(CodecError.MissingMethod(cosine2))
    )
    assertEquals(
      plans
        .decode(
          withSchema(parse(SavedStudyFixtures.versionOne), "eyes4s.study", Json.fromInt(3))
        )
        .left
        .toOption,
      Some(CodecError.Schema(StudyCodecDefinitions.studyV2, id("eyes4s.study", 3)))
    )
    assertEquals(
      studies.codec.decode(nested(SavedStudyFixtures.versionOne, "layout", 2)).left.toOption,
      Some(CodecError.Schema(DefinitionId.studyLayout, id(DefinitionId.studyLayout.name, 2)))
    )
    assertEquals(
      keyed
        .decodeInput(nested(StudyInputFixtures.inputVersionOne, "keySchema", 2))
        .left
        .toOption,
      Some(CodecError.MissingKeySchema(id(DefinitionId.studyKey.name, 2)))
    )
    assertEquals(
      keyed
        .decodeLedger(nested(StudyInputFixtures.ledgerVersionOne, "keySchema", 2))
        .left
        .toOption,
      Some(CodecError.MissingKeySchema(id(DefinitionId.studyKey.name, 2)))
    )
    assertEquals(
      archived.decode(nested(StudyResultFixtures.resultVersionOne, "method", 2)).left.toOption,
      Some(CodecError.MissingResultCodec(cosine2))
    )
    // The recording and temporal archives dispatch on their method, and read
    // their embedded plan through its own versioned codec.
    val recordingArchives = get(
      RecordingResultRegistry.empty.register(ArchiveFixtures.recordingResults.registration)
    )
    val idt2 = id(ArchiveFixtures.recordingCodec.method.id.name, 2)
    assertEquals(
      recordingArchives
        .decode(nested(ResultArchiveMirrors.recordingResultVersionOne, "method", 2))
        .left
        .toOption,
      Some(CodecError.MissingResultCodec(idt2))
    )
    val temporalArchives = get(
      TemporalResultRegistry
        .empty[StudyKey, Px]
        .register(ArchiveFixtures.temporalResults.registration)
    )
    assertEquals(
      temporalArchives
        .decode(nested(ResultArchiveMirrors.temporalResultVersionOne, "method", 2))
        .left
        .toOption,
      Some(CodecError.MissingResultCodec(cosine2))
    )
    val temporalPlans = get(
      TemporalRegistry.empty[StudyKey, Px].register(ArchiveFixtures.temporalCodec.registration)
    )
    val nestedMethod = parse(ConventionalPlanFixtures.temporalStudyVersionOne).hcursor
      .downField("value")
      .downField("study")
      .downField("value")
      .downField("method")
      .downField("version")
      .withFocus(_ => Json.fromInt(2))
      .top
      .get
    assertEquals(
      temporalPlans.decode(nestedMethod).left.toOption,
      Some(CodecError.MissingMethod(cosine2))
    )
    val embeddedPlan = parse(ResultArchiveMirrors.recordingResultVersionOne).hcursor
      .downField("value")
      .downField("plan")
      .downField("schema")
      .downField("version")
      .withFocus(_ => Json.fromInt(2))
      .top
      .get
    assertEquals(
      ArchiveFixtures.recordingResults.codec.decode(embeddedPlan).left.toOption,
      Some(
        CodecError.Entry(
          "plan",
          CodecError.Schema(
            ArchiveFixtures.recordingCodec.schema,
            id(ArchiveFixtures.recordingCodec.schema.name, 2)
          )
        )
      )
    )
    assertEquals(
      results.codec
        .decode(nested(StudyResultFixtures.resultVersionOne, "scoreSchema", 2))
        .left
        .toOption,
      Some(CodecError.Schema(DefinitionId.similarity, id(DefinitionId.similarity.name, 2)))
    )
    assertEquals(
      results.codec
        .decode(nested(StudyResultFixtures.resultVersionOne, "differenceSchema", 2))
        .left
        .toOption,
      Some(
        CodecError.Schema(
          DefinitionId.signedDifference,
          id(DefinitionId.signedDifference.name, 2)
        )
      )
    )
    // The parameter payload of the cosine plan is a unit@1 envelope.
    val parameters = parse(SavedStudyFixtures.versionOne).hcursor
      .downField("value")
      .downField("parameters")
      .downField("schema")
      .downField("version")
      .withFocus(_ => Json.fromInt(2))
      .top
      .get
    assertEquals(
      studies.codec.decode(parameters).left.toOption,
      Some(CodecError.Schema(DefinitionId.unit, id(DefinitionId.unit.name, 2)))
    )
    // Standalone recordings are dispatched by their envelope schema.
    val decoders   = get(ArtifactDecoders.study[Px])
    val recording2 = withSchema(
      parse(InputsManifestV1Fixtures.standaloneRecordingVersionOne),
      DefinitionId.recording.name,
      Json.fromInt(2)
    )
    assertEquals(
      decoders.recording(recording2, _ => None).left.toOption,
      Some(
        CodecError.UnsupportedSchema(
          "recording",
          id(DefinitionId.recording.name, 2),
          Vector(
            DefinitionId.recording,
            DefinitionId.binocularRecording,
            DefinitionId.packedRecording
          )
        )
      )
    )
  }

  test("packed-recording@1 follows the policy: versions refused, members ignored and dropped") {
    val codec    = PackedRecordingCodecs.recording[Px]
    val payloads = get(codec.encode(InputPayloadFixtures.monocular)).payloads
    def lookup(ref: PayloadRef): Option[VerifiedPayload] = payloads.find(_.ref == ref)
    val document              = parse(InputsManifestV1Fixtures.packedRecordingVersionOne)
    def reencoded(json: Json) = codec.decode(json, lookup).flatMap(codec.encode).map(_.document)
    assertEquals(reencoded(document), Right(document))
    val packed2 = id(DefinitionId.packedRecording.name, 2)
    val bumped  = withSchema(document, DefinitionId.packedRecording.name, Json.fromInt(2))
    assertEquals(
      codec.decode(bumped, lookup).left.toOption,
      Some(CodecError.Schema(DefinitionId.packedRecording, packed2))
    )
    assertEquals(
      PackedRecordingCodecs.references(bumped).left.toOption,
      Some(CodecError.Schema(DefinitionId.packedRecording, packed2))
    )
    assertEquals(
      codec
        .decode(
          withSchema(document, DefinitionId.packedRecording.name, Json.fromInt(0)),
          lookup
        )
        .left
        .toOption,
      Some(
        CodecError.Definition(PlanError.InvalidDefinition(DefinitionId.packedRecording.name, 0))
      )
    )
    assertEquals(
      get(ArtifactDecoders.study[Px]).recording(bumped, lookup).left.toOption,
      Some(
        CodecError.UnsupportedSchema(
          "recording",
          packed2,
          Vector(
            DefinitionId.recording,
            DefinitionId.binocularRecording,
            DefinitionId.packedRecording
          )
        )
      )
    )
    val extended = annotated(document, unknown)
    assert(members(extended).count(_ == unknown._1) > 1)
    assertEquals(reencoded(extended), Right(document))
  }

  test("the resolver refuses an unknown manifest version and an unknown artifact version") {
    val decoders = get(ArtifactDecoders.study[Px])
    // A manifest@2 document stored under its own address.
    val manifest2 = utf8(
      withSchema(
        parse(ManifestV1Fixtures.manifestVersionOne),
        "eyes4s.manifest",
        Json.fromInt(2)
      ).spaces2
    )
    val address2 = ByteDigest.sha256(manifest2)
    assertEquals(
      ArtifactResolver.manifest(address2, ByteSource(_ => Right(manifest2))),
      Left(
        ResolveError.ManifestDecode(
          address2,
          CodecError.Schema(DefinitionId.manifest, id(DefinitionId.manifest.name, 2))
        )
      )
    )
    val manifest0 = utf8(
      withSchema(
        parse(ManifestV1Fixtures.manifestVersionOne),
        "eyes4s.manifest",
        Json.fromInt(0)
      ).spaces2
    )
    assertEquals(
      ArtifactResolver
        .manifest(ByteDigest.sha256(manifest0), ByteSource(_ => Right(manifest0))),
      Left(
        ResolveError.ManifestDecode(
          ByteDigest.sha256(manifest0),
          CodecError.Definition(PlanError.InvalidDefinition(DefinitionId.manifest.name, 0))
        )
      )
    )
    // An input stored as study-input@2 is declared as such, and refused by its decoder.
    val input2 = utf8(
      withSchema(
        parse(StudyInputFixtures.inputVersionOne),
        "eyes4s.study-input",
        Json.fromInt(2)
      ).spaces2
    )
    val declared = get(
      StoredArtifact.bytes(
        "input",
        ArtifactRole.StudyInput,
        input2,
        Some(ManifestFixtures.input.hash)
      )
    )
    assertEquals(declared.entry.schema, id(DefinitionId.studyInput.name, 2))
    val saved = get(SavedManifest.of(Vector(declared), Vector.empty))
    assertEquals(
      ArtifactResolver.resolve(saved.address, saved.source, decoders).left.map(_.toVector),
      Left(
        Vector(
          ResolveError.Decode(
            name("input"),
            CodecError.Schema(DefinitionId.studyInput, id(DefinitionId.studyInput.name, 2))
          )
        )
      )
    )
    // A manifest that declares @1 for bytes holding @2 is refused before decoding.
    val entry = get(
      ManifestEntry.of(
        name("input"),
        ArtifactRole.StudyInput,
        DefinitionId.studyInput,
        MediaKind.JsonText,
        input2.length.toLong,
        ByteDigest.sha256(input2),
        Some(ManifestFixtures.input.hash),
        None
      )
    )
    val claimed = get(ScientificManifest.of(Vector(entry), Vector.empty))
    assertEquals(
      ArtifactResolver
        .resolveManifest(
          claimed,
          ByteSource.inMemory(Map.empty, Map(name("input") -> input2)),
          decoders
        )
        .left
        .map(_.toVector),
      Left(
        Vector(
          ResolveError.Schema(
            name("input"),
            DefinitionId.studyInput,
            id(DefinitionId.studyInput.name, 2)
          )
        )
      )
    )
  }

  test("unknown members are ignored at every level and dropped on re-encoding") {
    pinned.foreach { p =>
      val extended = annotated(p.json, unknown)
      assert(members(extended).count(_ == unknown._1) > 1, p.label)
      val reencoded = p.reencoded(extended)
      assertEquals(reencoded, Right(p.json), p.label)
      assert(!reencoded.toOption.exists(members(_).contains(unknown._1)), p.label)
    }
    // Identity-bearing documents re-derive the same semantic identity.
    assertEquals(
      inputs.input
        .decode(annotated(parse(StudyInputFixtures.inputVersionOne), unknown))
        .map(_.hash),
      Right(ManifestFixtures.input.hash)
    )
  }

  test("an ignored member cannot pass verification unnoticed") {
    val decoders = get(ArtifactDecoders.study[Px])
    val original = get(
      StoredArtifact.bytes(
        "input",
        ArtifactRole.StudyInput,
        utf8(StudyInputFixtures.inputVersionOne),
        Some(ManifestFixtures.input.hash)
      )
    )
    val saved    = get(SavedManifest.of(Vector(original), Vector.empty))
    val extended = utf8(annotated(parse(StudyInputFixtures.inputVersionOne), unknown).noSpaces)
    // Under the manifest that listed the original bytes, the extended bytes are refused
    // before anything is decoded.
    assertEquals(
      ArtifactResolver
        .resolve(
          saved.address,
          ByteSource
            .inMemory(Map(saved.address -> saved.bytes), Map(name("input") -> extended)),
          decoders
        )
        .left
        .map(_.toVector),
      Left(Vector(ResolveError.Length(name("input"), original.entry.length, extended.length)))
    )
    // Padded to the original length, the extended bytes pass the length check
    // and are refused by their SHA-256.
    val pretty = utf8(parse(StudyInputFixtures.inputVersionOne).spaces2)
    val stored = get(
      StoredArtifact.bytes(
        "input",
        ArtifactRole.StudyInput,
        pretty,
        Some(ManifestFixtures.input.hash)
      )
    )
    val listed = get(SavedManifest.of(Vector(stored), Vector.empty))
    val noted  =
      parse(StudyInputFixtures.inputVersionOne).mapObject(_.add(unknown._1, unknown._2))
    val padded = utf8(noted.noSpaces.padTo(pretty.length, ' '))
    assertEquals(padded.length, pretty.length)
    assertEquals(
      ArtifactResolver
        .resolve(
          listed.address,
          ByteSource
            .inMemory(Map(listed.address -> listed.bytes), Map(name("input") -> padded)),
          decoders
        )
        .left
        .map(_.toVector),
      Left(
        Vector(
          ResolveError.Digest(name("input"), stored.entry.sha256, ByteDigest.sha256(padded))
        )
      )
    )
    // A manifest written over the extended bytes admits them: the member carries no
    // v1 meaning, and the input's semantic identity is the one re-derived from its values.
    val rewritten = get(
      StoredArtifact.bytes(
        "input",
        ArtifactRole.StudyInput,
        extended,
        Some(ManifestFixtures.input.hash)
      )
    )
    val resaved = get(SavedManifest.of(Vector(rewritten), Vector.empty))
    assertNotEquals(rewritten.entry.sha256, original.entry.sha256)
    val resolved = get(
      ArtifactResolver.resolve(resaved.address, resaved.source, decoders).left.map(_.toVector)
    )
    assertEquals(resolved.input(name("input")).map(_.hash), Some(ManifestFixtures.input.hash))
    // The manifest itself: a stored manifest with an extra member has another address; under
    // its own address it decodes to the same manifest, whose canonical bytes omit the member.
    val frozen = get(ScientificManifest.codec.parse(ManifestV1Fixtures.manifestVersionOne))
    val annotatedManifest =
      utf8(annotated(parse(ManifestV1Fixtures.manifestVersionOne), unknown).spaces2)
    val notedAt  = ByteDigest.sha256(annotatedManifest)
    val frozenAt = get(ByteDigest.parse(ManifestV1Fixtures.address))
    assertEquals(
      ArtifactResolver.manifest(frozenAt, ByteSource(_ => Right(annotatedManifest))),
      Left(ResolveError.ManifestDigest(frozenAt, notedAt))
    )
    assertEquals(
      ArtifactResolver.manifest(notedAt, ByteSource(_ => Right(annotatedManifest))),
      Right(frozen)
    )
    assertNotEquals(get(ScientificManifest.bytes(frozen)).toVector, annotatedManifest.toVector)
  }

  test("unknown enumerated values are refused with a located error") {
    def entry0(field: String, value: String): Json =
      parse(ManifestV1Fixtures.manifestVersionOne).hcursor
        .downField("value")
        .downField("entries")
        .downArray
        .downField(field)
        .withFocus(_ => Json.fromString(value))
        .top
        .get
    def located(result: Either[CodecError, ?], path: String, field: String): Unit =
      result match
        case Left(CodecError.Entry(`path`, CodecError.Field(`field`, _, _))) => ()
        case other => fail(s"expected a refusal of $field at $path, got $other")
    located(
      ScientificManifest.codec.decode(entry0("role", "study-protocol")),
      "entries[0]",
      "role"
    )
    located(
      ScientificManifest.codec.decode(entry0("media", "application/gzip")),
      "entries[0]",
      "media"
    )
    val relation = parse(ManifestV1Fixtures.manifestVersionOne).hcursor
      .downField("value")
      .downField("relations")
      .downArray
      .downField("kind")
      .withFocus(_ => Json.fromString("derived-from"))
      .top
      .get
    located(ScientificManifest.codec.decode(relation), "relations[0]", "kind")
    val layout = parse(InputsManifestV1Fixtures.manifestInputsVersionOne).hcursor
      .downField("value")
      .downField("entries")
      .downN(5)
      .downField("layout")
      .downField("element")
      .withFocus(_ => Json.fromString("float32"))
      .top
      .get
    located(ScientificManifest.codec.decode(layout), "entries[5].layout", "element")
    val estimator = parse(SavedStudyFixtures.versionOne).hcursor
      .downField("value")
      .downField("estimates")
      .downArray
      .downField("kind")
      .withFocus(_ => Json.fromString("kde"))
      .top
      .get
    assertEquals(
      studies.codec.decode(estimator).left.toOption,
      Some(
        CodecError.Field(
          "estimate",
          Json.obj("kind" -> Json.fromString("kde")),
          "unknown estimator kde"
        )
      )
    )
    val state = parse(InputsManifestV1Fixtures.standaloneRecordingVersionOne).hcursor
      .downField("value")
      .downField("recording")
      .downField("samples")
      .downField("state")
      .downArray
      .withFocus(_ => Json.fromString("saccade"))
      .top
      .get
    assertEquals(
      RecordingInputCodecs.recording[Px].decode(state).left.toOption,
      Some(
        CodecError.Entry(
          "recording.samples[0]",
          CodecError.Field(
            "state",
            Json.fromString("saccade"),
            "unknown sample support category"
          )
        )
      )
    )
    val embedding = parse(InputPayloadFixtures.temporalInputVersionOne).hcursor
      .downField("value")
      .downField("study")
      .downField("kind")
      .withFocus(_ => Json.fromString("external"))
      .top
      .get
    temporals.input.decode(embedding) match
      case Left(CodecError.Entry("study", CodecError.Field("kind", _, reason))) =>
        assertEquals(reason, "unknown study embedding external")
      case other => fail(s"unexpected $other")
  }

  test("members whose v1 meaning belongs elsewhere are refused where they would be misread") {
    // The parameterless cosine method's unit@1 parameter payload is exactly `{}`.
    val parameters = parse(SavedStudyFixtures.versionOne).hcursor
      .downField("value")
      .downField("parameters")
      .downField("value")
      .withFocus(_ => Json.obj("sigma" -> Json.fromInt(2)))
      .top
      .get
    assertEquals(
      studies.codec.decode(parameters).left.toOption,
      Some(
        CodecError.Field(
          "unit",
          Json.obj("sigma" -> Json.fromInt(2)),
          "expected an empty object"
        )
      )
    )
    // So is a trial's unit@1 metadata payload.
    val metadata = parse(StudyInputFixtures.inputVersionOne).hcursor
      .downField("value")
      .downField("trials")
      .downField("value")
      .downArray
      .downField("meta")
      .downField("value")
      .withFocus(_ => Json.obj("session" -> Json.fromInt(2)))
      .top
      .get
    assertEquals(
      inputs.input.decode(metadata).left.toOption,
      Some(
        CodecError.Entry(
          "trials.rows[0]",
          CodecError.Field(
            "unit",
            Json.obj("session" -> Json.fromInt(2)),
            "expected an empty object"
          )
        )
      )
    )
    // A planned or observed timeline is not a neutral timeline.
    val planned = parse(InputsManifestV1Fixtures.timelineVersionOne).hcursor
      .downField("value")
      .withFocus(_.mapObject(_.add("timing", Json.fromString("planned"))))
      .top
      .get
    InputsManifestV1Fixtures.timelineCodec.decode(planned) match
      case Left(CodecError.Field("timing", _, _)) => ()
      case other                                  => fail(s"unexpected $other")
    // A source-supported fixation's dispersion is derived from its samples, never declared.
    val declared = parse(InputPayloadFixtures.sourceSupportedVersionOne).hcursor
      .downField("value")
      .downField("trials")
      .downField("value")
      .downArray
      .downField("value")
      .downField("value")
      .downField("fixations")
      .downArray
      .downField("dispersion")
      .withFocus(_.mapObject(_.add("value", Json.fromDoubleOrNull(1.0))))
      .top
    assert(declared.isDefined, "the pinned source-supported fixation declares a dispersion")
    inputs.input.decode(declared.get).left.toOption match
      case Some(CodecError.Entry(path, CodecError.Field("value", _, reason))) =>
        assertEquals(path, "trials.rows[0].fixations[0]")
        assertEquals(
          reason,
          "a source-supported dispersion carries its method only; its value is derived"
        )
      case other => fail(s"unexpected $other")
  }

  test("a refusal is a value: resolution reports every refused entry of a phase") {
    // Two entries of unknown versions are both reported, not just the first.
    val decoders = get(ArtifactDecoders.study[Px])
    val plan2    = utf8(
      withSchema(parse(SavedStudyFixtures.versionOne), "eyes4s.study", Json.fromInt(2)).noSpaces
    )
    val input2 = utf8(
      withSchema(
        parse(StudyInputFixtures.inputVersionOne),
        "eyes4s.study-input",
        Json.fromInt(2)
      ).noSpaces
    )
    val saved = get(
      for
        p <- StoredArtifact.bytes("plan", ArtifactRole.StudyPlan, plan2, None)
        i <- StoredArtifact.bytes(
          "input",
          ArtifactRole.StudyInput,
          input2,
          Some(ManifestFixtures.input.hash)
        )
        s <- SavedManifest.of(Vector(p, i), Vector(ManifestRelation.PlanInput(p.name, i.name)))
      yield s
    )
    ArtifactResolver.resolve(saved.address, saved.source, decoders) match
      case Left(errors: NonEmptyVector[ResolveError]) =>
        assertEquals(
          errors.toVector.flatMap(_.entryName).map(_.value).sorted,
          Vector("input", "plan")
        )
      case other => fail(s"unexpected $other")
  }
