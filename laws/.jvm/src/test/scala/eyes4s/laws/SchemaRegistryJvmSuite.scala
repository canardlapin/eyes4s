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
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Every schema and definition identity eyes4s ships, with the published
  * round-trip law and the pinned v1 fixture that stand behind it.
  *
  * The registry is checked by reflection against the `DefinitionId`
  * companion and every `*Definitions` object in the `eyes4s` packages, where
  * new built-in identities are declared file by file (see
  * `DefinitionId.builtIn`), so adding a built-in identity without an entry here, or an
  * entry without a law suite that actually registers the named law or a
  * resource that actually carries the identity, fails the build. Conversely
  * every pinned fixture under `codec/src/test/resources/eyes4s` must be
  * claimed by an entry and decode on the current code.
  */
object SchemaRegistry:
  /** What an identity names on the wire. */
  enum Kind derives CanEqual:
    /** The envelope schema of a stored document; its fixtures decode through it. */
    case Document

    /** A schema nested inside documents (keys, trials, scanpaths, scores). */
    case Nested

    /** A method or layout identity carried inside a plan or result. */
    case Definition

    /** The schema of a binary payload declared by a manifest entry. */
    case Payload

  /** A published law registered by `suite` under `checkAll(name, ...)`,
    * identified by one of its property names.
    */
  final case class Law(suite: () => munit.Suite, name: String, property: String):
    def test: String = s"$name: $property"

  private val roundTrip = "versionedCodec.round trip preserves the value"
  private val canonical = "versionedCodec.reencoding is canonical"
  private def codecLaw(suite: () => munit.Suite, name: String): Vector[Law] =
    Vector(Law(suite, name, roundTrip), Law(suite, name, canonical))

  private val plans      = () => new PlanCodecLawSuite
  private val inputs     = () => new StudyInputCodecLawSuite
  private val results    = () => new StudyResultCodecLawSuite
  private val recordings = () => new RecordingInputCodecLawSuite
  private val artifacts  = () => new ArtifactCodecLawSuite
  private val codecs     = () => new CodecLawsSuite
  private val graphs     = () => new ManifestLawSuite
  private val recorded   = () => new RecordingResultCodecLawSuite
  private val temporal   = () => new TemporalResultCodecLawSuite

  /** One identity: its kind, its pinned fixtures and its laws. A document's
    * first fixture carries it as its envelope schema.
    */
  final case class Entry(
      id: DefinitionId,
      kind: Kind,
      fixtures: Vector[String],
      laws: Vector[Law]
  )

  val builtIns: Vector[Entry] = Vector(
    Entry(
      DefinitionId.cosine,
      Kind.Definition,
      Vector("study-v1.json", "study-result-v1.json"),
      codecLaw(plans, "study plan") ++ codecLaw(results, "cosine study result")
    ),
    Entry(
      DefinitionId.study,
      Kind.Document,
      Vector("study-v1.json"),
      codecLaw(plans, "study plan")
    ),
    Entry(
      DefinitionId.studyKey,
      Kind.Nested,
      Vector("study-v1.json", "study-input-v1.json", "timeline-v1.json"),
      codecLaw(codecs, "key") ++ codecLaw(inputs, "study input")
    ),
    Entry(
      DefinitionId.studyLayout,
      Kind.Definition,
      Vector("study-v1.json", "study-input-v1.json", "study-result-v1.json"),
      codecLaw(plans, "study plan") ++ codecLaw(inputs, "study input")
    ),
    Entry(
      DefinitionId.unit,
      Kind.Nested,
      Vector("study-v1.json", "study-input-v1.json"),
      codecLaw(plans, "study plan") ++ codecLaw(inputs, "study input")
    ),
    Entry(
      DefinitionId.studyInput,
      Kind.Document,
      Vector(
        "study-input-v1.json",
        "study-input-source-supported-v1.json",
        "temporal-base-v1.json"
      ),
      codecLaw(inputs, "study input")
    ),
    Entry(
      DefinitionId.trials,
      Kind.Nested,
      Vector("study-input-v1.json"),
      codecLaw(inputs, "study input") ++ codecLaw(inputs, "generic trials")
    ),
    Entry(
      DefinitionId.scanpath,
      Kind.Nested,
      Vector("study-input-v1.json", "study-input-source-supported-v1.json"),
      codecLaw(inputs, "study input")
    ),
    Entry(
      DefinitionId.admissionLedger,
      Kind.Document,
      Vector("admission-ledger-v1.json", "admission-ledger-complete-v1.json"),
      codecLaw(inputs, "admission ledger")
    ),
    Entry(
      StudyCodecDefinitions.studyV2,
      Kind.Document,
      Vector("study-v2.json", "study-trial-v2.json"),
      codecLaw(plans, "configured study plan") ++ codecLaw(plans, "trial study plan")
    ),
    Entry(
      TrialKeyDefinitions.trialKey,
      Kind.Nested,
      Vector("study-trial-v2.json"),
      codecLaw(codecs, "trial key") ++ codecLaw(plans, "trial study plan")
    ),
    Entry(
      TrialKeyDefinitions.trialLayout,
      Kind.Definition,
      Vector("study-trial-v2.json"),
      codecLaw(plans, "trial study plan")
    ),
    Entry(
      StudyInputDefinitions.admissionLedgerV2,
      Kind.Document,
      Vector("admission-ledger-v2.json"),
      codecLaw(inputs, "admission ledger with policy")
    ),
    Entry(
      InventoryDefinitions.admissionLedgerV3,
      Kind.Document,
      Vector("admission-ledger-v3.json"),
      codecLaw(inputs, "admission ledger with inventory")
    ),
    Entry(
      DefinitionId.recording,
      Kind.Document,
      Vector("recording-standalone-v1.json"),
      codecLaw(recordings, "recording")
    ),
    Entry(
      DefinitionId.binocularRecording,
      Kind.Document,
      Vector("binocular-recording-v1.json"),
      codecLaw(recordings, "binocular recording")
    ),
    Entry(
      DefinitionId.recordingInput,
      Kind.Document,
      Vector("recording-input-v1.json", "binocular-recording-input-v1.json"),
      codecLaw(recordings, "recording input")
    ),
    Entry(
      DefinitionId.temporalStudyInput,
      Kind.Document,
      Vector("temporal-study-input-v1.json"),
      codecLaw(recordings, "temporal input")
    ),
    Entry(
      DefinitionId.timeline,
      Kind.Document,
      Vector("timeline-v1.json"),
      codecLaw(recordings, "timeline")
    ),
    Entry(
      DefinitionId.studyResult,
      Kind.Document,
      Vector("study-result-v1.json"),
      codecLaw(results, "cosine study result")
    ),
    Entry(
      DefinitionId.similarity,
      Kind.Nested,
      Vector("score-codecs-v1.json", "study-result-v1.json"),
      codecLaw(artifacts, "similarity") ++ codecLaw(results, "cosine study result")
    ),
    Entry(
      DefinitionId.measureDistance,
      Kind.Nested,
      Vector("score-codecs-v1.json"),
      codecLaw(artifacts, "measure distance")
    ),
    Entry(
      DefinitionId.scalar,
      Kind.Nested,
      Vector("score-codecs-v1.json"),
      codecLaw(artifacts, "scalar")
    ),
    Entry(
      DefinitionId.signedDifference,
      Kind.Nested,
      Vector("score-codecs-v1.json", "study-result-v1.json"),
      codecLaw(artifacts, "signed difference") ++ codecLaw(results, "cosine study result")
    ),
    Entry(
      DefinitionId.manifest,
      Kind.Document,
      Vector("manifest-v1.json", "manifest-inputs-v1.json"),
      codecLaw(artifacts, "manifest") :+
        Law(
          graphs,
          "saved study graph",
          "verifiedManifest.a written graph resolves by address, reading each artifact once"
        )
    ),
    Entry(
      DefinitionId.packedRecording,
      Kind.Document,
      Vector("packed-recording-v1.json"),
      Vector(
        Law(
          artifacts,
          "packed recording",
          "packedRecording.decoding from its own payloads reproduces the recording"
        ),
        Law(
          artifacts,
          "packed recording",
          "packedRecording.re-packing the decoded recording is canonical"
        )
      )
    ),
    Entry(
      DefinitionId.packedArray,
      Kind.Payload,
      Vector(
        "manifest-inputs-v1.json",
        "packed-recording-v1.tMicros.bin",
        "packed-recording-v1.support.bin",
        "packed-recording-v1.lineage.bin",
        "packed-recording-v1.values.bin"
      ),
      Vector("packed float64", "packed int64", "packed int32", "packed uint8").map(name =>
        Law(
          artifacts,
          name,
          "packedArray.unpacking a packed array returns every element exactly"
        )
      )
    ),
    Entry(
      DefinitionId.recordingResult,
      Kind.Document,
      Vector("recording-result-v1.json"),
      codecLaw(recorded, "I-VT recording result") ++ codecLaw(recorded, "I-DT recording result")
    ),
    Entry(
      DefinitionId.temporalResult,
      Kind.Document,
      Vector("temporal-result-v1.json"),
      codecLaw(temporal, "temporal study result")
    )
  )

  /** Shipped codecs whose schema identity the caller supplies, pinned under
    * the conventional identity their fixture uses.
    */
  val conventional: Vector[Entry] = Vector(
    Entry(
      AdditionalRecipeCodecs.point.resultSchema,
      Kind.Document,
      Vector("point-sampling-v1.json"),
      codecLaw(() => new AdditionalRecipeCodecLawSuite, "static point sampling archive")
    ),
    Entry(
      AdditionalRecipeCodecs.repetition.schema,
      Kind.Document,
      Vector("repetition-plan-v1.json"),
      codecLaw(() => new AdditionalRecipeCodecLawSuite, "all-occasion repetition plan")
    ),
    Entry(
      get(DefinitionId.of("eyes4s.recording-plan", 1)),
      Kind.Document,
      Vector("recording-v1.json"),
      codecLaw(plans, "I-VT recording plan") ++ codecLaw(plans, "I-DT recording plan") ++
        codecLaw(plans, "Engbert-Kliegl recording plan")
    ),
    Entry(
      get(DefinitionId.of("eyes4s.temporal-study", 1)),
      Kind.Document,
      Vector("temporal-study-v1.json"),
      codecLaw(plans, "temporal study plan")
    )
  )

  private val ladderProperties = Vector(
    "a value is written under the earliest version that expresses it",
    "a document written as vN decodes with a vN-only reader",
    "upcasting a vN payload writes exactly what each later version writes",
    "a lifted document decodes to the value and re-encodes to the earliest document"
  ).map(p => s"schemaLadder.$p")
  private def ladderLaw(suite: () => munit.Suite, name: String): Vector[Law] =
    ladderProperties.map(Law(suite, name, _))

  /** One shipped ladder of a multi-version schema and the pinned documents
    * it reads.
    */
  final case class Rungs(ladder: SchemaLadder[?], fixtures: Vector[String])

  /** A schema with more than one version: the shipped ladders that carry its
    * versions (one per key layout), and the published ladder laws.
    */
  final case class Versioned(name: String, ladders: Vector[Rungs], laws: Vector[Law])

  val versioned: Vector[Versioned] = Vector(
    Versioned(
      "eyes4s.study",
      Vector(
        Rungs(StudyCodecs.cosine[Px].ladder, Vector("study-v1.json", "study-v2.json")),
        Rungs(StudyCodecs.trialCosine[Px].ladder, Vector("study-trial-v2.json"))
      ),
      ladderLaw(plans, "study plan versions") ++ ladderLaw(plans, "trial study plan versions")
    ),
    Versioned(
      "eyes4s.admission-ledger",
      Vector(
        Rungs(
          StudyInputCodecs.study[Px].ledgerLadder,
          Vector(
            "admission-ledger-v1.json",
            "admission-ledger-complete-v1.json",
            "admission-ledger-v2.json"
          )
        ),
        Rungs(StudyInputCodecs.trial[Px].ledgerLadder, Vector("admission-ledger-v3.json"))
      ),
      ladderLaw(inputs, "admission ledger versions") ++
        ladderLaw(inputs, "inventory ledger versions")
    )
  )

  def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new IllegalStateException(s"$e"), identity)

class SchemaRegistryJvmSuite extends munit.FunSuite:
  import SchemaRegistry.*

  private def resource(name: String): Option[Array[Byte]] =
    Option(getClass.getResourceAsStream(s"/eyes4s/$name")).map { stream =>
      try stream.readAllBytes()
      finally stream.close()
    }
  private def json(name: String): Json =
    val bytes = resource(name).getOrElse(fail(s"missing pinned fixture $name"))
    io.circe.parser
      .parse(new String(bytes, "UTF-8"))
      .fold(e => fail(s"$name: ${e.message}"), identity)

  /** Every `{"name": ..., "version": ...}` identity carried anywhere in a document. */
  private def identities(document: Json): Set[DefinitionId] =
    val here = for
      name    <- document.hcursor.get[String]("name").toOption
      version <- document.hcursor.get[Int]("version").toOption
      id      <- DefinitionId.of(name, version).toOption
      if document.asObject.exists(_.size == 2)
    yield id
    here.toSet ++ document.asArray.toVector.flatten.flatMap(identities) ++
      document.asObject.toVector.flatMap(_.values).flatMap(identities)

  /** Binary names of the classes under the `eyes4s` packages on the test
    * classpath whose names end in `suffix`, from class directories and jars.
    */
  private def classesEndingWith(suffix: String): Vector[String] =
    val loader = getClass.getClassLoader
    val files  = loader.getResources("eyes4s/").asScala.toVector.flatMap { url =>
      url.getProtocol match
        case "file" =>
          val root = Paths.get(url.toURI).getParent
          Using.resource(Files.walk(root.resolve("eyes4s")))(
            _.iterator.asScala.map(p => root.relativize(p).toString).toVector
          )
        case "jar" =>
          val jar = url.openConnection().asInstanceOf[java.net.JarURLConnection].getJarFile
          jar.entries.asScala.map(_.getName).toVector
        case other => fail(s"cannot list classes under $url ($other)")
    }
    files
      .map(_.replace(java.io.File.separatorChar, '/'))
      .filter(name => name.startsWith("eyes4s/") && name.endsWith(s"$suffix.class"))
      .map(_.stripSuffix(".class").replace('/', '.'))
      .distinct
      .sorted

  /** The `DefinitionId` companion and every `*Definitions` object: per-file
    * holders of built-in identities (see `DefinitionId.builtIn`), found on the
    * classpath so that a new holder needs no edit here.
    */
  private lazy val holders: Vector[(String, AnyRef)] =
    ("DefinitionId" -> (DefinitionId: AnyRef)) +: classesEndingWith("Definitions$").map {
      name =>
        val holder =
          Class.forName(name, true, getClass.getClassLoader).getField("MODULE$").get(null)
        name.stripPrefix("eyes4s.").stripSuffix("$") -> holder
    }

  /** Every identity a holder declares, by reflection, keyed by `Holder.field`. */
  private lazy val declarations: Vector[(String, DefinitionId)] =
    holders.flatMap { (label, holder) =>
      holder.getClass.getDeclaredMethods.toVector
        .filter(m =>
          m.getParameterCount == 0 && m.getReturnType == classOf[DefinitionId] &&
            java.lang.reflect.Modifier.isPublic(m.getModifiers)
        )
        .flatMap(m =>
          m.invoke(holder) match
            case id: DefinitionId => Vector(s"$label.${m.getName}" -> id)
            case _                => Vector.empty
        )
    }

  private lazy val declared: Map[String, DefinitionId] = declarations.toMap

  /** Identities declared twice or invalid, declared identities without an
    * entry, entries twice, and entries that are not built in.
    */
  private def coverage(
      entries: Vector[Entry],
      declarations: Vector[(String, DefinitionId)] = declarations
  ): Vector[String] =
    val registered = entries.map(_.id)
    val builtIn    = declarations.map(_._2).toSet
    registered.diff(registered.distinct).map(id => s"$id is registered twice") ++
      declarations.groupBy(_._2).toVector.sortBy(_._1.toString).collect {
        case (id, fields) if fields.size > 1 =>
          s"$id is declared twice: ${fields.map(_._1).sorted.mkString(", ")}"
      } ++
      declarations.sortBy(_._1).collect {
        case (field, id) if DefinitionId.of(id.name, id.version).isLeft =>
          s"$field ($id) is not a valid identity"
      } ++
      declarations.sortBy(_._1).collect {
        case (field, id) if !registered.contains(id) =>
          s"$field ($id) has no registry entry: add a published round-trip law " +
            "in eyes4s-laws and a pinned v1 fixture, then register both here"
      } ++
      registered
        .filterNot(builtIn.contains)
        .map(id => s"$id is not a built-in identity")

  /** Entries without a law, and laws their suite does not register. */
  private def lawProblems(entries: Vector[Entry]): Vector[String] =
    entries.filter(_.laws.isEmpty).map(e => s"${e.id} has no law") ++
      entries.flatMap(_.laws).groupBy(_.suite).toVector.flatMap { (factory, laws) =>
        val suite = factory()
        val names = suite.munitTests().map(_.name).toSet
        laws.distinct
          .filterNot(law => names.contains(law.test))
          .map(law => s"${suite.getClass.getSimpleName} registers no test '${law.test}'")
      }

  /** Entries without a fixture, fixtures that are missing or do not carry the
    * identity, and document fixtures that do not decode and re-encode to
    * themselves through the identity's shipped codec.
    */
  private def fixtureProblems(
      entries: Vector[Entry],
      read: String => Option[Array[Byte]] = resource
  ): Vector[String] =
    def parsed(file: String): Option[Json] =
      read(file).flatMap(bytes => io.circe.parser.parse(new String(bytes, "UTF-8")).toOption)
    entries.flatMap { entry =>
      def document(file: String, json: Json): Option[String] =
        if entry.kind != Kind.Document then None
        else if !json.hcursor
            .get[Json]("schema")
            .toOption
            .map(identities)
            .contains(Set(entry.id))
        then Some(s"$file is not a ${entry.id} document")
        else
          Decoders.reencode(entry.id, json, read) match
            case Right(same) if same == json => None
            case other                       => Some(s"$file does not round-trip: $other")
      def carried(file: String): Vector[String] =
        if read(file).isEmpty then Vector(s"missing pinned fixture $file")
        else if !file.endsWith(".json") then Vector.empty
        else
          parsed(file) match
            case None       => Vector(s"$file is not JSON")
            case Some(json) =>
              Option
                .when(!identities(json).contains(entry.id))(s"$file does not carry ${entry.id}")
                .toVector ++ document(file, json).toVector
      val none = Option.when(entry.fixtures.isEmpty)(s"${entry.id} has no pinned fixture")
      none.toVector ++ entry.fixtures.flatMap(carried)
    }

  /** Resource files no entry claims, and claimed fixtures that do not exist. */
  private def unclaimed(files: Set[String], entries: Vector[Entry]): Vector[String] =
    val claimed = entries.flatMap(_.fixtures).toSet
    (files -- claimed).toVector.sorted.map(f => s"$f is not claimed by any entry") ++
      (claimed -- files).toVector.sorted.map(f => s"$f is registered but does not exist")

  /** The regular files of the pinned fixture directory. */
  private def fixtureFiles: Set[String] =
    val anchor = getClass.getResource("/eyes4s/manifest-v1.json")
    assert(
      anchor != null && anchor.getProtocol == "file",
      s"resources are not a directory: $anchor"
    )
    val directory: Path = Paths.get(anchor.toURI).getParent
    // Compiled test classes share the directory; the fixtures are its regular files.
    Using.resource(Files.list(directory))(
      _.iterator.asScala.filter(Files.isRegularFile(_)).map(_.getFileName.toString).toSet
    )

  /** The version of a document's envelope. */
  private def envelope(document: Json): Option[DefinitionId] =
    Envelopes.schemaOf(document).toOption

  /** Multi-version schemas without a ladder, ladders whose versions are not
    * exactly the registered versions, versions without a pinned fixture, and
    * pinned documents the ladder cannot lift: each must lift to the latest
    * version, decode there to the value it decodes to as written, and
    * re-encode to itself.
    */
  private def versionProblems(
      entries: Vector[Entry],
      schemas: Vector[Versioned],
      read: String => Option[Array[Byte]] = resource
  ): Vector[String] =
    def parsed(file: String): Option[Json] =
      read(file).flatMap(bytes => io.circe.parser.parse(new String(bytes, "UTF-8")).toOption)
    def lifts[A](ladder: SchemaLadder[A], file: String, document: Json): Option[String] =
      val codec   = ladder.codec
      val lifted  = ladder.lift(document)
      val direct  = codec.decode(document).flatMap(codec.encode)
      val through = lifted.flatMap(codec.decode).flatMap(codec.encode)
      Option.when(direct != Right(document) || through != Right(document))(
        s"$file does not lift: decoded ${direct.map(_ => "ok")}, lifted ${through.map(_ => "ok")}"
      )
    val registered =
      entries.map(_.id).groupBy(_.name).map((n, ids) => n -> ids.sortBy(_.version))
    val multi      = registered.filter(_._2.size > 1)
    val unladdered = multi.keys.toVector.sorted
      .filterNot(name => schemas.exists(_.name == name))
      .map(name => s"$name has versions ${multi(name).map(_.version)} but no registered ladder")
    unladdered ++ schemas.flatMap { schema =>
      val versions = registered.getOrElse(schema.name, Vector.empty)
      val ladders  = schema.ladders.flatMap { rungs =>
        Option
          .when(rungs.ladder.versions != versions)(
            s"${schema.name} ladder has versions ${rungs.ladder.versions.map(_.version)}, " +
              s"but ${versions.map(_.version)} are registered"
          )
          .toVector
      }
      val pinned = schema.ladders.flatMap { rungs =>
        rungs.fixtures.flatMap(file =>
          parsed(file) match
            case None           => Vector(file -> None)
            case Some(document) => Vector(file -> Some(document -> rungs.ladder))
        )
      }
      val missingVersions = versions
        .filterNot(v => pinned.exists(_._2.exists((doc, _) => envelope(doc).contains(v))))
        .map(v => s"${schema.name}@${v.version} has no pinned fixture")
      val lifting = pinned.flatMap {
        case (file, None)                    => Vector(s"missing pinned fixture $file")
        case (file, Some((document, rungs))) => lifts(rungs, file, document).toVector
      }
      val laws = Option.when(schema.laws.isEmpty)(s"${schema.name} has no ladder law").toVector
      ladders ++ missingVersions ++ lifting ++ laws
    }

  test("every multi-version schema has a ladder, a fixture per version and lifts them") {
    assertEquals(versionProblems(builtIns ++ conventional, versioned), Vector.empty)
    assertEquals(
      lawProblems(
        versioned.map(v => Entry(DefinitionId.study, Kind.Document, Vector.empty, v.laws))
      ),
      Vector.empty
    )
  }

  test("the version checks fail for a missing ladder, version, fixture or upcast") {
    val entries = builtIns ++ conventional
    val study   = versioned.find(_.name == "eyes4s.study").get
    // A schema with two registered versions and no ladder.
    assertEquals(
      versionProblems(entries, versioned.filterNot(_ == study)),
      Vector("eyes4s.study has versions Vector(1, 2) but no registered ladder")
    )
    // A version declared and registered without extending the ladder.
    val v3 = Entry(DefinitionId.builtIn("eyes4s.study", 3), Kind.Document, Vector(), Vector())
    assert(
      versionProblems(entries :+ v3, versioned)
        .exists(
          _.startsWith("eyes4s.study ladder has versions Vector(1, 2), but Vector(1, 2, 3)")
        )
    )
    assert(
      versionProblems(entries :+ v3, versioned).contains("eyes4s.study@3 has no pinned fixture")
    )
    // A version whose only fixture is withdrawn.
    val withoutV1 = study.copy(ladders =
      study.ladders.map(r => r.copy(fixtures = r.fixtures.filterNot(_ == "study-v1.json")))
    )
    assertEquals(
      versionProblems(entries, versioned.map(v => if v == study then withoutV1 else v)),
      Vector("eyes4s.study@1 has no pinned fixture")
    )
    // A ladder whose upcast is dropped: the v1 fixture no longer lifts.
    val cosine  = StudyCodecs.cosine[Px].ladder
    val v1      = cosine.versions.head
    val dropped = SchemaLadder
      .of[
        StudyPlan[StudyKey, Px, Unit, eyes4s.compare.Similarity, eyes4s.design.SignedDifference]
      ](
        cosine.role,
        v1
      )(cosine.writeAt(v1, _))(cosine.readAt(v1, _))
      .next(p => cosine.earliest(p) == v1, identity)(cosine.writeAt(cosine.latest, _))(
        cosine.readAt(cosine.latest, _)
      )
    val broken =
      study.copy(ladders = Vector(Rungs(dropped, Vector("study-v1.json", "study-v2.json"))))
    assertEquals(
      versionProblems(entries, versioned.map(v => if v == study then broken else v))
        .map(_.takeWhile(_ != ':')),
      Vector("study-v1.json does not lift")
    )
  }

  test("every built-in DefinitionId has exactly one registry entry, and nothing else does") {
    assert(declared.size >= 22, s"reflection found only ${declared.keys}")
    assertEquals(coverage(builtIns), Vector.empty)
  }

  test("the scan for per-file Definitions holders reads main and test classes") {
    // The scan that finds `*Definitions` objects, shown on two known objects:
    // one in a dependency's main classes, one in this module's test classes.
    assert(classesEndingWith("DefinitionId$").contains("eyes4s.plan.DefinitionId$"))
    assert(classesEndingWith("SchemaRegistry$").contains("eyes4s.laws.SchemaRegistry$"))
    assertEquals(holders.map(_._1).take(1), Vector("DefinitionId"))
    val duplicate = "KdeDefinitions.cosine" -> DefinitionId.cosine
    assert(
      coverage(builtIns, declarations :+ duplicate).exists(
        _.startsWith(
          s"${DefinitionId.cosine} is declared twice"
        )
      )
    )
    val blank = "KdeDefinitions.blank" -> DefinitionId.builtIn(" ", 1)
    assert(
      coverage(builtIns, declarations :+ blank).exists(_.contains("is not a valid identity"))
    )
    val unregistered = "KdeDefinitions.kde" -> DefinitionId.builtIn("eyes4s.kde", 1)
    assert(
      coverage(builtIns, declarations :+ unregistered)
        .exists(_.startsWith("KdeDefinitions.kde (DefinitionId(eyes4s.kde,1)) has no registry"))
    )
  }

  test("every registered law is registered by its published law suite") {
    assertEquals(lawProblems(builtIns ++ conventional), Vector.empty)
  }

  test(
    "every registered fixture exists and carries its identity; documents decode through it"
  ) {
    assertEquals(fixtureProblems(builtIns ++ conventional), Vector.empty)
  }

  test("the registry checks fail for an identity without an entry, a law or a fixture") {
    val distance = builtIns.find(_.id == DefinitionId.measureDistance).get
    assert(coverage(builtIns.filterNot(_ == distance)).exists(_.contains("measureDistance")))
    assert(coverage(builtIns :+ distance).exists(_.contains("registered twice")))
    assert(lawProblems(Vector(distance.copy(laws = Vector.empty))).nonEmpty)
    val renamed = distance.laws.head.copy(name = "distance")
    assert(
      lawProblems(Vector(distance.copy(laws = Vector(renamed))))
        .exists(_.contains("registers no test"))
    )
    assert(fixtureProblems(Vector(distance.copy(fixtures = Vector.empty))).nonEmpty)
    assert(
      fixtureProblems(Vector(distance.copy(fixtures = Vector("study-v1.json"))))
        .exists(_.contains("does not carry"))
    )
    assert(
      fixtureProblems(Vector(distance.copy(fixtures = Vector("no-such-fixture-v1.json"))))
        .exists(_.contains("missing"))
    )
    val plan = builtIns.find(_.id == DefinitionId.study).get
    assert(
      fixtureProblems(Vector(plan.copy(fixtures = Vector("study-input-v1.json"))))
        .exists(_.contains("is not a"))
    )
    // A study-v1 that decodes but would be written differently: an extra member.
    val annotated = json("study-v1.json")
      .mapObject(_.add("x-note", Json.fromString("not a v1 member")))
      .noSpaces
      .getBytes("UTF-8")
    val read =
      (file: String) => if file == "study-v1.json" then Some(annotated) else resource(file)
    assertEquals(
      fixtureProblems(Vector(plan), read).map(_.takeWhile(_ != ':')),
      Vector("study-v1.json does not round-trip")
    )
    // An unclaimed resource file, and a registered fixture that does not exist.
    val entries = builtIns ++ conventional
    assertEquals(unclaimed(fixtureFiles, entries), Vector.empty)
    assertEquals(
      unclaimed(fixtureFiles + "orphan-v1.json", entries),
      Vector("orphan-v1.json is not claimed by any entry")
    )
    assertEquals(
      unclaimed(fixtureFiles - "timeline-v1.json", entries),
      Vector("timeline-v1.json is registered but does not exist")
    )
  }

  test("the score fixture decodes every envelope through its registered schema") {
    val envelopes = json("score-codecs-v1.json").asArray.getOrElse(fail("expected an array"))
    assertEquals(
      envelopes.map(e => Decoders.reencode(get(Envelopes.schemaOf(e)), e, resource)),
      envelopes.map(Right(_))
    )
  }

  test("every packed-array payload verifies against its manifest declaration and unpacks") {
    val manifest = get(ScientificManifest.codec.decode(json("manifest-inputs-v1.json")))
    val payloads = manifest.entries.filter(_.schema == DefinitionId.packedArray)
    assertEquals(payloads.size, 4)
    payloads.foreach { entry =>
      val layout   = entry.layout.getOrElse(fail(s"${entry.name} has no layout"))
      val bytes    = resource(entry.name.value).getOrElse(fail(s"missing ${entry.name}"))
      val verified =
        get(VerifiedPayload.verify(PayloadRef(entry.sha256, layout), IArray.from(bytes)))
      val count = layout.element match
        case ElementKind.UInt8   => PackedArrays.unpack[Byte](verified).map(_.length)
        case ElementKind.Int32   => PackedArrays.unpack[Int](verified).map(_.length)
        case ElementKind.Int64   => PackedArrays.unpack[Long](verified).map(_.length)
        case ElementKind.Float64 => PackedArrays.unpack[Double](verified).map(_.length)
      assertEquals(count, Right(layout.count), entry.name.value)
    }
  }

  test("every pinned fixture under codec/src/test/resources/eyes4s is claimed by an entry") {
    assertEquals(unclaimed(fixtureFiles, builtIns ++ conventional), Vector.empty)
  }

/** The shipped decoder of every registered document schema: decode, then
  * re-encode, so a fixture that decodes but would be written differently is
  * caught as well.
  */
private object Decoders:
  import SchemaRegistry.get

  private def through[A](codec: VersionedCodec[A], document: Json): Either[CodecError, Json] =
    codec.decode(document).flatMap(codec.encode)

  def reencode(
      id: DefinitionId,
      document: Json,
      resource: String => Option[Array[Byte]]
  ): Either[CodecError, Json] =
    val recordingPlan = RecordingCodecs.ivt(
      id,
      get(DefinitionId.of("eyes4s.recording.ivt", 1)),
      get(DefinitionId.of("eyes4s.ivt-parameters", 1))
    )
    id match
      case DefinitionId.study           => through(StudyCodecs.cosine[Px].codec, document)
      case DefinitionId.studyInput      => through(StudyInputCodecs.study[Px].input, document)
      case DefinitionId.admissionLedger => through(StudyInputCodecs.study[Px].ledger, document)
      case StudyCodecDefinitions.studyV2
          if document.hcursor
            .downField("value")
            .downField("layout")
            .get[String]("name")
            .contains(TrialKeyDefinitions.trialLayout.name) =>
        through(StudyCodecs.trialCosine[Px].codec, document)
      case StudyCodecDefinitions.studyV2 => through(StudyCodecs.cosine[Px].codec, document)
      case StudyInputDefinitions.admissionLedgerV2 =>
        through(StudyInputCodecs.study[Px].ledger, document)
      case InventoryDefinitions.admissionLedgerV3 =>
        through(StudyInputCodecs.trial[Px].ledger, document)
      case DefinitionId.recording => through(RecordingInputCodecs.recording[Px], document)
      case DefinitionId.binocularRecording =>
        through(RecordingInputCodecs.binocular[Px], document)
      case DefinitionId.recordingInput     => through(RecordingInputCodecs.input[Px], document)
      case DefinitionId.temporalStudyInput =>
        through(TemporalInputCodecs.study[Px]().input, document)
      case DefinitionId.timeline =>
        through(TimelineCodecs.timeline(id, StudyCodecs.key(DefinitionId.studyKey)), document)
      case DefinitionId.studyResult     => through(StudyResultCodecs.cosine[Px].codec, document)
      case DefinitionId.manifest        => through(ScientificManifest.codec, document)
      case DefinitionId.similarity      => through(StudyResultCodecs.similarity(), document)
      case DefinitionId.measureDistance =>
        through(StudyResultCodecs.measureDistance(), document)
      case DefinitionId.scalar           => through(StudyResultCodecs.scalar(), document)
      case DefinitionId.signedDifference =>
        through(StudyResultCodecs.signedDifference(), document)
      case DefinitionId.packedRecording =>
        val codec = PackedRecordingCodecs.recording[Px]
        for
          refs     <- PackedRecordingCodecs.references(document)
          payloads <- Right(refs.flatMap { ref =>
            Vector("tMicros", "support", "lineage", "values")
              .flatMap(c => resource(s"packed-recording-v1.$c.bin"))
              .flatMap(bytes => VerifiedPayload.verify(ref, IArray.from(bytes)).toOption)
          })
          value   <- codec.decode(document, ref => payloads.find(_.ref == ref))
          encoded <- codec.encode(value)
        yield encoded.document
      case DefinitionId.recordingResult =>
        val idt = RecordingCodecs.idt(
          get(DefinitionId.of("eyes4s.recording-plan", 1)),
          get(DefinitionId.of("eyes4s.recording.idt", 1)),
          get(DefinitionId.of("eyes4s.idt-parameters", 1))
        )
        through(idt.results.codec, document)
      case DefinitionId.temporalResult =>
        through(
          TemporalResultCodecs
            .cosine[Px](get(DefinitionId.of("eyes4s.temporal-study", 1)))
            .codec,
          document
        )
      case other if other.name == "eyes4s.recording-plan" =>
        through(recordingPlan.codec, document)
      case other if other.name == "eyes4s.temporal-study" =>
        through(new TemporalStudyCodec(other, StudyCodecs.cosine[Px]).codec, document)
      case other if other == AdditionalRecipeCodecs.point.resultSchema =>
        through(AdditionalRecipeCodecs.point.archive, document)
      case other if other == AdditionalRecipeCodecs.repetition.schema =>
        through(AdditionalRecipeCodecs.repetition, document)
      case other => Left(CodecError.Unsupported(other.name, "no registered decoder"))

private object Envelopes:
  def schemaOf(envelope: Json): Either[String, DefinitionId] = for
    schema  <- envelope.hcursor.get[Json]("schema").left.map(_.message)
    name    <- schema.hcursor.get[String]("name").left.map(_.message)
    version <- schema.hcursor.get[Int]("version").left.map(_.message)
    id      <- DefinitionId.of(name, version).left.map(_.message)
  yield id
