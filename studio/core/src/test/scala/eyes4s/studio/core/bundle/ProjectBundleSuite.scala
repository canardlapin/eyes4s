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

package eyes4s.studio.core.bundle

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentSamples.{t2, t3}
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import munit.CatsEffectSuite
import org.scalacheck.Prop.forAll

/** The `.eyes` bundle (ticket S2.3) through the in-memory store. */
class ProjectBundleSuite extends CatsEffectSuite:
  import BundleSamples.*

  private def ok[E, A](io: IO[Either[E, A]]): IO[A] =
    io.flatMap(_.fold(e => IO.raiseError(new AssertionError(e.toString)), IO.pure))

  private val me = right(LockOwner.of("ProjectBundleSuite"))

  /** A new in-memory bundle holding `document`, and its writer lock. */
  private def saved(
      document: StudioDocument,
      sharing: SharingOptions = SharingOptions.complete
  ): IO[(ProjectStore[IO], WriterLock, EncodedBundle)] =
    for
      store   <- InMemoryProjectStore.create[IO]
      lock    <- ok(store.acquire(me))
      encoded <- IO.fromEither(
        ProjectBundle
          .encode(document, sharing, inputsFor(document))
          .leftMap(e => AssertionError(e.message))
      )
      _ <- ok(ProjectBundle.save(store, lock, None, encoded))
    yield (store, lock, encoded)

  private def hex(bytes: IArray[Byte]): String = ByteDigest.sha256(bytes).hex

  // --- The golden t2 bundle -----------------------------------------------

  test("t2: open → save → open is lossless and the second save is byte-identical") {
    for
      (store, lock, first) <- saved(t2)
      opened               <- ok(ProjectBundle.open(store))
      second = right(
        ProjectBundle.encode(opened.document, SharingOptions.complete, opened.manifest.inputs)
      )
      _        <- ok(ProjectBundle.save(store, lock, Some(opened.manifestDigest), second))
      reopened <- ok(ProjectBundle.open(store))
      listed   <- ok(store.list)
    yield
      assertEquals(opened.document, t2)
      assertEquals(reopened.document, t2)
      assertEquals(opened.manifest, first.manifest)
      assertEquals(hex(second.manifestBytes), hex(first.manifestBytes))
      assertEquals(
        second.parts.map((p, b) => (p, hex(b))),
        first.parts.map((p, b) => (p, hex(b)))
      )
      assertEquals(listed, first.parts.map(_._1).sorted)
  }

  test("t2: the bundle's layout and manifest digest are pinned (the golden bundle)") {
    val encoded = right(ProjectBundle.encode(t2, SharingOptions.complete, inputsFor(t2)))
    assertEquals(encoded.parts.map(_._1.value), BundlePins.t2Parts)
    assertEquals(hex(encoded.manifestBytes), BundlePins.t2ManifestSha256)
    assertEquals(
      encoded.manifest.inputs.flatMap(_.path).map(_.value),
      Vector(
        "inputs/0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99/trials.csv",
        "inputs/19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2/fixations.csv"
      )
    )
    assertEquals(
      Right(encoded.manifest.science),
      StudioDocument.scienceDigest(t2).map(ScienceRecord.Verified(_))
    )
    // Source.path is the import name; inputPath is its one stored path.
    assertEquals(
      t2.datasets.head.sources.entries.map(s => ProjectBundle.inputPath(s).map(_.value)),
      encoded.manifest.inputs.reverse.flatMap(_.path).map(p => Right(p.value))
    )
  }

  test("numbers are written alike on every platform and read back to the same doubles") {
    val cases = Vector(
      35.0            -> "35",
      0.6             -> "0.6",
      -2.5            -> "-2.5",
      -0.0            -> "-0.0",
      1e-7            -> "1e-7",
      1.5e-7          -> "1.5e-7",
      0.000001        -> "0.000001",
      123456789.125   -> "123456789.125",
      1e21            -> "1e+21",
      1.5e300         -> "1.5e+300",
      5e-324          -> "5e-324",
      Math.pow(2, 60) -> "1152921504606847000",
      Double.MaxValue -> "1.7976931348623157e+308"
    )
    cases.foreach { (d, text) =>
      val printed = right(PortableJson.print("n", Json.fromDoubleOrNull(d)))
      assertEquals(printed, text, d)
      val back = parse(printed).toOption.flatMap(_.asNumber).map(_.toDouble)
      assertEquals(
        back.map(java.lang.Double.doubleToLongBits),
        Some(java.lang.Double.doubleToLongBits(d)),
        text
      )
    }
    assertEquals(
      PortableJson.print("n", Json.fromLong(-(1L << 53))),
      Right("-9007199254740992")
    )
  }

  test("a non-finite or inexact number is refused, never rounded, when written and when read") {
    def number(text: String) =
      Json.fromJsonNumber(io.circe.JsonNumber.fromDecimalStringUnsafe(text))
    val refusedOnWrite = Vector(
      "1e400"                  -> BundleError.NonFiniteNumber("w", "1e400"),
      "-1e400"                 -> BundleError.NonFiniteNumber("w", "-1e400"),
      "0.10000000000000000001" -> BundleError.InexactNumber("w", "0.10000000000000000001"),
      "9007199254740993"       -> BundleError.InexactNumber("w", "9007199254740993")
    )
    refusedOnWrite.foreach { (text, error) =>
      assertEquals(PortableJson.print("w", Json.arr(number(text))), Left(error), text)
    }
    assertEquals(PortableJson.print("w", number("0.1")), Right("0.1"))
    // Reading scans the literals of the text itself, so both platforms agree.
    val refusedOnRead = Vector(
      "[1e400]"                  -> BundleError.NonFiniteNumber("r", "1e400"),
      "{\"a\":-1E999}"           -> BundleError.NonFiniteNumber("r", "-1E999"),
      "[0.10000000000000000001]" -> BundleError.InexactNumber("r", "0.10000000000000000001"),
      "[9007199254740993]"       -> BundleError.InexactNumber("r", "9007199254740993")
    )
    refusedOnRead.foreach { (text, error) =>
      assertEquals(PortableJson.checkText("r", text), Left(error), text)
    }
    assertEquals(
      PortableJson.checkText("r", "{\"s\":\"1e400 \\\" 2\",\"n\":[0.6,-0.0,5e-324]}"),
      Right(())
    )
    assertEquals(
      ProjectBundle.readManifest(
        utf8("{\"schema\":{\"name\":\"studio.project\",\"version\":1e999}}")
      ),
      Left(BundleError.NonFiniteNumber("project.json", "1e999"))
    )
  }

  test("every path the manifest names is relative and inside a scientific area") {
    val encoded = right(ProjectBundle.encode(t2, SharingOptions.complete, inputsFor(t2)))
    val paths   = encoded.manifest.paths
    assert(paths.nonEmpty)
    assert(paths.forall(p => !p.area.disposable && !p.value.startsWith("/")), paths)
    assertEquals(paths.map(p => BundlePath.of(p.value)), paths.map(Right(_)))
  }

  // --- cache/ is disposable -----------------------------------------------

  test("deleting cache/ loses nothing scientific: the same document and science hash") {
    for
      (store, lock, encoded) <- saved(t2)
      _ <- ok(store.write(lock, right(BundlePath.of("cache/thumbs/run7.png")), utf8("png")))
      _ <- ok(
        store.write(lock, right(BundlePath.of("cache/maps/run7/sigma2.bin")), utf8("map"))
      )
      before  <- ok(ProjectBundle.open(store))
      cache   <- ok(store.list).map(_.filter(_.area == BundleArea.Cache))
      _       <- cache.traverse_(p => ok(store.delete(lock, p)))
      after   <- ok(ProjectBundle.open(store))
      remains <- ok(store.list)
    yield
      assertEquals(cache.size, 2)
      assertEquals(after, before)
      assertEquals(after.document, t2)
      assertEquals(
        StudioDocument.scienceDigest(after.document).map(_.display),
        StudioDocument.scienceDigest(t2).map(_.display)
      )
      assertEquals(after.manifest.science, encoded.manifest.science)
      assert(remains.forall(_.area != BundleArea.Cache), remains)
  }

  // --- Job handles are session state -------------------------------------

  test(
    "t3's job handle is returned as not saved; reopened, run 8 is still running with no job"
  ) {
    for
      (store, _, encoded) <- saved(t3)
      opened              <- ok(ProjectBundle.open(store))
    yield
      assertEquals(encoded.sessionOnly, t3.jobs)
      assert(t3.jobs.nonEmpty)
      assertEquals(opened.document.jobs, Vector.empty)
      assertEquals(Right(opened.document), t3.withJobs(Vector.empty))
      assertEquals(opened.document.running.map(_.id), Vector(RunId(8)))
  }

  // --- Upcasting ------------------------------------------------------------

  test(
    "a hand-written version 1 manifest upcasts: sharing is complete; it opens unverified"
  ) {
    for
      (store, lock, encoded) <- saved(t2)
      v1 = utf8(BundlePins.t2ManifestV1)
      _      <- ok(store.swapManifest(lock, Some(ByteDigest.sha256(encoded.manifestBytes)), v1))
      opened <- ok(ProjectBundle.open(store))
      resaved = right(
        ProjectBundle.encode(opened.document, opened.manifest.sharing, opened.manifest.inputs)
      )
    yield
      assertEquals(opened.document, t2)
      assertEquals(opened.manifest.sharing, SharingOptions.complete)
      assertEquals(opened.manifest.science, ScienceRecord.Unrecorded)
      assert(!opened.science.verified)
      assertEquals(
        Right(opened.science),
        StudioDocument.scienceDigest(t2).map(ScienceCheck.Unverified(_))
      )
      assertEquals(opened.manifest.parts, encoded.manifest.parts)
      assertEquals(opened.manifest.inputs, encoded.manifest.inputs)
      assertEquals(resaved.manifest, encoded.manifest)
      assertEquals(hex(resaved.manifestBytes), BundlePins.t2ManifestSha256)
  }

  test(
    "lifting the version 1 manifest keeps its meaning and gives the version 2 writer's document"
  ) {
    val v1                   = parse(BundlePins.t2ManifestV1).toOption.get
    val ladder               = ProjectManifest.ladder.toOption.get
    val value                = ladder.codec.decode(v1).toOption.get
    val (first, latest)      = (ladder.versions.head, ladder.latest)
    def envelope(json: Json) =
      Json.obj(
        "schema" -> Json.obj("name" -> "studio.project".asJson, "version" -> 1.asJson),
        "value"  -> json
      )
    // The hand-written document lifts to the same value.
    assertEquals(ladder.lift(v1).flatMap(ladder.codec.decode), Right(value))
    // What the version 1 writer writes lifts to exactly what version 2 writes.
    val written = ladder.writeAt(first, value).map(envelope)
    assertEquals(
      written.flatMap(ladder.lift).map(_.hcursor.downField("value").focus),
      ladder.writeAt(latest, value).map(Some(_))
    )
    assertEquals(ladder.versions.map(_.version), Vector(1, 2, 3))
    assertEquals(ladder.earliest(value), first)
    // A manifest with a science digest or withheld inputs needs version 2.
    val current =
      right(ProjectBundle.encode(t2, SharingOptions.complete, inputsFor(t2))).manifest
    assertEquals(ladder.earliest(current), ladder.versions(1))
    assertEquals(
      ladder.lift(v1).map(_.hcursor.downField("value").downField("science").focus),
      Right(Some(Json.obj("Unrecorded" -> Json.obj())))
    )
  }

  test("version 2 requires the science record: missing or null is refused") {
    val encoded = right(ProjectBundle.encode(t2, SharingOptions.complete, inputsFor(t2)))
    val json    = parse(String(Array.from(encoded.manifestBytes), "UTF-8")).toOption.get
    def without(science: Option[Json]) =
      val value = json.hcursor
        .downField("value")
        .withFocus(v =>
          v.mapObject(o => science.fold(o.remove("science"))(s => o.add("science", s)))
        )
      utf8(value.top.get.noSpaces)
    val missing = ProjectBundle.readManifest(without(None))
    val nulled  = ProjectBundle.readManifest(without(Some(Json.Null)))
    assert(missing.left.exists(_.message.contains("science")), missing)
    assert(nulled.left.exists(_.message.contains("science is required")), nulled)
    assertEquals(
      ProjectBundle.readManifest(encoded.manifestBytes).map(_.science),
      Right(encoded.manifest.science)
    )
  }

  // --- Paths ----------------------------------------------------------------

  test("absolute and traversing paths are refused with typed errors naming the path") {
    val refused = Vector(
      "/etc/passwd"               -> PathProblem.Absolute,
      "\\\\server\\share\\x"      -> PathProblem.Absolute,
      "C:/Users/x/project.json"   -> PathProblem.Absolute,
      "c:\\x"                     -> PathProblem.Absolute,
      "../outside.json"           -> PathProblem.Traversal,
      "datasets/../../etc/passwd" -> PathProblem.Traversal,
      "inputs/abc/.."             -> PathProblem.Traversal,
      "datasets\\r1.json"         -> PathProblem.Reserved('\\', 8),
      "datasets/r1:stream"        -> PathProblem.Reserved(':', 11),
      "datasets/r1\u0000.json"    -> PathProblem.Reserved('\u0000', 11),
      "datasets/./r1.json"        -> PathProblem.CurrentSegment,
      "datasets//r1.json"         -> PathProblem.EmptySegment,
      "datasets/r1.json/"         -> PathProblem.EmptySegment,
      "datasets/.r1.json.tmp"     -> PathProblem.Hidden(".r1.json.tmp"),
      "project.json"              -> PathProblem.OutsideAreas("project.json"),
      "elsewhere/r1.json"         -> PathProblem.OutsideAreas("elsewhere"),
      "datasets"                  -> PathProblem.NoName(BundleArea.Datasets),
      "  "                        -> PathProblem.Blank
    )
    refused.foreach { (text, problem) =>
      assertEquals(BundlePath.of(text), Left(BundleError.BadPath(text, problem)), text)
    }
    assertEquals(
      BundlePath.of("runs/7/run.json").map(p => (p.area, p.name)),
      Right((BundleArea.Runs, "run.json"))
    )
  }

  test("an input name cannot escape its digest directory") {
    val digest = ByteDigest.sha256(utf8("x"))
    assertEquals(
      InputEntry.of(InputKind.StimulusImage, "../../project.json", digest, 1),
      Left(BundleError.BadPath("../../project.json", PathProblem.Reserved('/', 2)))
    )
    assertEquals(
      InputEntry.of(InputKind.StimulusImage, "..", digest, 1),
      Left(BundleError.BadPath(s"inputs/${digest.hex}/..", PathProblem.Traversal))
    )
  }

  test("a manifest naming a traversing or absolute part is refused, naming the path") {
    val v1 = BundlePins.t2ManifestV1
    for
      (store, lock, encoded) <- saved(t2)
      evil = v1.replaceFirst("\"path\": \"analyses/", "\"path\": \"../analyses/")
      abs  = v1.replaceFirst("\"path\": \"analyses/", "\"path\": \"/analyses/")
      d0   = ByteDigest.sha256(encoded.manifestBytes)
      _     <- ok(store.swapManifest(lock, Some(d0), utf8(evil)))
      first <- ProjectBundle.open(store)
      _     <- ok(store.swapManifest(lock, Some(ByteDigest.sha256(utf8(evil))), utf8(abs)))
      again <- ProjectBundle.open(store)
    yield
      assert(evil != v1 && abs != v1)
      assert(
        first.left.exists(e =>
          e.message.contains("'../analyses/") && e.message.contains("'..'")
        ),
        first
      )
      assert(
        again.left.exists(e =>
          e.message.contains("'/analyses/") && e.message.contains("absolute")
        ),
        again
      )
  }

  // --- Refusals -------------------------------------------------------------

  test(
    "a document source not stored at its mapped input path is refused by dataset, role and path"
  ) {
    val inputs = inputsFor(t2).filterNot(_.kind == InputKind.Source(SourceRole.Trials))
    val trials = t2.datasets.head.sources.trials.get
    val path   = right(ProjectBundle.inputPath(trials))
    assertEquals(path.value, s"inputs/${trials.bytes.hex}/trials.csv")
    assertEquals(
      ProjectBundle.encode(t2, SharingOptions.complete, inputs).map(_ => ()),
      Left(BundleError.UnlistedSource(t2.datasets.head.id, SourceRole.Trials, path))
    )
    // The same bytes under another name are not the mapped path.
    val renamed = inputs :+ right(
      InputEntry.of(InputKind.Source(SourceRole.Trials), "t.csv", trials.bytes, 1)
    )
    assertEquals(
      ProjectBundle.encode(t2, SharingOptions.complete, renamed).map(_ => ()),
      Left(BundleError.UnlistedSource(t2.datasets.head.id, SourceRole.Trials, path))
    )
  }

  test("a changed or truncated part is refused by path and digest") {
    for
      (store, lock, encoded) <- saved(t2)
      (path, bytes) = encoded.parts.head
      flipped       = IArray.tabulate(bytes.length)(i =>
        if i == 0 then (bytes(0) ^ 1).toByte else bytes(i)
      )
      _         <- ok(store.write(lock, path, flipped))
      changed   <- ProjectBundle.open(store)
      _         <- ok(store.write(lock, path, bytes.take(3)))
      truncated <- ProjectBundle.open(store)
      _         <- ok(store.delete(lock, path))
      missing   <- ProjectBundle.open(store)
    yield
      assertEquals(
        changed.map(_ => ()),
        Left(BundleError.PartDigest(path, ByteDigest.sha256(bytes), ByteDigest.sha256(flipped)))
      )
      assertEquals(
        truncated.map(_ => ()),
        Left(BundleError.PartLength(path, bytes.length.toLong, 3L))
      )
      assertEquals(missing.map(_ => ()), Left(BundleError.Store(StoreError.Missing(path))))
  }

  test("parts whose science is not the recorded science are refused, naming both digests") {
    val other = CanonicalDigest.parse[ScienceContent]("ab" * 32).toOption.get
    for
      (store, lock, encoded) <- saved(t2)
      m      = encoded.manifest
      forged = right(
        ProjectManifest.of(
          m.document,
          ScienceRecord.Verified(other),
          m.sharing,
          m.inputs,
          m.parts,
          m.presentation
        )
      )
      bytes <- IO(right(ProjectBundle.manifestBytes(forged)))
      _ <- ok(store.swapManifest(lock, Some(ByteDigest.sha256(encoded.manifestBytes)), bytes))
      open <- ProjectBundle.open(store)
    yield assertEquals(
      open.map(_ => ()),
      Left(BundleError.ScienceMismatch(other, StudioDocument.scienceDigest(t2).toOption.get))
    )
  }

  test("a save against a stale manifest is refused and changes nothing") {
    for
      (store, lock, encoded) <- saved(t2)
      stale                  <- ProjectBundle.save(store, lock, None, encoded)
      opened                 <- ok(ProjectBundle.open(store))
    yield
      val found = Some(ByteDigest.sha256(encoded.manifestBytes))
      assertEquals(stale, Left(BundleError.Store(StoreError.ManifestMoved(None, found))))
      assertEquals(opened.document, t2)
  }

  test("an existing part is never replaced by other bytes") {
    for
      (store, lock, encoded) <- saved(t2)
      (path, bytes) = encoded.parts.head
      _       <- ok(store.write(lock, path, utf8("other")))
      refused <- ProjectBundle.save(
        store,
        lock,
        Some(ByteDigest.sha256(encoded.manifestBytes)),
        encoded
      )
    yield assertEquals(
      refused,
      Left(
        BundleError.Collision(path, ByteDigest.sha256(utf8("other")), ByteDigest.sha256(bytes))
      )
    )
  }

  // --- Inputs and sharing -------------------------------------------------

  private val fixations = utf8("participant,phase,trial\nP01,Encoding,enc_01\n")
  private val trials    = utf8("participant,trial,response\nP01,enc_01,remember\n")
  private val image     = IArray.tabulate[Byte](64)(i => (i * 7).toByte)
  private val people    = utf8("participant,age\nP01,24\n")
  private val shared    = withSources(t2, fixations, trials)

  /** A bundle of `shared` with every input's bytes imported. */
  private def withInputs: IO[(ProjectStore[IO], WriterLock, Vector[InputEntry])] =
    for
      store   <- InMemoryProjectStore.create[IO]
      lock    <- ok(store.acquire(me))
      entries <- Vector(
        (InputKind.Source(SourceRole.Fixations), "fixations.csv", fixations),
        (InputKind.Source(SourceRole.Trials), "trials.csv", trials),
        (InputKind.StimulusImage, "scene_01.png", image),
        (InputKind.ParticipantMetadata, "participants.csv", people)
      ).traverse((k, n, b) => ok(ProjectBundle.importInput(store, lock, k, n, b)))
      encoded = right(ProjectBundle.encode(shared, SharingOptions.complete, entries))
      _ <- ok(ProjectBundle.save(store, lock, None, encoded))
    yield (store, lock, entries)

  test(
    "inputs are copied under inputs/<sha256>/ and checked by digest: present, changed, missing"
  ) {
    for
      (store, lock, entries) <- withInputs
      opened                 <- ok(ProjectBundle.open(store))
      present                <- ProjectBundle.checkInputs(store, opened.manifest)
      again                  <- ok(
        ProjectBundle.importInput(store, lock, entries(2).kind, "scene_01.png", image)
      )
      _     <- ok(store.write(lock, entries(2).path.get, utf8("other")))
      _     <- ok(store.delete(lock, entries(3).path.get))
      after <- ProjectBundle.checkInputs(store, opened.manifest)
      byPath = entries.sortBy(_.path.map(_.value))
    yield
      assertEquals(
        entries.map(_.path.get.value.split('/').dropRight(1).mkString("/")),
        Vector(fixations, trials, image, people).map(b => s"inputs/${hex(b)}")
      )
      assertEquals(present, byPath.map(InputStatus.Present(_)))
      assertEquals(again, entries(2))
      assert(
        after.contains(InputStatus.Changed(entries(2), ByteDigest.sha256(utf8("other")))),
        after
      )
      assert(after.contains(InputStatus.Missing(entries(3))), after)
  }

  test("two stimulus images with identical bytes share without images: two withheld entries") {
    val noImages = SharingOptions(Inclusion.Included, Inclusion.Withheld)
    for
      store   <- InMemoryProjectStore.create[IO]
      lock    <- ok(store.acquire(me))
      entries <- Vector(
        (InputKind.Source(SourceRole.Fixations), "fixations.csv", fixations),
        (InputKind.Source(SourceRole.Trials), "trials.csv", trials),
        (InputKind.StimulusImage, "scene_01.png", image),
        (InputKind.StimulusImage, "scene_01_copy.png", image)
      ).traverse((k, n, b) => ok(ProjectBundle.importInput(store, lock, k, n, b)))
      encoded = right(ProjectBundle.encode(shared, SharingOptions.complete, entries))
      _      <- ok(ProjectBundle.save(store, lock, None, encoded))
      target <- InMemoryProjectStore.create[IO]
      lock2  <- ok(target.acquire(me))
      _      <- ok(ProjectBundle.share(store, target, lock2, noImages))
      opened <- ok(ProjectBundle.open(target))
      status <- ProjectBundle.checkInputs(target, opened.manifest)
      text   <- ok(target.readManifest).map(b => String(Array.from(b), "UTF-8"))
    yield
      val withheld = entries(2).withheld
      assertEquals(entries(3).withheld, withheld)
      assertEquals(opened.document, shared)
      assertEquals(opened.manifest.inputs.count(_ == withheld), 2)
      assertEquals(status.count(_ == InputStatus.Withheld(withheld)), 2)
      assert(!text.contains("scene_01"), text)
  }

  test("a stored input listed twice is still refused") {
    def stored(kind: InputKind, name: String, bytes: IArray[Byte]) =
      right(InputEntry.of(kind, name, ByteDigest.sha256(bytes), bytes.length.toLong))
    val sources = Vector(
      stored(InputKind.Source(SourceRole.Fixations), "fixations.csv", fixations),
      stored(InputKind.Source(SourceRole.Trials), "trials.csv", trials)
    )
    val picture = stored(InputKind.StimulusImage, "scene_01.png", image)
    assert(ProjectBundle.encode(shared, SharingOptions.complete, sources :+ picture).isRight)
    val twice =
      ProjectBundle.encode(shared, SharingOptions.complete, sources :+ picture :+ picture)
    assert(
      twice.left.exists {
        case BundleError.DuplicateInput(_) | BundleError.DuplicatePath(_) => true
        case _                                                            => false
      },
      twice.map(_ => ()).toString
    )
  }

  test(
    "sharing withholds stimulus images explicitly: same science, image listed unnamed, not copied"
  ) {
    val noImages = SharingOptions(Inclusion.Included, Inclusion.Withheld)
    for
      (source, _, entries) <- withInputs
      target               <- InMemoryProjectStore.create[IO]
      lock                 <- ok(target.acquire(me))
      _                    <- ok(ProjectBundle.share(source, target, lock, noImages))
      again                <- ProjectBundle.share(source, target, lock, noImages)
      opened               <- ok(ProjectBundle.open(target))
      status               <- ProjectBundle.checkInputs(target, opened.manifest)
      listed               <- ok(target.list)
      onward               <- InMemoryProjectStore.create[IO]
      lock2                <- ok(onward.acquire(me))
      refused      <- ProjectBundle.share(target, onward, lock2, SharingOptions.complete)
      metadataOnly <- InMemoryProjectStore.create[IO]
      lock3        <- ok(metadataOnly.acquire(me))
      _            <- ok(
        ProjectBundle.share(
          target,
          metadataOnly,
          lock3,
          SharingOptions(Inclusion.Withheld, Inclusion.Withheld)
        )
      )
      stripped      <- ok(ProjectBundle.open(metadataOnly))
      strippedFiles <- ok(metadataOnly.list)
      strippedText  <- ok(metadataOnly.readManifest).map(b => String(Array.from(b), "UTF-8"))
    yield
      val (image, people) = (entries(2).withheld, entries(3).withheld)
      assertEquals(opened.document, shared)
      assert(opened.science.verified)
      assertEquals(opened.manifest.sharing, noImages)
      assertEquals(
        Right(opened.manifest.science),
        StudioDocument.scienceDigest(shared).map(ScienceRecord.Verified(_))
      )
      assert(status.contains(InputStatus.Withheld(image)), status)
      assertEquals(
        (image.name, image.path, image.sha256, image.kind),
        (None, None, entries(2).sha256, entries(2).kind)
      )
      assertEquals(status.count(_.isInstanceOf[InputStatus.Present]), 3)
      assert(!listed.contains(entries(2).path.get), listed)
      assertEquals(again.map(_ => ()), Left(BundleError.TargetNotEmpty(true, listed)))
      assertEquals(refused, Left(BundleError.WithheldInSource(image)))
      // Participant metadata withheld: participants.csv is neither stored nor named.
      assertEquals(stripped.document, shared)
      assert(stripped.manifest.inputs.contains(people), stripped.manifest.inputs)
      assert(!strippedFiles.contains(entries(3).path.get), strippedFiles)
      assert(!strippedFiles.exists(_.value.contains("participants")), strippedFiles)
      assert(
        !strippedText.contains("participants.csv") && !strippedText.contains("scene_01"),
        strippedText
      )
      assertEquals(strippedFiles.count(_.area == BundleArea.Inputs), 2)
  }

  test("a manifest must name exactly the inputs its sharing includes") {
    val m     = right(ProjectBundle.encode(t2, SharingOptions.complete, inputsFor(t2))).manifest
    val image =
      right(InputEntry.withheld(InputKind.StimulusImage, ByteDigest.sha256(image0), 3))
    assertEquals(
      ProjectManifest
        .of(
          m.document,
          m.science,
          SharingOptions.complete,
          m.inputs :+ image,
          m.parts,
          m.presentation
        )
        .map(_ => ()),
      Left(BundleError.InputNaming(image, SharingOptions.complete))
    )
  }

  private val image0 = utf8("img")

/** Every generated document survives the bundle, through the pure encoder
  * and assembler.
  */
class ProjectBundlePropertySuite extends munit.ScalaCheckSuite:
  import BundleSamples.*

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(50)

  property("encode → assemble is the document without its job handles") {
    forAll(DocumentGen.document) { (d: StudioDocument) =>
      val encoded = right(ProjectBundle.encode(d, SharingOptions.complete, inputsFor(d)))
      val files   = encoded.parts.toMap
      val back    = ProjectBundle
        .readManifest(encoded.manifestBytes)
        .flatMap(m =>
          ProjectBundle
            .assemble(m, p => files.get(p).toRight(BundleError.Store(StoreError.Missing(p))))
        )
      assertEquals(back.map(_.document), Right(d.withJobs(Vector.empty).toOption.get))
      assert(back.exists(_.science.verified))
      assertEquals(encoded.sessionOnly, d.jobs)
    }
  }
