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
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.document.*
import io.circe.Json

class FamilyBundleSuite extends munit.CatsEffectSuite:
  import FamilySamples.*
  private val fixations = BundleSamples.utf8("fixations\n")
  private val trials    = BundleSamples.utf8("trials\n")
  private val value     =
    BundleSamples.withSources(document(draft = Some(draftA)), fixations, trials)
  private val inputBytes = Map(SourceRole.Fixations -> fixations, SourceRole.Trials -> trials)
  private val inputs     =
    value.datasets.flatMap(_.sources.entries).distinctBy(_.role).map { source =>
      get(
        InputEntry.of(
          InputKind.Source(source.role),
          source.path.value.split('/').last,
          source.bytes,
          inputBytes(source.role).length.toLong
        )
      )
    }
  private val encoded = get(ProjectBundle.encode(value, SharingOptions.complete, inputs))
  private def ok[E, A](effect: IO[Either[E, A]]): IO[A] =
    effect.flatMap(result => IO(get(result)))

  test("explicit ownership has an immutable listed part and survives save/open/share") {
    for
      store <- InMemoryProjectStore.create[IO]
      lock  <- ok(store.acquire(get(LockOwner.of("FamilyBundleSuite"))))
      _     <- inputs.traverse_(entry =>
        entry.kind match
          case InputKind.Source(role) =>
            ok(store.write(lock, entry.path.get, inputBytes(role))).void
          case other => IO.raiseError(new AssertionError(s"Unexpected input $other"))
      )
      _          <- ok(ProjectBundle.save(store, lock, None, encoded))
      opened     <- ok(ProjectBundle.open(store))
      shared     <- InMemoryProjectStore.create[IO]
      sharedLock <- ok(shared.acquire(get(LockOwner.of("FamilyBundleSuite-share"))))
      _          <- ok(
        ProjectBundle.share(
          store,
          shared,
          sharedLock,
          SharingOptions(Inclusion.Withheld, Inclusion.Withheld)
        )
      )
      reopened <- ok(ProjectBundle.open(shared))
    yield
      val familyPart = encoded.manifest.parts.analysisFamilies.get
      assert(encoded.manifest.parts.all.contains(familyPart))
      assertEquals(familyPart.path.area, BundleArea.Analyses)
      assertEquals(opened.document, value)
      assertEquals(reopened.document.science, value.science)
      assertEquals(
        get(StudioDocument.scienceDigest(reopened.document)),
        get(StudioDocument.scienceDigest(value))
      )
      val second = get(ProjectBundle.encode(opened.document, SharingOptions.complete, inputs))
      assertEquals(
        ByteDigest.sha256(second.manifestBytes),
        ByteDigest.sha256(encoded.manifestBytes)
      )
      assertEquals(
        second.parts.map((path, bytes) => (path, ByteDigest.sha256(bytes))),
        encoded.parts.map((path, bytes) => (path, ByteDigest.sha256(bytes)))
      )
  }

  test("missing and corrupted ownership parts fail before assembling science") {
    val familyPart = encoded.manifest.parts.analysisFamilies.get
    val files      = encoded.parts.toMap
    val missing    = ProjectBundle.assemble(
      encoded.manifest,
      path =>
        if path == familyPart.path then Left(BundleError.Store(StoreError.Missing(path)))
        else Right(files(path))
    )
    assert(missing.isLeft)
    val corrupted = ProjectBundle.assemble(
      encoded.manifest,
      path =>
        if path == familyPart.path then
          val original = files(path)
          Right(IArray.from(Vector.from(original).updated(0, (original(0) ^ 1).toByte)))
        else Right(files(path))
    )
    corrupted match
      case Left(BundleError.PartDigest(path, _, _)) => assertEquals(path, familyPart.path)
      case other => fail(s"Expected corrupt family digest, got $other")
  }

  test(
    "earlier manifest readers/writers refuse the family part, including a relabelled envelope"
  ) {
    val ladder = get(ProjectManifest.ladder)
    val stored = get(get(ProjectManifest.codec).encode(encoded.manifest))
    assertEquals(stored.hcursor.downField("schema").get[Int]("version"), Right(3))
    val raw = stored.hcursor.get[Json]("value").toOption.get
    ladder.versions.dropRight(1).foreach { version =>
      assert(ladder.readAt(version, raw).isLeft)
      assert(ladder.writeAt(version, encoded.manifest).isLeft)
    }
    val nullPart = raw.hcursor
      .downField("parts")
      .withFocus(
        _.mapObject(_.add("analysisFamilies", Json.Null))
      )
      .top
      .get
    assert(ladder.readAt(ladder.latest, nullPart).isLeft)
  }
