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

package eyes4s.studio.core.assets

import eyes4s.codec.{ByteDigest, CodecError}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.bundle.{BundleSamples, ProjectBundle, SharingOptions}
import eyes4s.studio.core.command.{Command, CommandError, Reducer}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import io.circe.Json

import java.nio.charset.StandardCharsets.UTF_8

/** Repairing a missing display asset (tickets S2.5 and S5.7): the repair is
  * a document command naming the inventory's file and the stored bytes'
  * digest; it is recorded beside the science, never in it, survives the
  * document codec and the bundle, and is undone exactly. A document without
  * repairs or display columns is written as before, byte for byte.
  */
class AssetRepairSuite extends munit.FunSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(m => fail(s"unexpected: $m"), identity)

  private lazy val t2 = ok(StoryMoments.t2)
  private val r3      = StoryMoments.r3

  private def file(name: String): AssetFile  = ok(AssetFile.of(name))
  private def stored(name: String): AssetRef =
    AssetRef(file(name), ByteDigest.sha256(IArray.from(s"bytes of $name".getBytes(UTF_8))))

  private val forest   = file("forest-044.png")
  private val restored = stored("forest_044_restored.png")
  private val relink   = Command.RelinkAsset(r3, forest, Some(restored))

  private def version(json: Json) =
    json.hcursor.downField("schema").downField("version").as[Int]

  test("a repair is recorded beside the science, and undone exactly") {
    val (repaired, _) = ok(Reducer.step(t2, relink))
    assertEquals(repaired.relinks.entries, Vector(AssetRelink(r3, forest, restored)))
    assertEquals(repaired.relinks.find(r3, forest).map(_.asset), Some(restored))
    // Not science: the scientific identity is unchanged.
    assertEquals(StudioDocument.scienceDigest(repaired), StudioDocument.scienceDigest(t2))
    // Its inverse unlinks; a second repair replaces the first.
    val outcome = ok(Reducer.run(t2, relink))
    assertEquals(
      outcome.recording,
      eyes4s.studio.core.command.Recording.Reversible(Command.RelinkAsset(r3, forest, None))
    )
    assertEquals(ok(Reducer.step(repaired, Command.RelinkAsset(r3, forest, None)))._1, t2)
    val again = stored("forest-044-v2.png")
    assertEquals(
      ok(
        Reducer.step(repaired, Command.RelinkAsset(r3, forest, Some(again)))
      )._1.relinks.entries,
      Vector(AssetRelink(r3, forest, again))
    )
    assertEquals(Command.RelinkAsset(r3, forest, None).kind.label, "Assets · no rerun")
    // A science edit after it keeps it: the reducer rebuilds the science only.
    val discarded = ok(Reducer.step(repaired, Command.DiscardDraft))._1
    assertEquals(discarded.draft, None)
    assertEquals(discarded.relinks, repaired.relinks)
  }

  test("a repair of an unknown revision, or of nothing, is refused") {
    assertEquals(
      Reducer.step(t2, Command.RelinkAsset(DatasetRevision(9), forest, Some(restored))),
      Left(CommandError.UnknownDataset(DatasetRevision(9)))
    )
    assert(
      Reducer.step(t2, Command.RelinkAsset(r3, forest, None)).left.exists {
        case CommandError.NoChange(_, _) => true
        case _                           => false
      }
    )
    assertEquals(
      t2.withRelinks(AssetRelinks.empty.set(DatasetRevision(9), forest, Some(restored))),
      Left(DocumentError.RelinkUnknownDataset(DatasetRevision(9), "forest-044.png"))
    )
    assertEquals(
      AssetRelinks.of(
        Vector(AssetRelink(r3, forest, restored), AssetRelink(r3, forest, stored("x.png")))
      ),
      Left(DocumentError.DuplicateRelink(r3, "forest-044.png"))
    )
  }

  test("a document with a repair is version 4 and round-trips; one without is unchanged") {
    val repaired = ok(Reducer.step(t2, relink))._1
    val encoded  = ok(StudioDocument.encode(repaired))
    assertEquals(version(encoded), Right(4))
    assertEquals(StudioDocument.decode(encoded), Right(repaired))
    assert(encoded.noSpaces.contains("\"relinks\""))
    // Without one, the bytes are t2's own: no member, the earlier version.
    val plain = ok(StudioDocument.encode(t2))
    assert(!plain.noSpaces.contains("\"relinks\""))
    assertEquals(version(plain), Right(3))
    // A version-3 reader refuses it rather than dropping the repair.
    val ladder = ok(StudioDocument.ladder)
    val older  = ok(ladder.upTo(ladder.versions(2)))
    assertEquals(
      older.codec.decode(encoded),
      Left(
        CodecError.UnsupportedSchema("studio document", ladder.latest, ladder.versions.take(3))
      )
    )
  }

  test("display columns: written only when mapped, version 4, refused on a role's column") {
    val r3spec   = t2.dataset(r3).get
    val mapping  = r3spec.inventory.get
    val kind     = ok(ColumnName.of("display_kind"))
    val image    = ok(ColumnName.of("image_file"))
    val withCols =
      ok(InventoryMapping.withDisplays(mapping, Some(DisplayColumns(kind, Some(image)))))
    import io.circe.syntax.*
    assertEquals(mapping.asJson.asObject.map(_.contains("displays")), Some(false))
    assertEquals(withCols.asJson.as[InventoryMapping], Right(withCols))
    assertEquals(
      InventoryMapping
        .withDisplays(mapping, Some(DisplayColumns(ok(ColumnName.of("item")), None))),
      Left(DocumentError.DisplayColumnMapped("item", "item"))
    )
    assertEquals(
      InventoryMapping.withDisplays(mapping, Some(DisplayColumns(kind, Some(kind)))),
      Left(DocumentError.DisplayColumnsShared("display_kind"))
    )
    val mapped = ok(
      StudioDocument.of(
        t2.datasets.map(d => if d.id == r3 then d.copy(inventory = Some(withCols)) else d),
        t2.analyses,
        t2.draft,
        t2.runs,
        t2.reporting,
        t2.figures,
        t2.presentation,
        t2.jobs
      )
    )
    val encoded = ok(StudioDocument.encode(mapped))
    assertEquals(version(encoded), Right(4))
    assertEquals(StudioDocument.decode(encoded), Right(mapped))
  }

  test("the bundle keeps a revision's repairs with it, and reassembles them") {
    val repaired = ok(Reducer.step(t2, relink))._1
    val encoded  =
      ok(
        ProjectBundle.encode(
          repaired,
          SharingOptions.complete,
          BundleSamples.inputsFor(repaired)
        )
      )
    val parts       = encoded.parts.toMap
    val withRelinks = parts.collect {
      case (path, bytes) if new String(Array.from(bytes), UTF_8).contains("\"relinks\"") =>
        path.value
    }
    // Only r3's dataset part (named by its content digest).
    assertEquals(withRelinks.toVector.map(_.takeWhile(_ != '.')), Vector("datasets/r3"))
    val assembled =
      ok(ProjectBundle.assemble(encoded.manifest, p => parts.get(p).toRight(fail(s"no $p"))))
    assertEquals(assembled.document, repaired)
    // t2 without repairs keeps its pinned bundle (ProjectBundleSuite).
  }

  test("a pending revision with a repair can be discarded; its undo puts the repair back") {
    val t1      = ok(StoryMoments.t1)
    val pending =
      t1.datasets.find(!_.decision.isAdmitted).getOrElse(fail("t1 has no pending revision"))
    val repair   = Command.RelinkAsset(pending.id, forest, Some(restored))
    val repaired = ok(Reducer.step(t1, repair))._1
    val outcome  = ok(Reducer.run(repaired, Command.DiscardDataset(pending.id)))
    assertEquals(outcome.document.dataset(pending.id), None)
    assertEquals(outcome.document.relinks.of(pending.id), Vector.empty)
    val undo = outcome.recording match
      case eyes4s.studio.core.command.Recording.Reversible(inverse) => inverse
      case other => fail(s"not reversible: $other")
    assertEquals(
      undo,
      Command.RestoreRepairedDataset(pending, Vector(AssetRelink(pending.id, forest, restored)))
    )
    assertEquals(ok(Reducer.step(outcome.document, undo))._1, repaired)
    // Without a repair, discarding is undone by RestoreDataset, as before.
    val plain = ok(Reducer.run(t1, Command.DiscardDataset(pending.id)))
    assert(
      plain.recording == eyes4s.studio.core.command.Recording
        .Reversible(Command.RestoreDataset(pending))
    )
    // A restore whose repairs name another revision is refused.
    assert(
      Reducer
        .step(
          outcome.document,
          Command.RestoreRepairedDataset(
            pending,
            Vector(AssetRelink(DatasetRevision(9), forest, restored))
          )
        )
        .isLeft
    )
  }
