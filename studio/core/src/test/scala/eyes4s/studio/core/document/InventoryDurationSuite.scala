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

package eyes4s.studio.core.document

import eyes4s.studio.core.command.{Command, CommandJournal, History, JournalEntry, JournalLine}
import eyes4s.studio.core.importing.{CsvSniffer, MappingError, TrialMetadataDraft}
import io.circe.Json
import io.circe.syntax.*

class InventoryDurationSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def name(value: String)              = ok(ColumnName.of(value))
  private val base                             = DocumentSamples.t1
  private val data                             = base.datasets.last
  private val original    = data.inventory.getOrElse(fail("missing inventory"))
  private val declaration = ok(InventoryDurationColumn.of(name("elapsed"), TimeUnit.Seconds))
  private val mapping     = ok(InventoryMapping.withDuration(original, Some(declaration)))
  private val revise      = Command.ReviseDataset(
    data.id,
    data.mapping,
    data.units,
    data.geometry,
    data.attributes,
    Some(mapping)
  )
  private def document = ok(History.start(base).apply(revise)).history.document

  private def relabel(envelope: Json, version: Int): Json =
    envelope.hcursor
      .downField("schema")
      .downField("version")
      .withFocus(_ => Json.fromInt(version))
      .top
      .getOrElse(fail("missing envelope"))

  test(
    "mapping captures exact duration text, explicit unit and rounding; None preserves old bytes"
  ) {
    assertEquals(mapping.duration, Some(declaration))
    assertEquals(
      mapping.attributes.bindings.last,
      AttributeBinding(name("elapsed"), AttributeKindChoice.Text)
    )
    assertEquals(mapping.asJson.as[InventoryMapping], Right(mapping))
    assert(!original.asJson.hcursor.downField("duration").succeeded)
    assertEquals(InventoryMapping.withDuration(original, None), Right(original))
    assertEquals(
      ok(InventoryMapping.withDuration(mapping, None)).attributes,
      mapping.attributes
    )
  }

  test("timing conflicts with identity roles, display columns and lossy attribute kinds") {
    val role = ok(InventoryDurationColumn.of(name("participant"), TimeUnit.Seconds))
    assert(InventoryMapping.withDuration(original, Some(role)).isLeft)
    val display =
      ok(InventoryMapping.withDisplays(original, Some(DisplayColumns(name("elapsed"), None))))
    assert(InventoryMapping.withDuration(display, Some(declaration)).isLeft)
    assert(
      InventoryMapping.withDisplays(mapping, Some(DisplayColumns(name("elapsed"), None))).isLeft
    )
    val numeric = ok(
      DeclaredAttributes.of(
        original.attributes.bindings :+ AttributeBinding(
          name("elapsed"),
          AttributeKindChoice.Number
        )
      )
    )
    assert(
      InventoryMapping
        .withDuration(ok(InventoryMapping.of(original.bindings, numeric)), Some(declaration))
        .isLeft
    )
  }

  test(
    "duration re-mapping follows original column names, persists resolve, and clears deliberately"
  ) {
    val preview = ok(
      CsvSniffer.sniff(
        "trials.csv",
        "participant,phase,trial,occurrence,item,response,display_kind,image_file,elapsed\nP,Encoding,t,1,i,,blank,,5\n"
      )
    )
    val draft = ok(TrialMetadataDraft.ofDataset(preview, data.id, mapping))
    assertEquals(draft.duration, Some(declaration))
    assertEquals(draft.resolve, Right(mapping))
    assertEquals(ok(draft.declareDuration(None)).resolve.map(_.duration), Right(None))
    val absent = ok(InventoryDurationColumn.of(name("missing"), TimeUnit.Milliseconds))
    assertEquals(
      draft.declareDuration(Some(absent)),
      Left(MappingError.UnknownColumn("trials.csv", name("missing")))
    )
  }

  test(
    "declaration selects document9 and science5; older read/write and relabel paths refuse"
  ) {
    val value  = document
    val ladder = ok(StudioDocument.ladder)
    assertEquals(ladder.earliest(value).version, 9)
    assertEquals(StudioDocument.decode(ok(StudioDocument.encode(value))), Right(value))
    val encoded = ok(ladder.codec.encode(value))
    ladder.versions.filter(_.version < 9).foreach { version =>
      assert(ladder.writeAt(version, value).isLeft, version)
      assert(ladder.readAt(version, value.asJson).isLeft, version)
      assert(ladder.codec.decode(relabel(encoded, version.version)).isLeft, version)
    }
    val science = ok(ScienceContent.ladder)
    assertEquals(science.earliest(value.science).version, 5)
    val scienceWire = ok(science.codec.encode(value.science))
    science.versions.filter(_.version < 5).foreach { version =>
      assert(science.writeAt(version, value.science).isLeft, version)
      assert(science.readAt(version, value.science.asJson).isLeft, version)
      assert(science.codec.decode(relabel(scienceWire, version.version)).isLeft, version)
    }
  }

  test("all journal carriers require version8; old readers cannot silently lose timing") {
    val sample   = ok(History.start(base).apply(revise)).history.document.datasets.last
    val imported = Command.ImportSources(
      sample.parent,
      sample.sources,
      sample.mapping,
      sample.units,
      sample.geometry,
      sample.attributes,
      Some(sample.admission),
      sample.inventory
    )
    val commands = Vector[Command](
      revise,
      imported,
      Command.RestoreDataset(sample),
      Command.RestoreRepairedDataset(sample, Vector.empty)
    )
    val ladder = ok(CommandJournal.ladder)
    commands.foreach { command =>
      val line    = JournalLine.Entry(1, JournalEntry.Apply(command))
      val encoded = ok(ladder.codec.encode(line))
      assertEquals(ladder.earliest(line).version, 8)
      assertEquals(ladder.codec.decode(encoded), Right(line))
      ladder.versions.filter(_.version < 8).foreach { version =>
        assert(ladder.writeAt(version, line).isLeft, version)
        assert(ladder.readAt(version, line.asJson).isLeft, version)
        assert(ladder.codec.decode(relabel(encoded, version.version)).isLeft, version)
      }
    }
  }

  test(
    "duration declarations change science and native content identity; undo and journal restore exactly"
  ) {
    val live = ok(History.start(base).apply(revise)).history
    assertNotEquals(
      StudioDocument.scienceDigest(live.document),
      StudioDocument.scienceDigest(base)
    )
    assertNotEquals(
      DatasetRevisionSpec.contentDigest(live.document.datasets.last),
      DatasetRevisionSpec.contentDigest(data)
    )
    assertEquals(live.document.presentation, base.presentation)
    assertEquals(ok(live.undo).history.document, base)
    assertEquals(ok(ok(live.undo).history.redo).history.document, live.document)
    val entries = Vector(JournalEntry.Apply(revise), JournalEntry.Undo, JournalEntry.Redo)
    val journal = ok(CommandJournal.write(base, entries, checkpointEvery = 1))
    assertEquals(ok(CommandJournal.replay(base, journal)).history.document, live.document)
  }
