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

package eyes4s.studio.core.command

import eyes4s.codec.CodecError
import eyes4s.studio.core.document.{
  DocumentSamples,
  Preset,
  RevisionName,
  StudioDocument,
  StudioFields,
  StudioSchemaIds
}
import io.circe.Json
import io.circe.syntax.*
import org.scalacheck.Prop.forAll
import org.scalacheck.Test

/** The command wire contract and the autosave journal (ticket S2.2): every
  * command case is pinned, and replaying a journal rebuilds the session's
  * history exactly.
  */
class CommandJournalSuite extends munit.ScalaCheckSuite:
  import CommandGen.*

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(40)

  private val t2 = DocumentSamples.t2

  // Pins compare as JSON values: Scala.js prints the double 2.0 as `2`.
  private def same(pin: String, json: String) =
    io.circe.parser.parse(pin) == io.circe.parser.parse(json)

  test("every command case and journal entry encodes exactly as pinned") {
    val actual =
      (CommandSamples.commands.map((n, c) => n -> c.asJson.noSpaces) ++
        CommandSamples.entries.map((n, e) => n -> e.asJson.noSpaces)).toMap
    val drift =
      actual.toVector.sortBy(_._1).filter((n, j) => !CommandPins.pins.get(n).exists(same(_, j)))
    // A deliberate wire change updates CommandPins from these lines.
    drift.foreach((n, j) => println(s"PIN\t$n\t$j"))
    assertEquals(drift.map(_._1), Vector.empty)
    assertEquals(CommandPins.pins.keySet -- CommandPins.journalKeys, actual.keySet)
  }

  test("the first-analysis command has an exact version-4 journal envelope") {
    val command = CommandSamples.commands.find(_._1 == "StartAnalysis").get._2
    val encoded =
      CommandJournal.encode(JournalLine.Entry(1, JournalEntry.Apply(command))).toOption.get
    assert(same(CommandPins.initialAnalysisV4, encoded), encoded)
  }

  test("the samples cover every command case, and each round-trips") {
    val sampled = CommandSamples.commands.map(_._2.name).toSet
    assertEquals(allCommands.filterNot(sampled), Vector.empty)
    CommandSamples.commands.foreach { (n, c) =>
      assertEquals(io.circe.parser.decode[Command](c.asJson.noSpaces), Right(c), n)
    }
  }

  test("the story session's journal is pinned line by line") {
    val text  = CommandJournal.write(t2, CommandSamples.session, checkpointEvery = 4)
    val lines = text.map(_.split("\n").toVector)
    lines.foreach(
      _.foreach(l =>
        assert(l.startsWith("""{"schema":{"name":"studio.journal","version":1}"""), l)
      )
    )
    val drift = lines.toOption.get.zipWithIndex.filterNot((l, i) =>
      CommandPins.pins.get(s"journal.$i").exists(same(_, l))
    )
    drift.foreach((l, i) => println(s"PIN\tjournal.$i\t$l"))
    assertEquals(drift.map(_._2), Vector.empty)
  }

  test("replaying the story session rebuilds its history: t3's science, view unchanged") {
    val replay =
      CommandJournal
        .write(t2, CommandSamples.session, checkpointEvery = 4)
        .flatMap(CommandJournal.replay(t2, _))
    val live = CommandSamples.session.foldLeft(History.start(t2))((h, e) =>
      h.perform(e).toOption.get.history
    )
    assertEquals(replay.map(_.history), Right(live))
    assertEquals(replay.map(_.entries), Right(CommandSamples.session))
    assertEquals(replay.map(_.torn), Right(None))
    assertEquals(live.document.science.analyses, DocumentSamples.t3.science.analyses)
    assertEquals(live.document.presentation, t2.presentation)
  }

  property("replaying a journal of a generated session rebuilds the same history") {
    forAll(session(12)) { (start, traces) =>
      val performed = traces.filter(_.result.isRight).map(_.entry)
      val end       = traces.lastOption.fold(History.start(start))(t =>
        t.result.fold(_ => t.before, _.history)
      )
      val replay =
        CommandJournal
          .write(start, performed, checkpointEvery = 3)
          .flatMap(CommandJournal.replay(start, _))
      assertEquals(replay.map(_.history), Right(end))
      assertEquals(replay.map(_.entries), Right(performed))
    }
  }

  private val journal =
    CommandJournal.write(t2, CommandSamples.session, checkpointEvery = 4).toOption.get
  private val lines = journal.split("\n").toVector

  test("a torn final line is reported and skipped") {
    val torn   = (lines.init :+ lines.last.take(25)).mkString("\n")
    val replay = CommandJournal.replay(t2, torn)
    assertEquals(replay.map(_.torn), Right(Some(TornLine(lines.size, lines.last.take(25)))))
    assertEquals(replay.map(_.entries), Right(CommandSamples.session.init))
  }

  test("an unreadable line before the end is an error naming the line") {
    val broken = lines.updated(2, "{").mkString("\n")
    assertEquals(
      CommandJournal.replay(t2, broken).left.map(_.productPrefix),
      Left("Unreadable")
    )
    assert(CommandJournal.replay(t2, broken).left.exists(_.message.contains("line 3")))
  }

  test("the pinned journal checkpoints the science after entry 4") {
    val cp = io.circe.parser.parse(lines(5)).toOption.get
    assertEquals(
      cp.hcursor.downField("value").downField("Checkpoint").get[Int]("seq"),
      Right(4)
    )
    assertEquals(CommandJournal.replay(t2, journal).map(_.checkpoints), Right(Vector(4)))
  }

  test("a checkpoint the replay does not reach is a typed drift error") {
    val wrong = CommandJournal.checkpoint(4, DocumentSamples.t1).toOption.get
    val live  =
      CommandSamples.session
        .take(4)
        .foldLeft(History.start(t2))((h, e) => h.perform(e).toOption.get.history)
    assertEquals(
      CommandJournal.replay(t2, lines.updated(5, wrong).mkString("\n")),
      Left(
        JournalError.Drift(
          6,
          4,
          StudioDocument.scienceDigest(DocumentSamples.t1).toOption.get,
          StudioDocument.scienceDigest(live.document).toOption.get
        )
      )
    )
    val early = CommandJournal.checkpoint(3, live.document).toOption.get
    assertEquals(
      CommandJournal.replay(t2, lines.updated(5, early).mkString("\n")),
      Left(JournalError.CheckpointOutOfPlace(6, 4, 3))
    )
  }

  test("a journal over another document is refused") {
    val other = CommandJournal.replay(DocumentSamples.t1, journal)
    assertEquals(other.left.map(_.productPrefix), Left("BaseMismatch"))
  }

  test("entries out of sequence, a second start line and an empty journal are refused") {
    val swapped = lines.updated(1, lines(2)).updated(2, lines(1)).mkString("\n")
    assertEquals(CommandJournal.replay(t2, swapped), Left(JournalError.OutOfSequence(2, 1, 2)))
    val restarted = (lines :+ lines.head).mkString("\n") + "\n"
    assertEquals(
      CommandJournal.replay(t2, restarted),
      Left(JournalError.RepeatedStart(lines.size + 1))
    )
    assertEquals(CommandJournal.replay(t2, ""), Left(JournalError.Empty))
    assertEquals(
      CommandJournal.replay(t2, lines(1)).left.map(_.productPrefix),
      Left("NotStart")
    )
  }

  test("an entry the document refuses stops the replay and names the line") {
    val bad =
      CommandJournal.write(t2, Vector(JournalEntry.Undo)).flatMap(CommandJournal.replay(t2, _))
    assertEquals(
      bad,
      Left(
        JournalError.Rejected(
          2,
          JournalEntry.Undo,
          CommandError.NothingToUndo(HistoryStack.Science)
        )
      )
    )
  }

  test("the journal is built on the studio.journal schema id") {
    // Version 1 (S2.2) is the ladder's first rung; version 2 (S7.1) its next.
    assertEquals(
      CommandJournal.ladder.map(_.versions.head),
      StudioSchemaIds.ids
        .map(_.journal)
        .left
        .map(e => CodecError.Unsupported("schema", e.message))
    )
    assertEquals(
      CommandJournal.codec.map(c => (c.schema.name, c.schema.version)),
      Right(("studio.journal", 7))
    )
  }

  // --- Version 2: the PerceptionImagery preset (S7.1) ---------------------------------

  private val ladder = CommandJournal.ladder.toOption.get

  private def imageryRun(preset: Preset) =
    JournalLine.Entry(
      lines.size,
      JournalEntry.Apply(
        Command.SaveAndRun(
          Some(StudioFields(preset, RevisionName.of("imagery").toOption.get, ""))
        )
      )
    )

  private def version(text: String) =
    io.circe.parser
      .parse(text)
      .flatMap(_.hcursor.downField("schema").downField("version").as[Int])

  test("a line naming the PerceptionImagery preset is version 2 and round-trips") {
    val line    = imageryRun(Preset.PerceptionImagery)
    val encoded = CommandJournal.encode(line).toOption.get
    assertEquals(version(encoded), Right(2))
    assertEquals(CommandJournal.codec.flatMap(_.parse(encoded)), Right(line))
    assertEquals(ladder.earliest(line).version, 2)
    // Any other line keeps version 1, byte for byte as before.
    val custom = imageryRun(Preset.Custom)
    assertEquals(ladder.earliest(custom).version, 1)
    assertEquals(CommandJournal.encode(custom).map(version), Right(Right(1)))
  }

  test("a version-1 reader refuses a version-2 line, and never takes it for a torn write") {
    val older       = ladder.upTo(ladder.versions.head).toOption.get
    val encoded     = CommandJournal.encode(imageryRun(Preset.PerceptionImagery)).toOption.get
    val unsupported = CodecError.UnsupportedSchema(
      "studio journal",
      ladder.versions(1),
      Vector(ladder.versions.head)
    )
    assertEquals(older.codec.parse(encoded), Left(unsupported))
    // As the final line of a journal it is refused, not skipped as torn.
    val text = (lines :+ encoded).mkString("\n")
    assertEquals(
      CommandJournal.replay(Right(older.codec), t2, text),
      Left(JournalError.Unreadable(lines.size + 1, unsupported))
    )
    // A preset cannot be dropped: the version-1 writer refuses the line, and
    // a version-1 envelope that names it is refused.
    val refusal = CodecError.Unsupported("studio journal", "a preset needs version 2")
    assertEquals(
      older.writeAt(ladder.versions.head, imageryRun(Preset.PerceptionImagery)),
      Left(refusal)
    )
    val asV1 = io.circe.parser
      .parse(encoded)
      .toOption
      .get
      .hcursor
      .downField("schema")
      .downField("version")
      .withFocus(_ => Json.fromInt(1))
      .top
      .get
    assertEquals(CommandJournal.codec.flatMap(_.decode(asV1)), Left(refusal))
  }

  test(
    "a line written with \"inventory\":null still reads, and is written without it (S5.4 follow-up)"
  ) {
    import io.circe.parser.decode
    import io.circe.syntax.*
    val older = Vector(
      "ImportSources" -> """{"ImportSources":{"parent":2,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"attributes":[],"admission":null,"inventory":null}}""",
      "ImportSources.admission" -> """{"ImportSources":{"parent":3,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"attributes":[],"admission":{"offScreen":{"QuarantineTrial":{}},"corrections":[{"target":{"AllTrials":{}},"correction":{"FlipY":{}}}]},"inventory":null}}""",
      "ImportSources.attributes" -> """{"ImportSources":{"parent":2,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"attributes":[{"column":"Pupil","kind":{"Number":{}}}],"admission":null,"inventory":null}}""",
      "ReviseDataset" -> """{"ReviseDataset":{"dataset":3,"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Seconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"attributes":[{"column":"Pupil","kind":{"Number":{}}}],"inventory":null}}"""
    )
    older.foreach { (name, line) =>
      assert(line.contains("\"inventory\":null"), name)
      val command = decode[Command](line).fold(e => fail(s"$name: $e"), identity)
      val written = command.asJson.noSpaces
      // Compared as JSON: a platform may print a number differently.
      assertEquals(
        io.circe.parser.parse(written),
        io.circe.parser.parse(line.replace(",\"inventory\":null", "")),
        name
      )
      assert(!written.contains("\"inventory\":null"), name)
    }
  }
