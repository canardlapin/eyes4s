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

import cats.syntax.all.*
import eyes4s.codec.{CanonicalDigest, CodecError, SchemaLadder, VersionedCodec}
import eyes4s.studio.core.document.DigestJson.given
import eyes4s.studio.core.document.{
  CanonicalJson,
  ScienceContent,
  StudioDocument,
  StudioSchemaIds
}
import io.circe.syntax.*
import io.circe.{Codec, Json}

/** One step of an editing session, as the journal records it. */
enum JournalEntry derives CanEqual, Codec.AsObject:
  case Apply(command: Command)
  case Undo, Redo, UndoView, RedoView

/** One line of a command journal. The first line names the document the
  * session started from by its CR3 digest; each later line is one entry,
  * numbered from 1, or a checkpoint: the science digest
  * ([[StudioDocument.scienceDigest]]) after the first `seq` entries.
  */
enum JournalLine derives CanEqual:
  case Start(base: CanonicalDigest[StudioDocument])
  case Entry(seq: Int, entry: JournalEntry)
  case Checkpoint(seq: Int, science: CanonicalDigest[ScienceContent])

object JournalLine:
  given Codec.AsObject[JournalLine] = Codec.AsObject.derived

/** Why a journal could not be written or replayed. Lines are numbered from
  * 1, the start line included.
  */
enum JournalError derives CanEqual:
  case Empty
  case Unreadable(line: Int, error: CodecError)
  case NotStart(line: Int)
  case RepeatedStart(line: Int)
  case BaseMismatch(
      journal: CanonicalDigest[StudioDocument],
      document: CanonicalDigest[StudioDocument]
  )
  case OutOfSequence(line: Int, expected: Int, found: Int)
  case Rejected(line: Int, entry: JournalEntry, error: CommandError)

  /** A checkpoint after `seq` entries on a line where `entries` had been
    * replayed.
    */
  case CheckpointOutOfPlace(line: Int, entries: Int, seq: Int)

  /** Replaying the first `seq` entries gave science `replayed`, but the
    * session recorded `journal`: the replay has drifted from the session.
    */
  case Drift(
      line: Int,
      seq: Int,
      journal: CanonicalDigest[ScienceContent],
      replayed: CanonicalDigest[ScienceContent]
  )
  case Unwritable(error: CodecError)

  def message: String = this match
    case Empty                => "The journal has no start line."
    case Unreadable(line, e)  => s"Journal line $line is unreadable: ${e.message}"
    case NotStart(line)       => s"Journal line $line should be the start line."
    case RepeatedStart(line)  => s"Journal line $line is a second start line."
    case BaseMismatch(j, doc) =>
      s"The journal starts from ${j.display}, but the document is ${doc.display}."
    case OutOfSequence(line, expected, found) =>
      s"Journal line $line is entry $found; entry $expected was expected."
    case Rejected(line, entry, e) =>
      s"Journal line $line (${entry.productPrefix}) cannot be replayed: ${e.message}"
    case CheckpointOutOfPlace(line, entries, seq) =>
      s"Journal line $line is a checkpoint after $seq entries, but $entries were replayed."
    case Drift(line, seq, journal, replayed) =>
      s"Journal line $line records science ${journal.display} after $seq entries, but the " +
        s"replay has ${replayed.display}."
    case Unwritable(e) => s"The journal cannot be written: ${e.message}"

/** A final line that did not decode: the write a crash interrupted. */
final case class TornLine(line: Int, text: String) derives CanEqual

/** A replayed journal: the history it rebuilds, the entries it applied, the
  * entry counts at which a checkpoint was checked (in order), and the torn
  * final line, if any.
  */
final case class Replay(
    history: History,
    entries: Vector[JournalEntry],
    checkpoints: Vector[Int],
    torn: Option[TornLine]
) derives CanEqual

/** The autosave command journal (ticket S2.2, used by S2.4b): JSON lines,
  * each a CR3 versioned envelope (`studio.journal`) around a [[JournalLine]]
  * in canonical JSON. It is text only; reading and writing the file is the
  * store's (S2.4b).
  *
  * Replay is exact: it starts a [[History]] on the base document, checks the
  * base digest, performs every entry, undo and redo included, and checks
  * each checkpoint's science digest (a mismatch is [[JournalError.Drift]]).
  * The one tolerated fault is a final line that does not decode (a torn write),
  * which is reported and skipped. Effects are not returned: a recovered run
  * request is the recovery screen's decision, not the journal's.
  */
object CommandJournal:

  /** Whether `json` names an enum case added in journal version 2: the
    * `PerceptionImagery` preset (S7.1), which a line carries wherever it
    * holds studio fields (`SaveAndRun`) or an analysis. A case is encoded as
    * an object key, which no user text can be, so a key search finds every
    * carrier.
    */
  private def namesV2(json: Json): Boolean =
    json.arrayOrObject(
      false,
      _.exists(namesV2),
      o => o.contains("PerceptionImagery") || o.values.exists(namesV2)
    )

  private def read(json: Json): Either[CodecError, JournalLine] =
    json.as[JournalLine].left.map(f => CodecError.Field("journal", json, f.getMessage))

  private val refusal = CodecError.Unsupported("studio journal", "a preset needs version 2")

  private def expressedByV6(line: JournalLine): Boolean = line match
    case JournalLine.Entry(
          _,
          JournalEntry.Apply(_: Command.MovePanel | _: Command.SetFigureMethods)
        ) =>
      false
    case JournalLine.Entry(_, JournalEntry.Apply(Command.RestoreFigure(figure))) =>
      figure.methods.isEmpty
    case _ => true
  private def beforeV7(line: JournalLine): Either[CodecError, JournalLine] =
    Either.cond(
      expressedByV6(line),
      line,
      CodecError.Unsupported(
        "studio journal",
        "figure panel movement and stored methods need version 7"
      )
    )

  private def expressedByV5(line: JournalLine): Boolean = expressedByV6(line) && (line match
    case JournalLine.Entry(_, JournalEntry.Apply(Command.BindCompletedArtifacts(facts))) =>
      facts.datasetDefinition.isEmpty
    case _ => true)
  private def beforeV6(line: JournalLine): Either[CodecError, JournalLine] =
    beforeV7(line).flatMap(v =>
      Either.cond(
        expressedByV5(line),
        v,
        CodecError.Unsupported("studio journal", "dataset definition bindings need version 6")
      )
    )

  private def expressedByV3(line: JournalLine): Boolean = expressedByV4(line) && (line match
    case JournalLine.Entry(_, JournalEntry.Apply(_: Command.StartAnalysis))    => false
    case JournalLine.Entry(_, JournalEntry.Apply(Command.RestoreDraft(draft))) =>
      !draft.isInitial
    case _ => true)
  private def expressedByV4(line: JournalLine): Boolean = expressedByV5(line) && (line match
    case JournalLine.Entry(_, JournalEntry.Apply(_: Command.BindCompletedArtifacts)) => false
    case _                                                                           => true)
  private def beforeV5(line: JournalLine): Either[CodecError, JournalLine] =
    beforeV6(line).flatMap(v =>
      Either.cond(
        expressedByV4(line),
        v,
        CodecError.Unsupported(
          "studio journal",
          "verified native artifact bindings need version 5"
        )
      )
    )
  private def beforeV4(line: JournalLine): Either[CodecError, JournalLine] =
    beforeV5(line).flatMap(v =>
      Either.cond(
        expressedByV3(v),
        v,
        CodecError.Unsupported("studio journal", "an initial draft needs version 4")
      )
    )

  private def expressedByV2(line: JournalLine): Boolean = expressedByV3(line) && (line match
    case JournalLine.Entry(_, JournalEntry.Apply(Command.PutReporting(spec))) =>
      spec.contrast.isEmpty
    case _ => true)

  private val contrastRefusal =
    CodecError.Unsupported("studio journal", "explicit contrast operands need version 3")

  private def beforeV3(line: JournalLine): Either[CodecError, JournalLine] =
    beforeV4(line).flatMap(v => Either.cond(expressedByV2(v), v, contrastRefusal))

  /** Every version of the journal line schema (CR3).
    *
    * Version 2 (S7.1) adds the `PerceptionImagery` preset. A line that names
    * it is version 2, which a version-1 reader refuses
    * (`CodecError.UnsupportedSchema`); every other line is still written as
    * version 1, byte for byte, and the upcast is the identity. A preset
    * cannot be dropped as a member can, so the version-1 writer and reader
    * refuse a line that names it.
    *
    * Version 3 records explicitly ordered contrast operands in `PutReporting`.
    * Previous versions refuse those lines, and old lines lift unchanged.
    */
  // Version 4 expresses initial drafts; version 5 alone expresses verified
  // post-storage binding facts (bead q-native-stored-completion).
  val ladder: Either[CodecError, SchemaLadder[JournalLine]] =
    StudioSchemaIds.ids
      .leftMap(e => CodecError.Unsupported("schema", e.message))
      .map { ids =>
        SchemaLadder
          .of[JournalLine]("studio journal", ids.journal) { l =>
            val json = CanonicalJson(l.asJson)
            beforeV3(l).flatMap(_ => Either.cond(!namesV2(json), json, refusal))
          }(json => if namesV2(json) then Left(refusal) else read(json).flatMap(beforeV3))
          .next(l => expressedByV2(l) && !namesV2(l.asJson), identity)(l =>
            beforeV3(l).map(v => CanonicalJson(v.asJson))
          )(json => read(json).flatMap(beforeV3))
          .next(l => expressedByV3(l) && expressedByV2(l), identity)(l =>
            beforeV4(l).map(v => CanonicalJson(v.asJson))
          )(json => read(json).flatMap(beforeV4))
          .next(expressedByV3, identity)(l => beforeV5(l).map(v => CanonicalJson(v.asJson)))(
            json => read(json).flatMap(beforeV5)
          )
          .next(expressedByV4, identity)(l => beforeV6(l).map(v => CanonicalJson(v.asJson)))(
            json => read(json).flatMap(beforeV6)
          )
          .next(expressedByV5, identity)(l => beforeV7(l).map(v => CanonicalJson(v.asJson)))(
            json => read(json).flatMap(beforeV7)
          )
          .next(expressedByV6, identity)(l => Right(CanonicalJson(l.asJson)))(read)
      }

  val codec: Either[CodecError, VersionedCodec[JournalLine]] = ladder.map(_.codec)

  def encode(line: JournalLine): Either[JournalError, String] =
    codec.flatMap(_.encode(line)).bimap(JournalError.Unwritable(_), _.noSpaces)

  /** The start line of a journal over `base`. */
  def start(base: StudioDocument): Either[JournalError, String] =
    digest(base).flatMap(d => encode(JournalLine.Start(d)))

  /** The line recording entry number `seq`. */
  def entry(seq: Int, entry: JournalEntry): Either[JournalError, String] =
    encode(JournalLine.Entry(seq, entry))

  /** The checkpoint line after `seq` entries, when the session is at `now`. */
  def checkpoint(seq: Int, now: StudioDocument): Either[JournalError, String] =
    science(now).flatMap(d => encode(JournalLine.Checkpoint(seq, d)))

  /** A whole journal, newline-terminated: the start line, each entry, and a
    * checkpoint after every `checkpointEvery` entries. The entries are
    * performed to compute the checkpoints, so each must apply.
    */
  def write(
      base: StudioDocument,
      entries: Vector[JournalEntry],
      checkpointEvery: Int = 16
  ): Either[JournalError, String] =
    for
      head <- start(base)
      rest <- entries.zipWithIndex
        .foldLeftM((History.start(base), Vector.empty[String])) { case ((h, out), (e, i)) =>
          val seq = i + 1
          for
            line <- entry(seq, e)
            step <- h.perform(e).left.map(JournalError.Rejected(seq + 1, e, _))
            mark <-
              if checkpointEvery > 0 && seq % checkpointEvery == 0 then
                checkpoint(seq, step.history.document).map(Vector(_))
              else Right(Vector.empty)
          yield (step.history, out ++ (line +: mark))
        }
    yield (head +: rest._2).map(_ + "\n").mkString

  def replay(base: StudioDocument, text: String): Either[JournalError, Replay] =
    replay(codec, base, text)

  /** Replay through `codec`; a reader of an earlier version is a ladder's
    * `upTo` codec.
    */
  private[core] def replay(
      codec: Either[CodecError, VersionedCodec[JournalLine]],
      base: StudioDocument,
      text: String
  ): Either[JournalError, Replay] =
    val lines = text.split("\n", -1).toVector match
      case init :+ "" => init
      case all        => all
    def read(n: Int) =
      codec.flatMap(_.parse(lines(n - 1))).left.map(JournalError.Unreadable(n, _))
    // A line of a later schema version is not a torn write: it is refused,
    // never skipped.
    def torn(e: JournalError) = e match
      case JournalError.Unreadable(_, _: CodecError.UnsupportedSchema) => false
      case _                                                           => true
    for
      _     <- Either.cond(lines.nonEmpty, (), JournalError.Empty)
      first <- read(1)
      from  <- first match
        case JournalLine.Start(d) => Right(d)
        case _                    => Left(JournalError.NotStart(1))
      own    <- digest(base)
      _      <- Either.cond(from.sameAs(own), (), JournalError.BaseMismatch(from, own))
      replay <- (2 to lines.size).toVector
        // Replay reconstructs history under the stored-role rule (S5.3).
        .foldLeftM(
          Replay(
            History.start(base).under(MappingRule.Replay),
            Vector.empty,
            Vector.empty,
            None
          )
        ) { (acc, n) =>
          read(n) match
            case Left(e) if n == lines.size && torn(e) =>
              Right(acc.copy(torn = Some(TornLine(n, lines(n - 1)))))
            case Left(e)                          => Left(e)
            case Right(JournalLine.Start(_))      => Left(JournalError.RepeatedStart(n))
            case Right(JournalLine.Entry(seq, e)) =>
              val expected = acc.entries.size + 1
              for
                _ <- Either.cond(
                  seq == expected,
                  (),
                  JournalError.OutOfSequence(n, expected, seq)
                )
                step <- acc.history.perform(e).left.map(JournalError.Rejected(n, e, _))
              yield acc.copy(history = step.history, entries = acc.entries :+ e)
            case Right(JournalLine.Checkpoint(seq, recorded)) =>
              for
                _ <- Either.cond(
                  seq == acc.entries.size,
                  (),
                  JournalError.CheckpointOutOfPlace(n, acc.entries.size, seq)
                )
                replayed <- science(acc.history.document)
                _        <- Either.cond(
                  recorded.sameAs(replayed),
                  (),
                  JournalError.Drift(n, seq, recorded, replayed)
                )
              yield acc.copy(checkpoints = acc.checkpoints :+ seq)
        }
    yield replay.copy(history = replay.history.under(MappingRule.Commit))

  private def digest(document: StudioDocument) =
    StudioDocument.codec.flatMap(_.digest(document)).leftMap(JournalError.Unwritable(_))

  private def science(document: StudioDocument) =
    StudioDocument.scienceDigest(document).leftMap(JournalError.Unwritable(_))
