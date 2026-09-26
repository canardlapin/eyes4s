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
import eyes4s.codec.{CanonicalDigest, CodecError, VersionedCodec}
import eyes4s.studio.core.document.{CanonicalJson, StudioDocument, StudioSchemaIds}
import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder}

/** One step of an editing session, as the journal records it. */
enum JournalEntry derives CanEqual, Codec.AsObject:
  case Apply(command: Command)
  case Undo, Redo, UndoView, RedoView

/** One line of a command journal. The first line names the document the
  * session started from by its CR3 digest; each later line is one entry,
  * numbered from 1.
  */
enum JournalLine derives CanEqual:
  case Start(base: CanonicalDigest[StudioDocument])
  case Entry(seq: Int, entry: JournalEntry)

object JournalLine:
  private given Codec[CanonicalDigest[StudioDocument]] = Codec.from(
    Decoder[String].emap(hex => CanonicalDigest.parse[StudioDocument](hex).left.map(_.message)),
    Encoder[String].contramap(_.sha256.hex)
  )
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
    case Unwritable(e) => s"The journal cannot be written: ${e.message}"

/** A final line that did not decode: the write a crash interrupted. */
final case class TornLine(line: Int, text: String) derives CanEqual

/** A replayed journal: the history it rebuilds and the entries it applied. */
final case class Replay(history: History, entries: Vector[JournalEntry], torn: Option[TornLine])
    derives CanEqual

/** The autosave command journal (ticket S2.2, used by S2.4b): JSON lines,
  * each a CR3 versioned envelope (`studio.journal`) around a [[JournalLine]]
  * in canonical JSON. It is text only; reading and writing the file is the
  * store's (S2.4b).
  *
  * Replay is exact: it starts a [[History]] on the base document, checks the
  * base digest, and performs every entry, undo and redo included. The one
  * tolerated fault is a final line that does not decode (a torn write),
  * which is reported and skipped. Effects are not returned: a recovered run
  * request is the recovery screen's decision, not the journal's.
  */
object CommandJournal:

  val codec: Either[CodecError, VersionedCodec[JournalLine]] =
    StudioSchemaIds.ids
      .leftMap(e => CodecError.Unsupported("schema", e.message))
      .map { ids =>
        VersionedCodec.checked[JournalLine](ids.journal)(l => Right(CanonicalJson(l.asJson))) {
          json =>
            json.as[JournalLine].left.map(f => CodecError.Field("journal", json, f.getMessage))
        }
      }

  def encode(line: JournalLine): Either[JournalError, String] =
    codec.flatMap(_.encode(line)).bimap(JournalError.Unwritable(_), _.noSpaces)

  /** The start line of a journal over `base`. */
  def start(base: StudioDocument): Either[JournalError, String] =
    digest(base).flatMap(d => encode(JournalLine.Start(d)))

  /** The line recording entry number `seq`. */
  def entry(seq: Int, entry: JournalEntry): Either[JournalError, String] =
    encode(JournalLine.Entry(seq, entry))

  /** A whole journal: the start line and each entry, newline-terminated. */
  def write(base: StudioDocument, entries: Vector[JournalEntry]): Either[JournalError, String] =
    for
      head <- start(base)
      rest <- entries.zipWithIndex.traverse((e, i) => entry(i + 1, e))
    yield (head +: rest).map(_ + "\n").mkString

  def replay(base: StudioDocument, text: String): Either[JournalError, Replay] =
    val lines = text.split("\n", -1).toVector match
      case init :+ "" => init
      case all        => all
    def read(n: Int) =
      codec.flatMap(_.parse(lines(n - 1))).left.map(JournalError.Unreadable(n, _))
    for
      _     <- Either.cond(lines.nonEmpty, (), JournalError.Empty)
      first <- read(1)
      from  <- first match
        case JournalLine.Start(d) => Right(d)
        case _                    => Left(JournalError.NotStart(1))
      own    <- digest(base)
      _      <- Either.cond(from.sameAs(own), (), JournalError.BaseMismatch(from, own))
      replay <- (2 to lines.size).toVector
        .foldLeftM(Replay(History.start(base), Vector.empty, None)) { (acc, n) =>
          read(n) match
            case Left(_) if n == lines.size =>
              Right(acc.copy(torn = Some(TornLine(n, lines(n - 1)))))
            case Left(e)                          => Left(e)
            case Right(JournalLine.Start(_))      => Left(JournalError.RepeatedStart(n))
            case Right(JournalLine.Entry(seq, e)) =>
              for
                _    <- Either.cond(seq == n - 1, (), JournalError.OutOfSequence(n, n - 1, seq))
                step <- acc.history.perform(e).left.map(JournalError.Rejected(n, e, _))
              yield acc.copy(history = step.history, entries = acc.entries :+ e)
        }
    yield replay

  private def digest(document: StudioDocument) =
    StudioDocument.codec.flatMap(_.digest(document)).leftMap(JournalError.Unwritable(_))
