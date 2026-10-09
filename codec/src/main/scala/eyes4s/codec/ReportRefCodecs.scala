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

import eyes4s.results.*
import io.circe.{ACursor, Codec, Decoder, DecodingFailure, Encoder, Json}

/** The JSON form of a report reference ([[eyes4s.results.ReportRef ReportRef]]) that an application
  * stores in its own documents. A reference is an object with one member
  * naming its level:
  *
  * {{{
  * {"cell": {"scale": 0, "group": [{"term": "memory", "level": "Remembered"}],
  *           "role": "difference", "component": "value"}}
  * {"participant": {"cell": {...the cell's members...}, "name": "P17"}}
  * }}}
  *
  * Roles are `matched`, `control` or `difference`; a group is its levels
  * in grouping order (an empty array for the whole report). Decoding
  * rebuilds each reference through its smart constructor and refuses a
  * missing or extra member, another level name, a scale not spelled as an
  * integer (on the JVM) or below 0, and a blank component or participant.
  * The form has no schema identity of its own: the document that holds it is
  * versioned.
  */
object ReportRefCodecs:
  private def fail[A](cursor: ACursor, message: String): Either[DecodingFailure, A] =
    Left(DecodingFailure(message, cursor.history))

  private def members(cursor: ACursor, names: Vector[String]): Either[DecodingFailure, Unit] =
    cursor.keys.map(_.toVector) match
      case Some(found) if found.sorted == names.sorted => Right(())
      case other                                       =>
        fail(
          cursor,
          s"Expected the members ${names.mkString("[", ", ", "]")}, found " +
            other.fold("no object")(_.mkString("[", ", ", "]")) + "."
        )

  private def role(value: Role): String = value match
    case Role.Matched    => "matched"
    case Role.Control    => "control"
    case Role.Difference => "difference"

  private def cellJson(cell: ReportRef.Cell): Json = Json.obj(
    "scale" -> Json.fromInt(cell.scale),
    "group" -> Json.arr(cell.group.levels.map { (term, level) =>
      Json.obj("term" -> Json.fromString(term), "level" -> Json.fromString(level))
    }*),
    "role"      -> Json.fromString(role(cell.role)),
    "component" -> Json.fromString(cell.component)
  )

  private def readCell(cursor: ACursor): Either[DecodingFailure, ReportRef.Cell] =
    for
      _     <- members(cursor, Vector("scale", "group", "role", "component"))
      scale <- cursor
        .downField("scale")
        .focus
        .toRight("nothing")
        .flatMap(summon[Wire.Member[Int]].read)
        .left
        .map(reason => DecodingFailure(s"Member 'scale': $reason.", cursor.history))
      levels <- cursor.downField("group").as[Vector[Json]].flatMap { entries =>
        entries.zipWithIndex.foldLeft[Either[DecodingFailure, Vector[(String, String)]]](
          Right(Vector.empty)
        ) { case (done, (entry, i)) =>
          done.flatMap { found =>
            val c = entry.hcursor
            for
              _     <- members(c, Vector("term", "level"))
              term  <- c.downField("term").as[String]
              level <- c.downField("level").as[String]
            yield found :+ (term -> level)
          }
        }
      }
      token <- cursor.downField("role").as[String]
      found <- Role.values
        .find(r => role(r) == token)
        .fold(fail[Role](cursor.downField("role"), s"Unknown role '$token'."))(Right(_))
      component <- cursor.downField("component").as[String]
      cell      <- ReportRef
        .cell(scale, GroupKey(levels), found, component)
        .left
        .map(e => DecodingFailure(e.message, cursor.history))
    yield cell

  given reportRef: Codec[ReportRef] = Codec.from(
    Decoder.instance { cursor =>
      cursor.keys.map(_.toVector) match
        case Some(Vector("cell"))        => readCell(cursor.downField("cell"))
        case Some(Vector("participant")) =>
          val inner = cursor.downField("participant")
          for
            _    <- members(inner, Vector("cell", "name"))
            cell <- readCell(inner.downField("cell"))
            name <- inner.downField("name").as[String]
            ref  <- ReportRef
              .participant(cell, name)
              .left
              .map(e => DecodingFailure(e.message, inner.history))
          yield ref
        case other =>
          fail(
            cursor,
            "Expected an object with the single member 'cell' or 'participant', found " +
              other.fold("no object")(_.mkString("members [", ", ", "]")) + "."
          )
    },
    Encoder.instance {
      case cell: ReportRef.Cell               => Json.obj("cell" -> cellJson(cell))
      case participant: ReportRef.Participant =>
        Json.obj(
          "participant" -> Json.obj(
            "cell" -> cellJson(participant.cell),
            "name" -> Json.fromString(participant.participant)
          )
        )
    }
  )
