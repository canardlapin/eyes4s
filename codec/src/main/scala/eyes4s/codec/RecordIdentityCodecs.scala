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

import eyes4s.plan.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}

/** JSON forms of the record and fixation identities that an application
  * stores in its own documents. Each value is an object with one member
  * whose name states the counting convention, so a stored number can never
  * be read in another one:
  *
  * {{{
  * {"data-record": 7214}        // DataRecord: from 1, header excluded
  * {"fixation-number": 6}       // FixationNumber: from 1
  * {"scanpath-position": 5}     // ScanpathPosition: from 0
  * }}}
  *
  * Decoding rebuilds each value through its smart constructor and refuses
  * any other member name, an extra member, a value that is not a JSON
  * integer (a string is refused), or a number outside the range.
  * These forms carry no schema identity of their own; the document that
  * holds them is versioned.
  */
object RecordIdentityCodecs:
  private def tagged[A](
      name: String,
      value: A => Int,
      of: Int => Either[RecordIdentityError, A]
  ): Codec[A] =
    Codec.from(
      Decoder.instance { cursor =>
        cursor.keys.map(_.toVector) match
          case Some(Vector(`name`)) =>
            val member = cursor.downField(name)
            member.focus.flatMap(_.asNumber).flatMap(_.toInt) match
              case Some(n) => of(n).left.map(e => DecodingFailure(e.message, member.history))
              case None    =>
                Left(
                  DecodingFailure(
                    s"Member '$name' must be an integer, found ${member.focus.fold("nothing")(_.noSpaces)}.",
                    member.history
                  )
                )
          case other =>
            Left(
              DecodingFailure(
                s"Expected an object with the single member '$name', found ${other
                    .fold("no object")(keys => keys.mkString("members [", ", ", "]"))}.",
                cursor.history
              )
            )
      },
      Encoder.instance(a => Json.obj(name -> Json.fromInt(value(a))))
    )

  given dataRecord: Codec[DataRecord] = tagged("data-record", _.value, DataRecord.of)

  given fixationNumber: Codec[FixationNumber] =
    tagged("fixation-number", _.value, FixationNumber.of)

  given scanpathPosition: Codec[ScanpathPosition] =
    tagged("scanpath-position", _.value, ScanpathPosition.of)
