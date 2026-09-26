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

import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** Conditional epoch codec: callers supply explicit recipe and mark-key schemas,
  * as for DomainCodecs and TimelineCodecs. No behavior is loaded by decoding.
  */
object EpochCodecs:
  def plan[K](schema: DefinitionId, key: VersionedCodec[K]): VersionedCodec[EpochPlan[K]] =
    VersionedCodec.checked[EpochPlan[K]](schema)(p =>
      key.encode(p.anchor.kind).map { kind =>
        val occurrence = p.anchor.occurrence match
          case Occurrence.Nth(index) =>
            Json.obj(
              "kind"  -> Json.fromString("nth-zero-based"),
              "index" -> DomainWire.time(index.toLong)
            )
          case other => Json.obj("kind" -> Json.fromString(other.toString))
        Json.obj(
          "anchor"         -> kind,
          "occurrence"     -> occurrence,
          "fromMicros"     -> DomainWire.time(p.window.from.toMicros),
          "untilMicros"    -> DomainWire.time(p.window.until.toMicros),
          "binWidthMicros" -> DomainWire.time(p.binWidth.toMicros),
          "finalBin"       -> Json.fromString(p.finalBin.toString)
        )
      }
    )(j =>
      for
        encodedKey        <- Wire.field[Json](j, "anchor")
        kind              <- key.decode(encodedKey)
        encodedOccurrence <- Wire.field[Json](j, "occurrence")
        occurrenceKind    <- Wire.field[String](encodedOccurrence, "kind")
        occurrence        <- occurrenceKind match
          case "RequireUnique"  => Right(Occurrence.RequireUnique)
          case "First"          => Right(Occurrence.First)
          case "Last"           => Right(Occurrence.Last)
          case "nth-zero-based" =>
            for
              i     <- DomainWire.micros(encodedOccurrence, "index")
              index <- NonNegativeLong
                .of(i)
                .left
                .map(e => CodecError.Field("occurrence.index", encodedOccurrence, e.message))
            yield Occurrence.Nth(index)
          case _ =>
            Left(
              CodecError.Field(
                "occurrence.kind",
                encodedOccurrence,
                "unknown occurrence policy"
              )
            )
        a      <- DomainWire.micros(j, "fromMicros")
        b      <- DomainWire.micros(j, "untilMicros")
        window <- Window
          .of(Span.micros(a), Span.micros(b))
          .left
          .map(e => CodecError.Field("window", j, e.message))
        w     <- DomainWire.micros(j, "binWidthMicros")
        width <- PositiveSpan
          .of(Span.micros(w))
          .left
          .map(e => CodecError.Field("binWidthMicros", j, e.message))
        finalName <- Wire.field[String](j, "finalBin")
        finalBin  <- FinalBin.values
          .find(_.toString == finalName)
          .toRight(CodecError.Field("finalBin", j, "unknown final-bin policy"))
        plan <- EpochPlan
          .of(MarkSelector(kind, occurrence), window, width, finalBin)
          .left
          .map(e => CodecError.Field("epochPlan", j, e.message))
      yield plan
    )
