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

import cats.syntax.all.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** The nested study codec retains the registered key and parameter types. */
final class TemporalStudyCodec[K, U <: Unit2D: UnitLabel, P, S, D](
    val schema: DefinitionId,
    val study: StudyCodec[K, U, P, S, D]
):
  val codec: VersionedCodec[TemporalStudyPlan[K, U, P, S, D]] =
    VersionedCodec.checked[TemporalStudyPlan[K, U, P, S, D]](schema)(plan =>
      study.codec
        .encode(plan.base)
        .map(base =>
          Json.obj(
            "study"    -> base,
            "input"    -> Json.fromString(plan.input.digest),
            "scope"    -> Json.fromString("withinParticipant"),
            "boundary" -> Json.fromString(plan.boundary.toString),
            "windows"  -> Json.arr(
              plan.windows.map(w =>
                Json.obj(
                  "name"        -> Json.fromString(w.name),
                  "fromMicros"  -> Json.fromString(w.window.from.toMicros.toString),
                  "untilMicros" -> Json.fromString(w.window.until.toMicros.toString)
                )
              )*
            ),
            "repetitions" -> Json.arr(
              plan.repetitions.map(r =>
                Json.obj(
                  "name"      -> Json.fromString(r.name),
                  "focal"     -> Json.fromString(r.focalPhase),
                  "reference" -> Json.fromString(r.referencePhase)
                )
              )*
            )
          )
        )
    ) { json =>
      def invalid(path: String, e: TemporalStudyError): CodecError =
        CodecError.Field(path, json, e.message)
      def micros(value: Json, field: String): Either[CodecError, Long] =
        Wire
          .field[String](value, field)
          .flatMap(raw =>
            raw.toLongOption
              .toRight(CodecError.Field(field, value, s"invalid integer microseconds '$raw'"))
          )
      for
        baseJson <- Wire.field[Json](json, "study")
        base     <- study.codec.decode(baseJson)
        digest   <- Wire.field[String](json, "input")
        input    <- ArtifactRef
          .parse[TemporalStudyInput[K, U]](digest)
          .left
          .map(CodecError.Definition.apply)
        scope <- Wire.field[String](json, "scope")
        _     <- Either.cond(
          scope == "withinParticipant",
          (),
          CodecError.Field("scope", json, s"unsupported repetition scope '$scope'")
        )
        rawBoundary <- Wire.field[String](json, "boundary")
        boundary    <- FixationBoundary.values
          .find(_.toString == rawBoundary)
          .toRight(CodecError.Field("boundary", json, s"unknown boundary '$rawBoundary'"))
        rawWindows <- Wire.field[Vector[Json]](json, "windows")
        windows    <- rawWindows.traverse { raw =>
          for
            name     <- Wire.field[String](raw, "name")
            start    <- micros(raw, "fromMicros")
            end      <- micros(raw, "untilMicros")
            relative <- Window
              .of(Span.micros(start), Span.micros(end))
              .left
              .map(e => CodecError.Field("window", raw, e.message))
            window <- StudyWindow.of(name, relative).left.map(invalid("window", _))
          yield window
        }
        rawRepeats <- Wire.field[Vector[Json]](json, "repetitions")
        repeats    <- rawRepeats.traverse { raw =>
          for
            name      <- Wire.field[String](raw, "name")
            focal     <- Wire.field[String](raw, "focal")
            reference <- Wire.field[String](raw, "reference")
            repeat    <- RepetitionContrast
              .withinParticipant(name, focal, reference)
              .left
              .map(invalid("repetition", _))
          yield repeat
        }
        plan <- TemporalStudyPlan
          .of(base, input, windows, repeats, boundary)
          .left
          .map(invalid("temporalStudy", _))
      yield plan
    }
