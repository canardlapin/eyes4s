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

package example

import cats.data.NonEmptyVector
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.codec.CodecDiagnostics.given
import eyes4s.fs2.*
import eyes4s.io.ArtifactFiles
import eyes4s.kernel.*
import eyes4s.plan.*
import example.FixationJourney.{JourneyError, Reloaded}
import io.circe.Json

import java.nio.file.{Files, Path, Paths}

/** The fresh reader of the fixation journey: a separate JVM, started over
  * the consumer's test classpath (its own classes and the packaged eyes4s
  * artifacts), that knows nothing but a route name and a directory. It reads
  * the manifest address from the directory, resolves the saved run through
  * `ArtifactFiles` with fresh registrations, preflights and reruns it
  * through the FS2 runner, and prints one receipt: the address it resolved,
  * the run id, the fingerprints of the archived and rerun results, the
  * rerun's canonical archive digest, the reconstructed identities, the
  * rerun's located failures and a drill-down to CSV records; or, when the run
  * is refused, each refusal's code and entry.
  */
object JourneyReader extends IOApp:
  val marker = "EYES4S_JOURNEY_READER="

  /** What can stop the reader after resolution, kept typed until printed. */
  private type Failure = ByteDigestError | JourneyError | CodecError | DrillDownError[?] |
    InspectionError[?] | RunOutcome[?, ?, ?, ?, ?]

  def run(arguments: List[String]): IO[ExitCode] = arguments match
    case List(route, directory) =>
      val receipt = route match
        case JourneyCases.cosine.name => read(JourneyCases.cosine, Paths.get(directory))
        case JourneyCases.scaled.name => read(JourneyCases.scaled, Paths.get(directory))
        case other => IO.pure(Json.obj("outcome" -> Json.fromString(s"unknown route $other")))
      receipt
        .map(_.deepMerge(Json.obj("process" -> Json.fromLong(ProcessHandle.current.pid))))
        .flatMap(json => IO.println(marker + json.noSpaces))
        .as(ExitCode.Success)
    case _ => IO.println("usage: JourneyReader <route> <directory>").as(ExitCode(2))

  private def strings(values: Vector[String]): Json = Json.arr(values.map(Json.fromString)*)

  private def refused(errors: NonEmptyVector[ResolveError]): Json =
    Json.obj(
      "outcome" -> Json.fromString("refused"),
      "errors"  -> Json.arr(errors.toVector.map { error =>
        Json.obj(
          "code"  -> Json.fromString(Diagnostic.of(error).code.render),
          "entry" -> Json.fromString(error.entryName.fold("")(_.value))
        )
      }*)
    )

  private def failed(error: Failure): Json =
    Json.obj("outcome" -> Json.fromString("failed"), "error" -> Json.fromString(error.toString))

  private def read[K, P, S, D](c: JourneyCase[K, P, S, D], directory: Path): IO[Json] =
    val route = c.route()
    IO.blocking(Files.readString(directory.resolve(FixationJourney.addressFile)).trim)
      .map(ByteDigest.parse)
      .flatMap {
        case Left(error)    => IO.pure(failed(error))
        case Right(address) =>
          FixationJourney
            .load(
              route,
              address,
              ArtifactFiles.directory[IO](directory, FixationJourney.manifestFile)
            )
            .flatMap {
              case Left(JourneyError.Resolve(errors)) => IO.pure(refused(errors))
              case Left(error)                        => IO.pure(failed(error))
              case Right(study)                       => rerun(c, route, study)
            }
            .map(_.deepMerge(Json.obj("address" -> Json.fromString(address.hex))))
      }

  private def rerun[K, P, S, D](
      c: JourneyCase[K, P, S, D],
      route: AnalysisRoute[K, P, S, D],
      study: Reloaded[K, P, S, D]
  ): IO[Json] =
    import JourneySetup.{comparison, pairs, quanta, render}
    FixationJourney.rerun(study, pairs, comparison, quanta).map {
      case Right(RunOutcome.Completed(run, last, result)) =>
        val receipt = for
          archived <- FixationJourney.fingerprint(study.plan, study.result).left.map(failed)
          again    <- FixationJourney.fingerprint(study.plan, result).left.map(failed)
          encoded  <- StoredArtifact
            .result(FixationJourney.resultEntry, route.results, result)
            .left
            .map(failed)
          keys <- study.input.trials.rows
            .traverse(trial => route.persistence.keys.encode(trial.key))
            .left
            .map(failed)
          view <- ResultInspection
            .study(study.plan, result, study.input, Some(study.ledger))
            .left
            .map(failed)
          records <- Journey(c)
            .drillDown(study.plan, study.result, study.input, study.ledger)
            .left
            .map(failed)
        yield Json.obj(
          "outcome" -> Json.fromString("completed"),
          "route"   -> Json.fromString(c.name),
          "run"     -> Json.fromString(render(run)),
          "input"   -> Json.fromString(study.input.reference.digest),
          "keys"    -> Json.arr(keys*),
          "late"    -> strings(
            study.input.trials.rows
              .filter(_.key == c.key("s2", "c", "recall"))
              .flatMap(_.value.fixations.toVector)
              .map(_.span.onset.toMicros.toString)
          ),
          "archived_bits" -> strings(render(archived)),
          "rerun_bits"    -> strings(render(again)),
          "rerun_sha256"  -> Json.fromString(encoded.entry.sha256.hex),
          "failures"      -> strings(view.failures.map(_.code.render)),
          "steps"         -> Json.fromLong(last.step),
          "total_units"   -> Json.fromLong(last.totalUnits),
          "records"       -> Json.arr(records.map(Json.fromInt)*)
        )
        receipt.merge
      case Right(other) => failed(other)
      case Left(error)  => failed(error)
    }
