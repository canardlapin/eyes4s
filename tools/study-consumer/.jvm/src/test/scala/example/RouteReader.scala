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
import eyes4s.codec.*
import eyes4s.codec.CodecDiagnostics.given
import eyes4s.compare.{ComparisonBudget, ComparisonQuantum}
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.fs2.*
import eyes4s.io.ArtifactFiles
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg
import eyes4s.plan.*
import io.circe.Json

import java.nio.file.{Files, Path, Paths}

/** The fresh reader of the recording and temporal routes: a separate JVM,
  * started over the consumer's test classpath, that knows nothing but a route
  * name and a directory. It makes the route's registrations from nothing,
  * reads the manifest address from the directory, resolves the saved run
  * through `ArtifactFiles`, reruns it through the FS2 runner and prints one
  * receipt: the address, the run id, the fingerprints of the archived and the
  * rerun result, the rerun's canonical archive digest and the reconstructed
  * identities; or each refusal's code and entry.
  */
object RouteReader extends IOApp:
  val marker = "EYES4S_ROUTE_READER="

  /** Budgets and quanta the application declares; the defaults are unbounded. */
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalStateException(s"reader setup: $error"), identity)
  private val pairs        = get(PairScheduleBudget.of(64, 512, 64))
  private val comparison   = get(ComparisonBudget.of(4))
  private val studyQuanta  = WorkQuanta(get(PairQuantum.of(4)), get(ComparisonQuantum.of(2)))
  private val sampleQuanta =
    WorkQuanta(PairQuantum.default, ComparisonQuantum.default, get(SampleQuantum.of(2)))

  /** The registrations: the shipped I-VT and the laboratory detector, with the
    * parameters the application offers, and the two temporal routes.
    */
  private def ivt = RecordingRoute.ivt(
    get(IvtThreshold.of(get(Velocity.perSecond[Deg](2000.0)))),
    get(MinimumEventDuration.of(Span.micros(2000)))
  )
  private def lab = RecordingRoute.lab(get(LabIvtParameters.of(2000.0, 2000L)))

  def run(arguments: List[String]): IO[ExitCode] = arguments match
    case List(route, directory) =>
      val path    = Paths.get(directory)
      val receipt = route match
        case "recording-ivt"   => recording(route, get(ivt), path)
        case "recording-lab"   => recording(route, get(lab), path)
        case "temporal-cosine" =>
          temporal(
            route,
            get(TemporalRoute.of(JourneyCases.cosine.route(), "eyes4s.temporal-study")),
            path
          )
        case "temporal-scaled" =>
          temporal(
            route,
            get(TemporalRoute.of(JourneyCases.scaled.route(), "my.lab.temporal-study")),
            path
          )
        case other => IO.pure(Json.obj("outcome" -> Json.fromString(s"unknown route $other")))
      receipt
        .map(_.deepMerge(Json.obj("process" -> Json.fromLong(ProcessHandle.current.pid))))
        .flatMap(json => IO.println(marker + json.noSpaces))
        .as(ExitCode.Success)
    case _ => IO.println("usage: RouteReader <route> <directory>").as(ExitCode(2))

  private def strings(values: Vector[String]): Json = Json.arr(values.map(Json.fromString)*)

  private def hex(bits: Vector[Long]): Vector[String] = bits.map(java.lang.Long.toHexString)

  private def refused(errors: NonEmptyVector[ResolveError]): Json =
    Json.obj(
      "outcome" -> Json.fromString("refused"),
      "errors"  -> Json.arr(errors.toVector.map { error =>
        val coded = Diagnostic.of(error)
        Json.obj(
          "code"   -> Json.fromString(coded.code.render),
          "causes" -> strings(coded.causes.map(_.code.render)),
          "entry"  -> Json.fromString(error.entryName.fold("")(_.value))
        )
      }*)
    )

  /** What can stop the reader after resolution, kept typed until printed. */
  private type Failure = ByteDigestError | JourneyError | CodecError | String |
    RunOutcome[?, ?, ?, ?, ?]

  private def failed(error: Failure): Json =
    Json.obj("outcome" -> Json.fromString("failed"), "error" -> Json.fromString(error.toString))

  private def address(directory: Path): IO[Either[ByteDigestError, ByteDigest]] =
    IO.blocking(Files.readString(directory.resolve(SavedRun.addressFile)).trim)
      .map(ByteDigest.parse)

  private def files(directory: Path) =
    ArtifactFiles.directory[IO](directory, SavedRun.manifestFile)

  private def recording[P](name: String, route: RecordingRoute[P], directory: Path): IO[Json] =
    address(directory).flatMap {
      case Left(error) => IO.pure(failed(error))
      case Right(at)   =>
        RecordingJourney
          .load(route, at, files(directory))
          .flatMap {
            case Left(JourneyError.Resolve(errors)) => IO.pure(refused(errors))
            case Left(error)                        => IO.pure(failed(error))
            case Right(run)                         =>
              RecordingJourney.rerun(run, sampleQuanta).map {
                case Right(RunOutcome.Completed(id, last, analysis)) =>
                  val encoded = StoredArtifact.recordingResult(
                    RecordingJourney.resultEntry,
                    route.results,
                    analysis
                  )
                  encoded.fold(
                    failed,
                    stored =>
                      Json.obj(
                        "outcome"       -> Json.fromString("completed"),
                        "route"         -> Json.fromString(name),
                        "run"           -> Json.fromString(id.toString),
                        "input"         -> Json.fromString(run.input.reference.digest),
                        "recording"     -> Json.fromString(run.recording.contentHash.render),
                        "archived_bits" -> strings(
                          hex(RecordingJourney.fingerprint(run.analysis))
                        ),
                        "rerun_bits"   -> strings(hex(RecordingJourney.fingerprint(analysis))),
                        "rerun_sha256" -> Json.fromString(stored.entry.sha256.hex),
                        "steps"        -> Json.fromLong(last.step),
                        "total_units"  -> Json.fromLong(last.totalUnits),
                        "events"       -> Json.arr(
                          analysis.detection.eventSeries.support
                            .map(r => Json.arr(Json.fromInt(r.from), Json.fromInt(r.until)))*
                        )
                      )
                  )
                case Right(other) => failed(other)
                case Left(error)  => failed(error)
              }
          }
          .map(_.deepMerge(Json.obj("address" -> Json.fromString(at.hex))))
    }

  private def temporal[K, P, S, D](
      name: String,
      route: TemporalRoute[K, P, S, D],
      directory: Path
  ): IO[Json] =
    def rendered(bits: Vector[Option[Long]]) = bits.map(_.fold("-")(java.lang.Long.toHexString))
    address(directory).flatMap {
      case Left(error) => IO.pure(failed(error))
      case Right(at)   =>
        TemporalJourney
          .load(route, at, files(directory))
          .flatMap {
            case Left(JourneyError.Resolve(errors)) => IO.pure(refused(errors))
            case Left(error)                        => IO.pure(failed(error))
            case Right(run)                         =>
              TemporalJourney.rerun(run, pairs, comparison, studyQuanta).map {
                case Right(RunOutcome.Completed(id, last, result)) =>
                  // Every refusal below is kept as its typed value's rendering.
                  val receipt: Either[String, Json] = for
                    archived <- TemporalJourney.fingerprint(run.result).left.map(_.toString)
                    again    <- TemporalJourney.fingerprint(result).left.map(_.toString)
                    stored   <- StoredArtifact
                      .temporalResult(TemporalJourney.resultEntry, route.results, result)
                      .left
                      .map(_.toString)
                    schema  <- ScoreSchema.study(run.plan.base).left.map(_.toString)
                    sources <- StudySources
                      .of(run.base, run.ledger)(using route.study.layout.digest)
                      .left
                      .map(_.toString)
                    view <- ResultInspection
                      .temporal(result, sources, schema)
                      .left
                      .map(_.toString)
                  yield Json.obj(
                    "outcome"       -> Json.fromString("completed"),
                    "route"         -> Json.fromString(name),
                    "run"           -> Json.fromString(id.toString),
                    "base"          -> Json.fromString(run.base.reference.digest),
                    "input"         -> Json.fromString(run.input.reference.digest),
                    "archived_bits" -> strings(rendered(archived)),
                    "rerun_bits"    -> strings(rendered(again)),
                    "rerun_sha256"  -> Json.fromString(stored.entry.sha256.hex),
                    "steps"         -> Json.fromLong(last.step),
                    "total_units"   -> Json.fromLong(last.totalUnits),
                    "failures"      -> strings(
                      view.cellNames.flatMap((r, w) =>
                        view
                          .cell(r, w)
                          .toOption
                          .toVector
                          .flatMap(_.study.failures.map(_.code.render))
                      )
                    )
                  )
                  receipt.fold(failed, identity)
                case Right(other) => failed(other)
                case Left(error)  => failed(error)
              }
          }
          .map(_.deepMerge(Json.obj("address" -> Json.fromString(at.hex))))
    }
