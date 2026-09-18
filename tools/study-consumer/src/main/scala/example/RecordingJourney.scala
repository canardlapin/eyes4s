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

import cats.effect.IO
import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.WorkQuanta
import eyes4s.fs2.*
import eyes4s.io.ArtifactLoading
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*

/** The steps of the recording journey an application repeats after a
  * recording has been analysed once: save the recording input, its packed
  * channels, the plan and the analysis under one manifest; reload them from
  * bytes alone; rerun the plan through the FS2 runner; and fingerprint the
  * analysis. Every step is a published eyes4s call.
  */
object RecordingJourney:

  /** Manifest entry names; the application chooses them. The packed
    * recording's four payloads are named `recording.tMicros`,
    * `recording.support`, `recording.lineage` and `recording.values`.
    */
  val inputEntry     = "recording-input.json"
  val recordingEntry = "recording"
  val planEntry      = "recording-plan.json"
  val resultEntry    = "recording-result.json"

  /** A saved recording run, typed again through the route's codecs. */
  final case class Reloaded[P](
      input: RecordingInput[Px],
      recording: Recording[Px],
      plan: RecordingPlan[P],
      analysis: RecordingAnalysis[P]
  )

  /** Save the recording input, its channels as a packed recording, the plan
    * and its completed analysis, with `RecordingOf`, `PayloadOf`,
    * `RecordingPlanInput` and `RecordingResultOf` relations.
    */
  def save[P](
      route: RecordingRoute[P],
      input: RecordingInput[Px],
      plan: RecordingPlan[P],
      analysis: RecordingAnalysis[P]
  ): Either[CodecError, SavedManifest] = for
    recording <- input.monocular.toRight(
      CodecError.Unsupported("recording-input", "a binocular input has no packed layout")
    )
    i <- StoredArtifact.recordingInput(inputEntry, input)
    r <- StoredArtifact.packedRecording(recordingEntry, recording)
    p <- StoredArtifact.recordingPlan(planEntry, route.persistence, plan)
    a <- StoredArtifact.recordingResult(resultEntry, route.results, analysis)
    s <- SavedManifest.of(
      Vector(i, r.recording) ++ r.payloads ++ Vector(p, a),
      (ManifestRelation.RecordingOf(i.name, r.recording.name) +: r.relations) ++ Vector(
        ManifestRelation.RecordingPlanInput(p.name, i.name),
        ManifestRelation.RecordingResultOf(a.name, p.name, i.name)
      )
    )
  yield s

  /** Resolve and verify a stored run from bytes, then type it again. */
  def resolve[P](
      route: RecordingRoute[P],
      address: ByteDigest,
      source: ByteSource
  ): Either[JourneyError, Reloaded[P]] = for
    decoders <- route.decoders.left.map(JourneyError.Codec.apply)
    resolved <- ArtifactResolver
      .resolve(address, source, decoders)
      .left
      .map(JourneyError.Resolve.apply)
    run <- reload(route, resolved)
  yield run

  /** The same over storage read in `IO`, such as `ArtifactFiles.directory`. */
  def load[P](
      route: RecordingRoute[P],
      address: ByteDigest,
      read: ByteRequest => IO[ArtifactLoading.Read]
  ): IO[Either[JourneyError, Reloaded[P]]] =
    route.decoders match
      case Left(error)     => IO.pure(Left(JourneyError.Codec(error)))
      case Right(decoders) =>
        ArtifactLoading
          .resolve(address, read, decoders)
          .map(_.left.map(JourneyError.Resolve.apply).flatMap(reload(route, _)))

  /** Type a verified graph again through the route's codecs: the resolver
    * keeps the detector's parameter type abstract, and the application knows
    * the registration it made.
    */
  def reload[P](
      route: RecordingRoute[P],
      resolved: ResolvedManifest[StudyKey, Px]
  ): Either[JourneyError, Reloaded[P]] =
    import SavedRun.single
    for
      input     <- single(ArtifactRole.RecordingInput, resolved.recordingInputs)
      loaded    <- single(ArtifactRole.RecordingPlan, resolved.recordingPlans)
      archived  <- single(ArtifactRole.RecordingResult, resolved.recordingResults)
      recording <- input.monocular.toRight(
        JourneyError.Recording(RecordingInputError.BinocularChannels(input.source))
      )
      plan <- loaded.encode
        .flatMap(route.persistence.codec.decode)
        .left
        .map(JourneyError.Codec.apply)
      analysis <- archived.encode
        .flatMap(route.results.codec.decode)
        .left
        .map(JourneyError.Codec.apply)
    yield Reloaded(input, recording, plan, analysis)

  /** Check the reloaded plan against its input's evidence, preflight it, and
    * run it on its own fiber; the outcome is the run's single authoritative
    * end.
    */
  def rerun[P](
      run: Reloaded[P],
      quanta: WorkQuanta
  ): IO[Either[JourneyError, RecordingOutcome[P]]] =
    RecordingInput.disagreements(run.input, run.plan).headOption match
      case Some(disagreement) => IO.pure(Left(JourneyError.Recording(disagreement)))
      case None               =>
        run.plan.preflight(Some(run.recording)).confirm(run.plan, run.recording) match
          case Left(error) => IO.pure(Left(JourneyError.Preflight(error)))
          case Right(())   =>
            RecordingExecution[IO]
              .start(run.plan, run.recording, quanta)
              .use(_.outcome)
              .map(Right(_))

  /** The numbers of an analysis in order, as raw IEEE 754 bits where they are
    * doubles: every angular and prepared sample (time, kind, position and
    * pupil), every event (span, kind and places), every event's sample
    * support and every sample's label. Equal fingerprints mean the same
    * trigonometric warp output, interpolation and detection, bit for bit.
    * The assignment and reports are compared structurally
    * (`RecordingResultEquivalence`) or by the canonical archive digest.
    */
  def fingerprint[P](analysis: RecordingAnalysis[P]): Vector[Long] =
    def bits(value: Double): Long     = java.lang.Double.doubleToRawLongBits(value)
    def point(p: Pt[?]): Vector[Long] = Vector(bits(p.x), bits(p.y))
    def samples(recording: Recording[Deg]): Vector[Long] =
      recording.samples.toVector.flatMap { sample =>
        sample.t.toMicros +: (sample.gaze match
          case Gaze.Tracked(at, pupil) => (0L +: point(at)) ++ pupil.map(bits).toVector
          case Gaze.OffScreen(at)      => 1L +: point(at)
          case Gaze.Blink()            => Vector(2L)
          case Gaze.Lost()             => Vector(3L))
      }
    def event(value: Event[Deg]): Vector[Long] =
      Vector(value.span.onset.toMicros, value.span.offset.toMicros) ++ (value match
        case f: Event.Fixation[Deg] => 0L +: point(f.centre)
        case s: Event.Saccade[Deg]  => (1L +: point(s.from)) ++ point(s.to)
        case _: Event.Blink[Deg]    => Vector(2L)
        case p: Event.Pursuit[Deg]  => 3L +: p.path.toVector.flatMap(point))
    val series = analysis.detection.eventSeries
    samples(analysis.angular) ++ samples(analysis.prepared) ++
      series.events.flatMap(event) ++
      series.support.flatMap(range => Vector(range.from.toLong, range.until.toLong)) ++
      analysis.detection.labels.toVector.map(_.ordinal.toLong)
