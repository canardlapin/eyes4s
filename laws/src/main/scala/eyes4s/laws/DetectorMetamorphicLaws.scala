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

package eyes4s.laws

import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Drive one machine across arbitrary chunks while retaining its hidden state
  * and flushing exactly once after the final chunk.
  */
object MachineChunking:
  def run[A, B](machine: Machine[A, B], chunks: Iterable[Iterable[A]]): Vector[B] =
    val stable = machine
    var state  = stable.detector.init
    val output = Vector.newBuilder[B]
    chunks.foreach { chunk =>
      chunk.foreach { value =>
        val (next, emitted) = stable.detector.step(state, value)
        state = next
        output ++= emitted
      }
    }
    output ++= stable.detector.flush(state)
    output.result()

/** Metamorphic laws for a dispersion-threshold detector.
  *
  * The fixture contains two valid stationary runs, invalid separators, and a
  * deliberately sub-duration tracked run. Its within-fixation spread is close
  * enough to the threshold that forgetting to scale the threshold changes the
  * result; the short run is long enough after time dilation that forgetting to
  * scale the duration also changes the result. These guards keep the spatial
  * and temporal laws non-vacuous.
  */
trait DetectorMetamorphicLaws extends Laws:

  type IdtFactory =
    (Extent[Deg], MinimumEventDuration, ClockId) => EventDetector[Deg]

  private final case class FixationSummary(
      onsetMicros: Long,
      offsetMicros: Long,
      centre: Pt[Deg],
      dispersion: Double
  )

  private val clock = ClockId("metamorphic-tracker")

  /** Translation and timing are exact; transformed floating-point event
    * summaries use the caller's explicit tolerance.
    */
  def idt(
      factory: IdtFactory,
      tolerance: Tolerance = Tolerance.exactish
  ): RuleSet =
    new SimpleRuleSet(
      "detector.idt.metamorphic",
      "joint translation preserves timing and translates centres" ->
        forAll(Gen.choose(-50.0, 50.0), Gen.choose(-50.0, 50.0)) { (dx, dy) =>
          compareTransformed(factory, translated(_, dx, dy), identity, tolerance) { summary =>
            Pt[Deg](summary.centre.x + dx, summary.centre.y + dy)
          }
        },
      "rotation preserves timing and rotates centres" ->
        forAll(Gen.choose(-math.Pi, math.Pi)) { angle =>
          val cosine = math.cos(angle)
          val sine   = math.sin(angle)
          compareTransformed(
            factory,
            rotated(_, cosine, sine),
            identity,
            tolerance
          ) { summary => rotate(summary.centre, cosine, sine) }
        },
      "coherent spatial scaling preserves timing and scales summaries" ->
        forAll(Gen.choose(0.25, 4.0)) { factor =>
          val result = for
            baseExtent   <- Extent.square[Deg](1.0).toOption
            scaledExtent <- Extent
              .of[Deg](baseExtent.width * factor, baseExtent.height * factor)
              .toOption
            minimum <- MinimumEventDuration.of(Span.millis(5)).toOption
            base    <- summaries(factory(baseExtent, minimum, clock).runAll(referenceInput))
            scaled  <- summaries(
              factory(scaledExtent, minimum, clock).runAll(
                referenceInput.map(scaled(_, factor))
              )
            )
          yield sameCardinality(base, scaled) && base.zip(scaled).forall {
            case (before, after) =>
              sameTiming(before, after) &&
              tolerance.approxEquals(after.centre.x, before.centre.x * factor) &&
              tolerance.approxEquals(after.centre.y, before.centre.y * factor) &&
              tolerance.approxEquals(after.dispersion, before.dispersion * factor)
          }
          Prop(result.contains(true)) :| clue("spatial scaling", result)
        },
      "coherent time scaling preserves support indices and scales event time" ->
        forAll(Gen.choose(2, 5)) { factor =>
          val result = for
            extent        <- Extent.square[Deg](1.0).toOption
            minimum       <- MinimumEventDuration.of(Span.millis(5)).toOption
            scaledMinimum <- MinimumEventDuration
              .of(Span.micros(minimum.span.toMicros * factor.toLong))
              .toOption
            base   <- summaries(factory(extent, minimum, clock).runAll(referenceInput))
            scaled <- summaries(
              factory(extent, scaledMinimum, clock)
                .runAll(referenceInput.map(timeScaled(_, factor)))
            )
          yield sameCardinality(base, scaled) && base.zip(scaled).forall {
            case (before, after) =>
              after.onsetMicros == before.onsetMicros * factor.toLong &&
              after.offsetMicros == before.offsetMicros * factor.toLong &&
              tolerance.approxEquals(after.centre, before.centre) &&
              tolerance.approxEquals(after.dispersion, before.dispersion)
          }
          Prop(result.contains(true)) :| clue("time scaling", result)
        },
      "arbitrary chunk boundaries equal whole-record execution" ->
        forAll(Gen.listOfN(24, Gen.choose(0, 8))) { plan =>
          val result = for
            extent  <- Extent.square[Deg](1.0).toOption
            minimum <- MinimumEventDuration.of(Span.millis(5)).toOption
          yield
            val machine = factory(extent, minimum, clock).machine
            val whole   = machine.runAll(referenceInput)
            val chunked = MachineChunking.run(machine, chunks(referenceInput, plan))
            whole == chunked
          Prop(result.contains(true)) :| clue("chunk plan", plan)
        }
    )

  /** Deterministic fixture shared by the JVM and JavaScript court. */
  private[laws] def referenceInput: Vector[Sample[Deg]] =
    Vector.tabulate(30) { index =>
      val gaze: Gaze[Deg] =
        if index == 12 || index == 16 then Gaze.Lost()
        else
          val baseline =
            if index < 12 then 0.0
            else if index < 16 then 2.0
            else 6.0
          Gaze.Tracked(Pt[Deg](baseline + spread(index), 0.0), None)
      Sample(Instant.millis(index.toLong), gaze)
    }

  private def spread(index: Int): Double =
    Vector(-0.4, -0.2, 0.0, 0.2, 0.4)(index % 5)

  private def compareTransformed(
      factory: IdtFactory,
      transform: Sample[Deg] => Sample[Deg],
      transformExtent: Extent[Deg] => Extent[Deg],
      tolerance: Tolerance
  )(
      expectedCentre: FixationSummary => Pt[Deg]
  ): Prop =
    val result = for
      extent  <- Extent.square[Deg](1.0).toOption
      minimum <- MinimumEventDuration.of(Span.millis(5)).toOption
      base    <- summaries(factory(extent, minimum, clock).runAll(referenceInput))
      changed <- summaries(
        factory(transformExtent(extent), minimum, clock).runAll(referenceInput.map(transform))
      )
    yield sameCardinality(base, changed) && base.zip(changed).forall { case (before, after) =>
      sameTiming(before, after) && tolerance.approxEquals(
        after.centre,
        expectedCentre(before)
      )
    }
    Prop(result.contains(true)) :| clue("transformed summaries", result)

  private def summaries(
      emissions: Vector[DetectionEmission[Deg]]
  ): Option[Vector[FixationSummary]] =
    if emissions.exists(_.isLeft) then None
    else
      val events = emissions.collect { case Right(fixation: Event.Fixation[Deg]) => fixation }
      Option.when(events.nonEmpty && events.length == emissions.length)(
        events.map { event =>
          FixationSummary(
            event.span.onset.toMicros,
            event.span.offset.toMicros,
            event.centre,
            event.dispersion.fold(0.0)(_.value)
          )
        }
      )

  private def sameCardinality(
      left: Vector[FixationSummary],
      right: Vector[FixationSummary]
  ): Boolean = left.nonEmpty && left.length == right.length

  private def sameTiming(left: FixationSummary, right: FixationSummary): Boolean =
    left.onsetMicros == right.onsetMicros && left.offsetMicros == right.offsetMicros

  private def translated(sample: Sample[Deg], dx: Double, dy: Double): Sample[Deg] =
    sample.copy(gaze = mapPositions(sample.gaze)(point => Pt(point.x + dx, point.y + dy)))

  private def rotated(sample: Sample[Deg], cosine: Double, sine: Double): Sample[Deg] =
    sample.copy(gaze = mapPositions(sample.gaze)(rotate(_, cosine, sine)))

  private def rotate(point: Pt[Deg], cosine: Double, sine: Double): Pt[Deg] =
    Pt(point.x * cosine - point.y * sine, point.x * sine + point.y * cosine)

  private def scaled(sample: Sample[Deg], factor: Double): Sample[Deg] =
    sample.copy(gaze =
      mapPositions(sample.gaze)(point => Pt(point.x * factor, point.y * factor))
    )

  private def timeScaled(sample: Sample[Deg], factor: Int): Sample[Deg] =
    sample.copy(t = Instant.micros(sample.t.toMicros * factor.toLong))

  private def mapPositions(gaze: Gaze[Deg])(f: Pt[Deg] => Pt[Deg]): Gaze[Deg] = gaze match
    case Gaze.Tracked(point, pupil) => Gaze.Tracked(f(point), pupil)
    case Gaze.OffScreen(point)      => Gaze.OffScreen(f(point))
    case Gaze.Blink()               => Gaze.Blink()
    case Gaze.Lost()                => Gaze.Lost()

  private def chunks[A](input: Vector[A], plan: List[Int]): Vector[Vector[A]] =
    val output    = Vector.newBuilder[Vector[A]]
    var remaining = input
    plan.foreach { requested =>
      val count = math.min(math.max(requested, 0), remaining.length)
      output += remaining.take(count)
      remaining = remaining.drop(count)
    }
    if remaining.nonEmpty then output += remaining
    output.result()

  private def clue[A](name: String, value: A): String = s"$name: $value"

end DetectorMetamorphicLaws

object DetectorMetamorphicLaws extends DetectorMetamorphicLaws
