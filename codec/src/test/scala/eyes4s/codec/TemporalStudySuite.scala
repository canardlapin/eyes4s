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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import io.circe.Json

class TemporalStudySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val OracleTolerance                   = 1e-12
  private val frame                             = get(Frame.screen("temporal", 2, 2))
  private val grid                              = get(Grid.over(frame, 2, 2))
  private def key(label: String): StudyKey      =
    val pieces = label.split("/")
    StudyKey(pieces(0), pieces(1), pieces(2))
  private def label(k: StudyKey): String  = s"${k.participant}/${k.stimulus}/${k.phase}"
  private def clock(k: StudyKey): ClockId = ClockId(
    s"fixation-trial:${KeyDigest[StudyKey].digest(k).render}"
  )
  private def interval(k: StudyKey, start: Long, end: Long) = get(
    Interval.of(clock(k), Instant.micros(start), Instant.micros(end))
  )
  private val trials = TemporalFixtures.csv.linesIterator
    .drop(1)
    .map(_.split(",").toVector)
    .toVector
    .groupBy(row => StudyKey(row(0), row(1), row(2)))
    .toVector
    .sortBy(_._1)
    .map { case (k, rows) =>
      val fixes = rows
        .sortBy(_(3).toInt)
        .map(row =>
          get(
            Event.Fixation.withoutDispersion(
              interval(k, row(6).toLong, row(6).toLong + row(7).toLong),
              Pt[Px](row(4).toDouble, row(5).toDouble),
              row(8).toInt
            )
          )
        )
      Trial(k, (), get(Scanpath.of(frame, clock(k), IArray.from(fixes))))
    }
  private val study  = StudyInput(Trials(trials))
  private val epochs = TemporalFixtures.coverage.toVector.map { case (name, spans) =>
    val k = key(name)
    k -> TrialEpoch(
      Instant.micros(0),
      get(ObservedCoverage.of(clock(k), spans.map { case (a, b) => interval(k, a, b) }))
    )
  }
  private val input = get(TemporalStudyInput.of(study, epochs))
  private val base  = get(
    StudyPlan.cosine(
      study.reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(
        StudyEstimate.Binned(),
        StudyEstimate.Gaussian(get(Sigma.px(1)), EdgePolicy.Truncate)
      ),
      FailurePolicy.RequireAll
    )
  )
  private val windows = TemporalFixtures.windows.map { case (name, a, b) =>
    get(StudyWindow.of(name, get(Window.of(Span.micros(a), Span.micros(b)))))
  }
  private val repeats = TemporalFixtures.repetitions.map { case (name, f, r) =>
    get(RepetitionContrast.withinParticipant(name, f, r))
  }
  private def plan(
      available: TemporalStudyInput[StudyKey, Px] = input,
      boundary: FixationBoundary = FixationBoundary.ClipDuration
  ) =
    get(TemporalStudyPlan.of(base, available.reference, windows, repeats, boundary))
  private val persistence = new TemporalStudyCodec(
    get(DefinitionId.of("eyes4s.temporal-study", 1)),
    StudyCodecs.cosine[Px]
  )

  test(
    "saved temporal repetition study matches all independent contrasts and coverage ledgers"
  ) {
    val original = plan()
    val decoded = get(persistence.codec.parse(get(persistence.codec.encode(original)).noSpaces))
    assertEquals(original.diff(decoded), Vector.empty)
    val direct = get(original.run(input))
    val result = get(decoded.run(input))
    assertEquals(result.cells.size, 8)
    val actual = result.cells
      .flatMap(cell =>
        cell.result.scales.flatMap(scale =>
          get(scale.contrast).rows.map(row =>
            (
              cell.repetition.name,
              label(row.key),
              cell.window.name,
              scale.estimate
            ) -> row.difference.toOption.map(_.value)
          )
        )
      )
      .toMap
    assertEquals(actual.size, 96)
    TemporalFixtures.targets.foreach { t =>
      val scale = t.sigma.fold[StudyEstimate[Px]](StudyEstimate.Binned())(s =>
        StudyEstimate.Gaussian(get(Sigma.px(s)), EdgePolicy.Truncate)
      )
      val value = actual((t.repetition, t.key, t.window, scale))
      (value, t.difference) match
        case (Some(a), Some(b)) =>
          assert(math.abs(a - b) <= OracleTolerance, s"${t.key}/${t.window}: $a != $b")
        case _ => assertEquals(value, t.difference)
    }
    result.cells.filter(_.window.name == "early").foreach { cell =>
      val contrast    = get(cell.result.scales.head.contrast)
      val actualPairs =
        contrast.matched.source.rows.map(r => (r.left.toString, r.right.toString, "matched")) ++
          contrast.control.source.rows.map(r => (r.left.toString, r.right.toString, "control"))
      val expectedPairs = TemporalFixtures.pairs.collect {
        case (repeat, left, right, kind) if repeat == cell.repetition.name =>
          (key(left).toString, key(right).toString, kind)
      }
      assertEquals(actualPairs.toSet, expectedPairs.toSet)
    }
    result.cells.zip(direct.cells).foreach { case (cell, before) =>
      assertEquals(
        cell.result.scales.map(s => get(s.contrast).rows.map(_.difference)),
        before.result.scales.map(s => get(s.contrast).rows.map(_.difference))
      )
      cell.occupancy.foreach { case (k, outcome) =>
        val (_, _, retained, observed, missing) = TemporalFixtures.ledgers
          .find(t => t._1 == label(k) && t._2 == cell.window.name)
          .getOrElse(fail(label(k)))
        val value = get(outcome)
        assertEquals(value.fixationTimes.map(_.retainedMicros), retained)
        assertEquals(value.observedMicros, observed)
        assertEquals(value.missingMicros, missing)
      }
      cell.result.scales.foreach(s =>
        get(s.contrast).rows.foreach { row =>
          assertEquals(row.matched.map(_.selected), Some(1))
          assertEquals(row.control.map(_.selected), Some(2))
        }
      )
    }
  }
  test(
    "missing epoch and wrong clock fail each affected comparison without dropping focal keys"
  ) {
    val incomplete =
      get(TemporalStudyInput.of(study, epochs.filterNot(_._1 == StudyKey("s1", "a", "recall"))))
    val results = get(plan(incomplete).run(incomplete))
    assert(results.cells.forall(_.occupancy.exists { case (k, v) =>
      k == StudyKey("s1", "a", "recall") && v.isLeft
    }))
    assert(results.cells.forall(_.result.scales.forall(s => get(s.contrast).rows.size == 6)))
    val wrong = epochs.map { case (k, e) =>
      k -> TrialEpoch(e.anchor, get(ObservedCoverage.of(ClockId("wrong"), Vector.empty)))
    }
    val mismatched = get(TemporalStudyInput.of(study, wrong))
    assert(get(plan(mismatched).run(mismatched)).cells.forall(_.occupancy.forall(_._2.isLeft)))
  }
  test(
    "coverage and anchors participate in identity and boundary choices produce a meaningful diff"
  ) {
    assert(plan().prerequisites(None).nonEmpty)
    val changed = get(
      TemporalStudyInput.of(
        study,
        epochs.map { case (k, e) => k -> TrialEpoch(Instant.micros(1), e.coverage) }
      )
    )
    assert(plan().run(changed).isLeft)
    assertEquals(
      plan().diff(plan(boundary = FixationBoundary.FullyContained)).map(_.field),
      Vector("temporal.boundary")
    )
    assert(TemporalStudyInput.of(study, epochs ++ epochs.take(1)).isLeft)
    assert(TemporalStudyInput.of(StudyInput(Trials(trials ++ trials.take(1))), epochs).isLeft)
  }
  test(
    "temporal schema rejects unknown scope, invalid boundaries and lossy microsecond encodings"
  ) {
    val json                                    = get(persistence.codec.encode(plan()))
    def change(name: String, value: Json): Json = json.mapObject(
      _.add("value", get(json.hcursor.get[Json]("value")).mapObject(_.add(name, value)))
    )
    assert(
      persistence.codec.decode(change("scope", Json.fromString("acrossParticipants"))).isLeft
    )
    assert(persistence.codec.decode(change("boundary", Json.fromString("ignore"))).isLeft)
    assert(
      persistence.codec
        .decode(
          change(
            "windows",
            Json.arr(
              Json.obj(
                "name"        -> Json.fromString("bad"),
                "fromMicros"  -> Json.fromDoubleOrNull(0),
                "untilMicros" -> Json.fromString("10")
              )
            )
          )
        )
        .isLeft
    )
    val window =
      get(StudyWindow.of("overflow", get(Window.of(Span.micros(0), Span.micros(10)))))
    assert(
      window.resolve(TrialEpoch(Instant.micros(Long.MaxValue), epochs.head._2.coverage)).isLeft
    )
    assert(
      TemporalStudyPlan
        .of(
          base,
          input.reference,
          windows ++ windows.take(1),
          repeats,
          FixationBoundary.ClipDuration
        )
        .isLeft
    )
  }
