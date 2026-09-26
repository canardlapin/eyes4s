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

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

/** Every registered map method on both key routes: the participant/stimulus/
  * phase route (`ArtifactDecoders.study`) and the trial-keyed route
  * (`ArtifactDecoders.trial`). A plan and its completed result, saved
  * through the method's own codecs, resolve through the route's registry and
  * re-encode to the same bytes; a document of one route is refused by the
  * other; and the cosine-only `trialCosine` still writes exactly what the
  * general trial route writes.
  */
class KeyRouteSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)

  private val frame = get(Frame.screen("route-display", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))

  private def scanpath(label: String, points: (Double, Double)*): Scanpath[Px] =
    val clock = ClockId(label)
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 2000L), Instant.micros(i * 2000L + 1000L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    get(Scanpath.of(frame, clock, IArray.from(fixes)))

  /** Two items per participant, each viewed at encoding and recalled once. */
  private val paths: Vector[(String, String, String, Scanpath[Px])] = Vector(
    ("p1", "a", "recall", scanpath("p1a-r", (0.5, 0.5), (1.5, 0.5), (0.5, 1.5))),
    ("p1", "b", "recall", scanpath("p1b-r", (1.5, 1.5), (0.5, 1.5))),
    ("p1", "a", "encode", scanpath("p1a-e", (0.5, 0.5), (1.5, 0.5))),
    ("p1", "b", "encode", scanpath("p1b-e", (1.5, 1.5), (1.5, 0.5), (0.5, 0.5))),
    ("p2", "a", "recall", scanpath("p2a-r", (1.5, 0.5), (0.5, 0.5))),
    ("p2", "b", "recall", scanpath("p2b-r", (0.5, 1.5))),
    ("p2", "a", "encode", scanpath("p2a-e", (1.5, 0.5), (1.5, 1.5))),
    ("p2", "b", "encode", scanpath("p2b-e", (0.5, 1.5), (0.5, 0.5)))
  )

  private val studyInput: StudyInput[StudyKey, Px] = StudyInput(
    Trials(paths.map((p, s, phase, path) => Trial(StudyKey(p, s, phase), (), path)))
  )
  private val trialInput: StudyInput[TrialKey, Px] = StudyInput(
    Trials(paths.zipWithIndex.map { case ((p, s, phase, path), i) =>
      Trial(get(TrialKey.of(p, phase, s"t$i", get(TrialOccurrence.of(1)), s)), (), path)
    })
  )

  private def plan[K](
      input: StudyInput[K, Px],
      layout: StudyLayout[K],
      method: ComparisonMethod
  ): StudyPlan[K, Px, Unit, Similarity, SignedDifference] =
    get(
      StudyPlan.configure(
        input.reference,
        layout,
        StudyGeometry.WholeFrame(grid),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyScale.Native(StudyEstimate.Binned())),
        None,
        FailurePolicy.RequireAll,
        method.study[Px],
        (),
        StudyPairing.version1
      )
    )

  private val studies = get(ArtifactDecoders.study[Px])
  private val trials  = get(ArtifactDecoders.trial[Px])

  private def roundTrip[K](
      route: ArtifactDecoders[K, Px],
      plans: StudyCodec[K, Px, Unit, Similarity, SignedDifference],
      results: StudyResultCodec[K, Px, Unit, Similarity, SignedDifference],
      input: StudyInput[K, Px],
      method: ComparisonMethod
  ): (Json, Json) =
    val p        = plan(input, plans.layout, method)
    val result   = get(p.run(input))
    val saved    = get(plans.codec.encode(p))
    val loaded   = get(route.plan(saved))
    val archived = get(results.codec.encode(result))
    val reloaded = get(route.result(archived))
    val clue     = s"${method.id} on ${plans.keys.schema}"
    assertEquals(loaded.plan.method.id, method.id, clue)
    assertEquals(loaded.description, p.description, clue)
    assertEquals(get(loaded.encode), saved, clue)
    assertEquals(reloaded.result.description, result.description, clue)
    assertEquals(get(reloaded.encode), archived, clue)
    (saved, archived)

  test("every registered method round-trips plan and result on both key routes") {
    assertEquals(ComparisonMethods.all.size, MapSimilarityMethod.values.length)
    ComparisonMethods.all.foreach { method =>
      val (studyPlan, studyResult) = roundTrip(
        studies,
        StudyCodecs.similarity[Px](method),
        StudyResultCodecs.registered[Px](method),
        studyInput,
        method
      )
      val (trialPlan, trialResult) = roundTrip(
        trials,
        StudyCodecs.trialSimilarity[Px](method),
        StudyResultCodecs.trialRegistered[Px](method),
        trialInput,
        method
      )
      // A document of one key route is refused by the other route's registry.
      assert(trials.plan(studyPlan).isLeft, s"${method.id} study plan read as trial plan")
      assert(studies.plan(trialPlan).isLeft, s"${method.id} trial plan read as study plan")
      assert(trials.result(studyResult).isLeft, s"${method.id} study result on trial route")
      assert(studies.result(trialResult).isLeft, s"${method.id} trial result on study route")
    }
  }

  test("a trial plan is read only by its own method's codec") {
    ComparisonMethods.all.foreach { method =>
      val written = get(
        StudyCodecs
          .trialSimilarity[Px](method)
          .codec
          .encode(plan(trialInput, TrialKey.layout(TrialKeyDefinitions.trialLayout), method))
      )
      ComparisonMethods.all.foreach { other =>
        val read = StudyCodecs.trialSimilarity[Px](other).codec.decode(written)
        assertEquals(read.isRight, other eq method, s"${method.id} read as ${other.id}")
      }
    }
  }

  test("trialCosine writes exactly what the general trial route writes for cosine") {
    val cosine  = ComparisonMethods.cosine
    val p       = StudyV2Fixtures.trialPlan
    val pinned  = get(StudyCodecs.trialCosine[Px].codec.encode(p))
    val general = get(StudyCodecs.trialSimilarity[Px](cosine).codec.encode(p))
    assertEquals(general, pinned)
    assertEquals(get(get(trials.plan(pinned)).encode), pinned)
    val mirror = get(io.circe.parser.parse(StudyV2Mirrors.trialStudyVersionTwo))
    assertEquals(get(get(trials.plan(mirror)).encode), mirror)
    val result = get(plan(trialInput, p.layout, cosine).run(trialInput))
    assertEquals(
      get(StudyResultCodecs.trialRegistered[Px](cosine).codec.encode(result)),
      get(
        StudyCodecs
          .trialCosine[Px]
          .results(
            StudyResultCodecs.similarity(),
            StudyResultCodecs.signedDifference()
          )
          .codec
          .encode(result)
      )
    )
  }
