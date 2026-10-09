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
import eyes4s.design.*
import eyes4s.fs2.*
import eyes4s.io.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.StudyResultEquivalence
import eyes4s.plan.*

/** Mixed successes and per-trial failures through packaged artifacts only. */
class MixedFailureJourneySuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  private def journey[K, P, S, D](c: JourneyCase[K, P, S, D]): Unit =
    val j = Journey(c)
    import j.route
    Vector(
      FailurePolicy.RequireAll,
      get(FailurePolicy.successfulOnly(1)),
      get(FailurePolicy.successfulOnly(2))
    ).foreach { policy =>
      test(
        s"${c.name}: mixed failures retain denominators through save/reload/rerun ($policy)"
      ) {
        val imported = j.read(JourneyFixtures.mixed)
        val input    = get(imported.requireComplete)
        val ledger   =
          get(FixationEvidence.ledger("mixed.csv", imported, AdmissionDecision.RequireComplete))
        assertEquals(imported.rejected, Vector.empty)
        assertEquals(ledger.records.size, 45)
        assertEquals(input.trials.rows.size, 12)
        assertEquals(ledger.checkAgainst(input), Right(()))
        val seed = get(
          route.plan(
            input.reference,
            JourneySetup.grid,
            JourneySetup.phases,
            JourneySetup.weight,
            Vector(StudyEstimate.Binned[Px]()),
            policy
          )
        )
        val study = get(
          seed.revise(
            Vector(
              StudyChange.InitialFixations(
                seed.initialFixations,
                InitialFixationPolicy.dropFirst[Px]
              )
            )
          )
        )
        val work      = get(study.prepare(input, JourneySetup.pairs))
        val schema    = get(ScoreSchema.study(study))
        val failedKey = j.key("s1", "b", "encode")
        val failure   = StudyFailure.InitialFixations(
          failedKey,
          InitialFixationError.NoFixationKept(1, 400000L)
        )
        def check(result: StudyResult[K, Px, S, D]): Unit =
          assertEquals(result.scales.size, 1)
          val scale = result.scales.head
          assertEquals(
            scale.estimation.collect { case (key, Left(e)) => key -> e },
            Vector(failedKey -> failure)
          )
          val matched = scale.analyses.matchedSource.rows
          val control = scale.analyses.source(StudyDesign.Control).rows
          assertEquals(
            (matched.count(_.result.isRight), matched.count(_.result.isLeft)),
            (5, 1)
          )
          assertEquals(
            (control.count(_.result.isRight), control.count(_.result.isLeft)),
            (10, 2)
          )
          assertEquals(
            (matched ++ control).filter(_.result.isLeft).map(_.right).distinct,
            Vector(failedKey)
          )
          assert((matched ++ control).filter(_.result.isLeft).forall(_.result == Left(failure)))
          val partials   = Set(j.key("s1", "a", "recall"), j.key("s1", "c", "recall"))
          val minimumOne = policy == get(FailurePolicy.successfulOnly(1))
          scale.analyses.reduced(StudyDesign.Control).entries.foreach { row =>
            if partials.contains(row.key) then
              assertEquals(
                (row.selected, row.successful, row.failed, row.contributing),
                (2, 1, 1, if minimumOne then 1 else 0)
              )
              policy match
                case FailurePolicy.RequireAll =>
                  assertEquals(row.result, Left(ReductionError.FailedScores(row.key, 1, 1)))
                case FailurePolicy.SuccessfulOnly(minimum) if minimum.value == 2 =>
                  assertEquals(
                    row.result,
                    Left(ReductionError.InsufficientSuccessful(row.key, 2, 1, 1))
                  )
                case _ =>
                  val only =
                    control.find(pair => pair.left == row.key && pair.result.isRight).get
                  assertEquals(
                    schema.score(get(row.result)).components,
                    schema.score(get(only.result)).components
                  )
            else
              assertEquals(
                (row.selected, row.successful, row.failed, row.contributing),
                (2, 2, 0, 2)
              )
          }
          val failedMatched = scale.analyses
            .reduced(StudyDesign.Matched)
            .entries
            .find(_.key == j.key("s1", "b", "recall"))
            .get
          assertEquals(
            (
              failedMatched.selected,
              failedMatched.successful,
              failedMatched.failed,
              failedMatched.contributing
            ),
            (1, 0, 1, 0)
          )
          assertEquals(
            get(scale.contrast).rows.count(_.difference.isRight),
            if minimumOne then 5 else if policy == FailurePolicy.RequireAll then 3 else 0
          )
          assertEquals(
            Diagnostic.of(failure).code.render,
            "study-failure.initial-fixations"
          )
        StudyExecution[IO]
          .start(work, JourneySetup.comparison, JourneySetup.quanta)
          .use(_.outcome)
          .flatMap { outcome =>
            val result = j.completed(outcome)
            check(result)
            val saved = get(FixationJourney.save(route, study, input, ledger, result))
            val back  = get(j.reload(j.storage(saved)))
            assertEquals(back.input.reference, input.reference)
            assertEquals(back.ledger, ledger)
            assertEquals(back.plan.diff(study), Vector.empty)
            assertEquals(back.plan.policy, policy)
            check(back.result)
            assert(
              StudyResultEquivalence.same(result, back.result)(
                (a, b) => schema.score(a).components == schema.score(b).components,
                (a, b) => schema.difference(a).components == schema.difference(b).components
              )
            )
            FixationJourney
              .rerun(back, JourneySetup.pairs, JourneySetup.comparison, JourneySetup.quanta)
              .map { rerun =>
                val again = j.completed(get(rerun))
                check(again)
                assertEquals(
                  get(FixationJourney.fingerprint(study, again)),
                  get(FixationJourney.fingerprint(study, result))
                )
                assertEquals(
                  get(StoredArtifact.result("again", route.results, again)).entry.sha256,
                  get(StoredArtifact.result("before", route.results, result)).entry.sha256
                )
              }
          }
      }
    }

  journey(JourneyCases.cosine)
  journey(JourneyCases.scaled)
