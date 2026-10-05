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

package eyes4s.fs2

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** The shared runner over the repetition cursor: its events replay the pure
  * cursor's stages with each stage's exact pair total, and a completed run is
  * the plan's `run`.
  */
class RepetitionExecutionSuite extends munit.CatsEffectSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)
  private def id(name: String)              = get(DefinitionId.of(name, 1))

  final case class Key(person: Int, stimulus: String, repeat: Int) derives CanEqual
  object Key:
    given KeyDigest[Key] = KeyDigest.derived[Key]
    given Ordering[Key]  = Ordering.by(k => (k.person, k.stimulus, k.repeat))

  private val layout = get(
    RepetitionLayout.of[Key, Int, String, Int](
      id("example.repetition-layout"),
      id("example.person"),
      Projection.named("person")(_.person),
      id("example.stimulus"),
      Projection.named("stimulus")(_.stimulus),
      id("example.occasion"),
      Projection.named("repeat")(_.repeat)
    )
  )
  private val grid = get(Grid.over(get(Frame.screen("repetition-execution", 5, 3)), 5, 3))
  private val rows =
    for person <- Vector(1, 2); stimulus <- Vector("a", "b"); repeat <- Vector(0, 1, 2) yield
      val values = IArray.tabulate(15)(i =>
        if i == (person + repeat + (if stimulus == "a" then 0 else 5)) % 15 then 2.0 else 1.0
      )
      Trial(
        Key(person, stimulus, repeat),
        (),
        get(
          Surface
            .intensity(grid, values, Provenance.raw(ContentHash.of(values)))
            .flatMap(_.normalised)
        )
      )
  private def plan(selection: Selection, trials: Vector[Trial[Key, Unit, Mass[Px]]] = rows) =
    get(
      RepetitionPlan.of(
        layout,
        RepetitionRelations.withinParticipant,
        MapSimilarityMethod.Cosine,
        selection,
        FailurePolicy.RequireAll,
        grid,
        Trials(trials)
      )
    )
  private val sampled =
    plan(Selection.BottomK(get(PairLimit.of(2)), Seed(11L), SampleId("execution")))
  private def quanta(pairs: Int) =
    WorkQuanta(get(PairQuantum.of(pairs)), ComparisonQuantum.default)
  private val runner = RepetitionExecution[IO]

  /** The pure cursor's (stage, units) per step at `q`. */
  private def pure(p: RepetitionPlan[Key, Px], q: WorkQuanta): Vector[(RepetitionStage, Int)] =
    @annotation.tailrec
    def loop(
        c: RepetitionCursor[Key],
        seen: Vector[(RepetitionStage, Int)]
    ): Vector[(RepetitionStage, Int)] =
      get(c.advance(q)) match
        case WorkStep.More(stage, units, next) => loop(next, seen :+ (stage -> units))
        case WorkStep.Done(units, _)           => seen :+ (c.stage -> units)
    loop(p.work, Vector.empty)

  test("events replay the pure cursor's stages with each stage's exact total, ending in run") {
    // Without person 2's second stimulus, person 2 has matched pairs but no controls.
    val uneven =
      plan(Selection.All, rows.filterNot(t => t.key.person == 2 && t.key.stimulus == "b"))
    assertNotEquals(uneven.run.matched.rows.size, uneven.run.controls.rows.size)
    Vector(sampled, plan(Selection.All), uneven).traverse_ { p =>
      val expected = p.run
      Vector(quanta(1), quanta(4), WorkQuanta.default).traverse_ { q =>
        runner.events(p, q).compile.toVector.map { events =>
          val progress = events.collect { case RunEvent.Advanced(pr) => pr }
          assertEquals(progress.map(pr => (pr.stage, pr.stepUnits)), pure(p, q), q)
          assertEquals(progress.map(_.step), (1L to progress.size.toLong).toVector)
          assert(progress.forall(_.run == RepetitionRunId.of(p, q)))
          progress.foreach { pr =>
            val total = pr.stage match
              case RepetitionStage.Matched => expected.matched.rows.size
              case RepetitionStage.Control => expected.controls.rows.size
            assertEquals(pr.segment, pr.stage)
            assertEquals(pr.segmentTotal, SegmentTotal.Exact(total.toLong), pr)
          }
          // Each segment ends at its total.
          assertEquals(
            progress.groupBy(_.stage).view.mapValues(_.map(_.segmentUnits).max).toMap,
            Map(
              RepetitionStage.Matched -> expected.matched.rows.size.toLong,
              RepetitionStage.Control -> expected.controls.rows.size.toLong
            )
          )
          events.last match
            case RunEvent.Finished(RunOutcome.Completed(_, last, result)) =>
              assertEquals(last, progress.last)
              assertEquals(result.matched, expected.matched)
              assertEquals(result.controls, expected.controls)
            case other => fail(s"$other")
        }
      }
    }
  }

  test("a run id is a function of the plan's input and identity and the pair quantum") {
    assertEquals(RepetitionRunId.of(sampled, quanta(4)), RepetitionRunId.of(sampled, quanta(4)))
    assertNotEquals(
      RepetitionRunId.of(sampled, quanta(4)),
      RepetitionRunId.of(sampled, quanta(5))
    )
    assertNotEquals(
      RepetitionRunId.of(sampled, quanta(4)),
      RepetitionRunId.of(plan(Selection.All), quanta(4))
    )
    assertEquals(RepetitionRunId.of(sampled, quanta(4)).input, sampled.inputHash.render)
  }

  test("start settles Completed with run's result, and cancelling first settles Cancelled") {
    for
      done <- runner.start(sampled, quanta(1)).use(_.outcome)
      none <- runner.start(sampled, quanta(1), IO.never).use(r => r.cancel >> r.outcome)
    yield
      done match
        case RunOutcome.Completed(_, _, result) =>
          assertEquals(result.controls, sampled.run.controls)
        case other => fail(s"$other")
      assertEquals(none, RunOutcome.Cancelled(RepetitionRunId.of(sampled, quanta(1)), None))
  }
