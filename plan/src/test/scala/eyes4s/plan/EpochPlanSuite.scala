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

package eyes4s.plan

import cats.instances.string.*
import eyes4s.kernel.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class EpochPlanSuite extends munit.ScalaCheckSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private val clock                         = ClockId("epoch")
  private val budget                        = get(NonNegativeLong.of(1000))
  private def timeline(marks: (Long, String)*): ObservedTimeline[String] =
    ObservedTimeline.from(
      get(Timeline.of(clock, marks.toVector.map { case (t, k) => Mark(Instant.micros(t), k) }))
    )
  private def plan(
      policy: FinalBin,
      from: Long = -5,
      until: Long = 6,
      width: Long = 4,
      occurrence: Occurrence = Occurrence.RequireUnique
  ): EpochPlan[String] =
    get(
      EpochPlan.of(
        MarkSelector("start", occurrence),
        get(Window.of(Span.micros(from), Span.micros(until))),
        get(PositiveSpan.of(Span.micros(width))),
        policy
      )
    )
  private def pairs(bins: EpochBins): Vector[(Long, Long)] =
    bins.bins.map(i => (i.onset.toMicros, i.offset.toMicros))

  test("all final-bin policies preserve the exact half-open partition") {
    val marks = timeline(100L -> "start")
    val short = get(plan(FinalBin.IncludeShortFinal).resolve("trial", marks, clock, budget))
    assertEquals(pairs(short), Vector(95L -> 99L, 99L -> 103L, 103L -> 106L))
    assertEquals(short.excludedTail, None)
    val excluded = get(plan(FinalBin.ExcludeAndReport).resolve("trial", marks, clock, budget))
    assertEquals(pairs(excluded), Vector(95L -> 99L, 99L -> 103L))
    assertEquals(
      excluded.excludedTail.map(i => (i.onset.toMicros, i.offset.toMicros)),
      Some(103L -> 106L)
    )
    assert(plan(FinalBin.RequireExactDivision).resolve("trial", marks, clock, budget) match
      case Left(EpochError.NonDivisible("trial", _, _, 3L)) => true
      case _                                                => false)
  }

  test("stable occurrence selection, zero-based Nth, and named missing/ambiguous errors") {
    val marks = timeline(20L -> "start", 10L -> "other", 5L -> "start", 5L -> "start")
    Vector(
      Occurrence.First                           -> 5L,
      Occurrence.Last                            -> 20L,
      Occurrence.Nth(get(NonNegativeLong.of(1))) -> 5L
    ).foreach { case (o, expected) =>
      assertEquals(
        get(
          plan(FinalBin.IncludeShortFinal, occurrence = o)
            .resolve("trial", marks, clock, budget)
        ).anchor.toMicros,
        expected
      )
    }
    Vector(
      Occurrence.RequireUnique,
      Occurrence.Nth(get(NonNegativeLong.of(3))),
      Occurrence.Nth(get(NonNegativeLong.of(Long.MaxValue)))
    ).foreach { o =>
      val p = plan(FinalBin.IncludeShortFinal, occurrence = o)
      assertEquals(
        p.resolve("trial", marks, clock, budget).left.toOption,
        Some(EpochError.AnchorMatches("trial", p.anchor, 3))
      )
    }
    val p = plan(FinalBin.IncludeShortFinal)
    assertEquals(
      p.resolve("missing", timeline(0L -> "other"), clock, budget).left.toOption,
      Some(EpochError.AnchorMatches("missing", p.anchor, 0))
    )
  }

  test("empty and sub-bin windows have explicit coverage") {
    val marks = timeline(0L -> "start")
    FinalBin.values.foreach { policy =>
      val empty = get(plan(policy, 0, 0).resolve("t", marks, clock, budget))
      assertEquals(empty.bins, Vector.empty)
      assertEquals(empty.excludedTail, None)
      val exact = get(plan(policy, 0, 8).resolve("t", marks, clock, budget))
      assertEquals(pairs(exact), Vector(0L -> 4L, 4L -> 8L))
    }
    assertEquals(
      pairs(get(plan(FinalBin.IncludeShortFinal, 0, 1).resolve("t", marks, clock, budget))),
      Vector(0L -> 1L)
    )
    val dropped = get(plan(FinalBin.ExcludeAndReport, 0, 1).resolve("t", marks, clock, budget))
    assertEquals(dropped.bins, Vector.empty)
    assertEquals(dropped.excludedTail.map(_.duration.toMicros), Some(1L))
  }

  test("clock mismatch, anchor overflow and allocation limits are failures") {
    val p = plan(FinalBin.IncludeShortFinal)
    assert(p.resolve("t", timeline(0L -> "start"), ClockId("foreign"), budget).isLeft)
    Vector(Long.MinValue, Long.MaxValue).foreach { t =>
      assert(p.resolve("t", timeline(t -> "start"), clock, budget) match
        case Left(EpochError.AnchorOverflow("t", _, _, _)) => true
        case _                                             => false)
    }
    assert(p.resolve("t", timeline(0L -> "start"), clock, NonNegativeLong.zero) match
      case Left(EpochError.BinLimit("t", _, _, n, _)) => n == 3
      case _                                          => false)
    val huge = plan(FinalBin.IncludeShortFinal, Long.MinValue, Long.MaxValue, 1)
    assert(
      huge.resolve(
        "t",
        timeline(0L -> "start"),
        clock,
        get(NonNegativeLong.of(Long.MaxValue))
      ) match
        case Left(EpochError.DurationOverflow("t", _, n)) => n == BigInt(2).pow(64) - 1
        case _                                            => false
    )
  }

  property("bins and reported tail cover each microsecond exactly once") {
    forAll(
      Gen.choose(0, 120),
      Gen.choose(1, 20),
      Gen.oneOf(FinalBin.IncludeShortFinal, FinalBin.ExcludeAndReport)
    ) { (length, width, policy) =>
      val result = get(
        plan(policy, -10, length.toLong - 10, width.toLong)
          .resolve("t", timeline(9007199254740995L -> "start"), clock, budget)
      )
      val intervals = result.bins ++ result.excludedTail.toVector
      val covered   =
        intervals.flatMap(i => (0L until i.duration.toMicros).map(i.onset.toMicros + _))
      assertEquals(covered, (0 until length).map(i => 9007199254740985L + i).toVector)
      assert(result.bins.forall(_.duration.toMicros <= width))
    }
  }
