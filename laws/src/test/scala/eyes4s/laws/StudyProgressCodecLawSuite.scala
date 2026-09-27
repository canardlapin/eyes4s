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

import eyes4s.codec.*
import eyes4s.compare.ComparisonQuantum
import eyes4s.design.PairQuantum
import eyes4s.plan.*
import org.scalacheck.Gen

class StudyProgressCodecLawSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val codec                             = StudyProgressCodec.codec[String, String]
  private val strings                           = VersionedCodec.string(DefinitionId.unit)
  private val stamp    = get(RunStamp.of("plan", "input", strings, strings))
  private val counters = Gen.oneOf(0L, 1L, 9007199254740992L, 9007199254740993L, Long.MaxValue)
  private val cases    = for
    n      <- counters
    choice <- Gen.choose(0, 4)
    scale  <- Gen.choose(0, 3)
    design <- Gen.oneOf(StudyDesign.Matched, StudyDesign.Control)
  yield
    val stage = choice match
      case 0 => StudyRunStage.Counting(design, n)
      case 1 =>
        StudyRunStage.Running(
          StudyStage.Estimating(scale, 0),
          get(StageMeter.of(StageKind.Estimating, CountUnit.Maps, n, SegmentTotal.Exact(n)))
        )
      case 2 =>
        StudyRunStage.Running(
          StudyStage.Comparing(scale, design),
          get(StageMeter.of(StageKind.Comparing, CountUnit.Pairs, n, SegmentTotal.Unknown))
        )
      case 3 =>
        StudyRunStage.Running(
          StudyStage.Reducing(scale, design),
          get(StageMeter.of(StageKind.Reducing, CountUnit.Keys, n, SegmentTotal.AtMost(n)))
        )
      case _ =>
        StudyRunStage.Running(
          StudyStage.Contrasting(scale),
          get(StageMeter.of(StageKind.Contrasting, CountUnit.Rows, n, SegmentTotal.Exact(n)))
        )
    get(
      StudyProgressSnapshot.of(
        stamp,
        PairQuantum.default,
        ComparisonQuantum.default,
        math.max(1L, n),
        stage,
        0,
        n,
        if choice == 0 then SegmentTotal.Counting else SegmentTotal.Unknown,
        n
      )
    )

  private def sameStage(a: StudyRunStage, b: StudyRunStage): Boolean = (a, b) match
    case (StudyRunStage.Counting(ad, av), StudyRunStage.Counting(bd, bv)) =>
      ad == bd && av == bv
    case (StudyRunStage.Running(as, am), StudyRunStage.Running(bs, bm)) =>
      as == bs && am.kind == bm.kind && am.unit == bm.unit && am.done == bm.done && am.total == bm.total
    case _ => false

  checkAll(
    "study progress",
    CodecLaws.roundTrip(
      codec,
      cases,
      (a, b) =>
        a.stamp.sameAs(b.stamp) && a.pairQuantum.value == b.pairQuantum.value &&
          a.comparisonQuantum.value == b.comparisonQuantum.value && a.step == b.step && sameStage(
            a.stage,
            b.stage
          ) &&
          a.stepUnits == b.stepUnits && a.segmentUnits == b.segmentUnits &&
          a.segmentTotal == b.segmentTotal && a.totalUnits == b.totalUnits
    )
  )
