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

package eyes4s.io

import eyes4s.design.{KeyDigest, SampleQuantum}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class LedgerPreflightCursorSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("frame", 100, 100))
  private val columns                           = get(
    SourceFixationColumns.of(
      "n",
      "x",
      "y",
      "onset",
      "duration",
      SampleCountRule.PositiveColumn("samples")
    )
  )
  private def description(using KeyDigest[StudyKey]) = get(
    ImportSpec.of(
      SourceKeyColumns.Study("p", "item", "phase"),
      columns,
      frame,
      SourceTimeUnit.Milliseconds,
      AdmissionPolicy.default[StudyKey],
      AdmissionDecision.ReviewExclusions
    )
  )
  private val spec     = description
  private val admitted = get(
    SourceAdmission.read(
      "fixture",
      "p,item,phase,n,x,y,onset,duration,samples\np,i,f,1,10,20,0,10,1\n",
      spec
    )
  )
  private def start(envelope: LedgerExecutionLimits) =
    get(LedgerPreflightCursor.start(spec, admitted.ledger, admitted.accepted, envelope))

  private def drain(
      initial: LedgerPreflightCursor[StudyKey, Px],
      budget: Int
  ): Either[LedgerDescriptionError, (Long, Long, Vector[(LedgerPreflightStage, Long)])] =
    val quantum = get(SampleQuantum.of(budget))
    @annotation.tailrec
    def loop(
        cursor: LedgerPreflightCursor[StudyKey, Px],
        retained: Long,
        units: Long,
        boundaries: Vector[(LedgerPreflightStage, Long)]
    ): Either[LedgerDescriptionError, (Long, Long, Vector[(LedgerPreflightStage, Long)])] =
      cursor.advance(quantum) match
        case Left(error) => Left(error)
        case Right(step) =>
          assert(step.workUnits > 0 && step.workUnits <= budget)
          assert(step.retainedUnits >= retained)
          assertEquals(step.stage, cursor.stage)
          val boundary = step.next.forall(_.stage != step.stage)
          val seen     =
            if boundary then boundaries :+ (step.stage -> step.retainedUnits) else boundaries
          step.next match
            case Some(next) => loop(next, step.retainedUnits, units + step.workUnits, seen)
            case None       => Right((step.retainedUnits, units + step.workUnits, seen))
    loop(initial, 0L, 0L, Vector.empty)

  private def visitor[A](initial: A)(advance: A => (Int, Long, Option[A])): (Long, Long) =
    @annotation.tailrec
    def loop(cursor: A, units: Long): (Long, Long) =
      val (used, retained, next) = advance(cursor)
      next match
        case Some(value) => loop(value, units + used)
        case None        => (retained, units + used)
    loop(initial, 0L)

  private def isolatedCounts =
    val quantum = get(SampleQuantum.of(7))
    val d       = visitor(get(LedgerDescriptionCursor.start(spec, limits()))) { c =>
      val s = get(c.advance(quantum)); (s.workUnits, s.retainedUnits, s.next)
    }
    val e = visitor(get(LedgerEvidenceCursor.start(spec, admitted.ledger, limits()))) { c =>
      val s = get(c.advance(quantum)); (s.workUnits, s.retainedUnits, s.next)
    }
    val i = visitor(get(LedgerInputCursor.start(spec, admitted.accepted, limits()))) { c =>
      val s = get(c.advance(quantum)); (s.workUnits, s.retainedUnits, s.next)
    }
    Vector(d, e, i)

  test(
    "coordinator preserves visitor work counts and accumulates retention across phase barriers"
  ) {
    val counts     = isolatedCounts
    val cumulative = counts.map(_._1).scanLeft(0L)(_ + _).tail
    val stages     = Vector(
      LedgerPreflightStage.Description,
      LedgerPreflightStage.ExpectedLedger,
      LedgerPreflightStage.ExpectedInput
    )
    val expected = (cumulative.last, counts.map(_._2).sum, stages.zip(cumulative))
    val initial  = start(limits())
    Vector(1, 7, 1024).foreach(b => assertEquals(drain(initial, b), Right(expected)))
    val q = get(SampleQuantum.of(1))
    assertEquals(initial.advance(q), initial.advance(q))
    assertEquals(initial.scope.before, 0L)
  }

  test("individually fitting components must still fit the single aggregate cap") {
    val counts = isolatedCounts.map(_._1)
    val cap    = counts.max
    assert(counts.forall(_ <= cap))
    assert(counts.sum > cap)
    val expected = drain(start(limits(LedgerResource.RetainedEvidenceUnits -> cap)), 1)
    expected match
      case Left(
            LedgerDescriptionError.Resource(
              LedgerResourceError.Exceeded(dimension, limit, observed, _)
            )
          ) =>
        assertEquals(dimension, LedgerResource.RetainedEvidenceUnits)
        assertEquals(limit, cap)
        assert(observed > BigInt(cap))
      case other => fail(s"Expected aggregate resource refusal, got $other")
    Vector(7, 1024).foreach(b =>
      assertEquals(
        drain(start(limits(LedgerResource.RetainedEvidenceUnits -> cap)), b),
        expected
      )
    )
  }

  test(
    "exact aggregate limit succeeds and the adjacent lower limit reports original operands"
  ) {
    val total = isolatedCounts.map(_._1).sum
    Vector(1, 7, 1024).foreach { b =>
      assert(drain(start(limits(LedgerResource.RetainedEvidenceUnits -> total)), b).isRight)
      assert(
        drain(start(limits(LedgerResource.RetainedEvidenceUnits -> (total + 1))), b).isRight
      )
      assertEquals(
        drain(start(limits(LedgerResource.RetainedEvidenceUnits -> (total - 1))), b),
        Left(
          LedgerDescriptionError.Resource(
            LedgerResourceError.Exceeded(
              LedgerResource.RetainedEvidenceUnits,
              total - 1,
              BigInt(total),
              LedgerResourceLocation(LedgerResourceSource.ExpectedInput)
            )
          )
        )
      )
    }
  }

  test("retention scope preserves every other dimension and exact overflow evidence") {
    val at       = LedgerResourceLocation(LedgerResourceSource.Inventory, Some(3), Some(2))
    val original = limits(LedgerResource.RetainedEvidenceUnits -> Long.MaxValue)
    val scope    = get(LedgerRetentionScope.start(original, Long.MaxValue - 2, at))
    LedgerResource.values.foreach { d =>
      assertEquals(
        scope.limits(d),
        if d == LedgerResource.RetainedEvidenceUnits then 2L else original(d)
      )
    }
    val local =
      scope.limits.add(LedgerResource.RetainedEvidenceUnits, 2, 1, at).swap.toOption.get
    val expected = LedgerResourceError.Exceeded(
      LedgerResource.RetainedEvidenceUnits,
      Long.MaxValue,
      BigInt(Long.MaxValue) + 1,
      at
    )
    assertEquals(scope.restore(local), expected)
    assertEquals(scope.total(3, at), Left(expected))
    assertEquals(scope.total(2, at), Right(Long.MaxValue))
    val field = LedgerResourceError.Exceeded(LedgerResource.FieldCodeUnits, 4, BigInt(5), at)
    assertEquals(scope.restore(field), field)
    assert(
      LedgerRetentionScope
        .start(limits(LedgerResource.RetainedEvidenceUnits -> 4), 5, at)
        .isLeft
    )
  }

  test("coordinator refuses noncanonical evidence before any custom digest callback") {
    var calls  = 0
    val custom = new KeyDigest[StudyKey]:
      def digest(key: StudyKey): ContentHash =
        calls += 1
        ContentHash.empty
    val alternate = description(using custom)
    assertEquals(
      LedgerPreflightCursor.start(alternate, admitted.ledger, admitted.accepted, limits()),
      Left(LedgerDescriptionError.Unsupported(LedgerExecutionEvidence.Unsupported.KeyDigest))
    )
    assertEquals(calls, 0)
  }
