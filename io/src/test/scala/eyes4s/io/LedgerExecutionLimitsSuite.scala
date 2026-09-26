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

private[io] object LedgerExecutionLimitFixtures:
  def checked(values: Vector[Long]): Either[LedgerResourceError, LedgerExecutionLimits] =
    LedgerExecutionLimits.of(
      values(0),
      values(1),
      values(2),
      values(3),
      values(4),
      values(5),
      values(6),
      values(7),
      values(8),
      values(9),
      values(10),
      values(11)
    )

  def limits(overrides: (LedgerResource, Long)*): LedgerExecutionLimits =
    val values = overrides.foldLeft(Vector.fill(LedgerResource.values.length)(1000000L)) {
      case (current, (dimension, value)) => current.updated(dimension.ordinal, value)
    }
    checked(values).toOption.get

class LedgerExecutionLimitsSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*

  test("every dimension is explicit, non-negative, and independently checked") {
    val zero = checked(Vector.fill(LedgerResource.values.length)(0L)).toOption.get
    LedgerResource.values.foreach { dimension =>
      assertEquals(zero(dimension), 0L)
      val values = Vector.fill(LedgerResource.values.length)(1L).updated(dimension.ordinal, -1L)
      val error  = LedgerResourceError.NegativeLimit(dimension, -1L)
      assertEquals(checked(values), Left(error))
      assert(error.message.contains(dimension.toString))
      assert(error.message.contains("-1"))
    }
  }

  test("resource checks retain exact below, at and above boundary evidence") {
    val at = LedgerResourceLocation(LedgerResourceSource.ExpectedLedger, Some(3), Some(2))
    LedgerResource.values.foreach { dimension =>
      val envelope = limits(dimension -> 2L)
      assertEquals(envelope.check(dimension, 1, at), Right(()))
      assertEquals(envelope.check(dimension, 2, at), Right(()))
      val error = LedgerResourceError.Exceeded(dimension, 2, 3, at)
      assertEquals(envelope.check(dimension, 3, at), Left(error))
      assert(error.message.contains(dimension.toString))
      assert(error.message.contains("ExpectedLedger"))
      assert(error.message.contains("observed 3"))
    }
  }

  test("Long limits do not overflow during construction or comparison") {
    val envelope =
      checked(Vector.fill(LedgerResource.values.length)(Long.MaxValue)).toOption.get
    LedgerResource.values.foreach { dimension =>
      assertEquals(envelope(dimension), Long.MaxValue)
      assertEquals(
        envelope.check(
          dimension,
          Long.MaxValue,
          LedgerResourceLocation(LedgerResourceSource.Primary)
        ),
        Right(())
      )
    }
  }

  test("retained counters refuse before overflow and keep the exact first excess") {
    val at       = LedgerResourceLocation(LedgerResourceSource.ExpectedInput)
    val envelope = limits(LedgerResource.RetainedEvidenceUnits -> Long.MaxValue)
    assertEquals(
      envelope.add(LedgerResource.RetainedEvidenceUnits, Long.MaxValue - 1, 1, at),
      Right(Long.MaxValue)
    )
    assertEquals(
      envelope.add(LedgerResource.RetainedEvidenceUnits, Long.MaxValue, 1, at),
      Left(
        LedgerResourceError.Exceeded(
          LedgerResource.RetainedEvidenceUnits,
          Long.MaxValue,
          BigInt(Long.MaxValue) + 1,
          at
        )
      )
    )
  }
