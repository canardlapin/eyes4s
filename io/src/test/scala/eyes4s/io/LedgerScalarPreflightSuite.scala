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

class LedgerScalarPreflightSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*
  private val at = LedgerResourceLocation(LedgerResourceSource.Primary, Some(42), Some(6))

  test("decimal digit precision is counted before conversion, including leading zeros") {
    val envelope = limits(LedgerResource.NumericDigits -> 3L)
    Vector("12", "123", "+1.23", "-.123", "000", "1e200").foreach { value =>
      assertEquals(LedgerScalarPreflight.decimal(value, envelope, at), Right(()), value)
    }
    Vector("1234", "00.00", "-1.234", "1234" + "0" * 100000).foreach { value =>
      assertEquals(
        LedgerScalarPreflight.decimal(value, envelope, at),
        Left(LedgerResourceError.Exceeded(LedgerResource.NumericDigits, 3, BigInt(4), at))
      )
    }
  }

  test("positive and negative exponent magnitude refuse at the first excess digit") {
    val envelope = limits(LedgerResource.NumericExponentMagnitude -> 12L)
    Vector("1e11", "1e12", "1e-12", "+.1E+012", "1.0e00000000012").foreach { value =>
      assertEquals(LedgerScalarPreflight.decimal(value, envelope, at), Right(()), value)
    }
    Vector("1e13", "1e-13", "1e1300000000", "1E+13", "1e00013").foreach { value =>
      assertEquals(
        LedgerScalarPreflight.decimal(value, envelope, at),
        Left(
          LedgerResourceError.Exceeded(
            LedgerResource.NumericExponentMagnitude,
            12,
            BigInt(13),
            at
          )
        )
      )
    }
  }

  test("a short huge exponent cannot overflow the guard even at a Long maximum limit") {
    val envelope = limits(LedgerResource.NumericExponentMagnitude -> Long.MaxValue)
    assertEquals(
      LedgerScalarPreflight.decimal("1e9223372036854775807", envelope, at),
      Right(())
    )
    assertEquals(
      LedgerScalarPreflight.decimal("1e922337203685477580800000", envelope, at),
      Left(
        LedgerResourceError.Exceeded(
          LedgerResource.NumericExponentMagnitude,
          Long.MaxValue,
          BigInt(Long.MaxValue) + 1,
          at
        )
      )
    )
  }

  test("text is bounded before examining numeric syntax, and invalid syntax remains semantic") {
    assertEquals(
      LedgerScalarPreflight.decimal("1e99", limits(LedgerResource.FieldCodeUnits -> 3L), at),
      Left(LedgerResourceError.Exceeded(LedgerResource.FieldCodeUnits, 3, BigInt(4), at))
    )
    Vector("", "+", "-", ".", "1e", "e100", "NaN", "wrong", "1..2").foreach { value =>
      assertEquals(LedgerScalarPreflight.decimal(value, limits(), at), Right(()), value)
    }
  }
