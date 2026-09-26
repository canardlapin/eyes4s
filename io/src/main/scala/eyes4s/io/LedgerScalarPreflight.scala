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

/** Resource admission only: malformed numeric syntax is left to the existing
  * semantic parser. A token has a checked code-unit bound before this scan;
  * no numeric conversion, trimming, exponent expansion or callback is invoked.
  */
private[io] object LedgerScalarPreflight:
  def decimal(
      token: String,
      limits: LedgerExecutionLimits,
      at: LedgerResourceLocation
  ): Either[LedgerResourceError, Unit] =
    limits.check(LedgerResource.FieldCodeUnits, token.length.toLong, at).flatMap { _ =>
      var index                                = 0
      var digits                               = 0L
      var exponent                             = 0L
      var decimalPoint                         = false
      var inExponent                           = false
      var signAllowed                          = true
      var possible                             = true
      var failure: Option[LedgerResourceError] = None
      while index < token.length && possible && failure.isEmpty do
        val character = token.charAt(index)
        if character >= '0' && character <= '9' then
          signAllowed = false
          if inExponent then
            val digit = (character - '0').toLong
            val bound = limits(LedgerResource.NumericExponentMagnitude)
            if exponent > bound / 10 || (exponent == bound / 10 && digit > bound % 10) then
              failure = Some(
                LedgerResourceError.Exceeded(
                  LedgerResource.NumericExponentMagnitude,
                  bound,
                  BigInt(exponent) * 10 + digit,
                  at
                )
              )
            else exponent = exponent * 10 + digit
          else
            digits += 1
            failure = limits.check(LedgerResource.NumericDigits, digits, at).left.toOption
        else if (character == '+' || character == '-') && signAllowed then signAllowed = false
        else if character == '.' && !inExponent && !decimalPoint then
          decimalPoint = true
          signAllowed = false
        else if (character == 'e' || character == 'E') && !inExponent && digits > 0 then
          inExponent = true
          signAllowed = true
        else possible = false
        index += 1
      failure.toLeft(())
    }
