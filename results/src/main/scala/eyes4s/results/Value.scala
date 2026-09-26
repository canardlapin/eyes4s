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

package eyes4s.results

import eyes4s.plan.DiagnosticCode

/** Why a quantity is undefined although nothing failed. */
enum UndefinedReason derives CanEqual:
  /** A standard deviation or standard error needs at least two values; `n`
    * were available.
    */
  case TooFewForSpread(n: Int)

  /** A duration share of a trial whose fixations have no duration at all. */
  case ZeroDuration

  /** The arithmetic left the finite doubles: `operation` over `n` values. */
  case NotFinite(operation: String, n: Int)

/** Why a [[Value]] has no value. Every reason is distinct from zero, and each
  * says where the absence came from.
  */
enum Absence derives CanEqual:
  /** The source recorded nothing: a blank or absent covariate cell, or no
    * stored row for this role.
    */
  case NotRecorded

  /** A covariate cell holds a value that is not of its declared type (a
    * level the covariate does not declare, text where a number is declared).
    */
  case Unparsed

  /** The stored result failed here; `code` is the failure's diagnostic
    * code. The full diagnostic is reachable through the cell's result
    * references (`ResultInspection`).
    */
  case Failed(code: DiagnosticCode, message: String)

  /** Nothing failed, but the quantity is not defined. */
  case Undefined(reason: UndefinedReason)

  /** The group has no query for this role. */
  case EmptyGroup

  /** A within-participant contrast has this participant in one level only. */
  case Unpaired

  /** The participant has `queries` queries in the group, fewer than the
    * `required` minimum, and is left out of that group only.
    */
  case BelowMinimum(queries: Int, required: Int)

/** A value that may be absent, with the reason it is.
  *
  * There is deliberately no `Numeric`, `Monoid` or `orZero` for a value, and
  * no `getOrElse`: a missing mean is never silently a zero, and a report
  * that wants a number must say what to do with each reason.
  */
enum Value[+A] derives CanEqual:
  case Present(value: A)
  case Missing(reason: Absence)

  def isPresent: Boolean = this match
    case Present(_) => true
    case Missing(_) => false

  def toOption: Option[A] = this match
    case Present(v) => Some(v)
    case Missing(_) => None

  /** The absence, when there is one. */
  def absence: Option[Absence] = this match
    case Present(_) => None
    case Missing(r) => Some(r)

  def map[B](f: A => B): Value[B] = this match
    case Present(v) => Present(f(v))
    case Missing(r) => Missing(r)

  def flatMap[B](f: A => Value[B]): Value[B] = this match
    case Present(v) => f(v)
    case Missing(r) => Missing(r)

  def fold[B](missing: Absence => B)(present: A => B): B = this match
    case Present(v) => present(v)
    case Missing(r) => missing(r)
