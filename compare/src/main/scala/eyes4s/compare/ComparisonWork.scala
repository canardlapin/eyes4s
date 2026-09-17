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

package eyes4s.compare

/** Failures declaring or bounding one comparison's work. */
enum ComparisonWorkError derives CanEqual:
  case InvalidQuantum(value: Int)
  case InvalidBudget(maxWorkUnits: Long)
  case WorkBudget(measure: String, workUnits: Long, maximum: Long)

  def message: String = this match
    case InvalidQuantum(value) => s"Comparison quantum must be positive, got $value."
    case InvalidBudget(max)    => s"Comparison work budget must be nonnegative, got $max."
    case WorkBudget(measure, units, maximum) =>
      s"$measure declares $units work units, exceeding the comparison budget $maximum."

/** Maximum work units one [[ComparisonCursor.advance]] may visit. For a cell
  * comparison one unit is one grid cell. This bounds the supported instances
  * shipped here; it is not a wall-clock promise about custom instances.
  */
final class ComparisonQuantum private (val value: Int)
object ComparisonQuantum:
  val default: ComparisonQuantum = new ComparisonQuantum(1 << 16)

  /** Drives a cursor to completion in as few steps as the instance allows. */
  private[compare] val whole: ComparisonQuantum = new ComparisonQuantum(Int.MaxValue)

  def of(value: Int): Either[ComparisonWorkError, ComparisonQuantum] =
    if value <= 0 then Left(ComparisonWorkError.InvalidQuantum(value))
    else Right(new ComparisonQuantum(value))

/** The declared bound on one comparison's total work. Like
  * `PairScheduleBudget.default`, the default is effectively unbounded;
  * consumers that want refusals must supply their own.
  */
final class ComparisonBudget private (val maxWorkUnits: Long):
  def check(measure: String, workUnits: Long): Either[ComparisonWorkError, Long] =
    if workUnits > maxWorkUnits then
      Left(ComparisonWorkError.WorkBudget(measure, workUnits, maxWorkUnits))
    else Right(workUnits)

object ComparisonBudget:
  val default: ComparisonBudget = new ComparisonBudget(Long.MaxValue)

  def of(maxWorkUnits: Long): Either[ComparisonWorkError, ComparisonBudget] =
    if maxWorkUnits < 0 then Left(ComparisonWorkError.InvalidBudget(maxWorkUnits))
    else Right(new ComparisonBudget(maxWorkUnits))

/** One bounded step of a comparison. `workUnits` is what this step visited. */
enum ComparisonStep[+S]:
  case More(workUnits: Int, next: ComparisonCursor[S])
  case Done(workUnits: Int, result: Either[CompareError, S])

/** An immutable position inside one comparison.
  *
  * The contract for every instance, supported or custom:
  *   - `remaining` is declared before any unit is visited and decreases by
  *     exactly the `workUnits` of each step;
  *   - `advance(q)` visits at most `q.value` units, so the caller may yield
  *     between steps;
  *   - re-advancing the same cursor is deterministic, and the final result
  *     does not depend on where the steps were cut.
  */
trait ComparisonCursor[+S]:
  def remaining: Long
  def advance(quantum: ComparisonQuantum): ComparisonStep[S]

object ComparisonCursor:
  /** A comparison whose outcome is already decided; no work remains. */
  def decided[S](result: Either[CompareError, S]): ComparisonCursor[S] =
    new ComparisonCursor[S]:
      val remaining: Long                                        = 0L
      def advance(quantum: ComparisonQuantum): ComparisonStep[S] =
        ComparisonStep.Done(0, result)

/** A comparison that can be evaluated in bounded, resumable steps.
  *
  * `compare` is final and drives the same cursor to completion, so the whole
  * synchronous result and the incremental result are one implementation. A
  * plain `Compare` built from an arbitrary closure cannot extend this trait
  * without supplying a cursor; there is no lifting constructor on purpose.
  */
trait BoundedCompare[-A, -B, +S] extends Compare[A, B, S]:
  def start(a: A, b: B): ComparisonCursor[S]

  final def compare(a: A, b: B): Either[CompareError, S] =
    ComparisonWork.complete(start(a, b))._1

object BoundedCompare:
  extension [A, B, S](source: BoundedCompare[A, B, S])
    /** Derive a bounded comparison by a constant-time transformation of the
      * finished score. The derived cursor performs exactly the source's work;
      * this is the supported route for an extension such as a scaled cosine.
      */
    def mapScore[T](
        derived: MeasureInfo
    )(f: S => Either[CompareError, T]): BoundedCompare[A, B, T] =
      new BoundedCompare[A, B, T]:
        val info: MeasureInfo                      = derived
        def start(a: A, b: B): ComparisonCursor[T] =
          ComparisonWork.mapCursor(source.start(a, b))(_.flatMap(f))

object ComparisonWork:

  /** Begin one comparison after checking its declared work against a budget. */
  def start[A, B, S](
      comparison: BoundedCompare[A, B, S],
      a: A,
      b: B,
      budget: ComparisonBudget
  ): Either[ComparisonWorkError, ComparisonCursor[S]] =
    val cursor = comparison.start(a, b)
    budget.check(comparison.info.name, cursor.remaining).map(_ => cursor)

  /** Drive a cursor to completion with the largest quantum, returning the
    * result and the total units visited.
    */
  def complete[S](cursor: ComparisonCursor[S]): (Either[CompareError, S], Long) =
    @annotation.tailrec
    def loop(cursor: ComparisonCursor[S], visited: Long): (Either[CompareError, S], Long) =
      cursor.advance(ComparisonQuantum.whole) match
        case ComparisonStep.More(units, next)   => loop(next, visited + units)
        case ComparisonStep.Done(units, result) => (result, visited + units)
    loop(cursor, 0L)

  private[compare] def mapCursor[S, T](
      cursor: ComparisonCursor[S]
  )(f: Either[CompareError, S] => Either[CompareError, T]): ComparisonCursor[T] =
    new ComparisonCursor[T]:
      def remaining: Long                                        = cursor.remaining
      def advance(quantum: ComparisonQuantum): ComparisonStep[T] =
        cursor.advance(quantum) match
          case ComparisonStep.More(units, next) =>
            ComparisonStep.More(units, mapCursor(next)(f))
          case ComparisonStep.Done(units, result) => ComparisonStep.Done(units, f(result))
