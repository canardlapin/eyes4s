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

package eyes4s.core

/** Independently interruptible phases of a detection artifact's assembly. */
enum AssemblyPhase derives CanEqual:
  case Emissions, Support, EventValidation, EventSummaries, SourceIdentity, Gaps,
    LabelsAndReport

/** Immutable, replayable pure work. Each suspension charges one operation;
  * advances stop at phase boundaries as well as at the requested quantum.
  */
private[eyes4s] sealed trait AssemblyWork[+A]:
  import AssemblyWork.*
  def flatMap[B](f: A => AssemblyWork[B]): AssemblyWork[B] = this match
    case Done(value)       => f(value)
    case More(phase, next) => More(phase, () => next().flatMap(f))
  def map[B](f: A => B): AssemblyWork[B] = flatMap(a => Done(f(a)))
  def phase: Option[AssemblyPhase]       = this match
    case Done(_)        => None
    case More(stage, _) => Some(stage)
  def advance(maximum: Int): (Int, AssemblyWork[A]) =
    var work: AssemblyWork[A] = this
    var units                 = 0
    val initial               = phase
    while units < math.max(1, maximum) && work.phase.nonEmpty && work.phase == initial do
      work match
        case More(_, next) =>
          work = next()
          units += 1
        case Done(_) => ()
    (units, work)
  def complete: A =
    @annotation.tailrec
    def loop(work: AssemblyWork[A]): A = work match
      case Done(value) => value
      case _           => loop(work.advance(4096)._2)
    loop(this)

private[eyes4s] object AssemblyWork:
  final case class Done[A](value: A) extends AssemblyWork[A]
  final case class More[A](stage: AssemblyPhase, next: () => AssemblyWork[A])
      extends AssemblyWork[A]
  def defer[A](phase: AssemblyPhase)(next: => AssemblyWork[A]): AssemblyWork[A] =
    More(phase, () => next)
  def fold[A](phase: AssemblyPhase, from: Int, until: Int, initial: A)(
      f: (A, Int) => A
  ): AssemblyWork[A] =
    if from >= until then Done(initial)
    else defer(phase)(fold(phase, from + 1, until, f(initial, from))(f))
  def foldEither[E, A](phase: AssemblyPhase, from: Int, until: Int, initial: A)(
      f: (A, Int) => AssemblyWork[Either[E, A]]
  ): AssemblyWork[Either[E, A]] =
    if from >= until then Done(Right(initial))
    else
      defer(phase) {
        f(initial, from).flatMap {
          case Left(error)  => Done(Left(error))
          case Right(value) => foldEither(phase, from + 1, until, value)(f)
        }
      }
