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

package eyes4s.studio.core.session

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.*

/** Where an injected fault strikes a mutation. */
enum Fault derives CanEqual:
  /** Before the mutation reaches the store. */
  case Before

  /** After the store completed it. */
  case After

  /** Midway: an append leaves the first half of its bytes; any other
    * mutation behaves as `Before` (it is atomic).
    */
  case Torn

/** What the fault does. */
enum Failure derives CanEqual:
  /** The process dies: the call and every later call raise [[Crashed]]. */
  case Crash

  /** The store reports an error and carries on. */
  case Report

/** A paused mutation: `reached` completes when the store reaches it, and it
  * resumes when the test completes `gate`.
  */
final case class Paused(reached: Deferred[IO, Unit], gate: Deferred[IO, Unit])

/** What is armed: a failure, or a pause (Before or After the mutation). */
private[session] enum Plan:
  case Fail(step: Int, fault: Fault, failure: Failure)
  case Pause(step: Int, fault: Fault, paused: Paused)

  /** Fail the next mutation named `name`, once. */
  case Named(name: String, fault: Fault, failure: Failure)

/** The exception a crashed store raises. */
case object Crashed extends RuntimeException("injected crash")

/** A [[ProjectStore]] decorator that numbers every mutation (entry writes and
  * deletes, the manifest swap, sidecar writes) from 1 after [[arm]], fails the
  * armed one, and logs each mutation. It remembers the writer lock it passed
  * on, so a test can release it as the operating system does when a process
  * dies.
  */
final class FaultyStore private (
    val underlying: ProjectStore[IO],
    count: Ref[IO, Int],
    plan: Ref[IO, Vector[Plan]],
    dead: Ref[IO, Boolean],
    seen: Ref[IO, Option[WriterLock]],
    log: Ref[IO, Vector[String]]
) extends ProjectStore[IO]:

  /** Number mutations from 1 again, fail number `step` as `fault`, and
    * clear the log.
    */
  def arm(step: Int, fault: Fault, failure: Failure): IO[Unit] =
    count.set(0) *> log.set(Vector.empty) *> plan.set(Vector(Plan.Fail(step, fault, failure)))

  /** Number mutations from 1 again and pause number `step` before or after
    * the store performs it.
    */
  def pause(step: Int, fault: Fault): IO[Paused] =
    (Deferred[IO, Unit], Deferred[IO, Unit])
      .mapN(Paused(_, _))
      .flatTap(p =>
        count.set(0) *> log.set(Vector.empty) *> plan.set(Vector(Plan.Pause(step, fault, p)))
      )

  /** Number mutations from 1 again with no fault armed, and clear the log. */
  def reset: IO[Unit] = count.set(0) *> log.set(Vector.empty) *> plan.set(Vector.empty)

  /** Also fail the next mutation named `name` (as [[mutations]] lists it),
    * once, whatever its number.
    */
  def armNamed(name: String, fault: Fault, failure: Failure): IO[Unit] =
    plan.update(_ :+ Plan.Named(name, fault, failure))

  /** The mutations since the last [[arm]] or [[reset]], in order. */
  def mutations: IO[Vector[String]] = log.get

  /** Release the lock the dead process held, as its exit would. */
  def bury: IO[Unit] = seen.get.flatMap(_.traverse_(underlying.release(_).void))

  private def alive: IO[Unit] = dead.get.flatMap(d => IO.raiseError(Crashed).whenA(d))

  private def mutation[A](name: String, target: String)(
      run: IO[Either[StoreError, A]],
      torn: Option[IO[Either[StoreError, A]]] = None
  ): IO[Either[StoreError, A]] =
    val injected: Either[StoreError, A] = Left(StoreError.Unwritable(target, "injected fault"))
    def fail(failure: Failure): IO[Either[StoreError, A]] = failure match
      case Failure.Crash  => dead.set(true) *> IO.raiseError(Crashed)
      case Failure.Report => IO.pure(injected)
    for
      _     <- alive
      step  <- count.updateAndGet(_ + 1)
      _     <- log.update(_ :+ name)
      armed <- plan.modify { plans =>
        val i = plans.indexWhere {
          case Plan.Fail(n, _, _)  => n == step
          case Plan.Pause(n, _, _) => n == step
          case Plan.Named(m, _, _) => m == name
        }
        plans.lift(i) match
          case Some(p @ Plan.Named(_, _, _)) => (plans.patch(i, Nil, 1), Some(p))
          case found                         => (plans, found)
      }
      failing = armed.collect {
        case Plan.Fail(_, fault, failure)  => (fault, failure)
        case Plan.Named(_, fault, failure) => (fault, failure)
      }
      out <- (failing, armed) match
        case (Some((Fault.Before, failure)), _) => fail(failure)
        case (Some((Fault.After, failure)), _)  => run *> fail(failure)
        case (Some((Fault.Torn, failure)), _)   => torn.fold(IO.unit)(_.void) *> fail(failure)
        case (None, Some(Plan.Pause(_, Fault.After, p))) =>
          run <* (p.reached.complete(()) *> p.gate.get)
        case (None, Some(Plan.Pause(_, _, p))) =>
          p.reached.complete(()) *> p.gate.get *> run
        case _ => run
    yield out

  def read(path: BundlePath): IO[Either[StoreError, IArray[Byte]]] =
    alive *> underlying.read(path)
  def list: IO[Either[StoreError, Vector[BundlePath]]]   = alive *> underlying.list
  def readManifest: IO[Either[StoreError, IArray[Byte]]] = alive *> underlying.readManifest
  def readSidecar(file: Sidecar): IO[Either[StoreError, IArray[Byte]]] =
    alive *> underlying.readSidecar(file)

  def write(lock: WriterLock, path: BundlePath, bytes: IArray[Byte]) =
    mutation(s"write ${path.value}", path.value)(underlying.write(lock, path, bytes))
  def delete(lock: WriterLock, path: BundlePath) =
    mutation(s"delete ${path.value}", path.value)(underlying.delete(lock, path))
  def swapManifest(lock: WriterLock, expected: Option[ByteDigest], next: IArray[Byte]) =
    mutation("swap manifest", ProjectStore.ManifestName)(
      underlying.swapManifest(lock, expected, next)
    )
  def appendSidecar(lock: WriterLock, file: Sidecar, bytes: IArray[Byte]) =
    mutation(s"append ${file.fileName}", file.fileName)(
      underlying.appendSidecar(lock, file, bytes),
      Some(underlying.appendSidecar(lock, file, bytes.take(bytes.length / 2)))
    )
  def replaceSidecar(lock: WriterLock, file: Sidecar, bytes: IArray[Byte]) =
    mutation(s"replace ${file.fileName}", file.fileName)(
      underlying.replaceSidecar(lock, file, bytes)
    )
  def removeSidecar(lock: WriterLock, file: Sidecar) =
    mutation(s"remove ${file.fileName}", file.fileName)(underlying.removeSidecar(lock, file))

  def acquire(owner: LockOwner): IO[Either[StoreError, WriterLock]] =
    alive *> underlying.acquire(owner).flatTap(_.traverse_(l => seen.set(Some(l))))
  def release(lock: WriterLock): IO[Either[StoreError, Unit]] =
    alive *> underlying.release(lock)

object FaultyStore:
  def over(underlying: ProjectStore[IO]): IO[FaultyStore] =
    (
      Ref.of[IO, Int](0),
      Ref.of[IO, Vector[Plan]](Vector.empty),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Option[WriterLock]](None),
      Ref.of[IO, Vector[String]](Vector.empty)
    ).mapN(new FaultyStore(underlying, _, _, _, _, _))
