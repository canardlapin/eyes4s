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

package eyes4s.design

import eyes4s.aoi.AoiSet
import eyes4s.core.Recording
import eyes4s.kernel.*

/** A nonblank, immutable key. Whitespace in a nonblank key is significant. */
opaque type SessionKey = String
object SessionKey:
  def of(value: String): Either[SessionError, SessionKey] =
    Either.cond(value.trim.nonEmpty, value, SessionError.InvalidKey(value))
  extension (key: SessionKey) def value: String = key

enum SessionRole derives CanEqual:
  case Recording, AoiSet, Grid

enum SessionError derives CanEqual:
  case InvalidKey(value: String)
  case DuplicateKey(role: SessionRole, key: SessionKey)
  case MissingKey(role: SessionRole, key: SessionKey)
  case Frame(role: SessionRole, key: SessionKey, underlying: GeometryError)
  case GridIdentity(key: SessionKey, existingKey: SessionKey, underlying: SurfaceError)

  def message: String = this match
    case InvalidKey(value)       => s"Session key '$value' is blank"
    case DuplicateKey(role, key) => s"Session $role key '${key.value}' already exists"
    case MissingKey(role, key)   => s"Session $role key '${key.value}' does not exist"
    case Frame(role, key, error) => s"Session $role '${key.value}': ${error.message}"
    case GridIdentity(key, existing, error) =>
      s"Session grid '${key.value}' conflicts with '${existing.value}': ${error.message}"

/** Immutable checked membership in one spatial frame.
  *
  * Every member shares the frame. Recordings do NOT share a timeline: each
  * keeps its own `ClockId`, because independently acquired recordings are on
  * independent clocks, and admission is not a synchronisation claim. An
  * analysis that needs two recordings on one timeline checks their clocks
  * through `Agreement.clocks` itself.
  *
  * Keys have separate namespaces for recordings, AOI sets and grids. Insertion
  * appends, replacement retains position, and removal preserves survivor order.
  * A failed operation leaves this value unchanged. Accessors return the stored
  * objects without revalidating them; they confer no authority over other values.
  */
final class Session[U <: Unit2D] private (
    val frame: Frame[U],
    val recordings: Vector[(SessionKey, Recording[U])],
    val aoiSets: Vector[(SessionKey, AoiSet[U])],
    val grids: Vector[(SessionKey, Grid[U])]
):
  private def checkFrame(
      role: SessionRole,
      key: SessionKey,
      candidate: Frame[U]
  ): Either[SessionError, Unit] =
    Agreement.frames(frame, candidate).left.map(SessionError.Frame(role, key, _)).map(_ => ())

  private def checkRecording(key: SessionKey, value: Recording[U]): Either[SessionError, Unit] =
    checkFrame(SessionRole.Recording, key, value.frame)

  private def checkAoiSet(key: SessionKey, value: AoiSet[U]): Either[SessionError, Unit] =
    checkFrame(SessionRole.AoiSet, key, value.frame)

  private def checkGrid(key: SessionKey, value: Grid[U]): Either[SessionError, Unit] =
    checkFrame(SessionRole.Grid, key, value.frame).flatMap { _ =>
      grids
        .filter { case (otherKey, grid) => otherKey != key && grid.id == value.id }
        .foldLeft[Either[SessionError, Unit]](Right(())) { case (checked, (otherKey, grid)) =>
          checked.flatMap(_ =>
            Agreement
              .grids(grid, value)
              .left
              .map(SessionError.GridIdentity(key, otherKey, _))
              .map(_ => ())
          )
        }
    }

  private def put[A](
      entries: Vector[(SessionKey, A)],
      role: SessionRole,
      key: SessionKey,
      value: A,
      replace: Boolean,
      check: (SessionKey, A) => Either[SessionError, Unit]
  ): Either[SessionError, Vector[(SessionKey, A)]] =
    val index = entries.indexWhere(_._1 == key)
    if replace && index < 0 then Left(SessionError.MissingKey(role, key))
    else if !replace && index >= 0 then Left(SessionError.DuplicateKey(role, key))
    else
      check(key, value).map { _ =>
        if replace then entries.updated(index, key -> value) else entries :+ (key -> value)
      }

  private def remove[A](
      entries: Vector[(SessionKey, A)],
      role: SessionRole,
      key: SessionKey
  ): Either[SessionError, Vector[(SessionKey, A)]] =
    if entries.exists(_._1 == key) then Right(entries.filterNot(_._1 == key))
    else Left(SessionError.MissingKey(role, key))

  def recording(key: SessionKey): Option[Recording[U]] =
    recordings.find(_._1 == key).map(_._2)

  def addRecording(key: SessionKey, value: Recording[U]): Either[SessionError, Session[U]] =
    put(recordings, SessionRole.Recording, key, value, false, checkRecording).map(entries =>
      new Session(frame, entries, aoiSets, grids)
    )

  def replaceRecording(key: SessionKey, value: Recording[U]): Either[SessionError, Session[U]] =
    put(recordings, SessionRole.Recording, key, value, true, checkRecording).map(entries =>
      new Session(frame, entries, aoiSets, grids)
    )

  def removeRecording(key: SessionKey): Either[SessionError, Session[U]] =
    remove(recordings, SessionRole.Recording, key).map(entries =>
      new Session(frame, entries, aoiSets, grids)
    )

  def aoiSet(key: SessionKey): Option[AoiSet[U]] =
    aoiSets.find(_._1 == key).map(_._2)

  def addAoiSet(key: SessionKey, value: AoiSet[U]): Either[SessionError, Session[U]] =
    put(aoiSets, SessionRole.AoiSet, key, value, false, checkAoiSet).map(entries =>
      new Session(frame, recordings, entries, grids)
    )

  def replaceAoiSet(key: SessionKey, value: AoiSet[U]): Either[SessionError, Session[U]] =
    put(aoiSets, SessionRole.AoiSet, key, value, true, checkAoiSet).map(entries =>
      new Session(frame, recordings, entries, grids)
    )

  def removeAoiSet(key: SessionKey): Either[SessionError, Session[U]] =
    remove(aoiSets, SessionRole.AoiSet, key).map(entries =>
      new Session(frame, recordings, entries, grids)
    )

  def grid(key: SessionKey): Option[Grid[U]] =
    grids.find(_._1 == key).map(_._2)

  def addGrid(key: SessionKey, value: Grid[U]): Either[SessionError, Session[U]] =
    put(grids, SessionRole.Grid, key, value, false, checkGrid).map(entries =>
      new Session(frame, recordings, aoiSets, entries)
    )

  def replaceGrid(key: SessionKey, value: Grid[U]): Either[SessionError, Session[U]] =
    put(grids, SessionRole.Grid, key, value, true, checkGrid).map(entries =>
      new Session(frame, recordings, aoiSets, entries)
    )

  def removeGrid(key: SessionKey): Either[SessionError, Session[U]] =
    remove(grids, SessionRole.Grid, key).map(entries =>
      new Session(frame, recordings, aoiSets, entries)
    )

object Session:
  /** Empty membership needs no validation beyond the already parsed identities. */
  def empty[U <: Unit2D](frame: Frame[U]): Session[U] =
    new Session(frame, Vector.empty, Vector.empty, Vector.empty)
