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

import eyes4s.kernel.{GeometryError, SurfaceError, TimeError}

/** Anything that can go wrong in the core layer.
  *
  * ==Why a sum rather than a widening==
  *
  * An operation here can fail for reasons from several layers at once:
  * windowing a recording can fail on the clock (time) or on the resulting
  * sample set (recording), and warping one can fail on the frame (geometry).
  * The tempting shortcut is to pick one error type and map the others onto a
  * plausible-looking case, which is how a "frame mismatch" ends up reported for
  * a recording that simply had no samples left after filtering.
  *
  * Wrapping keeps the original, so the message a caller sees is the one the
  * failing layer wrote -- and PRD APP-14 still holds, since each underlying
  * case names its own operands.
  */
enum CoreError derives CanEqual:
  case OfTime(underlying: TimeError)
  case OfGeometry(underlying: GeometryError)
  case OfSurface(underlying: SurfaceError)
  case OfRecording(underlying: RecordingError)
  case OfScanpath(underlying: ScanpathError)
  case OfEvent(underlying: EventError)
  case OfDetectionSupport(underlying: DetectionSupportError)

  def message: String = this match
    case OfTime(e)             => e.message
    case OfGeometry(e)         => e.message
    case OfSurface(e)          => e.message
    case OfRecording(e)        => e.message
    case OfScanpath(e)         => e.message
    case OfEvent(e)            => e.message
    case OfDetectionSupport(e) => e.message

object CoreError:
  extension [A](e: Either[TimeError, A])
    def widen: Either[CoreError, A] = e.left.map(CoreError.OfTime.apply)

  extension [A](e: Either[GeometryError, A])
    def widenGeometry: Either[CoreError, A] = e.left.map(CoreError.OfGeometry.apply)

  extension [A](e: Either[SurfaceError, A])
    def widenSurface: Either[CoreError, A] = e.left.map(CoreError.OfSurface.apply)

  extension [A](e: Either[RecordingError, A])
    def widenRecording: Either[CoreError, A] = e.left.map(CoreError.OfRecording.apply)

  extension [A](e: Either[ScanpathError, A])
    def widenScanpath: Either[CoreError, A] = e.left.map(CoreError.OfScanpath.apply)

  extension [A](e: Either[EventError, A])
    def widenEvent: Either[CoreError, A] = e.left.map(CoreError.OfEvent.apply)

  extension [A](e: Either[DetectionSupportError, A])
    def widenDetectionSupport: Either[CoreError, A] =
      e.left.map(CoreError.OfDetectionSupport.apply)
