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

package eyes4s.studio.core.geometry

import eyes4s.kernel.{
  Bounds,
  Frame,
  FrameId,
  GeometryError,
  LinearAngularScale,
  Pt,
  SurfaceError,
  Subframe,
  Unit2D,
  Warp,
  YAxis
}
import eyes4s.studio.core.backend.{DatasetRevision, TrialKey}
import eyes4s.studio.core.document.{ColumnName, ColumnRole, Geometry}
import eyes4s.studio.core.importing.SniffError

/** Why a dataset revision's geometry could not be checked against its
  * source (ticket S5.5). Every case names its operands.
  */
enum GeometryProblem derives CanEqual:
  /** eyes4s refused the declared geometry as frames. */
  case Frames(dataset: DatasetRevision, error: GeometryError)

  /** eyes4s refused the placed records as a measure. */
  case Measure(dataset: DatasetRevision, error: SurfaceError)

  /** The bytes read are not the revision's fixation source. */
  case NotTheSource(dataset: DatasetRevision, path: String, expected: String, actual: String)

  /** The fixation source could not be read as delimited text. */
  case Unreadable(dataset: DatasetRevision, error: SniffError)

  /** The revision maps no column to `role`, which placement needs. */
  case Unmapped(dataset: DatasetRevision, role: ColumnRole)

  /** The source header lacks the column the revision maps to `role`. */
  case MissingColumn(dataset: DatasetRevision, role: ColumnRole, column: ColumnName)

  /** Correction rules `first` and `second` (0-based) both cover `trial`;
    * eyes4s refuses such an admission.
    */
  case CorrectionConflict(trial: TrialKey, first: Int, second: Int)

  /** Correction rule `rule` (0-based) carries record `record` off the finite
    * plane.
    */
  case Uncorrectable(record: Int, rule: Int)

  def message: String = this match
    case Frames(d, e)  => s"The geometry of ${d.label} is not a set of frames: ${e.message}"
    case Measure(d, e) => s"The records of ${d.label} are not a measure: ${e.message}"
    case NotTheSource(d, path, expected, actual) =>
      s"The bytes read for $path are not ${d.label}'s fixation source " +
        s"(sha256 $actual, expected $expected)."
    case Unreadable(d, e)            => s"${d.label}'s fixation source: ${e.message}"
    case Unmapped(d, role)           => s"${d.label} maps no column to ${role.label}."
    case MissingColumn(d, role, col) =>
      s"${d.label} maps ${role.label} to column ${col.value}, which the source header lacks."
    case CorrectionConflict(t, a, b) =>
      s"Correction rules ${a + 1} and ${b + 1} both cover ${t.label}; eyes4s refuses the admission."
    case Uncorrectable(record, rule) =>
      s"Correction rule ${rule + 1} moves record $record off the finite plane."

/** A dataset revision's declared display geometry as eyes4s frames (UI-A):
  * the screen (the admission frame, origin top-left, y down), the image
  * frame as a half-open [[Subframe]] of it (the analysis window), and
  * eyes4s's declared [[LinearAngularScale]] on that window, which measures
  * degrees from the image centre with x right and y up. Studio converts
  * nothing itself: every containment decision and every change of unit is
  * an eyes4s one.
  */
final class DisplayFrames private (
    val dataset: DatasetRevision,
    val screen: Frame[Unit2D.Px],
    val image: Subframe[Unit2D.Px],
    val pixelsPerDegree: Double,
    degrees: Warp[Unit2D.Px, Unit2D.Deg]
):
  /** eyes4s's degrees of an image-frame position. */
  def toDegrees(image: Pt[Unit2D.Px]): Option[Pt[Unit2D.Deg]] = degrees(image)

object DisplayFrames:
  val ScreenId: FrameId  = FrameId("screen")
  val ImageId: FrameId   = FrameId("image")
  val DegreesId: FrameId = FrameId("image/degrees")

  def of(dataset: DatasetRevision, g: Geometry): Either[GeometryProblem, DisplayFrames] =
    val p = g.image
    (for
      bounds <- Bounds.sized[Unit2D.Px](g.screen.width.toDouble, g.screen.height.toDouble)
      screen = Frame.of(ScreenId, bounds, YAxis.Down)
      region <- Bounds.of[Unit2D.Px](
        p.left.toDouble,
        p.top.toDouble,
        p.left.toDouble + p.width,
        p.top.toDouble + p.height
      )
      image    <- Subframe.of(screen, ImageId, region)
      scale    <- LinearAngularScale.of(screen, g.pixelsPerDegree.value)
      onWindow <- scale.on(image)
      warp     <- onWindow.angular(DegreesId)
    yield new DisplayFrames(dataset, screen, image, g.pixelsPerDegree.value, warp)).left
      .map(GeometryProblem.Frames(dataset, _))
