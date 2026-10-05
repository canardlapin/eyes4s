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

import cats.syntax.all.*
import eyes4s.kernel.*
import eyes4s.studio.core.backend.{
  DatasetRevision,
  ImagePosition,
  PlanePoint,
  ScaleSource,
  SourceRecordsError
}
import eyes4s.studio.core.document.{Geometry, Recipe}

/** A source record's image position and degrees (protocol 1.7, S6.4), as the
  * kernel gives them: the image `Subframe` entered from the screen frame, and
  * the kernel's `LinearAngularScale` on that image, from its centre, y up.
  * Both backends read them here; nothing here is geometry arithmetic.
  */
object RecordPositions:

  /** The display frames at the plan's declared pixels per degree when the
    * recipe states one, else the dataset's, and where that scale comes from.
    */
  def frames(
      dataset: DatasetRevision,
      recipe: Recipe,
      geometry: Geometry
  ): Either[String, (DisplayFrames, ScaleSource)] =
    val (perDegree, source) = recipe.angularScale.fold(
      (geometry.pixelsPerDegree, ScaleSource.Dataset)
    )(s => (s, ScaleSource.Recipe))
    for
      scaled <- Geometry.of(geometry.screen, geometry.image, perDegree).leftMap(_.message)
      frames <- DisplayFrames.of(dataset, scaled).leftMap(_.message)
    yield (frames, source)

  /** A screen centre's image position (and whether the image's half-open
    * frame holds it) and its degrees from the image centre.
    */
  def position(
      frames: DisplayFrames,
      record: Int,
      x: Double,
      y: Double
  ): Either[SourceRecordsError, Option[(ImagePosition, PlanePoint)]] =
    val centre = Pt[Unit2D.Px](x, y)
    frames.image
      .enter(centre)
      .flatMap(local => frames.toDegrees(local).map(local -> _))
      .traverse((local, d) =>
        for
          at  <- PlanePoint.of(record, "image", local.x, local.y)
          deg <- PlanePoint.of(record, "degrees", d.x, d.y)
        yield (ImagePosition(at, frames.image.locate(centre).isInside), deg)
      )
