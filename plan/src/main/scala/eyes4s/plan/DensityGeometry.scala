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

package eyes4s.plan

import eyes4s.kernel.*

/** Reconstructs the checked rendering geometry carried by a saved study
  * description. A result's grid supplies the local frame; the description
  * supplies the admission frame and window when the map was cropped.
  */
object DensityGeometry:
  private type Description = Vector[(String, Vector[Provenance.Param])]

  def of[K, U <: Unit2D](
      grid: Grid[U],
      description: Description,
      reference: ResultRef[K]
  )(using units: UnitLabel[U]): Either[InspectionError[K], GridGeometry[U]] =
    import Provenance.Param.*

    def malformed(field: String, found: Vector[Provenance.Param]) =
      InspectionError.GeometryDescription(reference, field, found)

    def field(name: String): Either[InspectionError[K], Vector[Provenance.Param]] =
      description.collect { case (`name`, values) => values } match
        case Vector(values) => Right(values)
        case values         => Left(malformed(name, values.flatten))

    def optional(name: String): Either[InspectionError[K], Option[Vector[Provenance.Param]]] =
      description.collect { case (`name`, values) => values } match
        case Vector()       => Right(None)
        case Vector(values) => Right(Some(values))
        case values         => Left(malformed(name, values.flatten))

    def frame(
        field: String,
        values: Vector[Provenance.Param]
    ): Either[InspectionError[K], Frame[U]] = values match
      case values @ Vector(
            Text(id),
            Text(symbol),
            Num(xMin),
            Num(yMin),
            Num(xMax),
            Num(yMax),
            Text(axis)
          ) if symbol == units.symbol =>
        val yAxis = axis match
          case "Down" => Right(YAxis.Down)
          case "Up"   => Right(YAxis.Up)
          case _      => Left(malformed(field, values))
        for
          bounds <- Bounds
            .of[U](xMin, yMin, xMax, yMax)
            .left
            .map(InspectionError.Geometry(reference, _))
          direction <- yAxis
        yield Frame.of(FrameId(id), bounds, direction)
      case _ => Left(malformed(field, values))

    def checkedGrid: Either[InspectionError[K], Unit] =
      for
        declaredFrame <- field("frame").flatMap(frame("frame", _))
        declaredGrid  <- field("grid")
        _             <- Either.cond(
          declaredGrid == Vector(
            Text(grid.id.name),
            Num(grid.nx.toDouble),
            Num(grid.ny.toDouble)
          ),
          (),
          malformed("grid", declaredGrid)
        )
        _ <- Agreement
          .frames(declaredFrame, grid.frame)
          .left
          .map(InspectionError.Geometry(reference, _))
      yield ()

    def angular(
        admission: Frame[U]
    ): Either[InspectionError[K], Option[LinearAngularScale[U]]] =
      optional("angularScale").flatMap {
        case None                                                 => Right(None)
        case Some(values @ Vector(Text(id), Num(unitsPerDegree))) =>
          val declared = Frame.of(FrameId(id), admission.bounds, admission.yAxis)
          LinearAngularScale
            .of(declared, unitsPerDegree)
            .left
            .map(InspectionError.Geometry(reference, _))
            .map(Some(_))
        case Some(values) => Left(malformed("angularScale", values))
      }

    def whole: Either[InspectionError[K], GridGeometry[U]] =
      for
        admission <- optional("admission")
        window    <- optional("window")
        offWindow <- optional("offWindow")
        _         <- Either.cond(
          admission.isEmpty && window.isEmpty && offWindow.isEmpty,
          (),
          malformed(
            "window",
            window.getOrElse(admission.getOrElse(offWindow.getOrElse(Vector.empty)))
          )
        )
        scale    <- angular(grid.frame)
        geometry <- GridGeometry
          .of(grid, angularScale = scale)
          .left
          .map(InspectionError.Geometry(reference, _))
      yield geometry

    def windowed(
        admissionValues: Vector[Provenance.Param],
        windowValues: Vector[Provenance.Param],
        offWindowValues: Vector[Provenance.Param]
    ): Either[InspectionError[K], GridGeometry[U]] =
      for
        admission <- frame("admission", admissionValues)
        window    <- windowValues match
          case Vector(Text(id), Num(xMin), Num(yMin), Num(xMax), Num(yMax)) =>
            Bounds
              .of[U](xMin, yMin, xMax, yMax)
              .left
              .map(InspectionError.Geometry(reference, _))
              .flatMap(bounds =>
                Subframe
                  .of(admission, FrameId(id), bounds)
                  .left
                  .map(InspectionError.Geometry(reference, _))
              )
          case _ => Left(malformed("window", windowValues))
        _ <- Either.cond(
          offWindowValues == Vector(Text(OffWindowPolicy.Exclude.toString)) ||
            offWindowValues == Vector(Text(OffWindowPolicy.FailTrial.toString)),
          (),
          malformed("offWindow", offWindowValues)
        )
        scale    <- angular(admission)
        geometry <- GridGeometry
          .of(grid, Some(window), scale)
          .left
          .map(InspectionError.Geometry(reference, _))
      yield geometry

    for
      _         <- checkedGrid
      admission <- optional("admission")
      window    <- optional("window")
      offWindow <- optional("offWindow")
      geometry  <- (admission, window, offWindow) match
        case (None, None, None)               => whole
        case (Some(a), Some(w), Some(policy)) => windowed(a, w, policy)
        case _                                =>
          Left(
            malformed(
              "window",
              window.getOrElse(admission.getOrElse(offWindow.getOrElse(Vector.empty)))
            )
          )
    yield geometry
