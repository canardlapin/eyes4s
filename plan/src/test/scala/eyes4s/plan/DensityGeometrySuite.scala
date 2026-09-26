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
import eyes4s.kernel.Unit2D.Px

class DensityGeometrySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val screen = get(Frame.screen("screen", 20, 10))
  private val window = get(
    Subframe.of(screen, FrameId("image"), get(Bounds.of[Px](5, 2, 15, 8)))
  )
  private val grid = get(Grid.over(window.frame, 5, 3))
  private val ref  = ResultRef.Estimation(0, "trial")

  private def frameValues(frame: Frame[Px]): Vector[Provenance.Param] =
    import Provenance.Param.*
    Vector(
      Text(frame.id.name),
      Text("px"),
      Num(frame.bounds.xMin),
      Num(frame.bounds.yMin),
      Num(frame.bounds.xMax),
      Num(frame.bounds.yMax),
      Text(frame.yAxis.toString)
    )

  private def description(
      angular: Option[Vector[Provenance.Param]] = None
  ): Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    Vector(
      "frame"     -> frameValues(grid.frame),
      "grid"      -> Vector(Text(grid.id.name), Num(5), Num(3)),
      "admission" -> frameValues(screen),
      "window"    -> Vector(Text("image"), Num(5), Num(2), Num(15), Num(8)),
      "offWindow" -> Vector(Text(OffWindowPolicy.Exclude.toString))
    ) ++ angular.toVector.map("angularScale" -> _)

  test("a cropped density retains its admission frame and translated origin") {
    val geometry = get(DensityGeometry.of(grid, description(), ref))
    assertEquals(geometry.admissionFrame, screen)
    assertEquals(geometry.origin, Pt[Px](5, 2))
    assertEquals(geometry.cell, get(Extent.of[Px](2, 2)))
  }

  test("the public density view factory exposes only checked geometry") {
    val mass = get(
      Surface.mass(
        grid,
        IArray.fill(grid.size)(1.0 / grid.size),
        Provenance.raw(ContentHash.empty)
      )
    )
    val view = get(DensityView.of(mass, description(), ref))
    assertEquals(view.geometry.admissionFrame, screen)
    assertEquals(view.geometry.origin, Pt[Px](5, 2))
    assertEquals(get(view.levels(Vector(0.5))).head.cells, grid.size)
    val whole = get(
      DensityView.of(
        mass,
        Vector(
          "frame" -> frameValues(grid.frame),
          "grid"  -> Vector(
            Provenance.Param.Text(grid.id.name),
            Provenance.Param.Num(5),
            Provenance.Param.Num(3)
          )
        ),
        ref
      )
    )
    assertNotEquals(view, whole)
    assertEquals(view.hashCode, get(DensityView.of(mass, description(), ref)).hashCode)
  }

  test("angular cell extents require an explicit admission-frame scale") {
    import Provenance.Param.*
    val withScale = get(
      DensityGeometry.of(grid, description(Some(Vector(Text("screen"), Num(2)))), ref)
    )
    assertEquals(withScale.cellDegrees, Some(get(Extent.of[Unit2D.Deg](1, 1))))
    assertEquals(get(DensityGeometry.of(grid, description(), ref)).cellDegrees, None)
  }

  test("cell order is named row-major with x varying fastest") {
    val geometry = get(DensityGeometry.of(grid, description(), ref))
    assertEquals(geometry.order, GridCellOrder.RowMajorXFastest)
    assertEquals(geometry.rowMajorIndex(0, 0), Some(0))
    assertEquals(geometry.rowMajorIndex(1, 0), Some(1))
    assertEquals(geometry.rowMajorIndex(0, 1), Some(5))
  }

  test("foreign angular frames and malformed geometry descriptions are refused") {
    import Provenance.Param.*
    assertEquals(
      DensityGeometry.of(grid, description(Some(Vector(Text("foreign"), Num(2)))), ref),
      Left(
        InspectionError.Geometry(
          ref,
          GeometryError.FrameMismatch(screen.id, FrameId("foreign"))
        )
      )
    )
    val malformed = description().filterNot(_._1 == "window")
    assertEquals(
      DensityGeometry.of(grid, malformed, ref),
      Left(InspectionError.GeometryDescription(ref, "window", frameValues(screen)))
    )
    val wrongUnit = description().map {
      case ("frame", values) => "frame" -> values.updated(1, Text("deg"))
      case field             => field
    }
    assertEquals(
      DensityGeometry.of(grid, wrongUnit, ref),
      Left(InspectionError.GeometryDescription(ref, "frame", wrongUnit.head._2))
    )
  }
