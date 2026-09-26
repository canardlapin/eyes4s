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

package eyes4s.kernel

import eyes4s.kernel.Unit2D.Px

class GridGeometrySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val screen = get(Frame.screen("screen", 20, 10))
  private val window = get(
    Subframe.of(screen, FrameId("image"), get(Bounds.of[Px](5, 2, 15, 8)))
  )
  private val grid = get(Grid.over(window.frame, 5, 3))

  test("window geometry retains the parent frame and translated origin") {
    val geometry = get(GridGeometry.of(grid, Some(window)))
    assertEquals(geometry.admissionFrame, screen)
    assertEquals(geometry.frame, window.frame)
    assertEquals(geometry.bounds, window.frame.bounds)
    assertEquals(geometry.yAxis, screen.yAxis)
    assertEquals(geometry.nx, 5)
    assertEquals(geometry.ny, 3)
    assertEquals(geometry, get(GridGeometry.of(grid, Some(window))))
    assertEquals(geometry.hashCode, get(GridGeometry.of(grid, Some(window))).hashCode)
    assert(!geometry.equals("not geometry"))
    assertEquals(geometry.origin, Pt[Px](5, 2))
    assertEquals(geometry.cell, get(Extent.of[Px](2, 2)))
  }

  test("angular cell extents exist only for an explicit scale on the admission frame") {
    val scale = get(LinearAngularScale.of(screen, 2))
    assertEquals(
      get(GridGeometry.of(grid, Some(window), Some(scale))).cellDegrees,
      Some(get(Extent.of[Unit2D.Deg](1, 1)))
    )
    assertEquals(get(GridGeometry.of(grid, Some(window))).cellDegrees, None)
  }

  test("grid order is named row-major with x varying fastest") {
    val geometry = get(GridGeometry.of(grid, Some(window)))
    assertEquals(geometry.order, GridCellOrder.RowMajorXFastest)
    assertEquals(geometry.rowMajorIndex(0, 0), Some(0))
    assertEquals(geometry.rowMajorIndex(1, 0), Some(1))
    assertEquals(geometry.rowMajorIndex(0, 1), Some(5))
  }

  test("foreign window and angular frames are refused through Agreement") {
    val foreign = get(Frame.screen("foreign", 20, 10))
    val other   = get(Subframe.of(foreign, FrameId("other"), get(Bounds.of[Px](5, 2, 15, 8))))
    assertEquals(
      GridGeometry.of(grid, Some(other)),
      Left(GeometryError.FrameMismatch(window.frame.id, other.frame.id))
    )
    assertEquals(
      GridGeometry.of(grid, Some(window), Some(get(LinearAngularScale.of(foreign, 2)))),
      Left(GeometryError.FrameMismatch(screen.id, foreign.id))
    )
  }
