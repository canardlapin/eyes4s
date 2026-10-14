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

package consumer

import scala.compiletime.testing.typeCheckErrors

/** Positive resolution plus negative construction checks for every validated kernel product. */
class DomainProductMirrorSuite extends munit.FunSuite:
  test("eyes4s.kernel.Correction.Translate has no public constructing product mirror") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Correction.Translate; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Correction.Translate]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.kernel.Sync has no public constructing product mirror") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Sync; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Sync]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.kernel.Perspective has no public constructing product mirror") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Perspective; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Perspective]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.kernel.Interval has no public constructing product mirror") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Interval; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Interval]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.kernel.Window has no public constructing product mirror") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Window; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Window]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.kernel.SyncResidual has no public constructing product mirror") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.SyncResidual; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.SyncResidual]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.kernel.RejectedSyncMark has no public constructing product mirror") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.RejectedSyncMark; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.RejectedSyncMark]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Extent[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Extent[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Extent[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Bounds[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Bounds[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Bounds[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.kernel.FrameSpec has no public constructing product mirror") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.FrameSpec; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.FrameSpec]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Frame[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Frame[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Frame[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Region.Rect[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Region.Rect[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Region.Rect[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Region.Ellipse[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Region.Ellipse[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Region.Ellipse[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Region.Polygon[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Region.Polygon[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Region.Polygon[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Region.Union[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Region.Union[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Region.Union[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Region.Intersect[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Region.Intersect[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Region.Intersect[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Region.Complement[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Region.Complement[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Region.Complement[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Region.Empty[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Region.Empty[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Region.Empty[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Region.Everything[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Region.Everything[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Region.Everything[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.kernel.GridSpec has no public constructing product mirror") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.GridSpec; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.GridSpec]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Grid[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Grid[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Grid[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Moving[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Moving[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Moving[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Warp.Id[eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Warp.Id[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Warp.Id[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Warp.Affine[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Warp.Affine[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Warp.Affine[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Warp.Homography[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Warp.Homography[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Warp.Homography[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Warp.Tangent[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Warp.Tangent[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Warp.Tangent[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.kernel.Warp.Then[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px] has no public constructing product mirror"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.kernel.Warp.Then[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.kernel.Warp.Then[eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px, eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }
