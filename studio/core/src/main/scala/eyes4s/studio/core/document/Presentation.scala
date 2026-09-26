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

package eyes4s.studio.core.document

import eyes4s.studio.core.backend.RunId
import io.circe.{Codec, Decoder, Encoder}

/** The five perspectives (DESIGN_SPEC section 2). */
enum Perspective derives CanEqual, Codec.AsObject:
  case Data, Explore, Analysis, Compare, Figures

  def label: String = productPrefix

enum Theme derives CanEqual, Codec.AsObject:
  case Light, Dark

/** The image-stage surround (DESIGN_SPEC section 4, Appearance). */
enum StageAppearance derives CanEqual, Codec.AsObject:
  case Dark, Mid, Light

/** Map opacity in [0, 1]; the default is 0.6 (DESIGN_SPEC section 12). */
final case class MapOpacity private (value: Double) derives CanEqual

object MapOpacity:
  val default: MapOpacity = new MapOpacity(0.6)

  def of(value: Double): Either[DocumentError, MapOpacity] =
    Checks
      .finite("map opacity", value)
      .flatMap(Checks.within("map opacity", _, 0.0, 1.0))
      .map(new MapOpacity(_))

  given Codec[MapOpacity] = DocumentCodecs.validated(of, _.value)

/** A perspective's docking layout as scaladock writes it (uPickle JSON owned
  * by scaladock). Studio stores it as an opaque blob and never reads it.
  */
final case class LayoutBlob(text: String) derives CanEqual

object LayoutBlob:
  given Codec[LayoutBlob] = Codec.from(
    Decoder[String].map(LayoutBlob(_)),
    Encoder[String].contramap(_.text)
  )

final case class SavedLayout(perspective: Perspective, layout: LayoutBlob)
    derives CanEqual,
      Codec.AsObject

/** How the project is being looked at. Kept strictly apart from the
  * document's science: nothing here changes the scientific hash
  * ([[StudioDocument.scienceDigest]]). `shownRun` is the run Compare and the
  * banners show until the user chooses Show on a newer one.
  */
final case class PresentationState private (
    perspective: Perspective,
    theme: Theme,
    stage: StageAppearance,
    mapOpacity: MapOpacity,
    underlay: Boolean,
    shownRun: Option[RunId],
    layouts: Vector[SavedLayout]
) derives CanEqual

object PresentationState:
  /** Data perspective, light theme, dark stage, opacity 0.6, the
    * remembered-image underlay off, no run shown, no saved layouts.
    */
  val default: PresentationState = new PresentationState(
    Perspective.Data,
    Theme.Light,
    StageAppearance.Dark,
    MapOpacity.default,
    false,
    None,
    Vector.empty
  )

  /** Layouts are one per perspective, kept in perspective order. */
  def of(
      perspective: Perspective,
      theme: Theme,
      stage: StageAppearance,
      mapOpacity: MapOpacity,
      underlay: Boolean,
      shownRun: Option[RunId],
      layouts: Vector[SavedLayout]
  ): Either[DocumentError, PresentationState] =
    val perspectives = layouts.map(_.perspective)
    val repeated     = perspectives.diff(perspectives.distinct).distinct
    Either.cond(
      repeated.isEmpty,
      new PresentationState(
        perspective,
        theme,
        stage,
        mapOpacity,
        underlay,
        shownRun,
        layouts.sortBy(_.perspective.ordinal)
      ),
      DocumentError.DuplicateLayouts(repeated)
    )

  given Encoder.AsObject[PresentationState] =
    Encoder.forProduct7(
      "perspective",
      "theme",
      "stage",
      "mapOpacity",
      "underlay",
      "shownRun",
      "layouts"
    )(p => (p.perspective, p.theme, p.stage, p.mapOpacity, p.underlay, p.shownRun, p.layouts))

  given Decoder[PresentationState] =
    Decoder
      .forProduct7(
        "perspective",
        "theme",
        "stage",
        "mapOpacity",
        "underlay",
        "shownRun",
        "layouts"
      )(of)
      .emap(_.left.map(_.message))
