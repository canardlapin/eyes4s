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

/** The four perspectives (overhaul v4, owner decision 2026-10-29): Data,
  * Analysis, Results and Report. Results holds what Compare and Explore
  * held (the summary, the pair and the drill to a trial, fixation and
  * record); Report is Figures.
  */
enum Perspective derives CanEqual:
  case Data, Analysis, Results, Report

  def label: String = productPrefix

object Perspective:

  /** A perspective a document saved before v4 names: Compare and Explore
    * are Results, Figures is Report.
    */
  def named(name: String): Option[Perspective] = name match
    case "Compare" | "Explore" => Some(Results)
    case "Figures"             => Some(Report)
    case other                 => values.find(_.productPrefix == other)

  /** Whether a saved name is one v4 renamed (its saved layout is dropped). */
  def renamed(name: String): Boolean = Set("Compare", "Explore", "Figures")(name)

  // The enum's object form, `{"Results":{}}`, as before; the old names decode.
  given Encoder[Perspective] =
    Encoder.instance(p => io.circe.Json.obj(p.productPrefix -> io.circe.Json.obj()))

  given Decoder[Perspective] = Decoder.instance { c =>
    c.keys.flatMap(_.headOption) match
      case Some(name) =>
        named(name).toRight(io.circe.DecodingFailure(s"unknown perspective '$name'", c.history))
      case None => Left(io.circe.DecodingFailure("a perspective is an object", c.history))
  }

enum Theme derives CanEqual, Codec.AsObject:
  case Light, Dark

/** The image-stage surround (DESIGN_SPEC section 4, Appearance). */
enum StageAppearance derives CanEqual, Codec.AsObject:
  case Dark, Mid, Light

/** Map opacity in [0, 1]; the default is 0.6 (DESIGN_SPEC section 12). */
sealed abstract case class MapOpacity private (value: Double) derives CanEqual

object MapOpacity:
  private def apply(value: Double): MapOpacity =
    new MapOpacity(value) {}

  val default: MapOpacity = MapOpacity(0.6)

  def of(value: Double): Either[DocumentError, MapOpacity] =
    Checks
      .finite("map opacity", value)
      .flatMap(Checks.within("map opacity", _, 0.0, 1.0))
      .map(MapOpacity(_))

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

object SavedLayout:

  /** A saved layout as stored, or none for a perspective v4 renamed: its
    * arrangement was Compare's, Explore's or Figures', which the v4 layouts
    * replace, so it reopens at the defaults.
    */
  val stored: Decoder[Option[SavedLayout]] = Decoder.instance { c =>
    val name = c.downField("perspective").keys.flatMap(_.headOption)
    if name.exists(Perspective.renamed) then Right(None)
    else c.as[SavedLayout].map(Some(_))
  }

/** How the project is being looked at. Kept strictly apart from the
  * document's science: nothing here changes the scientific hash
  * ([[StudioDocument.scienceDigest]]). `shownRun` is the run Compare and the
  * banners show until the user chooses Show on a newer one.
  */
sealed abstract case class PresentationState private (
    perspective: Perspective,
    theme: Theme,
    stage: StageAppearance,
    mapOpacity: MapOpacity,
    underlay: Boolean,
    shownRun: Option[RunId],
    layouts: Vector[SavedLayout]
) derives CanEqual

object PresentationState:
  private def apply(
      perspective: Perspective,
      theme: Theme,
      stage: StageAppearance,
      mapOpacity: MapOpacity,
      underlay: Boolean,
      shownRun: Option[RunId],
      layouts: Vector[SavedLayout]
  ): PresentationState =
    new PresentationState(perspective, theme, stage, mapOpacity, underlay, shownRun, layouts) {}

  /** Data perspective, light theme, dark stage, opacity 0.6, the
    * remembered-image underlay off, no run shown, no saved layouts.
    */
  val default: PresentationState = PresentationState(
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
      PresentationState(
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
    given Decoder[Vector[Option[SavedLayout]]] = Decoder.decodeVector(using SavedLayout.stored)
    Decoder
      .forProduct7[
        Either[DocumentError, PresentationState],
        Perspective,
        Theme,
        StageAppearance,
        MapOpacity,
        Boolean,
        Option[RunId],
        Vector[Option[SavedLayout]]
      ](
        "perspective",
        "theme",
        "stage",
        "mapOpacity",
        "underlay",
        "shownRun",
        "layouts"
      )((p, t, s, o, u, r, l) => of(p, t, s, o, u, r, l.flatten))
      .emap(_.left.map(_.message))
