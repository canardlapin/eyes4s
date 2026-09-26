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

package eyes4s.studio.app.tokens

/** A type family of the studio (DESIGN_SPEC section 7). `webName` is the
  * typographic family a web shell names; `styleClass` is the class that selects
  * it (none for [[Sans]], the default).
  */
enum TypeFamily(val webName: String, val styleClass: Option[String]) derives CanEqual:

  /** IBM Plex Sans: every word of the interface. */
  case Sans extends TypeFamily("IBM Plex Sans", None)

  /** IBM Plex Mono: every numeral and identifier. JavaFX has no tabular
    * figures, so numbers align only in a monospaced face.
    */
  case Mono extends TypeFamily("IBM Plex Mono", Some("mono"))

  /** Source Serif 4: methods prose only. */
  case Serif extends TypeFamily("Source Serif 4", Some("serif"))

/** A font weight the studio uses, as a CSS weight number. */
enum TypeWeight(val css: Int) derives CanEqual:
  case Regular  extends TypeWeight(400)
  case Medium   extends TypeWeight(500)
  case SemiBold extends TypeWeight(600)

/** One bundled static font file (ticket S1.2).
  *
  * JavaFX selects a weight only by family name: it maps every CSS weight below
  * bold to the regular face. Each static face therefore has its own family,
  * `javaFxFamily`, the legacy family name (name ID 1) in the file, which is
  * what `Font.getFamily` reports once the file is loaded.
  */
enum FontFace(
    val family: TypeFamily,
    val weight: TypeWeight,
    val fileName: String,
    val javaFxFamily: String
) derives CanEqual:
  case SansRegular
      extends FontFace(
        TypeFamily.Sans,
        TypeWeight.Regular,
        "IBMPlexSans-Regular.ttf",
        "IBM Plex Sans"
      )
  case SansMedium
      extends FontFace(
        TypeFamily.Sans,
        TypeWeight.Medium,
        "IBMPlexSans-Medium.ttf",
        "IBM Plex Sans Medm"
      )
  case SansSemiBold
      extends FontFace(
        TypeFamily.Sans,
        TypeWeight.SemiBold,
        "IBMPlexSans-SemiBold.ttf",
        "IBM Plex Sans SmBld"
      )
  case MonoRegular
      extends FontFace(
        TypeFamily.Mono,
        TypeWeight.Regular,
        "IBMPlexMono-Regular.ttf",
        "IBM Plex Mono"
      )
  case MonoMedium
      extends FontFace(
        TypeFamily.Mono,
        TypeWeight.Medium,
        "IBMPlexMono-Medium.ttf",
        "IBM Plex Mono Medm"
      )
  case SerifRegular
      extends FontFace(
        TypeFamily.Serif,
        TypeWeight.Regular,
        "SourceSerif4-Regular.ttf",
        "Source Serif 4"
      )
  case SerifSemiBold
      extends FontFace(
        TypeFamily.Serif,
        TypeWeight.SemiBold,
        "SourceSerif4-Semibold.ttf",
        "Source Serif 4 Semibold"
      )

object FontFace:

  /** The bundled faces of one family, lightest first. */
  def of(family: TypeFamily): List[FontFace] =
    values.toList.filter(_.family == family).sortBy(_.weight.css)

  /** The bundled face of `family` closest to `weight`, the lighter one on a
    * tie. The studio bundles Plex Mono at 400 and 500 only (DESIGN_SPEC
    * section 7; upstream also ships a SemiBold), so a 600 title set in Mono
    * uses 500. Source Serif 4 is bundled at 400 and 600, so 500 uses 400.
    */
  def nearest(family: TypeFamily, weight: TypeWeight): FontFace =
    // Every family has a regular face, so the list is never empty.
    of(family).minBy(f => (math.abs(f.weight.css - weight.css), f.weight.css))

/** The five sizes of the type scale (DESIGN_SPEC section 7). Nothing else
  * appears in studio CSS; `TypeScaleLint` (sbt `studioStyleCheck`) enforces it.
  */
enum TypeSize(val px: Int, val weight: TypeWeight, val use: String) derives CanEqual:
  case T11 extends TypeSize(11, TypeWeight.Regular, "labels and table headers")
  case T12 extends TypeSize(12, TypeWeight.Regular, "body text and controls")
  case T13 extends TypeSize(13, TypeWeight.SemiBold, "pane and section titles")
  case T16 extends TypeSize(16, TypeWeight.Regular, "section figures")
  case T28 extends TypeSize(28, TypeWeight.Medium, "the hero number")

  /** The style class that sets this size, e.g. `t13`. */
  def styleClass: String = s"t$px"

object TypeScale:

  /** The size of body text, set on the scene root. */
  val body: TypeSize = TypeSize.T12

  /** The family of every numeral in tables, readouts and identifiers. */
  val numerals: TypeFamily = TypeFamily.Mono

  /** The face that sets `family` at `size`. */
  def face(family: TypeFamily, size: TypeSize): FontFace =
    FontFace.nearest(family, size.weight)
