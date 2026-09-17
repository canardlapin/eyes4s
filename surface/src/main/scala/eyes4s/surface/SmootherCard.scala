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

package eyes4s.surface

/** Units of one smoother parameter, so that a method-selection UI can label
  * the control and a methods section can print the value with its dimension.
  */
enum SmootherParameterUnits derives CanEqual:

  /** The spatial unit of the frame the smoother runs in -- `px`, `deg` or
    * `norm` -- fixed by the smoother's type parameter rather than restated
    * here, which is how [[eyes4s.kernel.Sigma]] keeps a bandwidth comparable
    * across studies.
    */
  case FrameUnits

  /** One of a closed set of named alternatives. */
  case Alternatives(values: Vector[String])

  def render: String = this match
    case FrameUnits       => "frame units"
    case Alternatives(vs) => vs.mkString("one of {", ", ", "}")

/** One parameter of a smoother, as its card describes it.
  *
  * `name` is the key under which the running value appears in
  * [[Smoother.configuration]] and in the provenance of every estimate, so a
  * reader can match the description to the number.
  */
final case class SmootherParameter(
    name: String,
    meaning: String,
    units: SmootherParameterUnits
) derives CanEqual

/** A card was declared with text a reader could not use. */
enum SmootherCardError derives CanEqual:
  case EmptyText(field: String)
  case InvalidParameters(names: Vector[String])

  def message: String = this match
    case EmptyText(f)          => s"A smoother card needs a non-empty $f."
    case InvalidParameters(ns) =>
      s"A smoother card needs unique, non-empty parameter names, got $ns."

/** Scientific identity of a smoother: what a method-selection UI shows and a
  * methods section cites, without either hardcoding a table (PRD APP-15,
  * APP-16, A-21).
  *
  * The shape follows the detector `AlgorithmCard` in `eyes4s-detect` and the
  * `MeasureInfo` in `eyes4s-compare`: a stable id, display text, the
  * parameters with their units, the conventions an implementation commits to,
  * and a citation. The citation is optional here where a detector's is
  * mandatory: a paper-named detector has a paper, while a Gaussian kernel
  * estimate is textbook material and this repository's documentation cites no
  * primary source for it. A card records what is known, not what would look
  * respectable.
  */
final case class SmootherCard private (
    id: String,
    name: String,
    summary: String,
    parameters: Vector[SmootherParameter],
    bandwidthRule: String,
    conventions: Vector[String],
    citation: Option[String]
) derives CanEqual:

  def parameterNames: Vector[String] = parameters.map(_.name)

  def render: String =
    s"$name (${parameters.map(p => s"${p.name}: ${p.units.render}").mkString(", ")})"

object SmootherCard:

  def of(
      id: String,
      name: String,
      summary: String,
      parameters: Vector[SmootherParameter],
      bandwidthRule: String,
      conventions: Vector[String],
      citation: Option[String]
  ): Either[SmootherCardError, SmootherCard] =
    val names = parameters.map(_.name)
    if id.trim.isEmpty then Left(SmootherCardError.EmptyText("id"))
    else if name.trim.isEmpty then Left(SmootherCardError.EmptyText("name"))
    else if summary.trim.isEmpty then Left(SmootherCardError.EmptyText("summary"))
    else if bandwidthRule.trim.isEmpty then Left(SmootherCardError.EmptyText("bandwidthRule"))
    else if names.isEmpty || names.exists(
        _.trim.isEmpty
      ) || names.distinct.length != names.length
    then Left(SmootherCardError.InvalidParameters(names))
    else if parameters.exists(_.meaning.trim.isEmpty) then
      Left(SmootherCardError.EmptyText("parameter meaning"))
    else if citation.exists(_.trim.isEmpty) then Left(SmootherCardError.EmptyText("citation"))
    else
      Right(
        new SmootherCard(id, name, summary, parameters, bandwidthRule, conventions, citation)
      )

  private[surface] def literal(
      id: String,
      name: String,
      summary: String,
      parameters: Vector[SmootherParameter],
      bandwidthRule: String,
      conventions: Vector[String],
      citation: Option[String]
  ): SmootherCard =
    new SmootherCard(id, name, summary, parameters, bandwidthRule, conventions, citation)

/** Canonical cards for the smoothers shipped by eyes4s. */
object SmootherCards:

  val gaussian: SmootherCard = SmootherCard.literal(
    "eyes4s.surface.gaussian",
    "Separable isotropic Gaussian kernel density estimate",
    "Bins a weighted point measure onto the grid, then convolves it with a " +
      "normalised, truncated one-dimensional Gaussian along each axis in turn.",
    Vector(
      SmootherParameter(
        "sigma",
        "Standard deviation of the Gaussian kernel, in the frame's units. " +
          "Never a full width, a half width, or a backend's private multiple.",
        SmootherParameterUnits.FrameUnits
      ),
      SmootherParameter(
        "edges",
        "What happens to kernel mass that falls outside the grid: Truncate " +
          "loses it, so an edge point's estimate sums to less than its mass; " +
          "Renormalise rescales each point's kernel so the part landing on the " +
          "grid carries the whole of its mass.",
        SmootherParameterUnits.Alternatives(EdgePolicy.values.toVector.map(_.toString))
      )
    ),
    "sigma is a standard deviation in frame units, chosen explicitly by the " +
      "caller or by a named rule (Bandwidth.silverman, Bandwidth.scott, " +
      "Bandwidth.foveal); nothing selects it implicitly. It must be at least one " +
      "fifth of the smaller cell side, or the estimate is refused as " +
      "EstimateError.DegenerateBandwidth rather than returned as a histogram.",
    Vector(
      "separable: two one-dimensional passes, along x then along y",
      "each one-dimensional kernel is truncated at ceil(3 sigma / cell) cells, " +
        "never fewer than one, and normalised to sum to one",
      "Renormalise is a source-side correction whose on-grid share factorises per axis",
      "the result is an Intensity in the input's mass units; density normalises it separately"
    ),
    None
  )

  /** Every shipped smoother, so a test can insist that none lacks a card. */
  val all: Vector[SmootherCard] = Vector(gaussian)
