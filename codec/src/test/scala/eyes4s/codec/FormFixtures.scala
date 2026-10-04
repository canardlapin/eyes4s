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

package eyes4s.codec

import eyes4s.kernel.{LengthUnit, PlanarUnit}
import eyes4s.plan.*

/** The values pinned by form-view-v1.json and form-values-v1.json: a view
  * with every field kind, quantity, shape, endpoint, choice source and group
  * rule, and values with every raw-value case.
  */
object FormFixtures:
  private def get[A](e: Either[DescriptorError, A]): A =
    e.fold(error => throw IllegalStateException(error.message), identity)
  private def id(value: String): FieldId = get(FieldId.of(value))
  private def view(
      name: String,
      meaning: String,
      kind: FieldKind,
      default: Option[DefaultValue] = None
  ): FieldView = get(FieldView.of(id(name), 1, meaning, kind, default))

  private def numeric(name: String, q: Quantity, s: NumberShape, b: NumericBounds): FieldView =
    view(name, s"the $name", FieldKind.Numeric(q, s, b))

  private val sigma = view(
    "sigma",
    "the smoothing bandwidth",
    FieldKind
      .Numeric(Quantity.Planar(PlanarUnit.Deg), NumberShape.Real, NumericBounds.positive),
    Some(get(DefaultValue.of(RawValue.Number("1"), "the guide's bandwidth")))
  )
  private val xMin = numeric(
    "xMin",
    Quantity.Planar(PlanarUnit.Px),
    NumberShape.Real,
    get(NumericBounds.of(Some(Endpoint.Closed(-0.5)), Some(Endpoint.Open(1920.0))))
  )
  private val xMax =
    numeric("xMax", Quantity.Planar(PlanarUnit.Px), NumberShape.Real, NumericBounds.unbounded)

  val view: FormView = FormView(
    DefinitionId.study,
    Vector(
      sigma,
      numeric(
        "speed",
        Quantity.Rate(PlanarUnit.Deg),
        NumberShape.Real,
        NumericBounds.nonNegative
      ),
      numeric(
        "ppd",
        Quantity.UnitsPerDegree(PlanarUnit.Px),
        NumberShape.Real,
        NumericBounds.positive
      ),
      numeric(
        "distance",
        Quantity.Length(LengthUnit.Millimetres),
        NumberShape.Real,
        NumericBounds.positive
      ),
      numeric("gap", Quantity.Duration, NumberShape.Int64, NumericBounds.nonNegative),
      numeric(
        "cells",
        Quantity.Count(Counted.Cells),
        NumberShape.Int32,
        NumericBounds.atLeastOne
      ),
      numeric("ratio", Quantity.Dimensionless, NumberShape.Real, NumericBounds.unbounded),
      numeric(
        "patch",
        Quantity.Planar(PlanarUnit.Mm),
        NumberShape.Real,
        NumericBounds.positive
      ),
      numeric(
        "normed",
        Quantity.Planar(PlanarUnit.Norm),
        NumberShape.Real,
        NumericBounds.positive
      ),
      numeric(
        "screen",
        Quantity.Length(LengthUnit.Centimetres),
        NumberShape.Real,
        NumericBounds.positive
      ),
      numeric(
        "eye",
        Quantity.Length(LengthUnit.Metres),
        NumberShape.Real,
        NumericBounds.positive
      ),
      numeric(
        "samples",
        Quantity.Count(Counted.Samples),
        NumberShape.Int64,
        NumericBounds.nonNegative
      ),
      numeric(
        "scores",
        Quantity.Count(Counted.Scores),
        NumberShape.Int32,
        NumericBounds.atLeastOne
      ),
      view(
        "weight",
        "how fixations are weighted",
        FieldKind.Choice(
          ChoiceSource.Fixed(
            Vector(ChoiceOption("duration", "Duration"), ChoiceOption("count", "Count"))
          )
        )
      ),
      view(
        "focal",
        "the focal phase",
        FieldKind.Choice(ChoiceSource.FromInput(InputDomain.Phases))
      ),
      view("flip", "whether to flip", FieldKind.Toggle),
      view("label", "a name", FieldKind.Text),
      view("input", "the study input", FieldKind.Reference),
      view(
        "window",
        "the analysis window",
        FieldKind.Group(Vector(xMin, xMax), GroupRule.Ordered(Vector(xMin.id -> xMax.id)))
      ),
      view(
        "phases",
        "the compared phases",
        FieldKind.Group(
          Vector(
            view("query", "the query phase", FieldKind.Text),
            view("reference", "the reference phase", FieldKind.Text)
          ),
          GroupRule.Distinct(Vector(id("query"), id("reference")))
        )
      ),
      view(
        "named",
        "a named group",
        FieldKind.Group(Vector(view("part", "a part", FieldKind.Toggle)), GroupRule.Independent)
      ),
      view(
        "angle",
        "the units per degree",
        FieldKind.Optional(
          numeric(
            "value",
            Quantity.UnitsPerDegree(PlanarUnit.Px),
            NumberShape.Real,
            NumericBounds.positive
          ),
          "scales stay in frame units"
        )
      ),
      view(
        "scales",
        "the bandwidths",
        FieldKind.Repeated(
          numeric(
            "scale",
            Quantity.Planar(PlanarUnit.Norm),
            NumberShape.Real,
            NumericBounds.positive
          ),
          1,
          Some(8)
        )
      ),
      view(
        "marks",
        "the marks",
        FieldKind.Repeated(view("mark", "a mark", FieldKind.Text), 0, None)
      ),
      view(
        "initial",
        "the initial fixations",
        FieldKind.Variant(
          Vector(
            VariantCase("keepAll", "Keep every fixation", Vector.empty),
            VariantCase(
              "dropFirst",
              "Drop the first",
              Vector(
                numeric(
                  "count",
                  Quantity.Count(Counted.Occurrences),
                  NumberShape.Int32,
                  NumericBounds.atLeastOne
                )
              )
            )
          )
        )
      )
    )
  )

  val values: FormValues = FormValues.of(
    id("sigma")  -> RawValue.Number("1.5"),
    id("ratio")  -> RawValue.Number(""),
    id("gap")    -> RawValue.Number("9007199254740993"),
    id("weight") -> RawValue.Choice("duration"),
    id("flip")   -> RawValue.Flag(true),
    id("label")  -> RawValue.Text("été · P17"),
    id("window") -> RawValue.Group(
      Vector(id("xMin") -> RawValue.Number("0"), id("xMax") -> RawValue.Absent)
    ),
    id("initial") -> RawValue.Variant("dropFirst", Vector(id("count") -> RawValue.Number("1"))),
    id("scales")  -> RawValue.Items(Vector(RawValue.Number("0.5"), RawValue.Number("2e0")))
  )
