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

import eyes4s.core.Weight
import eyes4s.kernel.*
import eyes4s.surface.EdgePolicy

/** The [[FieldView]]s of the shipped recipe fields, as data. Numeric
  * parameters that parse on their own are [[NumericField]]s in
  * [[RecipeParameters]]; the recipe-level views here describe shape, bounds,
  * rules and library defaults, and are parsed in CR6b.
  */
private[plan] object RecipeViews:
  import FieldKind.*

  def view(
      id: String,
      meaning: String,
      kind: FieldKind,
      default: Option[DefaultValue] = None
  ): FieldView = FieldView.literal(id, meaning, kind, default)

  def reference(id: String, meaning: String): FieldView = view(id, meaning, Reference)
  def text(id: String, meaning: String): FieldView      = view(id, meaning, Text)

  def number(
      id: String,
      meaning: String,
      quantity: Quantity,
      shape: NumberShape,
      bounds: NumericBounds = NumericBounds.unbounded
  ): FieldView = view(id, meaning, Numeric(quantity, shape, bounds))

  def coordinate(id: String, meaning: String, unit: PlanarUnit): FieldView =
    number(id, meaning, Quantity.Planar(unit), NumberShape.Real)

  def positive(id: String, meaning: String, quantity: Quantity): FieldView =
    number(id, meaning, quantity, NumberShape.Real, NumericBounds.positive)

  def signedMicros(id: String, meaning: String): FieldView =
    number(id, meaning, Quantity.Duration, NumberShape.Int64)

  def atLeastOne(id: String, meaning: String, counted: Counted): FieldView =
    number(id, meaning, Quantity.Count(counted), NumberShape.Int32, one)

  private val one = NumericBounds.atLeastOne

  def options[A](values: Vector[A], token: A => String): ChoiceSource =
    ChoiceSource.Fixed(values.map(a => ChoiceOption(token(a), token(a))))

  def choice[A](
      id: String,
      meaning: String,
      values: Vector[A],
      token: A => String = (a: A) => a.toString,
      default: Option[DefaultValue] = None
  ): FieldView = view(id, meaning, Choice(options(values, token)), default)

  def phase(id: String, meaning: String): FieldView =
    view(id, meaning, Choice(ChoiceSource.FromInput(InputDomain.Phases)))

  def library(raw: RawValue, reason: String): Option[DefaultValue] =
    Some(DefaultValue.literal(raw, reason))

  private def fid(s: String) = FieldId.literal(s)

  val yAxis: FieldView =
    choice("yAxis", "Direction of increasing y on the frame", YAxis.values.toVector)

  /** `xMin`, `yMin`, `xMax`, `yMax` in `unit`, strictly increasing on each axis. */
  def box(
      id: String,
      meaning: String,
      unit: PlanarUnit,
      lead: Vector[FieldView],
      trail: Vector[FieldView] = Vector.empty
  ): FieldView =
    view(
      id,
      meaning,
      Group(
        lead ++ Vector(
          coordinate("xMin", "Left edge, inclusive", unit),
          coordinate("yMin", "Lower-y edge, inclusive", unit),
          coordinate("xMax", "Right edge, exclusive", unit),
          coordinate("yMax", "Upper-y edge, exclusive", unit)
        ) ++ trail,
        GroupRule.Ordered(Vector(fid("xMin") -> fid("xMax"), fid("yMin") -> fid("yMax")))
      )
    )

  def frameBox(id: String, meaning: String, unit: PlanarUnit, withUnit: Boolean): FieldView =
    box(
      id,
      meaning,
      unit,
      Vector(reference("frameId", "Nominal frame identity")) ++
        Option.when(withUnit)(reference("unit", "The frame's spatial unit")).toVector,
      Vector(yAxis)
    )

  def grid: FieldView =
    view(
      "grid",
      "Nominal grid and frame; columns nx and rows ny, row-major cells",
      Group(
        Vector(
          reference("gridId", "Nominal grid identity"),
          atLeastOne("nx", "Grid columns", Counted.Cells),
          atLeastOne("ny", "Grid rows", Counted.Cells)
        ),
        GroupRule.Independent
      )
    )

  def edges: FieldView =
    choice(
      "edges",
      "Truncate loses kernel mass outside the grid; Renormalise preserves each point's mass",
      EdgePolicy.values.toVector
    )

  def weight: FieldView =
    choice(
      "weight",
      "Per-fixation mass: uniform count or measured duration",
      Weight.values.toVector
    )

  def estimateCases(unit: PlanarUnit): Vector[VariantCase] =
    val sigma = Quantity.Planar(unit)
    Vector(
      VariantCase("binned", "Binned occupancy, no smoothing", Vector.empty),
      VariantCase(
        "gaussian",
        "Isotropic Gaussian",
        Vector(positive("sigma", "Gaussian standard deviation", sigma), edges)
      ),
      VariantCase(
        "anisotropic",
        "Axis-aligned Gaussian",
        Vector(
          positive("sigmaX", "Gaussian standard deviation along x", sigma),
          positive("sigmaY", "Gaussian standard deviation along y", sigma),
          edges
        )
      )
    )

  def failurePolicy: FieldView =
    view(
      "failurePolicy",
      "Require every selected pair, or explicitly require a minimum successful count",
      Optional(
        atLeastOne(
          "minimumSuccessful",
          "The fewest successful scores a reduction needs",
          Counted.Scores
        ),
        "Every selected pair must succeed"
      )
    )

  def phases(id: String, meaning: String, lead: Vector[FieldView] = Vector.empty): FieldView =
    view(
      id,
      meaning,
      Group(
        lead ++ Vector(
          phase("focal", "The focal (query) phase"),
          phase("reference", "The reference phase")
        ),
        GroupRule.Distinct(Vector(fid("focal"), fid("reference")))
      )
    )

  private val occurrence: FieldView =
    view(
      "occurrence",
      "Which occurrence of the matched item is the reference",
      Variant(
        Vector(
          VariantCase("first", "The first occurrence", Vector.empty),
          VariantCase("last", "The last occurrence", Vector.empty),
          VariantCase(
            "at",
            "A numbered occurrence",
            Vector(
              atLeastOne("n", "The occurrence number, 1 for the first", Counted.Occurrences)
            )
          )
        )
      )
    )

  def pairing: FieldView =
    val defaultReason = "StudyPairing.default"
    view(
      "pairing",
      "Matched-reference rule, control pool and handling of focal trials without a match",
      Group(
        Vector(
          view(
            "matched",
            "How a focal trial's matched references are chosen",
            Variant(
              Vector(
                VariantCase("requireOne", "Require exactly one", Vector.empty),
                VariantCase("sameOccurrence", "The same occurrence", Vector.empty),
                VariantCase("select", "Select one occurrence", Vector(occurrence)),
                VariantCase("meanOfAll", "Average every matched reference", Vector.empty)
              )
            ),
            library(RawValue.Variant("requireOne", Vector.empty), defaultReason)
          ),
          choice(
            "controls",
            "Which references of other items are controls",
            ControlReferences.values.toVector,
            _.name,
            library(RawValue.Choice(ControlReferences.SameSelection.name), defaultReason)
          ),
          choice(
            "unmatched",
            "Focal trials without a matched reference",
            UnmatchedFocalPolicy.values.toVector,
            _.name,
            library(RawValue.Choice(UnmatchedFocalPolicy.ReportNoMatch.name), defaultReason)
          )
        ),
        GroupRule.Independent
      )
    )

  def initialFixations(unit: PlanarUnit): FieldView =
    view(
      "initialFixations",
      "Initial fixations left out of every trial, focal and reference alike, before the " +
        "window is considered: the first fixation, or the leading run whose centres lie in the " +
        "closed disc of the given radius in degrees around the fixation cross (x, y in " +
        "admission-frame units); absent when every fixation is kept",
      Variant(
        Vector(
          VariantCase("keepAll", "Keep every fixation", Vector.empty),
          VariantCase("dropFirst", "Leave out the first fixation", Vector.empty),
          VariantCase(
            "dropLeadingInClosedDisc",
            "Leave out the leading fixations near the fixation cross",
            Vector(
              coordinate("crossX", "Fixation cross x on the admission frame", unit),
              coordinate("crossY", "Fixation cross y on the admission frame", unit),
              positive("radius", "Disc radius in degrees", Quantity.Planar(PlanarUnit.Deg))
            )
          )
        )
      ),
      library(
        RawValue.Variant("keepAll", Vector.empty),
        "StudyPlan.configure keeps every fixation unless a policy is given"
      )
    )

  def angularScale(unit: PlanarUnit): FieldView =
    view(
      "angularScale",
      "Declared linear units per degree on the admission frame; not a calibration",
      Group(
        Vector(
          reference("frameId", "The frame the scale is declared on"),
          positive("unitsPerDegree", "Linear units per degree", Quantity.UnitsPerDegree(unit))
        ),
        GroupRule.Independent
      )
    )

  def perspective: FieldView =
    view(
      "viewing",
      "Viewing distance, physical width and height in millimetres",
      Group(
        Vector(
          positive("distance", "Viewing distance", Quantity.Length(LengthUnit.Millimetres)),
          positive("width", "Physical display width", Quantity.Length(LengthUnit.Millimetres)),
          positive("height", "Physical display height", Quantity.Length(LengthUnit.Millimetres))
        ),
        GroupRule.Independent
      )
    )

  def syncModel: FieldView =
    choice(
      "syncModel",
      "Fit the declared clock mapping from named common marks",
      SyncFitMode.values.toVector
    )

  def syncMark(id: String, meaning: String): FieldView =
    view(
      id,
      meaning,
      Group(
        Vector(
          text("name", "The mark's common name"),
          signedMicros("source", "The mark on the source clock"),
          signedMicros("target", "The mark on the target clock")
        ),
        GroupRule.Independent
      )
    )

  def area(id: String, meaning: String): FieldView =
    box(
      id,
      meaning,
      PlanarUnit.Px,
      Vector(text("areaId", "The AOI's identity"), text("label", "The AOI's display label"))
    )

  def window(id: String, meaning: String): FieldView =
    view(
      id,
      meaning,
      Group(
        Vector(
          text("name", "The window's name"),
          signedMicros("start", "Offset of the window start from the anchor, inclusive"),
          signedMicros("end", "Offset of the window end from the anchor, exclusive")
        ),
        GroupRule.Ordered(Vector(fid("start") -> fid("end")))
      )
    )

  def relativeWindow: FieldView =
    view(
      "window",
      "Half-open signed offsets relative to a measured trial anchor",
      Group(
        Vector(
          signedMicros("start", "Offset of the window start from the anchor, inclusive"),
          signedMicros("end", "Offset of the window end from the anchor, exclusive")
        ),
        GroupRule.Ordered(Vector(fid("start") -> fid("end")))
      )
    )
