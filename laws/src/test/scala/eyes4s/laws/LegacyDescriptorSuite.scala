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

package eyes4s.laws

import eyes4s.codec.*
import eyes4s.compare.{MeasureScale, Similarity}
import eyes4s.design.SignedDifference
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** The deprecated `ParameterUnits`/`ParameterDomain` projections of every
  * shipped field, pinned exactly, and the deprecated constructors'
  * translation into `Quantity`/`FieldKind`.
  *
  * The suite is itself deprecated, so it exercises the deprecated API
  * without a `@nowarn`; it goes when the legacy vocabulary does.
  *
  * CR6a made the legacy values projections of the new vocabulary. These
  * projections changed with it, each to the value the field's shape now
  * implies (all other shipped projections are unchanged):
  *
  * {{{
  * | field                         | before                                   | after                                              |
  * |-------------------------------|------------------------------------------|----------------------------------------------------|
  * | input, layout, method, source,| Mixed                                    | NominalIdentity (units)                            |
  * | detector, angularFrame,       |                                          |                                                    |
  * | clocks, temporal.input/scope  |                                          |                                                    |
  * | phases                        | Mixed                                    | NominalIdentity (units)                            |
  * | frame, admission, window      | Mixed                                    | Spatial(px) (units)                                |
  * | offWindow                     | Mixed, DomainValue("OffWindowPolicy")    | Dimensionless, Alternatives(Exclude, FailTrial)    |
  * | angularScale                  | NominalReference                         | DomainValue("parts: frameId, unitsPerDegree")      |
  * | pairing                       | Mixed, DomainValue("StudyPairing")       | Dimensionless, DomainValue("parts: matched, ...")  |
  * | initialFixations              | DomainValue("InitialFixationPolicy")     | Alternatives(keepAll, dropFirst, dropLeading...)   |
  * | scale.i                       | Mixed, DomainValue("StudyScale")         | Spatial(deg), Alternatives(binned, gaussian, ...)  |
  * | estimate.i                    | Mixed, DomainValue("StudyEstimate")      | Spatial(px), Alternatives(binned, gaussian, ...)   |
  * | failurePolicy                 | DomainValue("FailurePolicy.success...")  | PositiveFinite                                     |
  * | detector.minimumSamples       | DomainValue("EkMinimumSamples.of")       | PositiveFinite                                     |
  * | RecipeParameters.frame        | DomainValue("Frame.of")                  | OrderedFiniteBounds                                |
  * | RecipeParameters.area         | DomainValue("RecordingArea.of")          | OrderedFiniteBounds                                |
  * | RecipeParameters.relativeWindow | DomainValue("Window.of")               | PositiveHalfOpenWindow                             |
  * | RecipeParameters.gaussian     | Mixed, DomainValue("Sigma.of and ...")   | Spatial(u), DomainValue("parts: sigma, edges")     |
  * | RecipeParameters.anisotropic  | Mixed, DomainValue("Sigma.of and ...")   | Spatial(u), DomainValue("parts: sigmaX, ...")      |
  * }}}
  */
@deprecated("Exercises the deprecated descriptor vocabulary", "0.1.0")
class LegacyDescriptorSuite extends munit.DisciplineSuite:
  import PlanCodecLawSuite.*
  import ParameterDomain.*
  import ParameterUnits.*

  private val nominal                  = (NominalIdentity, NominalReference)
  private def choices(tokens: String*) = (Dimensionless, Alternatives(tokens.toVector))
  private val estimates = Alternatives(Vector("binned", "gaussian", "anisotropic"))

  /** Every shipped field of a pixel-frame plan, by id (`.i` for an index). */
  private val expected: Map[String, (ParameterUnits, ParameterDomain)] = Map(
    "input"            -> nominal,
    "layout"           -> nominal,
    "method"           -> nominal,
    "phases"           -> (NominalIdentity, DistinctNonEmptyPhases),
    "weight"           -> choices("Uniform", "Duration"),
    "failurePolicy"    -> (Dimensionless, PositiveFinite),
    "frame"            -> (Spatial("px"), OrderedFiniteBounds),
    "grid"             -> (Cells, PositiveGridDimensions),
    "admission"        -> (Spatial("px"), OrderedFiniteBounds),
    "window"           -> (Spatial("px"), OrderedFiniteBounds),
    "offWindow"        -> choices("Exclude", "FailTrial"),
    "angularScale"     -> (Mixed, DomainValue("parts: frameId, unitsPerDegree")),
    "pairing"          -> (Dimensionless, DomainValue("parts: matched, controls, unmatched")),
    "initialFixations" -> (
      Mixed,
      Alternatives(Vector("keepAll", "dropFirst", "dropLeadingInClosedDisc"))
    ),
    "scale.i"                        -> (Spatial("deg"), estimates),
    "estimate.i"                     -> (Spatial("px"), estimates),
    "sigma"                          -> (Spatial("px"), PositiveFinite),
    "edges"                          -> choices("Truncate", "Renormalise"),
    "temporal.input"                 -> nominal,
    "temporal.boundary"              -> choices("ClipDuration", "FullyContained"),
    "temporal.scope"                 -> nominal,
    "window.i"                       -> (Microseconds, PositiveHalfOpenWindow),
    "repetition.i"                   -> (NominalIdentity, DistinctNonEmptyPhases),
    "source"                         -> nominal,
    "clocks"                         -> nominal,
    "angularFrame"                   -> nominal,
    "viewing"                        -> (Millimetres, PositivePhysicalDimensions),
    "syncModel"                      -> choices("OffsetOnly", "Affine"),
    "residualLimitMicros"            -> (Microseconds, NonNegativeMicroseconds),
    "interpolationGapMicros"         -> (Microseconds, NonNegativeMicroseconds),
    "detector"                       -> nominal,
    "detector.thresholdDegPerSecond" -> (PerSecond("deg"), PositiveFinite),
    "detector.minimumDurationMicros" -> (Microseconds, PositiveMicroseconds),
    "detector.extentWidthDeg"        -> (Spatial("deg"), PositiveFinite),
    "detector.extentHeightDeg"       -> (Spatial("deg"), PositiveFinite),
    "detector.etaXDegPerSecond"      -> (PerSecond("deg"), PositiveFinite),
    "detector.etaYDegPerSecond"      -> (PerSecond("deg"), PositiveFinite),
    "detector.minimumSamples"        -> (Dimensionless, PositiveFinite),
    "sync.i"                         -> (Microseconds, SignedMicroseconds),
    "area.i"                         -> (Spatial("px"), OrderedFiniteBounds)
  )

  private def key(id: String): String = id.replaceAll("\\.[0-9]+$", ".i")

  private def fields(fs: Vector[InspectedParameter]): Vector[InspectedParameter] =
    fs.flatMap(f => f +: fields(f.children))

  private def projections(i: Either[DescriptorError, RecipeInspection]) =
    i.fold(e => fail(e.message), r => fields(r.fields))
      .map(f => key(f.info.id) -> (f.info.units, f.info.allowed))

  private val recordingSchema = definition("eyes4s.recording-plan", 1)
  private val ivt             = RecordingCodecs.ivt(
    recordingSchema,
    definition("eyes4s.recording.ivt", 1),
    definition("eyes4s.ivt-parameters", 1)
  )
  private val idt = RecordingCodecs.idt(
    recordingSchema,
    definition("eyes4s.recording.idt", 1),
    definition("eyes4s.idt-parameters", 1)
  )
  private val ek = RecordingCodecs.engbertKliegl(
    recordingSchema,
    definition("eyes4s.recording.engbert-kliegl", 1),
    definition("eyes4s.engbert-kliegl-parameters", 1)
  )

  private val inspections: Gen[Either[DescriptorError, RecipeInspection]] = Gen.oneOf(
    Gen.oneOf(cosinePlans, configuredPlans, initialFixationPlans).map(_.inspect),
    temporalPlans.map(_.inspect),
    recordingPlans(ivt.method, ivtParameters).map(_.inspect),
    recordingPlans(idt.method, idtParameters).map(_.inspect),
    recordingPlans(ek.method, ekParameters).map(_.inspect)
  )

  property("every shipped field projects to its pinned legacy units and domain") {
    forAll(inspections) { i =>
      projections(i).foreach((id, found) => assertEquals(Some(found), expected.get(id), id))
    }
  }

  test("every pinned field is reached by some shipped inspection") {
    val reached = Iterator
      .continually(inspections.sample)
      .flatten
      .take(400)
      .flatMap(projections)
      .map(_._1)
      .toSet
    assertEquals(expected.keySet -- reached, Set.empty[String])
  }

  test("the recipe parameters outside the inspection tables project as pinned") {
    assertEquals(
      Vector(
        RecipeParameters.frame[Px].info,
        RecipeParameters.bounds[Px].info,
        RecipeParameters.area.info,
        RecipeParameters.relativeWindow.info,
        RecipeParameters.studyWindow.info,
        RecipeParameters.repetition.info,
        RecipeParameters.syncMark.info,
        RecipeParameters.perspective.info,
        RecipeParameters.gaussian[Px].info,
        RecipeParameters.anisotropic[Px].info,
        RecipeParameters.ekMinimumSamples.info,
        RecipeParameters.sigmaX[Px].info,
        RecipeParameters.sigmaY[Px].info
      ).map(i => (i.units, i.allowed)),
      Vector(
        (Spatial("px"), OrderedFiniteBounds),
        (Spatial("px"), OrderedFiniteBounds),
        (Spatial("px"), OrderedFiniteBounds),
        (Microseconds, PositiveHalfOpenWindow),
        (Microseconds, PositiveHalfOpenWindow),
        (NominalIdentity, DistinctNonEmptyPhases),
        (Microseconds, SignedMicroseconds),
        (Millimetres, PositivePhysicalDimensions),
        (Spatial("px"), DomainValue("parts: sigma, edges")),
        (Spatial("px"), DomainValue("parts: sigmaX, sigmaY, edges")),
        (Dimensionless, PositiveFinite),
        (Spatial("px"), PositiveFinite),
        (Spatial("px"), PositiveFinite)
      )
    )
  }

  test("the legacy constructor translates every translatable domain") {
    def kind(units: ParameterUnits, domain: ParameterDomain) =
      ParameterInfo.of("x", 1, "meaning", units, domain).map(_.kind)
    val positive = NumericBounds.positive
    assertEquals(
      kind(Spatial("deg"), PositiveFinite),
      Right(FieldKind.Numeric(Quantity.Planar(PlanarUnit.Deg), NumberShape.Real, positive))
    )
    assertEquals(
      kind(PerSecond("px"), NonNegativeFinite),
      Right(
        FieldKind.Numeric(
          Quantity.Rate(PlanarUnit.Px),
          NumberShape.Real,
          NumericBounds.nonNegative
        )
      )
    )
    assertEquals(
      kind(Microseconds, PositiveMicroseconds),
      Right(FieldKind.Numeric(Quantity.Duration, NumberShape.Int64, positive))
    )
    assertEquals(
      kind(Microseconds, SignedMicroseconds),
      Right(FieldKind.Numeric(Quantity.Duration, NumberShape.Int64, NumericBounds.unbounded))
    )
    assertEquals(
      kind(Cells, PositiveFinite),
      Right(FieldKind.Numeric(Quantity.Count(Counted.Cells), NumberShape.Int32, positive))
    )
    assertEquals(
      kind(Dimensionless, Alternatives(Vector("a", "b"))),
      Right(
        FieldKind.Choice(
          ChoiceSource.Fixed(Vector(ChoiceOption("a", "a"), ChoiceOption("b", "b")))
        )
      )
    )
    assertEquals(kind(Mixed, NominalReference), Right(FieldKind.Reference))
    // Each translation projects back to what it was given.
    val translated = ParameterInfo.of("x", 1, "meaning", Microseconds, NonNegativeMicroseconds)
    assertEquals(
      translated.map(i => (i.units, i.allowed)),
      Right((Microseconds, NonNegativeMicroseconds))
    )
  }

  test("a legacy domain with no translation is refused, naming the field") {
    assertEquals(
      ParameterInfo.of("frame", 1, "meaning", Mixed, DomainValue("Frame.of")).map(_.kind),
      Left(DescriptorError.UntranslatableLegacy("frame", "Mixed", "DomainValue(Frame.of)"))
    )
    assertEquals(
      ParameterInfo.of("n", 1, "meaning", NominalIdentity, PositiveFinite).map(_.kind),
      Left(DescriptorError.UntranslatableLegacy("n", "NominalIdentity", "PositiveFinite"))
    )
    assert(ParameterInfo.of("w", 1, "meaning", Spatial("furlong"), PositiveFinite).isLeft)
  }

  test("legacy score components and component values keep their units") {
    val component = ScoreComponent
      .of[Similarity, SignedDifference](
        "value",
        "meaning",
        Microseconds,
        MeasureScale.DistanceLike,
        ScoreDirection.LowerIsCloser
      )(_.value, _.value)
      .fold(e => fail(e.message), identity)
    assertEquals(component.quantity, Quantity.Duration)
    assertEquals(component.units, Microseconds)
    assertEquals(
      ComponentValue(
        "v",
        Quantity.Planar(PlanarUnit.Deg),
        MeasureScale.DistanceLike,
        ScoreDirection.LowerIsCloser,
        1
      ).units,
      Spatial("deg")
    )
  }
