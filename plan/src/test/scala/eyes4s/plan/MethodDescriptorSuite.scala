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

import eyes4s.compare.*
import eyes4s.detect.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.*
import eyes4s.surface.EdgePolicy
import scala.compiletime.testing.typeCheckErrors

class MethodDescriptorSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)

  test(
    "descriptor construction delegates sigma, grid, bounds and perspective to domain constructors"
  ) {
    val sigma = RecipeParameters.sigma[Px]
    Vector(-1.0, 0.0, Double.PositiveInfinity, 0.5).foreach { x =>
      assertEquals(
        sigma.construct(x),
        Sigma.px(x).left.map(RecipeParameterError.Geometry.apply)
      )
    }
    assert(sigma.construct(Double.NaN).left.exists {
      case RecipeParameterError.Geometry(GeometryError.NonFiniteSigma(x)) => x.isNaN
      case _                                                              => false
    })
    val frame = get(Frame.screen("screen", 20, 10))
    Vector((0, 2), (2, -1), (65536, 65536), (3, 5)).foreach { case (nx, ny) =>
      val raw = (GridId("g"), frame, nx, ny)
      assertEquals(
        RecipeParameters.grid[Px].construct(raw),
        Grid.of(raw._1, raw._2, nx, ny).left.map(RecipeParameterError.Geometry.apply)
      )
    }
    assert(RecipeParameters.bounds[Px].parse((0, 0, 0, 10)).isLeft)
    val failure = RecipeParameters.sigma[Px].parse(0).swap.toOption.get
    assertEquals(failure.field.id, "sigma")
    assertEquals(failure.input, 0.0)
    assert(failure.message.contains("sigma"))
    assert(RecipeParameters.perspective.parse((0, 10, 10)).isLeft)
    assertEquals(
      get(RecipeParameters.perspective.construct((600, 400, 300))),
      get(Perspective.millimetres(600, 400, 300))
    )
  }

  test("typed I-VT durations, thresholds, windows and policies retain exact boundaries") {
    assert(RecipeParameters.minimumDuration.parse(Span.zero).isLeft)
    assert(RecipeParameters.interpolationGap.parse(Span.micros(-1)).isLeft)
    assertEquals(
      get(RecipeParameters.interpolationGap.construct(Span.zero)),
      InterpolationGap.none
    )
    val zero = get(Velocity.perSecond[Deg](0))
    assert(RecipeParameters.ivtThreshold.parse(zero).isLeft)
    val threshold = get(Velocity.perSecond[Deg](30))
    assertEquals(
      get(RecipeParameters.ivtThreshold.construct(threshold)),
      get(IvtThreshold.of(threshold))
    )
    val w = get(
      RecipeParameters.relativeWindow.construct(
        (Span.micros(-10), Span.micros(9007199254740993L))
      )
    )
    assertEquals(w.until.toMicros, 9007199254740993L)
    assert(
      RecipeParameters.studyWindow.parse(("empty", get(Window.of(Span.zero, Span.zero)))).isLeft
    )
    assert(RecipeParameters.relativeWindow.parse((Span.micros(2), Span.micros(1))).isLeft)
    assert(RecipeParameters.repetition.parse(("same", "recall", "recall")).isLeft)
    assert(RecipeParameters.failurePolicy.parse(Some(0)).isLeft)
    assertEquals(
      get(RecipeParameters.failurePolicy.construct(Some(2))),
      get(FailurePolicy.successfulOnly(2))
    )
  }

  test("recipe constructors retain checked Gaussian, frame, synchronization and AOI values") {
    val bounds = get(RecipeParameters.bounds[Px].construct((0, 0, 800, 600)))
    val frame  = get(RecipeParameters.frame[Px].construct((FrameId("typed"), bounds, YAxis.Up)))
    assertEquals(frame.id, FrameId("typed"))
    assertEquals(frame.yAxis, YAxis.Up)
    val gaussian = get(RecipeParameters.gaussian[Px].construct((2, EdgePolicy.Renormalise)))
    assertEquals(gaussian, StudyEstimate.Gaussian(get(Sigma.px(2)), EdgePolicy.Renormalise))
    assert(RecipeParameters.gaussian[Px].parse((0, EdgePolicy.Truncate)).isLeft)
    assert(RecipeParameters.syncMark.parse(("", Instant.micros(0), Instant.micros(1))).isLeft)
    val mark = get(
      RecipeParameters.syncMark.construct(
        ("trigger", Instant.micros(9007199254740993L), Instant.micros(0))
      )
    )
    assertEquals(mark.onSource.toMicros, 9007199254740993L)
    assert(RecipeParameters.residualLimit.parse(Span.micros(-1)).isLeft)
    assertEquals(get(RecipeParameters.residualLimit.construct(Span.zero)).span, Span.zero)
    assert(RecipeParameters.area.parse(("", "Image", bounds)).isLeft)
    assertEquals(
      get(RecipeParameters.area.construct(("image", "Image", bounds))).bounds,
      bounds
    )
  }

  test("closed choices, units and defaults make scientific conventions explicit") {
    assertEquals(RecipeParameters.sigma[Px].info.units, ParameterUnits.Spatial("px"))
    assertEquals(RecipeParameters.sigma[Deg].info.units, ParameterUnits.Spatial("deg"))
    assertEquals(RecipeParameters.ivtThreshold.info.units, ParameterUnits.PerSecond("deg"))
    assertEquals(
      RecipeParameters.edges.info.allowed,
      ParameterDomain.Alternatives(Vector("Truncate", "Renormalise"))
    )
    assertEquals(
      get(RecipeParameters.edges.construct(EdgePolicy.Renormalise)),
      EdgePolicy.Renormalise
    )
    assert(RecipeParameters.sigma[Px].default.isEmpty)
    assert(RecipeParameters.ivtThreshold.default.isEmpty)
    assert(NamedParameterDefault.of("guess", 1.0, "").isLeft)
    assert(
      ParameterInfo
        .of("", 1, "meaning", ParameterUnits.Cells, ParameterDomain.PositiveGridDimensions)
        .isLeft
    )
    assert(
      ParameterInfo
        .of("id", 0, "meaning", ParameterUnits.Cells, ParameterDomain.PositiveGridDimensions)
        .isLeft
    )
    assert(
      ParameterInfo
        .of(
          "id",
          1,
          "meaning",
          ParameterUnits.Cells,
          ParameterDomain.Alternatives(Vector("a", "a"))
        )
        .isLeft
    )
  }

  test("cosine score meaning and detector scientific card are reused without stronger claims") {
    val descriptor = MethodDescriptor.cosine[Px](DefinitionId.cosine)
    assertEquals(descriptor.info(()), Distribution.cosine[Px].info)
    assertEquals(get(descriptor.components(())).map(_.id), Vector("value"))
    val component = get(descriptor.components(())).head
    assertEquals(component.range, MeasureScale.Bounded(0, 1))
    assertEquals(component.direction, ScoreDirection.HigherIsCloser)
    assertEquals(component.score(get(Similarity.of(0.75))), 0.75)
    assertEquals(component.difference(get(SignedDifference.between(0.75, 0.25))), 0.5)
    assertEquals(
      descriptor.properties,
      Set(
        ComparisonProperty.Symmetric,
        ComparisonProperty.NonNegative,
        ComparisonProperty.Bounded
      )
    )
    assertEquals(descriptor.execution, ExecutionCapability.SynchronousWholeOperation)
    val recording = RecordingMethod.ivt(get(DefinitionId.of("ivt", 1))).descriptor.get
    assertEquals(recording.card, AlgorithmCards.ivt)
    assert(recording.card.citations.nonEmpty)
    assertEquals(
      recording.parameters.fields.map(_.descriptor.info.id),
      Vector("thresholdDegPerSecond", "minimumDurationMicros")
    )
  }

  test(
    "a hidden or misencoded parameter is rejected, including same names with different values"
  ) {
    val set = get(
      ParameterSet.of(
        Vector(
          RecipeParameters
            .sigma[Px]
            .bind[Sigma[Px]](identity)(x => Provenance.Param.Num(x.value))
        )
      )
    )
    val sigma = get(Sigma.px(2))
    assert(set.verify(sigma, Vector.empty).isLeft) // omission mutant
    assert(set.verify(sigma, Vector("sigma" -> Provenance.Param.Num(3))).isLeft) // value mutant
    assert(set.verify(sigma, Vector("sigma" -> Provenance.Param.Num(2))).isRight)
    assert(ParameterSet.of(set.fields ++ set.fields).isLeft)
    val descriptor = MethodDescriptor.cosine[Px](DefinitionId.cosine)
    assert(descriptor.verify((), Vector.empty, Vector("unexpected")).isLeft)
  }

  test("structured score components keep distinct units and directions") {
    final case class Score(agreement: Double, delay: Double)
    final case class Delta(agreement: Double, delay: Double)
    val agreement = get(
      ScoreComponent.of[Score, Delta](
        "agreement",
        "Agreement and matched-minus-control agreement",
        ParameterUnits.Dimensionless,
        MeasureScale.Bounded(0, 1),
        ScoreDirection.HigherIsCloser
      )(_.agreement, _.agreement)
    )
    val delay = get(
      ScoreComponent.of[Score, Delta](
        "delay",
        "Delay and matched-minus-control delay",
        ParameterUnits.Microseconds,
        MeasureScale.DistanceLike,
        ScoreDirection.LowerIsCloser
      )(_.delay, _.delay)
    )
    val descriptor = MethodDescriptor.of[Unit, Score, Delta](
      get(DefinitionId.of("structured", 1)),
      ParameterSet.empty,
      _ =>
        MeasureInfo(
          "structured",
          "Separate agreement and delay",
          MeasureScale.Bounded(0, 1),
          None
        ),
      _ => Right(Vector(agreement, delay))
    )
    assert(descriptor.verify((), Vector.empty, Vector("agreement", "delay")).isRight)
    val components = get(descriptor.components(()))
    assertEquals(
      components.map(_.units),
      Vector(ParameterUnits.Dimensionless, ParameterUnits.Microseconds)
    )
    assertEquals(components.map(_.score(Score(0.75, 100))), Vector(0.75, 100.0))
    assertEquals(components.map(_.difference(Delta(0.25, -50))), Vector(0.25, -50.0))
  }

  test("wrong units and wrong raw parameters fail compilation") {
    assert(typeCheckErrors("""
      import eyes4s.plan.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.*
      val pixels = Velocity.perSecond[Px](30).toOption.get
      RecipeParameters.ivtThreshold.construct(pixels)
    """).nonEmpty)
    assert(typeCheckErrors("""
      import eyes4s.plan.*
      RecipeParameters.minimumDuration.construct(10.0)
    """).nonEmpty)
    assert(typeCheckErrors("""
      import eyes4s.plan.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.*
      val frame = Frame.screen("s", 2, 2).toOption.get
      RecipeParameters.grid[Deg].construct((GridId("g"), frame, 2, 2))
    """).nonEmpty)
  }
