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

package example

import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

/** An extension-owned parameter and score; no eyes4s implementation packages. */
final class Multiplier private (val value: Double)
object Multiplier:
  def of(value: Double): Either[String, Multiplier] =
    Either.cond(
      value.isFinite && value > 0,
      new Multiplier(value),
      s"Multiplier must be positive and finite, got $value."
    )

final class ScaledScore private (val value: Double)
object ScaledScore:
  def of(value: Double): Either[ComparisonValueError, ScaledScore] =
    Either.cond(
      value.isFinite,
      new ScaledScore(value),
      ComparisonValueError.NonFiniteSimilarity(value)
    )
  given ScoreMean[ScaledScore] with
    def mean(values: Vector[ScaledScore]): Either[ScoreMeanError, ScaledScore] =
      ScoreMean[Double]
        .mean(values.map(_.value))
        .flatMap(value =>
          of(value).left.map(ScoreMeanError.InvalidComparisonValue("scaled cosine", _))
        )
  given Contrastable[ScaledScore, SignedDifference] with
    val components = Vector("value")
    def subtract(
        matched: ScaledScore,
        control: ScaledScore
    ): Either[DifferenceError, SignedDifference] =
      SignedDifference.between(matched.value, control.value)

final case class SubjectItemKey(subject: String, item: Int, phase: String) derives CanEqual
object SubjectItemKey:
  given KeyDigest[SubjectItemKey] = KeyDigest.derived[SubjectItemKey]
  given Ordering[SubjectItemKey]  = Ordering.by(key => (key.subject, key.item, key.phase))

object CustomMethod:
  def parameterDescriptor
      : Either[DescriptorError, ParameterDescriptor[Double, Multiplier, String]] =
    ParameterInfo
      .of(
        "multiplier",
        2,
        "Positive dimensionless factor applied to cosine similarity",
        ParameterUnits.Dimensionless,
        ParameterDomain.PositiveFinite
      )
      .map(info => new ParameterDescriptor(info, Multiplier.of, identity))

  def describedMethod(
      id: DefinitionId
  ): Either[DescriptorError, StudyMethod[Multiplier, Px, ScaledScore, SignedDifference]] =
    for
      parameter <- parameterDescriptor
      fields    <- ParameterSet.of(
        Vector(parameter.bind[Multiplier](identity)(p => Provenance.Param.Num(p.value)))
      )
    yield
      val executable = method(id)
      val metadata   = MethodDescriptor.of[Multiplier, ScaledScore, SignedDifference](
        id,
        fields,
        p => executable.comparison(p).info,
        p =>
          ScoreComponent
            .of[ScaledScore, SignedDifference](
              "value",
              "Scaled cosine; matched minus control difference",
              ParameterUnits.Dimensionless,
              MeasureScale.Bounded(0, p.value),
              ScoreDirection.HigherIsCloser
            )(_.value, _.value)
            .map(Vector(_)),
        Set(
          ComparisonProperty.Symmetric,
          ComparisonProperty.NonNegative,
          ComparisonProperty.Bounded
        ),
        executable.capability
      )
      new StudyMethod(
        id,
        executable.name,
        executable.parameters,
        executable.execution,
        Some(metadata)
      )

  /** A bounded instance: the cosine cursor's work with a constant-time scaling
    * of its finished score, so the study runner can pause inside each pair.
    */
  def method(id: DefinitionId): StudyMethod[Multiplier, Px, ScaledScore, SignedDifference] =
    new StudyMethod(
      id,
      "Scaled cosine",
      p => Vector("multiplier" -> Provenance.Param.Num(p.value)),
      MethodExecution.Bounded((p: Multiplier) =>
        Distribution
          .cosine[Px]
          .mapScore(
            MeasureInfo(
              "Scaled cosine",
              "Cosine multiplied by an explicit positive factor",
              MeasureScale.Bounded(0, p.value),
              None
            )
          )(s =>
            ScaledScore
              .of(s.value * p.value)
              .left
              .map(CompareError.InvalidScore("scaled cosine", _))
          )
      ),
      None
    )

  def parameterCodec(schema: DefinitionId): VersionedCodec[Multiplier] =
    VersionedCodec.of[Multiplier](schema)(p =>
      Json.obj("multiplier" -> Json.fromDoubleOrNull(p.value))
    ) { json =>
      json.hcursor
        .get[Double]("multiplier")
        .left
        .map(e => CodecError.Field("multiplier", json, e.message))
        .flatMap(value =>
          Multiplier.of(value).left.map(e => CodecError.Field("multiplier", json, e))
        )
    }

  /** The extension's own score codec; a result archive registers it explicitly. */
  def scoreCodec(schema: DefinitionId): VersionedCodec[ScaledScore] =
    VersionedCodec.checked[ScaledScore](schema)(score =>
      Either.cond(
        score.value.isFinite,
        Json.obj("scaled" -> Json.fromDoubleOrNull(score.value)),
        CodecError.Field("scaled", Json.Null, "scaled cosine must be finite")
      )
    ) { json =>
      json.hcursor
        .get[Double]("scaled")
        .left
        .map(e => CodecError.Field("scaled", json, e.message))
        .flatMap(value =>
          ScaledScore.of(value).left.map(e => CodecError.Field("scaled", json, e.message))
        )
    }

  def keyCodec(schema: DefinitionId): VersionedCodec[SubjectItemKey] =
    VersionedCodec.of[SubjectItemKey](schema)(key =>
      Json.obj(
        "subject" -> Json.fromString(key.subject),
        "item"    -> Json.fromInt(key.item),
        "phase"   -> Json.fromString(key.phase)
      )
    ) { json =>
      for
        subject <- json.hcursor
          .get[String]("subject")
          .left
          .map(e => CodecError.Field("subject", json, e.message))
        item <- json.hcursor
          .get[Int]("item")
          .left
          .map(e => CodecError.Field("item", json, e.message))
        phase <- json.hcursor
          .get[String]("phase")
          .left
          .map(e => CodecError.Field("phase", json, e.message))
      yield SubjectItemKey(subject, item, phase)
    }

  def layout(id: DefinitionId): StudyLayout[SubjectItemKey] = new StudyLayout(
    id,
    Projection.named("subject")(_.subject),
    Projection.named("item")(_.item.toString),
    Projection.named("phase")(_.phase)
  )
