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
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import org.scalacheck.Gen

private[laws] object TemplateFamilyCodecFixture:
  private def get[E, A](value: Either[E, A]): A = value.toOption.get
  val keys: VersionedCodec[String]              =
    VersionedCodec.string(get(DefinitionId.of("example.template-key", 1)))
  val fixed: TemplateFitCodec[String, Vector[Double]] = TemplateFitCodec.fixed(keys)
  val meanMap: TemplateFitCodec[String, Mass[Px]]     = TemplateFitCodec.meanMap(keys)

class TemplateFitCodecLawSuite extends munit.DisciplineSuite:
  import TemplateFamilyCodecFixture.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val fixedPlans                        = for
    first    <- Gen.choose(1, 5)
    second   <- Gen.choose(1, 5)
    held     <- Gen.choose(-5, 5)
    grouped  <- Gen.oneOf(true, false)
    reverse  <- Gen.oneOf(true, false)
    overflow <- Gen.oneOf(true, false)
    imported <- Gen.oneOf(true, false)
  yield
    val basis = get(TemplateBasis.of("generated-template", Vector("x", "z"), "score"))
    def group(value: String) = Option.when(grouped)(value)
    val rows                 = Vector(
      get(TemplateObservation.of("x λ", "train", Vector(1.0, 0.0), first.toDouble, group("x"))),
      get(
        TemplateObservation.of("z λ", "train", Vector(0.0, 1.0), second.toDouble, group("z"))
      ),
      get(
        TemplateObservation.of(
          "held λ",
          "held",
          if overflow then Vector(1e308, 1e308) else Vector(2.0, 1.0),
          held.toDouble,
          group("held")
        )
      )
    ) ++
      Option
        .when(grouped)(
          get(
            TemplateObservation.of("excluded λ", "train", Vector(3.0, 4.0), 99.0, Some("held"))
          )
        )
        .toVector
    val design =
      if imported then TemplateDesign.importedLm(basis) else TemplateDesign.fixed(basis)
    val split = get(
      TemplateSplit.of(design, if reverse then rows.reverse else rows, Set("held"))
    )
    if imported then
      val receipt = get(
        TemplateImportedFit.of(
          split.training,
          split.training.hash.render,
          basis.columns,
          Vector(first.toDouble, second.toDouble),
          2,
          2,
          "generated external receipt"
        )
      )
      get(TemplateFitPlan.imported(split, receipt))
    else get(TemplateFitPlan.of(split))

  private val mapPlans = for
    x       <- Gen.choose(1, 4)
    y       <- Gen.choose(1, 4)
    first   <- Gen.choose(1, 5)
    second  <- Gen.choose(1, 5)
    reverse <- Gen.oneOf(true, false)
  yield
    val frame = get(Frame.screen("generated-template-map", 2, 1));
    val grid  = get(Grid.over(frame, 2, 1))
    def mass(a: Double, b: Double): Mass[Px] =
      val values = IArray(a, b)
      get(
        Surface
          .intensity(grid, values, Provenance.raw(ContentHash.of(values)))
          .flatMap(_.normalised)
      )
    val rows = Vector(
      get(
        TemplateObservation.of("a λ", "train", mass(x.toDouble, 1.0), first.toDouble, Some("a"))
      ),
      get(
        TemplateObservation
          .of("b λ", "train", mass(1.0, y.toDouble), second.toDouble, Some("b"))
      ),
      get(TemplateObservation.of("c λ", "held", mass(1.0, 1.0), 4.0, Some("c")))
    )
    val input = get(
      TemplateSplit.of(
        get(TemplateDesign.meanMap[Px]("participant", "score")),
        if reverse then rows.reverse else rows,
        Set("held")
      )
    )
    get(TemplateFitPlan.of(input))
  private def samePlan[X](
      a: TemplateFitPlan[String, X],
      b: TemplateFitPlan[String, X]
  ): Boolean =
    a.inputHash == b.inputHash && a.planHash == b.planHash && a.description == b.description
  private def sameArchive[X](
      a: TemplateFitArchive[String, X],
      b: TemplateFitArchive[String, X]
  ): Boolean =
    samePlan(
      a.plan,
      b.plan
    ) && a.validation.fitted.coefficients == b.validation.fitted.coefficients &&
      a.validation.evaluation.rows == b.validation.evaluation.rows
  checkAll(
    "fixed template plan",
    CodecLaws.roundTrip(fixed.plans, fixedPlans, samePlan[Vector[Double]])
  )
  checkAll(
    "fixed template result",
    CodecLaws.roundTrip(
      fixed.results,
      fixedPlans.map(p => get(TemplateFitArchive.run(p))),
      sameArchive[Vector[Double]]
    )
  )
  checkAll(
    "mean-map template plan",
    CodecLaws.roundTrip(meanMap.plans, mapPlans, samePlan[Mass[Px]])
  )
  checkAll(
    "mean-map template result",
    CodecLaws.roundTrip(
      meanMap.results,
      mapPlans.map(p => get(TemplateFitArchive.run(p))),
      sameArchive[Mass[Px]]
    )
  )
