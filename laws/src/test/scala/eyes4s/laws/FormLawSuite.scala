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
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Norm, Px}
import eyes4s.plan.*
import org.scalacheck.{Gen, Test}

/** Runs the published form laws against every shipped one-number parameter
  * and every shipped recipe inspection, and shows by mutation that the laws
  * discriminate.
  *
  * {{{
  * | mutant (built through the public API)                  | falsified law                            |
  * |--------------------------------------------------------|------------------------------------------|
  * | sigma's lower bound closed at 0 instead of open        | bounds and constructor agree at the edges |
  * | the interpolation gap's lower bound open at 0          | bounds and constructor agree at the edges, |
  * |                                                        | raw form parses back                      |
  * | sigma with no lower bound                              | bounds and constructor agree at the edges |
  * | an I-VT threshold bounded below at 1, not 0            | bounds and constructor agree at the edges, |
  * |                                                        | raw form parses back                      |
  * | a field that reads its value back doubled              | raw form parses back                      |
  * }}}
  *
  * Mutants of the library source, each applied, run against this suite,
  * `LegacyDescriptorSuite`, `FormDescriptorSuite`, `MethodDescriptorSuite`
  * and `PlanarUnitSuite` on the JVM, and reverted; every one was killed:
  *
  * {{{
  * | source mutant                                          | killed by (among others)                     |
  * |--------------------------------------------------------|----------------------------------------------|
  * | Endpoint.admits swaps closed and open                  | every field: bounds and constructor agree    |
  * | parse skips the declared-bounds stage                  | every field: outside the bounds is refused   |
  * | OutOfBounds names the other side                       | every field: outside the bounds is refused   |
  * | shipped interpolation gap bound Closed(0) -> Open(0)   | interpolationGap: agree at the edges         |
  * | shipped minimum duration bound Open(0) -> Closed(0)    | minimumDuration: agree at the edges          |
  * | shipped sigma loses its lower bound                    | sigma*: agree at the edges (shape extreme)   |
  * | canonical number text drops the sign                   | every field: raw form parses back            |
  * | a group's Ordered rule admits equal parts              | FormDescriptorSuite group checks             |
  * | legacy projection reads Closed(0) duration as positive | LegacyDescriptorSuite pins                   |
  * | UnitLabel[Deg] reports Px                              | PlanarUnitSuite, MethodDescriptorSuite       |
  * }}}
  *
  * CR6b's form and text mutants (the study, temporal and recording forms,
  * advisories, methods text and labels), each applied and reverted:
  *
  * {{{
  * | source mutant                                     | killed by                                   |
  * |---------------------------------------------------|---------------------------------------------|
  * | sigma-in-cells threshold 2 -> 1                   | StudyFormSuite advisories                   |
  * | near-uniform advisory never raised                | StudyFormSuite advisories                   |
  * | MissingAngularScale keyed to Scales               | StudyFormSuite whole-recipe checks          |
  * | off-window policy without a window accepted       | StudyFormSuite whole-recipe checks          |
  * | control token tagged Match in the sentence        | StudyFormSuite sentence roles               |
  * | recording area writes its label as its id         | recording form rebuild, EventRecording form |
  * | temporal window writes its start as its end       | temporal form fields, TemporalStudy form    |
  * | study values drop the units per degree            | StudyFormSuite fixture round trip           |
  * | study window writes yMin as xMin                  | study form fields and rebuild               |
  * | Weight label is its token                         | StudyFormSuite labels                       |
  * | host projection drops the recipe-parameter cause  | StudyFormSuite erased error                 |
  * | unmatched-focal refusals point to no form field   | study form: refusal keys point into it      |
  * | a window without a declared frame gets invented   | StudyFormKeysSuite undeclared window frame  |
  * | FormValues keeps Absent entries                   | StudyFormSuite fixture round trip           |
  * }}}
  */
class FormLawSuite extends munit.DisciplineSuite:
  import PlanCodecLawSuite.*

  private val F = RecipeParameters.forms

  private def numeric[E, N, A](name: String, field: NumericField[E, N, A]): Unit =
    checkAll(name, FormLaws.numeric(field, FormLaws.numbers(field)))

  numeric("sigma[Px]", F.sigma[Px])
  numeric("sigma[Deg]", F.sigma[Deg])
  numeric("sigma[Norm]", F.sigma[Norm])
  numeric("sigmaX[Px]", F.sigmaX[Px])
  numeric("sigmaY[Deg]", F.sigmaY[Deg])
  numeric("residualLimit", F.residualLimit)
  numeric("ivtThreshold", F.ivtThreshold)
  numeric("minimumDuration", F.minimumDuration)
  numeric("idtWidth", F.idtWidth)
  numeric("idtHeight", F.idtHeight)
  numeric("ekEtaX", F.ekEtaX)
  numeric("ekEtaY", F.ekEtaY)
  numeric("ekMinimumSamples", F.ekMinimumSamples)
  numeric("interpolationGap", F.interpolationGap)

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

  checkAll(
    "study inspection",
    FormLaws.inspection(
      Gen.oneOf(cosinePlans, configuredPlans, initialFixationPlans).map(_.inspect)
    )
  )
  checkAll("trial-keyed study inspection", FormLaws.inspection(trialPlans.map(_.inspect)))
  checkAll("temporal inspection", FormLaws.inspection(temporalPlans.map(_.inspect)))
  checkAll(
    "recording inspection",
    FormLaws.inspection(
      Gen.oneOf(
        recordingPlans(ivt.method, ivtParameters).map(_.inspect),
        recordingPlans(idt.method, idtParameters).map(_.inspect),
        recordingPlans(ek.method, ekParameters).map(_.inspect)
      )
    )
  )

  checkAll(
    "study form",
    FormLaws.studyForm(Gen.oneOf(cosinePlans, configuredPlans, initialFixationPlans))
  )
  checkAll("trial-keyed study form", FormLaws.studyForm(trialPlans))
  checkAll(
    "study form fields",
    FormLaws.form(
      "studyFormFields",
      Gen.oneOf(cosinePlans, configuredPlans, initialFixationPlans)
    ) { plan =>
      val f = new StudyForm(StudyFormContext.of(plan))
      (f.fields.map(_._2), f.values(plan))
    }
  )
  checkAll("temporal form", FormLaws.temporalForm(temporalPlans))
  checkAll(
    "recording form",
    FormLaws.recordingForm(recordingPlans(ivt.method, ivtParameters))
  )
  checkAll(
    "temporal form fields",
    FormLaws.form("temporalFormFields", temporalPlans) { plan =>
      val f = new TemporalForm
      (f.fields, f.values(plan))
    }
  )
  checkAll(
    "recording form fields",
    FormLaws.form(
      "recordingFormFields",
      Gen.oneOf(
        recordingPlans(ivt.method, ivtParameters).map(p => new RecordingForm().values(p)),
        recordingPlans(ek.method, ekParameters).map(p => new RecordingForm().values(p))
      )
    )(values => (new RecordingForm().fields, values))
  )

  // --- Stored forms (CR6c) ---------------------------------------------------

  private val probes = Vector(
    RawValue.Absent,
    RawValue.Number(""),
    RawValue.Number("x"),
    RawValue.Number("0"),
    RawValue.Number("-1"),
    RawValue.Number("1e400"),
    RawValue.Number("9223372036854775808"),
    RawValue.Choice("none"),
    RawValue.Flag(false),
    RawValue.Text(" ")
  )

  private val ids =
    Gen.oneOf("a", "b", "c", "window", "scales", "x.y", "é").map(id => get(FieldId.of(id)))
  private val leaves: Gen[RawValue] = Gen.oneOf(
    Gen.oneOf("", "0", "-0", "1.5", "2e0", "abc", "9007199254740993").map(RawValue.Number(_)),
    Gen.alphaStr.map(RawValue.Choice(_)),
    Gen.oneOf(true, false).map(RawValue.Flag(_)),
    Gen.oneOf("", " ", "été · P17", "\"quoted\"").map(RawValue.Text(_)),
    Gen.const(RawValue.Absent)
  )
  private def raws(depth: Int): Gen[RawValue] =
    if depth == 0 then leaves
    else
      val parts = Gen.listOfN(2, Gen.zip(ids, raws(depth - 1))).map(_.toVector)
      Gen.frequency(
        3 -> leaves,
        1 -> parts.map(RawValue.Group(_)),
        1 -> Gen.zip(Gen.alphaStr, parts).map(RawValue.Variant(_, _)),
        1 -> Gen.listOfN(3, raws(depth - 1)).map(v => RawValue.Items(v.toVector))
      )
  private val anyValues: Gen[FormValues] =
    Gen.listOfN(4, Gen.zip(ids, raws(2))).map(xs => FormValues.from(xs.toMap))

  private val shippedForms: Gen[(FormView, FormValues)] = Gen.oneOf(
    Gen.oneOf(cosinePlans, configuredPlans, initialFixationPlans).map { plan =>
      val f = new StudyForm(StudyFormContext.of(plan))
      FormView(DefinitionId.study, f.views) -> f.values(plan)
    },
    temporalPlans.map { plan =>
      val f = new TemporalForm
      FormView(DefinitionId.study, f.views) -> f.values(plan)
    },
    Gen
      .oneOf(
        recordingPlans(ivt.method, ivtParameters),
        recordingPlans(ek.method, ekParameters)
      )
      .map { plan =>
        val f = new RecordingForm()
        FormView(recordingSchema, f.views) -> f.values(plan)
      },
    Gen.zip(Gen.const(FormFixtures.view), anyValues),
    Gen.const(FormFixtures.view -> FormFixtures.values)
  )

  checkAll("stored forms", FormLaws.stored(shippedForms, probes))
  checkAll(
    "form view",
    CodecLaws.roundTrip(
      FormCodecs.view,
      shippedForms.map(_._1),
      (a: FormView, b: FormView) => a == b
    )
  )
  checkAll(
    "form values",
    CodecLaws.roundTrip(
      FormCodecs.values,
      Gen.oneOf(shippedForms.map(_._2), anyValues),
      (a: FormValues, b: FormValues) => a == b
    )
  )

  /** Mutant checks run from a fixed seed, so every kill is reproducible. */
  private val parameters =
    Test.Parameters.default.withMinSuccessfulTests(100).withInitialSeed(0x464f524dL)

  private def falsified[E, N, A](field: NumericField[E, N, A]): Vector[String] =
    FormLaws
      .numeric(field, FormLaws.numbers(field))
      .all
      .properties
      .toVector
      .collect {
        case (name, prop) if !Test.check(parameters, prop).passed =>
          name.drop(name.indexOf('.') + 1)
      }
      .sorted

  private def get[A](e: Either[DescriptorError, A]): A = e.fold(x => fail(x.message), identity)
  private val agree = "the declared bounds and the domain constructor agree at the edges"
  private val back  = "a typed value's raw form parses back to it"

  private def mutant[E, N: Numeral, A](
      original: NumericField[E, N, A],
      bounds: NumericBounds,
      number: A => N = null
  )(message: E => String): NumericField[E, N, A] =
    get(
      NumericField.of[E, N, A](
        original.view.id,
        original.view.version,
        original.view.meaning,
        original.quantity,
        bounds
      )(original.domain, Option(number).getOrElse(original.number), message)
    )

  test("every shipped field passes, so each mutant below differs by one change") {
    assertEquals(falsified(F.sigma[Px]), Vector.empty)
    assertEquals(falsified(F.interpolationGap), Vector.empty)
    assertEquals(falsified(F.ivtThreshold), Vector.empty)
  }

  test("closing sigma's open lower bound is caught at the edge") {
    val closed = mutant(F.sigma[Px], NumericBounds.nonNegative)(_.message)
    assertEquals(falsified(closed), Vector(agree))
  }

  test("opening the interpolation gap's closed lower bound is caught at the edge") {
    val open = mutant(F.interpolationGap, NumericBounds.positive)(_.message)
    // The constructor still builds a zero gap, whose raw form the bounds refuse.
    assertEquals(falsified(open), Vector(back, agree))
  }

  test("dropping sigma's lower bound is caught at the shape's extreme") {
    val unbounded = mutant(F.sigma[Px], NumericBounds.unbounded)(_.message)
    assertEquals(falsified(unbounded), Vector(agree))
  }

  test("a lower bound stricter than the constructor is caught at the edge") {
    val strict = mutant(F.ivtThreshold, get(NumericBounds.atLeast(1)))(_.message)
    assertEquals(falsified(strict), Vector(back, agree))
  }

  test("a field that reads its value back wrongly fails the raw round trip") {
    val doubled = mutant(F.sigma[Px], NumericBounds.positive, (s: Sigma[Px]) => s.value * 2)(
      _.message
    )
    assertEquals(falsified(doubled), Vector(back))
  }
