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

import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.results.*

/** Two components of an impostor score type. */
final case class TwoComponent(first: Double, second: Double) derives CanEqual
final case class TwoDifference(first: Double, second: Double) derives CanEqual

object TwoComponent:
  given ScoreMean[TwoComponent] with
    def mean(values: Vector[TwoComponent]): Either[ScoreMeanError, TwoComponent] =
      for
        a <- ScoreMean[Double].mean(values.map(_.first))
        b <- ScoreMean[Double].mean(values.map(_.second))
      yield TwoComponent(a, b)
  given Contrastable[TwoComponent, TwoDifference] with
    val components = Vector("first", "second")
    def subtract(m: TwoComponent, c: TwoComponent): Either[DifferenceError, TwoDifference] =
      Right(TwoDifference(m.first - c.first, m.second - c.second))

/** The review's reproducer: the plan registry decodes the ordinary cosine plan
  * with an impostor method that claims the cosine identity but scores another
  * type, beside the real cosine result codec. The description names only the
  * method's identity and parameters, so it agrees; re-evaluating the report
  * must still be a typed refusal, never a ClassCastException.
  */
class ReportImpostorSuite extends munit.FunSuite:
  import ManifestFixtures.*

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)

  private val keys    = StudyCodecs.key(DefinitionId.studyKey)
  private val reports = ReportCodecs.report(keys)

  private val comparison = new Compare[Mass[Px], Mass[Px], TwoComponent]:
    val info = MeasureInfo(
      "impostor",
      "Two components under the cosine identity",
      MeasureScale.UnboundedSimilarity,
      None
    )
    def compare(x: Mass[Px], y: Mass[Px]): Either[CompareError, TwoComponent] =
      Right(TwoComponent(1.0, 2.0))

  private def component(
      name: String,
      read: TwoComponent => Double,
      delta: TwoDifference => Double
  ) =
    get(
      ScoreComponent.of[TwoComponent, TwoDifference](
        name,
        s"the $name component",
        ParameterUnits.Dimensionless,
        MeasureScale.UnboundedSimilarity,
        ScoreDirection.HigherIsCloser
      )(read, delta)
    )

  private def impostor(described: Boolean) =
    val id = ComparisonMethods.cosine.id
    new StudyMethod[Unit, Px, TwoComponent, TwoDifference](
      id,
      "impostor",
      _ => Vector.empty,
      _ => comparison,
      Option.when(described)(
        MethodDescriptor.of[Unit, TwoComponent, TwoDifference](
          id,
          ParameterSet.empty,
          _ => comparison.info,
          _ =>
            Right(
              Vector(
                component("first", _.first, _.first),
                component("second", _.second, _.second)
              )
            )
        )
      )
    )

  private def decoders(described: Boolean): ArtifactDecoders[StudyKey, Px] =
    val plans = new StudyCodec(
      DefinitionId.study,
      StudyKey.layout(DefinitionId.studyLayout),
      StudyCodecs.key(DefinitionId.studyKey),
      impostor(described),
      VersionedCodec.unit(DefinitionId.unit)
    )
    ArtifactDecoders
      .of(
        get(StudyRegistry.empty[StudyKey, Px].register(plans.registration)),
        get(StudyInputRegistry.empty[StudyKey, Px].register(inputs)),
        get(StudyResultRegistry.empty[StudyKey, Px].register(results.registration))
      )
      .withReports(reports)

  private val spec = get(
    ReportSpec.of(
      get(ReportId.of("by item")),
      0,
      get(ReportSelection.of(Vector(Role.Difference), Vector("value"))),
      groupBy = Vector(Grouping.ByLevel(LevelTerm.Layout(LayoutField.Item)))
    )
  )

  private val saved =
    val source = get(
      ReportSources
        .study(studies, inputs, results)(plan, input, result, None, CovariateSchema.empty)
    )
    val report = get(Report.evaluate(spec, source))
    get(
      SavedManifest.of(
        Vector(
          planArtifact,
          inputArtifact,
          resultArtifact,
          get(StoredArtifact.reportSpec("spec", spec)),
          get(StoredArtifact.report("report", reports, report))
        ),
        Vector(
          ManifestRelation.PlanInput(name("plan"), name("input")),
          ManifestRelation.ResultOf(name("result"), name("plan"), name("input")),
          ManifestRelation.ReportOf(
            name("report"),
            name("spec"),
            name("result"),
            name("input"),
            None
          )
        )
      )
    )

  private def resolved(described: Boolean) =
    ArtifactResolver
      .resolve(saved.address, saved.source, decoders(described))
      .left
      .map(_.toVector)

  test("an impostor plan method with other components is a typed refusal, not a throw") {
    val refused = resolved(described = true)
    assert(
      refused.left.exists(_.exists {
        case ResolveError.Relation(
              ManifestRelation.ReportOf(_, _, _, _, _),
              RelationMismatch.ReportComponents(Vector("first", "second"), Vector("value"))
            ) =>
          true
        case _ => false
      }),
      refused
    )
  }

  test("an undescribed impostor is refused as well; the ordinary decoders resolve") {
    val refused = resolved(described = false)
    assert(
      refused.left.exists(_.exists {
        case ResolveError
              .Relation(_, RelationMismatch.ReportComponents(Vector(), Vector("value"))) =>
          true
        case _ => false
      }),
      refused
    )
    assert(
      ArtifactResolver
        .resolve(saved.address, saved.source, ManifestFixtures.decoders.withReports(reports))
        .isRight
    )
  }
