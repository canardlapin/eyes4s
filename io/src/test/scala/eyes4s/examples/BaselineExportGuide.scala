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

package eyes4s.examples

import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.*

/** Public adapters emit the same checked tables in tests and the actual artifact command. */
object BaselineExportGuide:
  private def checked[E, A](e: Either[E, A]): Either[String, A] = e.left.map(_.toString)
  private def id(name: String) = checked(DefinitionId.of("example.export." + name, 1))
  val participant: String      = "participant, \"λ🧠\"\nsecond line"
  def tables: Either[String, Vector[(String, ResultTable)]] = for
    keyId     <- id("key"); layoutId <- id("layout"); recipeId <- id("fixed-recipe");
    learnedId <- id("learned-recipe")
    pointId  <- id("point-plan"); pointResultId <- id("point-result");
    repeatId <- id("repetition")
    pId      <- id("participant"); sId          <- id("stimulus"); oId <- id("occasion")
    keys = StudyCodecs.key(keyId)
    frame  <- checked(Frame.screen("export-map", 2, 2)); grid <- checked(Grid.over(frame, 2, 2))
    masses <- Vector(
      Vector(1.0, 0.0, 0.0, 0.0),
      Vector(0.0, 1.0, 0.0, 0.0),
      Vector(.25, .75, 0.0, 0.0)
    ).traverse { v =>
      val a = IArray.from(v);
      checked(
        Surface.intensity(grid, a, Provenance.raw(ContentHash.of(a))).flatMap(_.normalised)
      )
    }
    baseKeys = Vector("A", "B", "C").map(s => StudyKey(participant, s, "source"))
    trials   = Trials(baseKeys.zipWithIndex.map((k, i) => Trial(k, (), i.toDouble + 1)))
    spec <- checked(
      EvaluationSpec.of(
        "example.sum",
        "1",
        Vector.empty,
        Vector("value"),
        EvaluationGeometry.Independent,
        EvaluationTime.OrderFree
      )
    )
    paired = pair(
      trials,
      trials,
      PairDesign.BetweenDirected(Relation.all[StudyKey, StudyKey], Selection.All)
    )
    evaluated = evaluatePairs(
      paired,
      ContentHash.ofString("export-analytic-pairs"),
      EvaluationInfo("example sum", EvaluationScale.Unitless, Some(spec))
    ) { (a, b) =>
      if a == 3 then Left(CompareError.ZeroNorm("analytic failure", a, b)) else Right(a + b)
    }
    pairTable <- checked(
      BaselineExports.pairs[StudyKey, Px, Double, SignedDifference](
        evaluated,
        keys,
        ScoreColumns.scalar,
        "unitless"
      )
    )
    reduced = evaluated.meanByLeft(FailurePolicy.RequireAll)
    reductionTable <- checked(
      BaselineExports.reductions[StudyKey, Px, Double, SignedDifference](
        reduced,
        evaluated,
        keys,
        ScoreColumns.scalar,
        "unitless"
      )
    )
    compared      <- checked(contrast(reduced, reduced))
    contrastTable <- checked(
      BaselineExports.contrasts[StudyKey, Px, Double, SignedDifference](
        compared,
        evaluated,
        evaluated,
        keys,
        ScoreColumns.scalar,
        "unitless"
      )
    )
    multi     <- checked(MultiMatchScore.of(.1, .2, .3, .4, .5))
    multiSpec <- checked(
      EvaluationSpec.of(
        "example.components",
        "1",
        Vector.empty,
        ScoreColumns.multiMatch.names,
        EvaluationGeometry.Independent,
        EvaluationTime.OrderFree
      )
    )
    structured = evaluatePairs(
      paired,
      ContentHash.ofString("export-analytic-components"),
      EvaluationInfo("named components", EvaluationScale.Unitless, Some(multiSpec))
    ) { (a, b) =>
      if a == 3 then Left(CompareError.ZeroNorm("analytic failure", a, b)) else Right(multi)
    }
    structuredTable <- checked(
      BaselineExports.pairs[StudyKey, Px, MultiMatchScore, MultiMatchDifference](
        structured,
        keys,
        ScoreColumns.multiMatch,
        "unitless"
      )
    )
    clock = ClockId("export-time")
    fixes <- Vector(0L, 10000L, 20000L).zipWithIndex.traverse { (t, i) =>
      for
        span <- checked(Interval.of(clock, Instant.micros(t), Instant.micros(t + 5000)))
        fix  <- checked(
          Event.Fixation.withoutDispersion(span, Pt[Px](if i == 0 then .5 else 1.5, .5), 1)
        )
      yield fix
    }
    path <- checked(Scanpath.of(frame, clock, IArray.from(fixes)))
    input = StudyInput(Trials(baseKeys.map(k => Trial(k, (), path))))
    fields <- masses.traverse(m => checked(Surface.signed(grid, m.values, m.provenance)))
    templates = Trials(
      baseKeys.zip(fields).map((k, f) => Trial(k.copy(phase = "template"), (), f))
    )
    bins <- checked(
      PointBins.of(
        clock,
        Vector(0L, 10000L, 20000L).map(Instant.micros),
        PointBinEndpoint.IncludeFinalEndpoint
      )
    )
    successful <- checked(FailurePolicy.successfulOnly(1))
    pointSpec  <- checked(
      PointSamplingSpec.of(
        clock,
        Vector(0L, 10000L, 20000L, 9007199254740993L, Long.MaxValue).map(Instant.micros),
        Some(bins),
        DensityNormalization.None,
        DensityLookupPolicy.NearestClampedRIndex,
        TrajectoryEndpoint.OnsetRange,
        PointControlSelection.Candidates(Selection.All),
        successful,
        successful
      )
    )
    pointPlan  = PointSamplingPlan.of(StudyKey.layout(layoutId), input, templates, pointSpec)
    pointCodec = new PointSamplingCodec[StudyKey, Px](
      pointId,
      pointResultId,
      StudyKey.layout(layoutId),
      keys
    )
    pointTables <- checked(
      BaselineExports.points(PointSamplingArchive.run(pointPlan), pointCodec)
    )
    repeatLayout <- checked(
      RepetitionLayout.of[StudyKey, String, String, String](
        layoutId,
        pId,
        Projection.named("participant")(_.participant),
        sId,
        Projection.named("stimulus")(_.stimulus),
        oId,
        Projection.named("occasion")(_.phase)
      )
    )
    repeatRegistry <- checked(RepetitionRegistry.empty[StudyKey].register(repeatLayout))
    repeatRows =
      for phase <- Vector("first", "second"); (k, m) <- baseKeys.zip(masses)
      yield Trial(k.copy(phase = phase), (), m)
    repeatPlan <- checked(
      RepetitionPlan.of(
        repeatLayout,
        RepetitionRelations.withinParticipant,
        MapSimilarityMethod.Cosine,
        Selection.All,
        FailurePolicy.RequireAll,
        grid,
        Trials(repeatRows)
      )
    )
    repeatTables <- checked(
      BaselineExports.repetition(
        repeatPlan,
        RepetitionPlanCodec.of[StudyKey, Px](repeatId, repeatRegistry, keys),
        keys
      )
    )
    basis <- checked(
      TemplateBasis.of("declared amplitude", Vector("amplitude"), "response units")
    )
    observations <- Vector((1.0, 2.0, "train"), (2.0, 4.0, "train"), (3.0, 7.0, "held"))
      .zip(baseKeys)
      .traverse { (r, k) => checked(TemplateObservation.of(k, r._3, Vector(r._1), r._2)) }
    split <- checked(TemplateSplit.of(TemplateDesign.fixed(basis), observations, Set("held")))
    fixedTables <- checked(BaselineExports.fixedTemplate(split, recipeId, keys))
    learnedRows <- Vector(
      ("train", "a", masses(0), 2.0),
      ("train", "b", masses(1), 1.0),
      ("held", "c", masses(0), 4.0),
      ("train", "c", masses(2), 100.0)
    ).zipWithIndex.traverse { (r, i) =>
      checked(
        TemplateObservation.of(
          StudyKey(participant, i.toString, r._1),
          r._1,
          r._3,
          r._4,
          Some(r._2)
        )
      )
    }
    learnedDesign <- checked(
      TemplateDesign.meanMap[Px]("participant split", "response units")
    )
    learnedSplit  <- checked(TemplateSplit.of(learnedDesign, learnedRows, Set("held")))
    learnedTables <- checked(BaselineExports.learnedTemplate(learnedSplit, learnedId, keys))
    predictors    <- Vector("first", "second").traverse(s => checked(PredictorId.of(s)))
    predictorSet  <- checked(PredictorSet.of(predictors.zip(masses.take(2))))
    ols           <- checked(Template.decompose(masses(2), predictorSet, Intercept.Exclude))
    olsTables     <- checked(BaselineExports.ols(ols))
    studyInput = StudyInput(
      Trials((for phase <- Vector("encode", "recall"); k <- baseKeys.take(2)
      yield Trial(k.copy(phase = phase), (), path)))
    )
    studyPlan <- checked(
      StudyPlan.cosine(
        studyInput.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll
      )
    )
    studyResult <- checked(studyPlan.run(studyInput))
    study       <- checked(
      BaselineExports.study(
        studyPlan,
        studyResult,
        StudyCodecs.cosine[Px],
        ScoreColumns.similarity
      )
    )
    span          <- checked(Interval.of(clock, Instant.micros(0), Instant.micros(25000)))
    coverage      <- checked(ObservedCoverage.of(clock, Vector(span)))
    temporalInput <- checked(
      TemporalStudyInput.of(
        studyInput,
        studyInput.trials.rows.map(r => r.key -> TrialEpoch(Instant.micros(0), coverage))
      )
    )
    window     <- checked(Window.of(Span.micros(0), Span.micros(25000)))
    named      <- checked(StudyWindow.of("whole", window))
    repetition <- checked(
      RepetitionContrast.withinParticipant("recall-encode", "recall", "encode")
    )
    temporalPlan <- checked(
      TemporalStudyPlan.of(
        studyPlan,
        temporalInput.reference,
        Vector(named),
        Vector(repetition),
        FixationBoundary.ClipDuration
      )
    )
    temporalResult <- checked(temporalPlan.run(temporalInput))
    temporalSchema <- id("temporal")
    temporal       <- checked(
      BaselineExports.temporal(
        temporalPlan,
        temporalResult,
        new TemporalStudyCodec(temporalSchema, StudyCodecs.cosine[Px]),
        ScoreColumns.similarity
      )
    )
    empty <- checked(
      ResultTable.of(pairTable.family, pairTable.columns, Vector.empty, pairTable.context)
    )
  yield Vector(
    "pairs"            -> pairTable,
    "structured-pairs" -> structuredTable,
    "reductions"       -> reductionTable,
    "contrasts"        -> contrastTable,
    "empty-pairs"      -> empty,
    "study"            -> study
  ) ++
    pointTables.map(t =>
      "point-" + t.family.toString.toLowerCase -> t
    ) ++ repeatTables.zipWithIndex.map((t, i) => s"repetition-$i" -> t) ++
    fixedTables.map(t => "fixed-" + t.family.toString.toLowerCase -> t) ++ learnedTables.map(
      t => "learned-" + t.family.toString.toLowerCase -> t
    ) ++
    olsTables.map(t => t.family.toString.toLowerCase -> t) ++ temporal.map(t =>
      t.family.toString.toLowerCase -> t
    )
