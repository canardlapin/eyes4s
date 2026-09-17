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
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import io.circe.Json

/** Inputs that make a fixation study record every failure family it can, a
  * two-component extension score, and the pinned result-v1 payload. The
  * compact string mirrors src/test/resources/eyes4s/study-result-v1.json; the
  * JVM suite checks the pretty-printed resource bytes.
  */
object StudyResultFixtures:
  private def get[E, A](e: Either[E, A]): A =
    e.fold(x => throw new AssertionError(s"$x"), identity)

  val frame: Frame[Px] = get(Frame.screen("result-display", 2, 2))
  val grid: Grid[Px]   = get(Grid.over(frame, 2, 2))

  /** A frame with the same extent under a different identity: `Agreement` refuses it. */
  val otherFrame: Frame[Px] = get(Frame.screen("other-display", 2, 2))

  def trial(
      key: StudyKey,
      in: Frame[Px],
      points: (Double, Double)*
  ): Trial[StudyKey, Unit, Scanpath[Px]] =
    val clock = ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 2000L), Instant.micros(i * 2000L + 1000L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(in, clock, IArray.from(fixes))))

  private def k(p: String, s: String, phase: String) = StudyKey(p, s, phase)

  /** Two participants; p1/b recall sits in a foreign frame so its estimate,
    * its matched pair and every control pair it enters fail; p2 has a
    * duplicated focal key, an unmatched reference stimulus and an excluded
    * practice trial.
    */
  val mixed: StudyInput[StudyKey, Px] = StudyInput(
    Trials(
      Vector(
        trial(k("p1", "a", "recall"), frame, (0.5, 0.5), (1.5, 0.5)),
        trial(k("p1", "b", "recall"), otherFrame, (0.5, 1.5)),
        trial(k("p1", "a", "encode"), frame, (0.5, 0.5)),
        trial(k("p1", "b", "encode"), frame, (1.5, 0.5), (0.5, 1.5)),
        trial(k("p2", "a", "recall"), frame, (1.5, 1.5)),
        trial(k("p2", "a", "recall"), frame, (0.5, 1.5)),
        trial(k("p2", "a", "encode"), frame, (1.5, 1.5), (0.5, 0.5)),
        trial(k("p2", "c", "encode"), frame, (0.5, 0.5)),
        trial(k("p2", "d", "practice"), frame, (0.5, 0.5))
      )
    )
  )

  /** All twelve matched-control trials in one frame; no failure of any kind. */
  val clean: StudyInput[StudyKey, Px] = StudyInputFixtures.matchedControl

  val requireAll: FailurePolicy     = FailurePolicy.RequireAll
  val successfulOnly: FailurePolicy = get(FailurePolicy.successfulOnly(1))

  /** Binned, a Gaussian that estimates, and a Gaussian too narrow for the cells. */
  val scales: Vector[StudyEstimate[Px]] = Vector(
    StudyEstimate.Binned(),
    StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate),
    StudyEstimate.Gaussian(get(Sigma.px(0.05)), EdgePolicy.Truncate)
  )

  def cosinePlan(
      input: StudyInput[StudyKey, Px],
      policy: FailurePolicy,
      estimates: Vector[StudyEstimate[Px]] = scales
  ): StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference] =
    get(
      StudyPlan.cosine[Px](
        input.reference,
        gridOver(input),
        "recall",
        "encode",
        Weight.Duration,
        estimates,
        policy
      )
    )

  /** The plan grid over the input's first trial frame, 2x2 like the study fixtures. */
  def gridOver(input: StudyInput[StudyKey, Px]): Grid[Px] =
    input.trials.rows.headOption.fold(grid)(t => get(Grid.over(t.value.frame, 2, 2)))

  /** An extension score with two components of different meaning: a bounded
    * similarity and a distance-like complement.
    */
  final case class TwoComponent(similarity: Double, distance: Double) derives CanEqual
  final case class TwoDifference(similarity: SignedDifference, distance: SignedDifference)
      derives CanEqual

  object TwoComponent:
    given ScoreMean[TwoComponent] with
      def mean(values: Vector[TwoComponent]): Either[ScoreMeanError, TwoComponent] =
        for
          s <- ScoreMean[Double].mean(values.map(_.similarity))
          d <- ScoreMean[Double].mean(values.map(_.distance))
        yield TwoComponent(s, d)
    given Contrastable[TwoComponent, TwoDifference] with
      val components = Vector("similarity", "distance")
      def subtract(
          matched: TwoComponent,
          control: TwoComponent
      ): Either[DifferenceError, TwoDifference] =
        for
          s <- SignedDifference.between(matched.similarity, control.similarity, "similarity")
          d <- SignedDifference.between(matched.distance, control.distance, "distance")
        yield TwoDifference(s, d)

  val twoComponentId: DefinitionId = get(DefinitionId.of("test.two-component", 1))

  /** A declared, score-affecting parameter: the distance component is scaled by `gain`. */
  val twoComponentMethod: StudyMethod[Double, Px, TwoComponent, TwoDifference] =
    new StudyMethod[Double, Px, TwoComponent, TwoDifference](
      twoComponentId,
      "Cosine with its scaled complement",
      gain => Vector("gain" -> Provenance.Param.Num(gain)),
      MethodExecution.Bounded((gain: Double) =>
        Distribution
          .cosine[Px]
          .mapScore(
            MeasureInfo(
              "cosine and complement",
              "Cosine similarity and gain times one minus it",
              MeasureScale.Bounded(0, 1),
              None
            )
          )(s => Right(TwoComponent(s.value, gain * (1.0 - s.value))))
      ),
      None
    )

  val gain: Double = 2.0

  def twoComponentPlan(
      input: StudyInput[StudyKey, Px],
      policy: FailurePolicy
  ): StudyPlan[StudyKey, Px, Double, TwoComponent, TwoDifference] =
    get(
      StudyPlan.of(
        input.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        grid,
        "recall",
        "encode",
        Weight.Duration,
        scales,
        policy,
        twoComponentMethod,
        gain
      )
    )

  private def finite(json: Json, field: String): Either[CodecError, Double] =
    json.hcursor
      .get[Double](field)
      .left
      .map(e => CodecError.Field(field, json, e.message))
      .flatMap(v => Either.cond(v.isFinite, v, CodecError.Field(field, json, "must be finite")))

  val twoComponentScores: VersionedCodec[TwoComponent] =
    VersionedCodec.of[TwoComponent](get(DefinitionId.of("test.two-component-score", 1)))(s =>
      Json.obj(
        "similarity" -> Json.fromDoubleOrNull(s.similarity),
        "distance"   -> Json.fromDoubleOrNull(s.distance)
      )
    )(json =>
      for
        s <- finite(json, "similarity")
        d <- finite(json, "distance")
      yield TwoComponent(s, d)
    )

  val twoComponentDifferences: VersionedCodec[TwoDifference] =
    VersionedCodec.of[TwoDifference](get(DefinitionId.of("test.two-component-difference", 1)))(
      d =>
        Json.obj(
          "similarity" -> Json.fromDoubleOrNull(d.similarity.value),
          "distance"   -> Json.fromDoubleOrNull(d.distance.value)
        )
    )(json =>
      for
        s  <- finite(json, "similarity")
        d  <- finite(json, "distance")
        sd <- SignedDifference
          .between(s, 0.0)
          .left
          .map(e => CodecError.Field("similarity", json, e.message))
        dd <- SignedDifference
          .between(d, 0.0)
          .left
          .map(e => CodecError.Field("distance", json, e.message))
      yield TwoDifference(sd, dd)
    )

  val twoComponentStudy: StudyCodec[StudyKey, Px, Double, TwoComponent, TwoDifference] =
    new StudyCodec(
      get(DefinitionId.of("test.two-component-study", 1)),
      StudyKey.layout(DefinitionId.studyLayout),
      StudyCodecs.key(DefinitionId.studyKey),
      twoComponentMethod,
      StudyResultCodecs.scalar(get(DefinitionId.of("test.gain", 1)))
    )

  val twoComponentCodec: StudyResultCodec[StudyKey, Px, Double, TwoComponent, TwoDifference] =
    twoComponentStudy.results(twoComponentScores, twoComponentDifferences)

  /** The pinned study-v1 plan run on the pinned study-input-v1 input. */
  val resultVersionOne: String =
    """{"schema":{"name":"eyes4s.study-result","version":1},"value":{"layout":{"name":"eyes4s.participant-stimulus-phase","version":1},"keySchema":{"name":"eyes4s.study-key","version":1},"method":{"name":"eyes4s.cosine","version":1},"scoreSchema":{"name":"eyes4s.similarity","version":1},"differenceSchema":{"name":"eyes4s.signed-difference","version":1},"unit":"px","input":"cebe7474ab5c2aec","description":[{"field":"input","values":[{"kind":"text","value":"cebe7474ab5c2aec"}]},{"field":"layout","values":[{"kind":"text","value":"eyes4s.participant-stimulus-phase"},{"kind":"num","value":1.0}]},{"field":"method","values":[{"kind":"text","value":"eyes4s.cosine"},{"kind":"num","value":1.0}]},{"field":"phases","values":[{"kind":"text","value":"recall"},{"kind":"text","value":"encode"}]},{"field":"weight","values":[{"kind":"text","value":"Duration"}]},{"field":"failurePolicy","values":[{"kind":"text","value":"require-all"}]},{"field":"frame","values":[{"kind":"text","value":"matched-control-display"},{"kind":"text","value":"px"},{"kind":"num","value":0.0},{"kind":"num","value":0.0},{"kind":"num","value":2.0},{"kind":"num","value":2.0},{"kind":"text","value":"Down"}]},{"field":"grid","values":[{"kind":"text","value":"matched-control-display@2x2"},{"kind":"num","value":2.0},{"kind":"num","value":2.0}]},{"field":"estimate.0","values":[{"kind":"text","value":"estimator"},{"kind":"text","value":"binned"}]}],"identities":{"frames":[{"id":"matched-control-display","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"}],"grids":[{"id":"matched-control-display@2x2","frame":"matched-control-display","nx":2,"ny":2}],"clocks":[]},"scales":[{"estimate":{"kind":"binned"},"estimation":[{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"encode"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.11111111111111112,0.22222222222222224,0.22222222222222224,0.4444444444444445],"provenance":{"inputs":"5dd8999b58bda362","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.11111111111111112,0.22222222222222224,0.22222222222222224,0.4444444444444445],"provenance":{"inputs":"5dd8999b58bda362","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"encode"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.4444444444444445,0.22222222222222224,0.22222222222222224,0.11111111111111112],"provenance":{"inputs":"56048b87a3aca137","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"recall"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.2222222222222222,0.4444444444444444,0.1111111111111111,0.2222222222222222],"provenance":{"inputs":"00b1964b5938a3ac","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"encode"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.2222222222222222,0.4444444444444444,0.1111111111111111,0.2222222222222222],"provenance":{"inputs":"00b1964b5938a3ac","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"recall"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.4444444444444445,0.22222222222222224,0.22222222222222224,0.11111111111111112],"provenance":{"inputs":"56048b87a3aca137","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"encode"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.2222222222222222,0.1111111111111111,0.4444444444444444,0.2222222222222222],"provenance":{"inputs":"692d3fa777b0c867","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"recall"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.44444444444444453,0.11111111111111113,0.22222222222222227,0.22222222222222227],"provenance":{"inputs":"55166d85f75f7500","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"encode"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.11111111111111113,0.44444444444444453,0.22222222222222227,0.22222222222222227],"provenance":{"inputs":"8b5e366eec11f3dd","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"recall"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.11111111111111113,0.44444444444444453,0.22222222222222227,0.22222222222222227],"provenance":{"inputs":"8b5e366eec11f3dd","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"encode"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.44444444444444453,0.11111111111111113,0.22222222222222227,0.22222222222222227],"provenance":{"inputs":"55166d85f75f7500","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"recall"}},"outcome":{"kind":"mass","grid":"matched-control-display@2x2","values":[0.2222222222222222,0.1111111111111111,0.4444444444444444,0.2222222222222222],"provenance":{"inputs":"692d3fa777b0c867","steps":[{"operation":"normalise","params":[{"name":"of","kind":"text","value":"surface"}]}]}}}],"excludedPhases":[],"analyses":{"matched":{"source":{"rows":[{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"encode"}},"result":{"kind":"score","score":1.0}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"encode"}},"result":{"kind":"score","score":0.7999999999999998}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"encode"}},"result":{"kind":"score","score":0.7999999999999998}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"encode"}},"result":{"kind":"score","score":0.8399999999999999}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"encode"}},"result":{"kind":"score","score":0.9999999999999998}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"encode"}},"result":{"kind":"score","score":0.8399999999999999}}],"diagnostics":{"pairSpace":{"kind":"betweenDirected","relation":"(participant == participant and stimulus == stimulus)","selection":{"kind":"all"}},"eligiblePairCount":"6","selectedPairCount":6,"unmatchedLeft":[],"unmatchedRight":[],"ambiguous":[]},"provenance":{"inputs":"cebe7474ab5c2aec","steps":[{"operation":"pair","params":[{"name":"relation","kind":"text","value":"(participant == participant and stimulus == stimulus)"},{"name":"storage","kind":"text","value":"BetweenDirected"},{"name":"eligible","kind":"text","value":"6"},{"name":"selected","kind":"num","value":6.0},{"name":"unmatchedLeft","kind":"num","value":0.0},{"name":"unmatchedRight","kind":"num","value":0.0},{"name":"ambiguous","kind":"num","value":0.0},{"name":"selection","kind":"text","value":"all"}]},{"operation":"evaluatePairs","params":[{"name":"evaluator","kind":"text","value":"cosine"},{"name":"scale","kind":"text","value":"bounded [0.00, 1.00]"},{"name":"successful","kind":"num","value":6.0},{"name":"failed","kind":"num","value":0.0}]},{"operation":"evaluationMethod","params":[{"name":"method","kind":"text","value":"eyes4s.cosine"},{"name":"revision","kind":"text","value":"1"}]},{"operation":"evaluationParameters","params":[{"name":"estimate.estimator","kind":"text","value":"binned"},{"name":"weight","kind":"text","value":"Duration"}]},{"operation":"evaluationComponents","params":[{"name":"0","kind":"text","value":"value"}]},{"operation":"evaluationDomain","params":[{"name":"unit","kind":"text","value":"px"},{"name":"unitName","kind":"text","value":"pixels"},{"name":"frame","kind":"text","value":"matched-control-display"},{"name":"xMin","kind":"num","value":0.0},{"name":"xMax","kind":"num","value":2.0},{"name":"yMin","kind":"num","value":0.0},{"name":"yMax","kind":"num","value":2.0},{"name":"yAxis","kind":"text","value":"Down"},{"name":"grid","kind":"text","value":"matched-control-display@2x2"},{"name":"nx","kind":"num","value":2.0},{"name":"ny","kind":"num","value":2.0},{"name":"time","kind":"text","value":"OrderFree"}]}]},"evaluation":{"name":"cosine","scale":{"kind":"measure","scale":{"kind":"bounded","lo":0.0,"hi":1.0}},"specification":{"method":"eyes4s.cosine","revision":"1","parameters":[{"name":"estimate.estimator","kind":"text","value":"binned"},{"name":"weight","kind":"text","value":"Duration"}],"components":["value"],"geometry":{"kind":"grid","unit":"px","frame":{"id":"matched-control-display","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"},"grid":{"id":"matched-control-display@2x2","nx":2,"ny":2}},"time":{"kind":"orderFree"}}}},"entries":[{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}},"result":{"kind":"score","score":1.0},"successful":1,"failed":0,"contributing":1},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"recall"}},"result":{"kind":"score","score":0.7999999999999998},"successful":1,"failed":0,"contributing":1},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"recall"}},"result":{"kind":"score","score":0.7999999999999998},"successful":1,"failed":0,"contributing":1},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"recall"}},"result":{"kind":"score","score":0.8399999999999999},"successful":1,"failed":0,"contributing":1},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"recall"}},"result":{"kind":"score","score":0.9999999999999998},"successful":1,"failed":0,"contributing":1},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"recall"}},"result":{"kind":"score","score":0.8399999999999999},"successful":1,"failed":0,"contributing":1}],"diagnostics":{"orientation":"byLeft","policy":{"kind":"requireAll"},"eligiblePairCount":"6","selectedPairCount":6,"successfulPairCount":6,"failedPairCount":0,"contributionCount":6,"reducedKeyCount":6,"failedKeys":[]},"provenance":{"inputs":"cebe7474ab5c2aec","steps":[{"operation":"pair","params":[{"name":"relation","kind":"text","value":"(participant == participant and stimulus == stimulus)"},{"name":"storage","kind":"text","value":"BetweenDirected"},{"name":"eligible","kind":"text","value":"6"},{"name":"selected","kind":"num","value":6.0},{"name":"unmatchedLeft","kind":"num","value":0.0},{"name":"unmatchedRight","kind":"num","value":0.0},{"name":"ambiguous","kind":"num","value":0.0},{"name":"selection","kind":"text","value":"all"}]},{"operation":"evaluatePairs","params":[{"name":"evaluator","kind":"text","value":"cosine"},{"name":"scale","kind":"text","value":"bounded [0.00, 1.00]"},{"name":"successful","kind":"num","value":6.0},{"name":"failed","kind":"num","value":0.0}]},{"operation":"evaluationMethod","params":[{"name":"method","kind":"text","value":"eyes4s.cosine"},{"name":"revision","kind":"text","value":"1"}]},{"operation":"evaluationParameters","params":[{"name":"estimate.estimator","kind":"text","value":"binned"},{"name":"weight","kind":"text","value":"Duration"}]},{"operation":"evaluationComponents","params":[{"name":"0","kind":"text","value":"value"}]},{"operation":"evaluationDomain","params":[{"name":"unit","kind":"text","value":"px"},{"name":"unitName","kind":"text","value":"pixels"},{"name":"frame","kind":"text","value":"matched-control-display"},{"name":"xMin","kind":"num","value":0.0},{"name":"xMax","kind":"num","value":2.0},{"name":"yMin","kind":"num","value":0.0},{"name":"yMax","kind":"num","value":2.0},{"name":"yAxis","kind":"text","value":"Down"},{"name":"grid","kind":"text","value":"matched-control-display@2x2"},{"name":"nx","kind":"num","value":2.0},{"name":"ny","kind":"num","value":2.0},{"name":"time","kind":"text","value":"OrderFree"}]},{"operation":"reducePairs","params":[{"name":"orientation","kind":"text","value":"ByLeft"},{"name":"failurePolicy","kind":"text","value":"require-all"},{"name":"contributions","kind":"num","value":6.0},{"name":"reducedKeys","kind":"num","value":6.0},{"name":"failedKeys","kind":"num","value":0.0}]}]}},"control":{"source":{"rows":[{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"encode"}},"result":{"kind":"score","score":0.64}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"encode"}},"result":{"kind":"score","score":0.7999999999999998}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"encode"}},"result":{"kind":"score","score":0.7999999999999998}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"encode"}},"result":{"kind":"score","score":0.9999999999999998}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"encode"}},"result":{"kind":"score","score":0.64}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"encode"}},"result":{"kind":"score","score":1.0}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"encode"}},"result":{"kind":"score","score":0.6399999999999999}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"encode"}},"result":{"kind":"score","score":0.9999999999999998}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"encode"}},"result":{"kind":"score","score":0.7199999999999999}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"encode"}},"result":{"kind":"score","score":0.6399999999999999}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"encode"}},"result":{"kind":"score","score":0.9999999999999998}},{"left":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"recall"}},"right":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"encode"}},"result":{"kind":"score","score":0.7199999999999999}}],"diagnostics":{"pairSpace":{"kind":"betweenDirected","relation":"(participant == participant and stimulus != stimulus)","selection":{"kind":"all"}},"eligiblePairCount":"12","selectedPairCount":12,"unmatchedLeft":[],"unmatchedRight":[],"ambiguous":[]},"provenance":{"inputs":"cebe7474ab5c2aec","steps":[{"operation":"pair","params":[{"name":"relation","kind":"text","value":"(participant == participant and stimulus != stimulus)"},{"name":"storage","kind":"text","value":"BetweenDirected"},{"name":"eligible","kind":"text","value":"12"},{"name":"selected","kind":"num","value":12.0},{"name":"unmatchedLeft","kind":"num","value":0.0},{"name":"unmatchedRight","kind":"num","value":0.0},{"name":"ambiguous","kind":"num","value":0.0},{"name":"selection","kind":"text","value":"all"}]},{"operation":"evaluatePairs","params":[{"name":"evaluator","kind":"text","value":"cosine"},{"name":"scale","kind":"text","value":"bounded [0.00, 1.00]"},{"name":"successful","kind":"num","value":12.0},{"name":"failed","kind":"num","value":0.0}]},{"operation":"evaluationMethod","params":[{"name":"method","kind":"text","value":"eyes4s.cosine"},{"name":"revision","kind":"text","value":"1"}]},{"operation":"evaluationParameters","params":[{"name":"estimate.estimator","kind":"text","value":"binned"},{"name":"weight","kind":"text","value":"Duration"}]},{"operation":"evaluationComponents","params":[{"name":"0","kind":"text","value":"value"}]},{"operation":"evaluationDomain","params":[{"name":"unit","kind":"text","value":"px"},{"name":"unitName","kind":"text","value":"pixels"},{"name":"frame","kind":"text","value":"matched-control-display"},{"name":"xMin","kind":"num","value":0.0},{"name":"xMax","kind":"num","value":2.0},{"name":"yMin","kind":"num","value":0.0},{"name":"yMax","kind":"num","value":2.0},{"name":"yAxis","kind":"text","value":"Down"},{"name":"grid","kind":"text","value":"matched-control-display@2x2"},{"name":"nx","kind":"num","value":2.0},{"name":"ny","kind":"num","value":2.0},{"name":"time","kind":"text","value":"OrderFree"}]}]},"evaluation":{"name":"cosine","scale":{"kind":"measure","scale":{"kind":"bounded","lo":0.0,"hi":1.0}},"specification":{"method":"eyes4s.cosine","revision":"1","parameters":[{"name":"estimate.estimator","kind":"text","value":"binned"},{"name":"weight","kind":"text","value":"Duration"}],"components":["value"],"geometry":{"kind":"grid","unit":"px","frame":{"id":"matched-control-display","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"},"grid":{"id":"matched-control-display@2x2","nx":2,"ny":2}},"time":{"kind":"orderFree"}}}},"entries":[{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}},"result":{"kind":"score","score":0.72},"successful":2,"failed":0,"contributing":2},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"recall"}},"result":{"kind":"score","score":0.8999999999999998},"successful":2,"failed":0,"contributing":2},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"recall"}},"result":{"kind":"score","score":0.8200000000000001},"successful":2,"failed":0,"contributing":2},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"recall"}},"result":{"kind":"score","score":0.8199999999999998},"successful":2,"failed":0,"contributing":2},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"recall"}},"result":{"kind":"score","score":0.6799999999999999},"successful":2,"failed":0,"contributing":2},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"recall"}},"result":{"kind":"score","score":0.8599999999999999},"successful":2,"failed":0,"contributing":2}],"diagnostics":{"orientation":"byLeft","policy":{"kind":"requireAll"},"eligiblePairCount":"12","selectedPairCount":12,"successfulPairCount":12,"failedPairCount":0,"contributionCount":12,"reducedKeyCount":6,"failedKeys":[]},"provenance":{"inputs":"cebe7474ab5c2aec","steps":[{"operation":"pair","params":[{"name":"relation","kind":"text","value":"(participant == participant and stimulus != stimulus)"},{"name":"storage","kind":"text","value":"BetweenDirected"},{"name":"eligible","kind":"text","value":"12"},{"name":"selected","kind":"num","value":12.0},{"name":"unmatchedLeft","kind":"num","value":0.0},{"name":"unmatchedRight","kind":"num","value":0.0},{"name":"ambiguous","kind":"num","value":0.0},{"name":"selection","kind":"text","value":"all"}]},{"operation":"evaluatePairs","params":[{"name":"evaluator","kind":"text","value":"cosine"},{"name":"scale","kind":"text","value":"bounded [0.00, 1.00]"},{"name":"successful","kind":"num","value":12.0},{"name":"failed","kind":"num","value":0.0}]},{"operation":"evaluationMethod","params":[{"name":"method","kind":"text","value":"eyes4s.cosine"},{"name":"revision","kind":"text","value":"1"}]},{"operation":"evaluationParameters","params":[{"name":"estimate.estimator","kind":"text","value":"binned"},{"name":"weight","kind":"text","value":"Duration"}]},{"operation":"evaluationComponents","params":[{"name":"0","kind":"text","value":"value"}]},{"operation":"evaluationDomain","params":[{"name":"unit","kind":"text","value":"px"},{"name":"unitName","kind":"text","value":"pixels"},{"name":"frame","kind":"text","value":"matched-control-display"},{"name":"xMin","kind":"num","value":0.0},{"name":"xMax","kind":"num","value":2.0},{"name":"yMin","kind":"num","value":0.0},{"name":"yMax","kind":"num","value":2.0},{"name":"yAxis","kind":"text","value":"Down"},{"name":"grid","kind":"text","value":"matched-control-display@2x2"},{"name":"nx","kind":"num","value":2.0},{"name":"ny","kind":"num","value":2.0},{"name":"time","kind":"text","value":"OrderFree"}]},{"operation":"reducePairs","params":[{"name":"orientation","kind":"text","value":"ByLeft"},{"name":"failurePolicy","kind":"text","value":"require-all"},{"name":"contributions","kind":"num","value":12.0},{"name":"reducedKeys","kind":"num","value":6.0},{"name":"failedKeys","kind":"num","value":0.0}]}]}}},"contrast":{"kind":"contrast","rows":[{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}},"matched":true,"control":true,"difference":{"kind":"value","value":0.28}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"recall"}},"matched":true,"control":true,"difference":{"kind":"value","value":-0.09999999999999998}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"recall"}},"matched":true,"control":true,"difference":{"kind":"value","value":-0.02000000000000024}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"recall"}},"matched":true,"control":true,"difference":{"kind":"value","value":0.020000000000000018}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"recall"}},"matched":true,"control":true,"difference":{"kind":"value","value":0.31999999999999984}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"recall"}},"matched":true,"control":true,"difference":{"kind":"value","value":-0.020000000000000018}}]}}]}}"""
