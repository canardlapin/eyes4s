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

import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

private[codec] object DecompositionCodecExamples:
  private def get[E, A](value: Either[E, A]): A = value.toOption.get
  val codec: DecompositionCodec[Px]             = new DecompositionCodec[Px]
  private val frame                           = get(Frame.screen("decomposition-family", 3, 1))
  private val grid                            = get(Grid.over(frame, 3, 1))
  private def mass(values: Double*): Mass[Px] =
    val cells = IArray.from(values)
    get(
      Surface
        .intensity(grid, cells, Provenance.raw(ContentHash.of(cells)))
        .flatMap(_.normalised)
    )
  val response: Mass[Px]           = mass(1, 0, 0)
  val predictors: PredictorSet[Px] = get(
    PredictorSet.of(
      Vector(
        get(PredictorId.of("a")) -> mass(3, 2, 0),
        get(PredictorId.of("b")) -> mass(0, 2, 3)
      )
    )
  )
  val specs: Vector[DecompositionSpec] = Vector(
    DecompositionSpec.Ols(Intercept.Exclude, RelativeRankTolerance.default),
    DecompositionSpec.NonNegative(RelativeRankTolerance.default, RelativeDualTolerance.default),
    DecompositionSpec.Mixture(
      Intercept.Exclude,
      RelativeRankTolerance.default,
      RelativeDualTolerance.default
    )
  )
  def plan(spec: DecompositionSpec): DecompositionPlan[Px] = get(
    DecompositionPlan.of(response, predictors, spec)
  )

class DecompositionCodecSuite extends munit.FunSuite:
  import DecompositionCodecExamples.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def alter(document: Json, field: String, value: Json): Json = document.mapObject(
    _.add("value", get(document.hcursor.get[Json]("value")).mapObject(_.add(field, value)))
  )

  test("OLS, NNLS and mixture plans and completed results round-trip under one family") {
    specs.foreach { spec =>
      val p     = plan(spec); val a                         = get(DecompositionArchive.run(p))
      val saved = get(codec.results.encode(a)); val decoded = get(codec.results.decode(saved))
      assertEquals(codec.results.encode(decoded), Right(saved))
      assertEquals(
        get(codec.plans.decode(get(codec.plans.encode(p)))).description,
        p.description
      )
      assertEquals(decoded.result.inputHash, p.inputHash)
      assertEquals(decoded.result.planHash, p.planHash)
    }
  }

  test("stamp refusal precedes result replay and numeric payload tampering is refused") {
    val saved = get(codec.results.encode(get(DecompositionArchive.run(plan(specs.head)))))
    val wrong =
      Json.obj("plan" -> Json.fromString("0" * 64), "input" -> Json.fromString("0" * 64))
    codec.results.decode(alter(alter(saved, "runStamp", wrong), "result", Json.Null)) match
      case Left(CodecError.Stamp(_)) => ()
      case other                     => fail(s"Expected stamp refusal, got $other")
    assert(codec.results.decode(alter(saved, "result", Json.Null)).isLeft)
  }

  test("method-specific wire refuses an intercept on NNLS instead of silently ignoring it") {
    val saved = get(codec.plans.encode(plan(specs(1))))
    val spec  = get(saved.hcursor.downField("value").get[Json]("spec"))
      .mapObject(_.add("intercept", Json.fromString("Include")))
    val refused = codec.plans.decode(alter(saved, "spec", spec)).left.toOption.get
    assert(refused.message.contains("intercept"))
  }

  test("completed binding refuses another method and retains the exact result object") {
    val p = plan(specs.head); val result = get(p.run)
    assert(DecompositionArchive.completed(plan(specs(1)), result).isLeft)
    assert(get(DecompositionArchive.completed(p, result)).result eq result)
  }

  test("generic manifest roles resolve the stamped result with its complete embedded inputs") {
    val p            = plan(specs(2)); val archive = get(DecompositionArchive.run(p))
    val storedPlan   = get(StoredArtifact.analysisPlan("decomposition-plan", codec.plans, p))
    val storedResult =
      get(StoredArtifact.analysisResult("decomposition-result", codec.results, archive))
    val graph = get(
      SavedManifest.of(
        Vector(storedPlan, storedResult),
        Vector(
          ManifestRelation.AnalysisResultOf(
            storedResult.name,
            storedPlan.name,
            AnalysisInputs.EmbeddedInPlan
          )
        )
      )
    )
    val registry = get(AnalysisRegistry.empty.register(codec.registration))
    val decoded  = get(
      ArtifactResolver.resolve(
        graph.address,
        graph.source,
        get(ArtifactDecoders.study[Px]).withAnalyses(registry)
      )
    )
    val result =
      decoded.analysisResult(storedResult.name).getOrElse(fail("no decomposition result"))
    assertEquals(result.inputs, Vector(p.inputHash))
    assert(LoadedAnalysisResult.typed(codec.results)(result).nonEmpty)
  }

  test("all six pinned method documents restore and re-encode on this runtime") {
    val documents = Vector(
      DecompositionFixtures.olsPlan     -> DecompositionFixtures.olsResult,
      DecompositionFixtures.nnlsPlan    -> DecompositionFixtures.nnlsResult,
      DecompositionFixtures.mixturePlan -> DecompositionFixtures.mixtureResult
    )
    specs.zip(documents).foreach { (spec, pinned) =>
      val p           = plan(spec)
      val savedPlan   = get(io.circe.parser.parse(pinned._1))
      val savedResult = get(io.circe.parser.parse(pinned._2))
      assertEquals(codec.plans.encode(p), Right(savedPlan))
      assertEquals(get(codec.plans.decode(savedPlan)).description, p.description)
      assertEquals(codec.results.encode(get(DecompositionArchive.run(p))), Right(savedResult))
      assertEquals(
        codec.results.encode(get(codec.results.decode(savedResult))),
        Right(savedResult)
      )
    }
  }

  test("a saved decomposition refuses another norm convention") {
    val saved = get(codec.plans.encode(plan(specs.head)))
    assert(
      codec.plans
        .decode(alter(saved, "normConvention", Json.fromString("runtime-native")))
        .isLeft
    )
  }
