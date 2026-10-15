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

private[codec] object TemplateFitCodecExamples:
  private def get[E, A](v: Either[E, A]): A = v.toOption.get
  val keys: VersionedCodec[String]          =
    VersionedCodec.string(get(DefinitionId.of("example.template-key", 1)))
  val fixedCodec: TemplateFitCodec[String, Vector[Double]] = TemplateFitCodec.fixed(keys)
  val mapCodec: TemplateFitCodec[String, Mass[Px]]         = TemplateFitCodec.meanMap(keys)
  def fixed(
      overflow: Boolean = false,
      excludedResponse: Double = 99.0
  ): TemplateFitPlan[String, Vector[Double]] =
    val basis = get(TemplateBasis.of("native-family", Vector("x", "z"), "score"))
    val rows  = Vector(
      get(TemplateObservation.of("train-x", "train", Vector(1.0, 0.0), 2.0, Some("x"))),
      get(TemplateObservation.of("train-z", "train", Vector(0.0, 1.0), 3.0, Some("z"))),
      get(
        TemplateObservation
          .of("excluded", "train", Vector(4.0, 5.0), excludedResponse, Some("held-item"))
      ),
      get(
        TemplateObservation.of(
          "held",
          "held",
          if overflow then Vector(1e308, 1e308) else Vector(2.0, 1.0),
          8.0,
          Some("held-item")
        )
      )
    )
    get(
      TemplateFitPlan.of(get(TemplateSplit.of(TemplateDesign.fixed(basis), rows, Set("held"))))
    )
  def imported: TemplateFitPlan[String, Vector[Double]] =
    val native = fixed()
    val design = TemplateDesign.importedLm(
      get(TemplateBasis.of("native-family", Vector("x", "z"), "score"))
    )
    val split   = get(TemplateSplit.of(design, native.split.rows, native.split.heldOutGroups))
    val receipt = get(
      TemplateImportedFit.of(
        split.training,
        split.training.hash.render,
        design.basis.columns,
        Vector(2.0, 3.0),
        2,
        2,
        "external fixture"
      )
    )
    get(TemplateFitPlan.imported(split, receipt))
  def meanMap: TemplateFitPlan[String, Mass[Px]] =
    val frame                           = get(Frame.screen("template-family-map", 2, 1));
    val grid                            = get(Grid.over(frame, 2, 1))
    def mass(values: Double*): Mass[Px] =
      val cells = IArray.from(values)
      get(
        Surface
          .intensity(grid, cells, Provenance.raw(ContentHash.of(cells)))
          .flatMap(_.normalised)
      )
    val rows = Vector(
      get(TemplateObservation.of("a", "train", mass(1, 0), 2.0, Some("a"))),
      get(TemplateObservation.of("b", "train", mass(0, 1), 3.0, Some("b"))),
      get(TemplateObservation.of("c", "held", mass(1, 1), 4.0, Some("c")))
    )
    get(
      TemplateFitPlan.of(
        get(
          TemplateSplit.of(
            get(TemplateDesign.meanMap[Px]("participant", "score")),
            rows,
            Set("held")
          )
        )
      )
    )

class TemplateFitCodecSuite extends munit.FunSuite:
  import TemplateFitCodecExamples.*
  private def get[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)
  private def alter(document: Json, field: String, value: Json): Json =
    document.mapObject(
      _.add("value", get(document.hcursor.get[Json]("value")).mapObject(_.add(field, value)))
    )

  test("fixed native plans and results round-trip and preserve the independent exact fit") {
    val p = fixed(); val a = get(TemplateFitArchive.run(p))
    assertEquals(a.validation.fitted.coefficients, Vector(2.0, 3.0))
    assertEquals(a.validation.evaluation.rows.map(_.result), Vector(Right((7.0, 1.0))))
    val saved    = get(fixedCodec.results.encode(a));
    val restored = get(fixedCodec.results.decode(saved))
    assertEquals(fixedCodec.results.encode(restored), Right(saved))
    assertEquals(
      get(fixedCodec.plans.decode(get(fixedCodec.plans.encode(p)))).description,
      p.description
    )
  }

  test("learned-map native plans restore a refitted training template and predictions") {
    val p        = meanMap; val a = get(TemplateFitArchive.run(p))
    val saved    = get(mapCodec.results.encode(a));
    val restored = get(mapCodec.results.decode(saved))
    assert(restored.validation.fitted.template.nonEmpty)
    assertEquals(restored.validation.evaluation.rows, a.validation.evaluation.rows)
    assertEquals(mapCodec.results.encode(restored), Right(saved))
    assertEquals(restored.result.inputHash, p.inputHash)
    assertEquals(restored.result.planHash, p.planHash)
    val result = get(saved.hcursor.downField("value").get[Json]("result"))
    assertEquals(
      get(result.hcursor.downField("learnedTemplate").get[Vector[Double]]("values")),
      a.validation.fitted.template.get.values.toVector
    )
    val changed = result.mapObject(_.add("learnedTemplate", Json.Null))
    assert(mapCodec.results.decode(alter(saved, "result", changed)).isLeft)
  }

  test("missing or stale stamp is refused before malformed saved results are replayed") {
    val saved   = get(fixedCodec.results.encode(get(TemplateFitArchive.run(fixed()))))
    val payload = get(saved.hcursor.get[Json]("value"))
    assert(
      fixedCodec.results
        .decode(saved.mapObject(_.add("value", payload.mapObject(_.remove("runStamp")))))
        .isLeft
    )
    val wrong =
      Json.obj("plan" -> Json.fromString("0" * 64), "input" -> Json.fromString("0" * 64))
    fixedCodec.results.decode(alter(alter(saved, "runStamp", wrong), "result", Json.Null)) match
      case Left(CodecError.Stamp(_)) => ()
      case other                     => fail(s"Expected canonical stamp refusal, got $other")
    assert(fixedCodec.results.decode(alter(saved, "result", Json.Null)).isLeft)
  }

  test(
    "completed binding includes excluded input, even when fitted training and held-out hashes agree"
  ) {
    val a      = fixed(); val b = fixed(excludedResponse = 100.0)
    val result = get(a.run)
    assertEquals(a.split.training.hash, b.split.training.hash)
    assertEquals(a.split.heldOut.hash, b.split.heldOut.hash)
    assert(TemplateFitArchive.completed(b, result).isLeft)
    val completed = get(TemplateFitArchive.completed(a, result))
    assert(completed.result eq result)
  }

  test("failed held-out predictions remain typed numerical failures after saved replay") {
    val a = get(TemplateFitArchive.run(fixed(overflow = true)))
    assert(a.validation.evaluation.rows.head.result.isLeft)
    val saved    = get(fixedCodec.results.encode(a))
    val restored = get(fixedCodec.results.decode(saved))
    assertEquals(restored.validation.evaluation.rows, a.validation.evaluation.rows)
    assert(restored.validation.evaluation.meanSquaredError.isLeft)
  }

  test("both typed subroutes coexist and resolve through generic manifest roles") {
    val fixed    = get(TemplateFitArchive.run(TemplateFitCodecExamples.fixed()))
    val learned  = get(TemplateFitArchive.run(meanMap))
    val registry = get(
      get(AnalysisRegistry.empty.register(fixedCodec.registration))
        .register(mapCodec.registration)
    )
    val p1    = get(StoredArtifact.analysisPlan("fixed-plan", fixedCodec.plans, fixed.plan))
    val r1    = get(StoredArtifact.analysisResult("fixed-result", fixedCodec.results, fixed))
    val p2    = get(StoredArtifact.analysisPlan("map-plan", mapCodec.plans, learned.plan))
    val r2    = get(StoredArtifact.analysisResult("map-result", mapCodec.results, learned))
    val graph = get(
      SavedManifest.of(
        Vector(p1, r1, p2, r2),
        Vector(
          ManifestRelation.AnalysisResultOf(r1.name, p1.name, AnalysisInputs.EmbeddedInPlan),
          ManifestRelation.AnalysisResultOf(r2.name, p2.name, AnalysisInputs.EmbeddedInPlan)
        )
      )
    )
    val decoded = get(
      ArtifactResolver.resolve(
        graph.address,
        graph.source,
        get(ArtifactDecoders.study[Px]).withAnalyses(registry)
      )
    )
    val one = decoded.analysisResult(r1.name).getOrElse(fail("no fixed fit"))
    val two = decoded.analysisResult(r2.name).getOrElse(fail("no mean-map fit"))
    assert(LoadedAnalysisResult.typed(fixedCodec.results)(one).nonEmpty)
    assert(LoadedAnalysisResult.typed(mapCodec.results)(two).nonEmpty)
    assertEquals(LoadedAnalysisResult.typed(fixedCodec.results)(two), None)
    assertEquals(one.inputs, Vector(fixed.plan.inputHash))
    assertEquals(two.inputs, Vector(learned.plan.inputHash))
  }

  test("imported fixed fits retain their external receipt and backend through saved replay") {
    val p = imported; val archive = get(TemplateFitArchive.run(p))
    assertEquals(p.work.stage, TemplateValidationStage.CheckingWholeImportedReceipt)
    assertEquals(archive.validation.fitted.backend, "external fixture")
    val saved    = get(fixedCodec.results.encode(archive));
    val restored = get(fixedCodec.results.decode(saved))
    assertEquals(fixedCodec.results.encode(restored), Right(saved))
    assertEquals(restored.validation.evaluation.rows.map(_.result), Vector(Right((7.0, 1.0))))
    assert(restored.plan.route.isInstanceOf[TemplateFitRoute.ImportedFixed])
  }

  test(
    "complete learned-map source identity includes provenance without changing training-data identity"
  ) {
    val original = meanMap
    val rows     = original.split.rows.map { row =>
      val mass = get(
        Surface.mass(
          row.input.grid,
          row.input.values,
          row.input.provenance.andThen(Provenance.Step("relabelled-source"))
        )
      )
      get(TemplateObservation.of(row.key, row.splitGroup, mass, row.response, row.matchGroup))
    }
    val changed = get(
      TemplateFitPlan.of(
        get(TemplateSplit.of(original.split.design, rows, original.split.heldOutGroups))
      )
    )
    assertEquals(original.split.training.hash, changed.split.training.hash)
    assertEquals(original.split.heldOut.hash, changed.split.heldOut.hash)
    assertNotEquals(original.inputHash, changed.inputHash)
    assertNotEquals(original.planHash, changed.planHash)
    assert(TemplateFitArchive.completed(changed, get(original.run)).isLeft)
  }

  test("all six pinned family documents restore and re-encode on this runtime") {
    def check[X](
        codec: TemplateFitCodec[String, X],
        plan: TemplateFitPlan[String, X],
        planText: String,
        resultText: String
    ): Unit =
      val savedPlan   = get(io.circe.parser.parse(planText))
      val savedResult = get(io.circe.parser.parse(resultText))
      assertEquals(codec.plans.encode(plan), Right(savedPlan))
      assertEquals(get(codec.plans.decode(savedPlan)).description, plan.description)
      assertEquals(codec.results.encode(get(TemplateFitArchive.run(plan))), Right(savedResult))
      assertEquals(
        codec.results.encode(get(codec.results.decode(savedResult))),
        Right(savedResult)
      )
    check(fixedCodec, fixed(), TemplateFitFixtures.fixedPlan, TemplateFitFixtures.fixedResult)
    check(
      fixedCodec,
      imported,
      TemplateFitFixtures.importedPlan,
      TemplateFitFixtures.importedResult
    )
    check(mapCodec, meanMap, TemplateFitFixtures.meanMapPlan, TemplateFitFixtures.meanMapResult)
  }

  test("a native saved plan refuses another norm convention") {
    val saved   = get(fixedCodec.plans.encode(fixed()))
    val fitting = get(saved.hcursor.downField("value").get[Json]("fitting"))
      .mapObject(_.add("normConvention", Json.fromString("runtime-native")))
    assert(fixedCodec.plans.decode(alter(saved, "fitting", fitting)).isLeft)
  }
