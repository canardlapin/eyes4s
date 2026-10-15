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
import io.circe.Json

private[codec] object ScanpathStudyCodecExamples:
  private def get[E, A](value: Either[E, A]): A = value.toOption.get
  val frame                                     = get(Frame.screen("scanpath-family", 100, 100))
  val clock                                     = ClockId("scanpath-family")
  val layout                                    = StudyKey.layout(DefinitionId.studyLayout)
  val keys                                      = StudyCodecs.key(DefinitionId.studyKey)
  val codec                              = new ScanpathStudyCodec[StudyKey, Px](layout, keys)
  private def path(x: Double, y: Double) =
    get(
      Scanpath.of(
        frame,
        clock,
        IArray.from(Vector((x, y), (x + 4, y + 3), (x + 2, y + 6)).zipWithIndex.map {
          (point, index) =>
            get(
              Event.Fixation.withoutDispersion(
                get(
                  Interval.of(
                    clock,
                    Instant.micros(index * 1000L),
                    Instant.micros(index * 1000L + 500)
                  )
                ),
                Pt[Px](point._1, point._2),
                1
              )
            )
        })
      )
    )
  val rows = Vector("a", "b", "c").zipWithIndex.flatMap { (item, index) =>
    Vector("focal", "reference").map { phase =>
      Trial(
        StudyKey("p", item, phase),
        (),
        path(index * 12 + 1.0, if phase == "focal" then 1.0 else 5.0)
      )
    }
  }
  val queries = Vector(Long.MinValue, 0L, 1000L, 1000L, Long.MaxValue).map(Instant.micros)
  def overlap(policy: MissingOverlapPolicy): ScanpathMethod[Px] =
    ScanpathMethod.Overlap(
      get(
        FixationOverlapSpec.of(
          clock,
          queries,
          get(OverlapThreshold.of[Px](5.0)),
          FixationGroundDistance.Euclidean,
          TrajectoryEndpoint.OnsetRange,
          policy
        )
      )
    )
  val methods: Vector[ScanpathMethod[Px]] = Vector(
    ScanpathMethods.baselineMultiMatch[Px],
    overlap(MissingOverlapPolicy.CountAsNonOverlap),
    ScanpathMethod.Transport(
      get(
        FixationTransportConfig
          .of[Px](20, 20, Span.micros(1000), 0.25, 0.5, 10000, 1e-10, 100, 1000000L)
      )
    )
  )
  def plan(
      method: ScanpathMethod[Px],
      source: Vector[Trial[StudyKey, Unit, Scanpath[Px]]] = rows,
      selection: Selection = Selection.All,
      target: Frame[Px] = frame,
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): ScanpathStudyPlan[StudyKey, Px] = get(
    ScanpathStudyPlan.of(
      layout,
      StudyInput(Trials(source)),
      target,
      "focal",
      "reference",
      method,
      controls = selection,
      budget = budget
    )
  )

class ScanpathStudyCodecSuite extends munit.FunSuite:
  import ScanpathStudyCodecExamples.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def alter(document: Json, field: String, value: Json): Json =
    document.mapObject(
      _.add("value", get(document.hcursor.get[Json]("value")).mapObject(_.add(field, value)))
    )
  private def updateAt(document: Json, fields: List[String])(f: Json => Json): Json =
    fields match
      case Nil          => f(document)
      case head :: tail =>
        document.mapObject(fields =>
          fields(head).fold(fields)(value => fields.add(head, updateAt(value, tail)(f)))
        )

  test("three method plans and complete keyed result ledgers round-trip") {
    methods.foreach { method =>
      val p       = plan(method); val archive = get(ScanpathStudyArchive.run(p))
      val saved   = get(codec.results.encode(archive))
      val decoded = get(codec.results.decode(saved))
      assertEquals(codec.results.encode(decoded), Right(saved))
      assertEquals(
        get(codec.plans.decode(get(codec.plans.encode(p)))).description,
        p.description
      )
      assertEquals(decoded.result.inputHash, p.inputHash)
      assertEquals(decoded.result.planHash, p.planHash)
      assertEquals(decoded.result.matched.rows.size, 3)
      assertEquals(decoded.result.controls.rows.size, 6)
      assertEquals(decoded.result.contrasts.size, 3 * method.componentNames.size)
    }
  }

  test("strict missing-support summaries preserve every original exact64 query row") {
    val p       = plan(overlap(MissingOverlapPolicy.RequireComplete))
    val archive = get(ScanpathStudyArchive.run(p))
    val saved   = get(codec.results.encode(archive))
    val decoded = get(codec.results.decode(saved))
    decoded.result.matched.rows.foreach { row =>
      get(row.result) match
        case ScanpathMethodOutput.Overlap(value) =>
          assertEquals(value.rows.map(_.time), queries)
          assertEquals(value.requested, 5)
          assertEquals(value.contributing, 3)
          assert(value.similarity.isLeft)
        case other => fail(s"$other")
    }
    assert(decoded.result.contrasts.forall(_.matched.result.isLeft))
    val changed = updateAt(saved, List("value", "result", "matched", "rows"))(
      _.mapArray(rows =>
        rows.updated(
          0,
          updateAt(rows.head, List("result", "value", "queries"))(_.mapArray(_.drop(1)))
        )
      )
    )
    assert(codec.results.decode(changed).isLeft)
  }

  test("completed archives preserve result identity and refuse a different plan binding") {
    val p       = plan(methods.head); val result = get(p.run)
    val archive = get(ScanpathStudyArchive.completed(p, result))
    assert(archive.result eq result)
    assert(ScanpathStudyArchive.completed(plan(methods(1)), result).isLeft)
    assert(codec.results.encode(archive).isRight)
  }

  test("run-stamp refusal precedes replay and forged complete results are rejected") {
    val saved = get(codec.results.encode(get(ScanpathStudyArchive.run(plan(methods.head)))))
    val wrong =
      Json.obj("plan" -> Json.fromString("0" * 64), "input" -> Json.fromString("0" * 64))
    codec.results.decode(alter(alter(saved, "runStamp", wrong), "result", Json.Null)) match
      case Left(CodecError.Stamp(_)) => ()
      case other                     => fail(s"Expected stamp refusal, found=$other")
    assert(codec.results.decode(alter(saved, "result", Json.Null)).isLeft)
  }

  test("nominal frame failures and duplicate source keys retain their keyed results") {
    val changed = get(Frame.screen("scanpath-family", 200, 100))
    val p       = plan(methods.head, target = changed)
    val archive = get(ScanpathStudyArchive.run(p))
    val saved   = get(codec.results.encode(archive))
    assert(get(codec.results.decode(saved)).result.matched.rows.forall(_.result.isLeft))
    val duplicate        = plan(methods(1), source = rows :+ rows.head)
    val duplicateArchive = get(ScanpathStudyArchive.run(duplicate))
    val duplicateSaved   = get(codec.results.encode(duplicateArchive))
    val restored         = get(codec.results.decode(duplicateSaved))
    assert(restored.result.matched.diagnostics.ambiguous.nonEmpty)
    assertEquals(codec.results.encode(restored), Right(duplicateSaved))
  }

  test("plan wires retain exact work limits and refuse unknown admission conventions") {
    val selection = Selection.BottomK(get(PairLimit.of(1)), Seed(Long.MinValue), SampleId("λ"))
    val p         = plan(
      methods.head,
      selection = selection,
      budget = get(PairScheduleBudget.of(100, Long.MaxValue, Int.MaxValue))
    )
    val saved = get(codec.plans.encode(p))
    assertEquals(get(codec.plans.decode(saved)).description, p.description)
    assert(
      codec.plans
        .decode(alter(saved, "preparationAdmission", Json.fromString("unknown")))
        .isLeft
    )
    assert(codec.plans.decode(alter(saved, "planHash", Json.fromString("0" * 16))).isLeft)
  }

  test("generic manifest roles resolve the stamped result and embedded complete input") {
    val p            = plan(methods(1)); val archive = get(ScanpathStudyArchive.run(p))
    val storedPlan   = get(StoredArtifact.analysisPlan("scanpath-plan", codec.plans, p))
    val storedResult =
      get(StoredArtifact.analysisResult("scanpath-result", codec.results, archive))
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
      decoded.analysisResult(storedResult.name).getOrElse(fail("missing scanpath result"))
    assertEquals(result.inputs, Vector(p.inputHash))
    assert(LoadedAnalysisResult.typed(codec.results)(result).nonEmpty)
  }

  Vector(
    (
      "MultiMatch",
      ScanpathStudyFixtures.multimatchPlan,
      ScanpathStudyFixtures.multimatchResult
    ),
    ("overlap", ScanpathStudyFixtures.overlapPlan, ScanpathStudyFixtures.overlapResult),
    ("transport", ScanpathStudyFixtures.transportPlan, ScanpathStudyFixtures.transportResult)
  ).foreach { (name, planText, resultText) =>
    test(s"$name pinned complete plan and result replay on this runtime") {
      val plan   = get(io.circe.parser.parse(planText))
      val result = get(io.circe.parser.parse(resultText))
      assertEquals(codec.plans.encode(get(codec.plans.decode(plan))), Right(plan))
      assertEquals(codec.results.encode(get(codec.results.decode(result))), Right(result))
    }
  }
