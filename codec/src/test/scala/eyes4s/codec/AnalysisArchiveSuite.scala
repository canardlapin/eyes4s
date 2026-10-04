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

import cats.data.NonEmptyVector
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

/** Analysis plans and results under the generic `analysis-plan` and
  * `analysis-result` roles (CR4 S2): the manifest form of their relation, its
  * structural checks, and resolution through an [[AnalysisRegistry]].
  */
class AnalysisArchiveSuite extends munit.FunSuite:
  import AnalysisFixtures.*

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private def name(value: String)           = get(ArtifactName.of(value))

  private val frame                = get(Frame.screen("analysis", 2, 2))
  private def clock(key: StudyKey) =
    ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")
  private def trial(key: StudyKey, n: Int) =
    val fixes = (0 until n).map { i =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval
              .of(clock(key), Instant.micros(i * 2000L), Instant.micros(i * 2000L + 1000L))
          ),
          Pt[Px](0.5, 0.5),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock(key), IArray.from(fixes))))

  private val study = StudyInput(
    Trials(
      Vector(
        trial(StudyKey("p1", "a", "recall"), 2),
        trial(StudyKey("p1", "a", "encode"), 3),
        trial(StudyKey("p1", "b", "recall"), 1)
      )
    )
  )
  private val other    = StudyInput(Trials(Vector(trial(StudyKey("p2", "a", "recall"), 4))))
  private val inputs   = StudyInputCodecs.study[Px]
  private val decoders = get(ArtifactDecoders.study[Px])
  private val plan     = CountPlan("recall")
  private val result   = run(plan, study, _.phase)

  /** A graph of the study input, the plan and its result. */
  private def saved(
      stored: CountResult = result,
      relation: (ArtifactName, ArtifactName, AnalysisInputs) => ManifestRelation =
        ManifestRelation.AnalysisResultOf.apply
  ) = for
    in    <- StoredArtifact.input("input", inputs, study)
    more  <- StoredArtifact.input("other", inputs, other)
    p     <- StoredArtifact.analysisPlan("count-plan", plans, plan)
    r     <- StoredArtifact.analysisResult("count", results, stored)
    graph <- SavedManifest.of(
      Vector(in, more, p, r),
      Vector(relation(r.name, p.name, entries(in.name)))
    )
  yield graph

  private def resolve(graph: SavedManifest, by: ArtifactDecoders[StudyKey, Px]) =
    ArtifactResolver.resolve(graph.address, graph.source, by)

  test("a result resolves with its plan through the registry, typed by its family") {
    val graph    = get(saved())
    val resolved = get(resolve(graph, decoders.withAnalyses(registry)))
    assertEquals(result.fixations, 3)
    resolved.analysisPlan(name("count-plan")) match
      case Some(p: LoadedPlan) => assertEquals(p.plan, plan)
      case other               => fail(s"$other")
    resolved.analysisResult(name("count")) match
      case Some(r: LoadedResult) => assertEquals(r.result, result)
      case other                 => fail(s"$other")
    assertEquals(resolved.analysisPlans.map(_._1), Vector(name("count-plan")))
  }

  test("an analysis schema no registration names is a typed codec error") {
    val graph = get(saved())
    assertEquals(
      resolve(graph, decoders),
      Left(
        NonEmptyVector.of(
          ResolveError.Decode(
            name("count-plan"),
            CodecError.UnsupportedSchema("analysis-plan", planSchema, Vector.empty)
          ),
          ResolveError.Decode(
            name("count"),
            CodecError.UnsupportedSchema("analysis-result", resultSchema, Vector.empty)
          )
        )
      )
    )
    // Another family's registry names the schemas it supports.
    val elsewhere = get(
      AnalysisRegistry.empty.register(
        registration.copy(
          plan = get(DefinitionId.of("test.other-plan", 1)),
          result = get(DefinitionId.of("test.other-result", 1))
        )
      )
    )
    assertEquals(
      elsewhere.result(resultSchema, Json.Null),
      Left(
        CodecError.UnsupportedSchema(
          "analysis-result",
          resultSchema,
          Vector(get(DefinitionId.of("test.other-result", 1)))
        )
      )
    )
  }

  test(
    "a result computed on another input, or under another plan, is refused by its relation"
  ) {
    val onOther  = get(saved(run(plan, other, _.phase)))
    val relation = get(onOther.manifest.relations.headOption.toRight("relation"))
    assertEquals(
      resolve(onOther, decoders.withAnalyses(registry)),
      Left(
        NonEmptyVector.one(
          ResolveError.Relation(
            relation,
            RelationMismatch.ResultInput(study.hash.render, other.hash.render)
          )
        )
      )
    )
    val replanned = get(saved(result.copy(plan = CountPlan("encode"))))
    assert(
      resolve(replanned, decoders.withAnalyses(registry)).left.exists(
        _.head match
          case ResolveError.Relation(_, RelationMismatch.Description(changes)) =>
            changes.map(_.field) == Vector("phase")
          case _ => false
      )
    )
  }

  test("an analysis result needs exactly one relation, to distinct input entries") {
    def refused(relations: ArtifactName => Vector[ManifestRelation]) = for
      in <- StoredArtifact.input("input", inputs, study)
      p  <- StoredArtifact.analysisPlan("count-plan", plans, plan)
      r  <- StoredArtifact.analysisResult("count", results, result)
      g  <- SavedManifest.of(Vector(in, p, r), relations(in.name))
    yield g
    val (p, r) = (name("count-plan"), name("count"))
    assertEquals(
      refused(_ => Vector.empty).left.toOption,
      Some(
        CodecError.Manifest(
          ManifestError.RelationCount(r, "analysis-result-of", 0, "exactly one")
        )
      )
    )
    assertEquals(
      refused(in =>
        Vector(ManifestRelation.AnalysisResultOf(r, p, entries(in, in)))
      ).left.toOption,
      Some(
        CodecError.Manifest(
          ManifestError.RelationCount(r, "analysis-input", 2, "at least one, each distinct")
        )
      )
    )
    val onPlan = ManifestRelation.AnalysisResultOf(r, p, entries(p))
    assertEquals(
      refused(_ => Vector(onPlan)).left.toOption,
      Some(
        CodecError.Manifest(ManifestError.AnalysisInput(onPlan, p, ArtifactRole.AnalysisPlan))
      )
    )
    val missing = ManifestRelation.AnalysisResultOf(r, p, entries(name("absent")))
    assertEquals(
      refused(_ => Vector(missing)).left.toOption,
      Some(CodecError.Manifest(ManifestError.UnknownEntry(missing, name("absent"))))
    )
    val swapped = ManifestRelation.AnalysisResultOf(p, r, entries(name("input")))
    assertEquals(
      refused(_ => Vector(swapped)).left.toOption,
      Some(
        CodecError.Manifest(
          ManifestError
            .RoleMismatch(swapped, p, ArtifactRole.AnalysisResult, ArtifactRole.AnalysisPlan)
        )
      )
    )
  }

  test("the relation's wire form lists its inputs, and the manifest reads back") {
    val graph = get(saved())
    val json  = get(ScientificManifest.codec.encode(graph.manifest))
    assertEquals(
      json.hcursor.downField("value").downField("relations").downN(0).focus,
      Some(
        Json.obj(
          "kind"   -> Json.fromString("analysis-result-of"),
          "result" -> Json.fromString("count"),
          "plan"   -> Json.fromString("count-plan"),
          "inputs" -> Json.arr(Json.fromString("input"))
        )
      )
    )
    assertEquals(ScientificManifest.codec.decode(json), Right(graph.manifest))
    // The analysis documents hold no floating-point number, so their stored
    // bytes are pinned on the JVM and Scala.js alike.
    assertEquals(
      graph.manifest.entries.filter(_.role.wire.startsWith("analysis")).map(_.sha256.hex),
      Vector(
        "c64d9173a46730344b9b4405e932821371545e6d0fe9da778fa6d277f2bf5268",
        "6b5d0f7d69a3ecb3596247bb196dc2a45076a2d333a650251ac03d7aed6dc031"
      )
    )
    assertEquals(
      graph.manifest.entries.map(_.role.wire).filter(_.startsWith("analysis")),
      Vector("analysis-plan", "analysis-result")
    )
  }

  /** A graph of both inputs, one plan, one result and one relation. */
  private def graph(
      plan: StoredArtifact,
      result: StoredArtifact,
      placed: (ArtifactName, ArtifactName) => AnalysisInputs
  ) = get(
    for
      in    <- StoredArtifact.input("input", inputs, study)
      more  <- StoredArtifact.input("other", inputs, other)
      saved <- SavedManifest.of(
        Vector(in, more, plan, result),
        Vector(
          ManifestRelation.AnalysisResultOf(result.name, plan.name, placed(in.name, more.name))
        )
      )
    yield saved
  )
  private def refusal(g: SavedManifest): Option[RelationMismatch] =
    resolve(g, decoders.withAnalyses(registry)).left.toOption.map(_.head).collect {
      case ResolveError.Relation(_, mismatch) => mismatch
    }

  test("input entries are checked in the order the result cites them") {
    val both = result.copy(inputs = Vector(study.hash, other.hash))
    val p    = get(StoredArtifact.analysisPlan("count-plan", plans, plan))
    val r    = get(StoredArtifact.analysisResult("count", results, both))
    assert(
      resolve(
        graph(p, r, (in, more) => entries(in, more)),
        decoders.withAnalyses(registry)
      ).isRight
    )
    assertEquals(
      refusal(graph(p, r, (in, more) => entries(more, in))),
      Some(
        RelationMismatch.ResultInput(
          s"${other.hash.render},${study.hash.render}",
          s"${study.hash.render},${other.hash.render}"
        )
      )
    )
  }

  test("a result related to another family's plan is refused, though the descriptions agree") {
    val embedded = get(
      StoredArtifact.analysisPlan(
        "count-plan",
        embeddedPlans,
        EmbeddedPlan("recall", study.hash)
      )
    )
    val r = get(StoredArtifact.analysisResult("count", results, result))
    assertEquals(embedded.entry.schema, embeddedPlanSchema)
    assertEquals(
      refusal(graph(embedded, r, (in, _) => entries(in))),
      Some(RelationMismatch.AnalysisFamily(resultSchema, embeddedPlanSchema, planSchema))
    )
  }

  test(
    "an input the plan embeds: checked against the plan's identity, and only for a family that declares it"
  ) {
    val p = get(
      StoredArtifact.analysisPlan(
        "count-plan",
        embeddedPlans,
        EmbeddedPlan("recall", study.hash)
      )
    )
    val r        = get(StoredArtifact.analysisResult("count", embeddedResults, result))
    val g        = graph(p, r, (_, _) => AnalysisInputs.EmbeddedInPlan)
    val resolved = get(resolve(g, decoders.withAnalyses(registry)).left.map(_.toVector))
    resolved.analysisResult(name("count")) match
      case Some(loaded: LoadedEmbeddedResult) => assertEquals(loaded.result, result)
      case other                              => fail(s"$other")
    // The result cites another input than the one the plan carries.
    val elsewhere = get(
      StoredArtifact.analysisResult("count", embeddedResults, run(plan, other, _.phase))
    )
    assertEquals(
      refusal(graph(p, elsewhere, (_, _) => AnalysisInputs.EmbeddedInPlan)),
      Some(RelationMismatch.ResultInput(study.hash.render, other.hash.render))
    )
    // A family that does not declare an embedded input.
    val counted = get(StoredArtifact.analysisPlan("count-plan", plans, plan))
    val r2      = get(StoredArtifact.analysisResult("count", results, result))
    assertEquals(
      refusal(graph(counted, r2, (_, _) => AnalysisInputs.EmbeddedInPlan)),
      Some(RelationMismatch.UndeclaredEmbedding(planSchema))
    )
    // The two placements have distinct wire forms, and an empty list is no placement.
    val json      = get(ScientificManifest.codec.encode(g.manifest))
    val relations = json.hcursor.downField("value").downField("relations")
    assertEquals(
      relations.downN(0).downField("inputs").focus,
      Some(Json.fromString("embedded-in-plan"))
    )
    assertEquals(ScientificManifest.codec.decode(json), Right(g.manifest))
    def withInputs(value: Json) =
      relations.downN(0).downField("inputs").set(value).top.getOrElse(fail("json"))
    assertEquals(
      ScientificManifest.codec.decode(withInputs(Json.arr())).left.toOption,
      Some(
        Wire.at("relations[0]")(
          CodecError.Manifest(
            ManifestError.RelationCount(
              name("count"),
              "analysis-input",
              0,
              "at least one, each distinct"
            )
          )
        )
      )
    )
    assert(ScientificManifest.codec.decode(withInputs(Json.fromString("nowhere"))).isLeft)
  }

  test("a registry refuses a schema registered twice") {
    assertEquals(
      registry.register(registration),
      Left(CodecError.DuplicateResultCodec(planSchema))
    )
    assertEquals(
      AnalysisRegistry.empty.register(registration.copy(result = planSchema)),
      Left(CodecError.DuplicateResultCodec(planSchema))
    )
  }
