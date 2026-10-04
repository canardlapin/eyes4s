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

package eyes4s.repetitionconsumer

import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.compare.eyesim.EyesimCompat
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.{Json, Decoder}
import scala.compiletime.testing.typeCheckErrors

class RepetitionPlanSuite extends munit.FunSuite:
  private def get[E, A](x: Either[E, A]): A = x.fold(e => fail(e.toString), identity)
  private def id(name: String)              = get(DefinitionId.of(name, 1))
  case class Occasion(phase: String, repeat: Int)
  object Occasion:
    given KeyDigest[Occasion] = KeyDigest.derived[Occasion]
  case class Key(person: Int, stimulus: String, occasion: Occasion)
  object Key:
    given KeyDigest[Key] = KeyDigest.derived[Key]
    given Ordering[Key]  =
      Ordering.by(k => (k.person, k.stimulus, k.occasion.phase, k.occasion.repeat))
  private def layout = get(
    RepetitionLayout.of[Key, Int, String, Occasion](
      id("example.repetition-layout"),
      id("example.person"),
      Projection.named("person")(_.person),
      id("example.stimulus"),
      Projection.named("stimulus")(_.stimulus),
      id("example.occasion"),
      Projection.named("occasion")(_.occasion)
    )
  )
  private def registry = get(RepetitionRegistry.empty[Key].register(layout))
  private def field[A: Decoder](json: Json, name: String): Either[CodecError, A] =
    json.hcursor.get[A](name).left.map(e => CodecError.Field(name, json, e.message))
  private def keys = VersionedCodec.of[Key](id("example.repetition-key"))(k =>
    Json.obj(
      "person"   -> Json.fromInt(k.person),
      "stimulus" -> Json.fromString(k.stimulus),
      "phase"    -> Json.fromString(k.occasion.phase),
      "repeat"   -> Json.fromInt(k.occasion.repeat)
    )
  ) { json =>
    for
      p     <- field[Int](json, "person"); s   <- field[String](json, "stimulus");
      phase <- field[String](json, "phase"); r <- field[Int](json, "repeat")
    yield Key(p, s, Occasion(phase, r))
  }
  private def codec =
    RepetitionPlanCodec.of[Key, Px](id("example.repetition-plan"), registry, keys)
  private val grid = get(Grid.over(get(Frame.screen("repetition-plan", 5, 3)), 5, 3))
  private val rows =
    (for person <- Vector(1, 2); stimulus <- Vector("a", "b"); repeat <- Vector(0, 1, 2) yield
      val key    = Key(person, stimulus, Occasion("repeat", repeat))
      val values = IArray.tabulate(15)(i =>
        if i == (person + repeat + (if stimulus == "a" then 0 else 5)) % 15 then 2.0 else 1.0
      )
      val map = get(
        Surface
          .intensity(grid, values, Provenance.raw(ContentHash.of(values)))
          .flatMap(_.normalised)
      )
      Trial(key, (), map)
    )
  private val selection = Selection.BottomK(
    get(PairLimit.of(2)),
    Seed(Long.MinValue + 7),
    SampleId("finite-controls")
  )
  private def plan(
      trials: Trials[Key, Unit, Mass[Px]] = Trials(rows),
      method: MapSimilarityMethod = MapSimilarityMethod.Cosine,
      sel: Selection = selection
  ) =
    get(
      RepetitionPlan.of(
        layout,
        RepetitionRelations.withinParticipant,
        method,
        sel,
        FailurePolicy.RequireAll,
        grid,
        trials
      )
    )

  test(
    "save and reopen through fresh typed registrations reproduces exact endpoints, controls, counts and provenance"
  ) {
    val p        = plan(); val json = get(codec.encode(p));
    val reopened = get(codec.parse(json.noSpaces))
    assertEquals(reopened.planHash, p.planHash); assertEquals(reopened.inputHash, p.inputHash)
    assertEquals(get(codec.encode(reopened)), json)
    assertEquals(reopened.controls, selection)
    val a = p.run; val b = reopened.run
    assertEquals(a.matched, b.matched); assertEquals(a.controls, b.controls)
    assertEquals(
      get(a.contrasts).rows.map(r => (r.key, r.matched, r.control, r.difference)),
      get(b.contrasts).rows.map(r => (r.key, r.matched, r.control, r.difference))
    )
    val expected = for
      x <- rows; y <- rows
      if x.key.person == y.key.person && x.key.stimulus == y.key.stimulus && x.key.occasion != y.key.occasion
    yield x.key -> y.key
    assertEquals(a.matched.rows.map(r => r.left -> r.right), expected)
    assertEquals(a.matched.diagnostics.eligiblePairCount, 24L)
    assertEquals(a.controls.diagnostics.eligiblePairCount, 24L)
    assertEquals(a.controls.diagnostics.selectedPairCount, 24)
    a.controls.rows.foreach(r =>
      assert(
        r.left.person == r.right.person && r.left.stimulus != r.right.stimulus && r.left.occasion != r.right.occasion
      )
    )
    val l      = layout
    val direct = RepetitionDesign
      .withinParticipant(
        Projection.named[Key, Int]("person")(_.person),
        Projection.named[Key, String]("stimulus")(_.stimulus),
        Projection.named[Key, Occasion]("occasion")(_.occasion),
        selection
      )
      .evaluate(
        p.trials,
        p.inputHash,
        MapSimilarityMethod.Cosine.similarity[Px],
        p.specification
      )
    assertEquals(a.matched, direct.matched); assertEquals(a.controls, direct.controls)
    assertEquals(l.id, reopened.layout.id)
    assertEquals(p.inputHash.render, "26dd8b9539bed70d")
    assertEquals(p.planHash.render, "18c9fcde538303e3")
    assertEquals(get(codec.parse(RepetitionPlanFixture.versionOne)).run.matched, a.matched)
    assertEquals(json, get(io.circe.parser.parse(RepetitionPlanFixture.versionOne)))
  }
  test(
    "finite controls use the original keyed sampler and survive order changes; higher cap nests"
  ) {
    def edges(p: RepetitionPlan[Key, Px]) =
      p.run.controls.rows.map(r => r.left -> r.right).toSet
    val one = Selection.BottomK(
      get(PairLimit.of(1)),
      Seed(Long.MinValue + 7),
      SampleId("finite-controls")
    )
    val a = plan(sel = one); val p = get(codec.parse(get(codec.encode(a)).noSpaces))
    assertEquals(edges(a), edges(p));
    assertEquals(edges(a), edges(plan(Trials(rows.reverse), sel = one)))
    assert(edges(a).subsetOf(edges(plan())))
    assertEquals(a.run.controls.diagnostics.selectedPairCount, 12)
    assertEquals(
      edges(a).toVector
        .map((l, r) =>
          s"${l.person}/${l.stimulus}/${l.occasion.repeat}->${r.person}/${r.stimulus}/${r.occasion.repeat}"
        )
        .sorted
        .mkString(","),
      "1/a/0->1/b/2,1/a/1->1/b/0,1/a/2->1/b/1,1/b/0->1/a/1,1/b/1->1/a/2,1/b/2->1/a/0,2/a/0->2/b/1,2/a/1->2/b/2,2/a/2->2/b/1,2/b/0->2/a/1,2/b/1->2/a/2,2/b/2->2/a/1"
    )
    val byKey = rows.map(_.key)
    byKey.foreach { focal =>
      val eligible = byKey.filter(k =>
        k.person == focal.person && k.stimulus != focal.stimulus && k.occasion != focal.occasion
      )
      val wanted = eligible.minBy(k =>
        Selection.priority(Seed(Long.MinValue + 7), SampleId("finite-controls"), focal, k)
      )
      assert(edges(a).contains(focal -> wanted))
    }
  }
  test("every finite map method saves and reruns without new opaque closures") {
    // The compatibility-only legacy Fisher z stays readable in saved plans.
    (MapSimilarityMethod.values ++ EyesimCompat.methods).foreach { m =>
      val p       = plan(method = m)
      val encoded = get(codec.encode(p))
      assertEquals(
        encoded.hcursor.downField("value").get[String]("method"),
        Right(m.token)
      )
      val restored = get(codec.parse(encoded.noSpaces))
      assertEquals(restored.method, m)
      assertEquals(restored.run.matched, p.run.matched);
      assertEquals(restored.run.controls, p.run.controls)
    }
  }
  test(
    "duplicate and unmatched keys, empty input and constant-method failures survive persistence"
  ) {
    val duplicate = plan(Trials(rows :+ rows.head));
    val reopened  = get(codec.parse(get(codec.encode(duplicate)).noSpaces))
    assertEquals(reopened.run.matched, duplicate.run.matched)
    assert(reopened.run.matched.diagnostics.ambiguous.nonEmpty)
    val singleton = plan(Trials(rows.take(1)))
    assert(
      get(
        codec.parse(get(codec.encode(singleton)).noSpaces)
      ).run.matched.diagnostics.unmatchedLeft.nonEmpty
    )
    val empty = plan(Trials(Vector.empty))
    assert(get(codec.parse(get(codec.encode(empty)).noSpaces)).run.matched.rows.isEmpty)
    val constant =
      get(Surface.mass(grid, IArray.fill(15)(1.0 / 15), Provenance.raw(ContentHash.empty)))
    val partial = plan(
      Trials(rows.updated(0, rows.head.copy(value = constant))),
      MapSimilarityMethod.Pearson
    )
    val result = get(codec.parse(get(codec.encode(partial)).noSpaces)).run
    assert(result.matched.rows.exists(_.result.isLeft))
    assert(get(result.contrasts).rows.exists(_.difference.isLeft))
  }
  test(
    "unknown schemas, projections, rules, method revisions, seeds and forged digests fail as values"
  ) {
    val encoded = get(codec.encode(plan()));
    val value   = encoded.hcursor.downField("value").focus.get
    def reject(field: String, replacement: Json): Unit = assert(
      codec
        .decode(encoded.mapObject(_.add("value", value.mapObject(_.add(field, replacement)))))
        .isLeft
    )
    reject(
      "layout",
      Json.obj("name" -> Json.fromString("unknown"), "version" -> Json.fromInt(1))
    )
    reject("projections", Json.arr())
    reject("matched", Json.arr(Json.fromString("UnknownProjection")))
    reject("control", value.hcursor.downField("matched").focus.get)
    reject("method", Json.fromString("emd")); reject("methodRevision", Json.fromInt(2))
    reject("self", Json.fromString("include"));
    reject("orientation", Json.fromString("undirected"))
    reject("inputHash", Json.fromString("0000000000000000"));
    reject("planHash", Json.fromString("stale"))
    reject(
      "selection",
      Json.obj(
        "kind"     -> Json.fromString("bottomK"),
        "cap"      -> Json.fromInt(0),
        "seed"     -> Json.fromString("1"),
        "sampleId" -> Json.fromString("s")
      )
    )
    reject(
      "selection",
      Json.obj(
        "kind"     -> Json.fromString("bottomK"),
        "cap"      -> Json.fromInt(1),
        "seed"     -> Json.fromString("9223372036854775808"),
        "sampleId" -> Json.fromString("s")
      )
    )
    val wrongSchema = encoded.mapObject(
      _.add(
        "schema",
        Json.obj(
          "name"    -> Json.fromString("example.repetition-plan"),
          "version" -> Json.fromInt(2)
        )
      )
    )
    assert(codec.decode(wrongSchema).isLeft)
    assert(RepetitionRegistry.empty[Key].resolve(layout.id).isLeft)
    assert(registry.register(layout).isLeft)
    assert(RepetitionRelations.of(Vector.empty, Vector(RepetitionRule.SameOccasion)).isLeft)
    assert(
      RepetitionRelations
        .of(
          Vector(RepetitionRule.SameOccasion, RepetitionRule.DifferentOccasion),
          Vector(RepetitionRule.SameStimulus)
        )
        .isLeft
    )
    assert(
      RepetitionRelations
        .of(
          Vector(RepetitionRule.SameOccasion, RepetitionRule.SameOccasion),
          Vector(RepetitionRule.DifferentOccasion)
        )
        .isLeft
    )
    assert(
      typeCheckErrors("new eyes4s.plan.RepetitionRelations(Vector.empty,Vector.empty)").nonEmpty
    )
    assert(typeCheckErrors("new eyes4s.plan.RepetitionRegistry[String](Vector.empty)").nonEmpty)
  }
  test("legacy v1 two-phase study remains readable with its original meaning") {
    val old     = StudyCodecs.cosine[Px]
    val decoded = get(old.codec.parse(SavedStudyFixtures.versionOne))
    assertEquals(decoded.focalPhase, "recall"); assertEquals(decoded.referencePhase, "encode")
    assertEquals(
      get(old.codec.encode(decoded)),
      get(io.circe.parser.parse(SavedStudyFixtures.versionOne))
    )
    assert(codec.parse(SavedStudyFixtures.versionOne).isLeft)
  }

  test("a repetition plan describes and explains every field, and diffs by field") {
    val p = plan()
    assertEquals(
      p.description.map(_._1),
      Vector(
        "repetition.input",
        "layout",
        "grid",
        "method",
        "matched",
        "controls",
        "controlSelection",
        "failurePolicy",
        "pairing"
      )
    )
    val inspected = get(p.inspect)
    assertEquals(inspected.description, p.description)
    assert(inspected.fields.forall(_.info.meaning.nonEmpty))
    assertEquals(
      p.description.find(_._1 == "controlSelection").map(_._2),
      Some(
        Vector(
          Provenance.Param.Text("bottomK"),
          Provenance.Param.Num(2.0),
          Provenance.Param.Text((Long.MinValue + 7).toString),
          Provenance.Param.Text("finite-controls")
        )
      )
    )
    assertEquals(
      p.diff(plan(sel = Selection.All, method = MapSimilarityMethod.Pearson)).map(_.field),
      Vector("controlSelection", "method")
    )
    assertEquals(p.diff(p), Vector.empty)
  }

  test("the repetition form restores every field and rebuilds the plan's description") {
    val form = new RepetitionForm
    Vector(plan(), plan(sel = Selection.All, method = MapSimilarityMethod.Spearman)).foreach {
      p =>
        val values = form.values(p)
        form.fields.foreach { f =>
          val raw = values.get(f.view.id)
          assertEquals(f.restore(raw), Right(raw), f.view.id)
          assertEquals(
            FieldView
              .of(f.view.id, f.view.version, f.view.meaning, f.view.kind, f.view.default),
            Right(f.view)
          )
        }
        val rebuilt = get(get(form.parse(values).left.map(_.toVector)).plan(p))
        assertEquals(rebuilt.description, p.description)
        assertEquals(rebuilt.planHash, p.planHash)
    }
    // The rules are checked together, by the plan: overlapping relations are refused.
    val overlapping = form
      .values(plan())
      .updated(
        form.controls.view.id,
        RawValue.Items(Vector(RawValue.Choice("SameParticipant")))
      )
    assert(
      get(form.parse(overlapping).left.map(_.toVector)).plan(plan()).left.exists {
        case RepetitionPlanError.OverlappingRelations(_, _) => true
        case _                                              => false
      }
    )
    // A field that cannot be read is refused on its own, naming the field.
    val unknown = form.values(plan()).updated(form.method.view.id, RawValue.Choice("Cubic"))
    assert(
      form
        .parse(unknown)
        .left
        .exists(_.head match
          case FieldError.NotAChoice(f, "Cubic", _) => f == form.method.view.id
          case FieldError.Refused(f, _, _, _)       => f == form.method.view.id
          case _                                    => false),
      form.parse(unknown)
    )
  }

  test("the repetition cursor equals run at every quanta, matched pairs first, then controls") {
    def quanta(pairs: Int) = WorkQuanta(get(PairQuantum.of(pairs)), ComparisonQuantum.default)
    Vector(plan(), plan(sel = Selection.All)).foreach { p =>
      val expected = p.run
      Vector(quanta(1), quanta(3), WorkQuanta.default).foreach { q =>
        val stepped = get(Stepwise.complete(p.work, q))
        assertEquals(stepped.matched, expected.matched, q)
        assertEquals(stepped.controls, expected.controls, q)
        // The contrasts are a function of these two analyses and the policy.
        assertEquals(stepped.policy, expected.policy)
        assertEquals(stepped.planHash, expected.planHash)
      }
      // One pair per step at the finest quantum: each stage's steps are its pairs,
      // and the stages run matched, then control.
      @annotation.tailrec
      def trace(
          c: RepetitionCursor[Key],
          seen: Vector[(RepetitionStage, Int)]
      ): Vector[(RepetitionStage, Int)] =
        get(c.advance(quanta(1))) match
          case WorkStep.More(stage, units, next) => trace(next, seen :+ (stage -> units))
          case WorkStep.Done(units, _)           => seen :+ (RepetitionStage.Control -> units)
      val steps = trace(p.work, Vector.empty)
      assertEquals(
        steps,
        Vector.fill(expected.matched.rows.size)(RepetitionStage.Matched -> 1) ++
          Vector.fill(expected.controls.rows.size)(RepetitionStage.Control -> 1)
      )
      assertEquals(p.work.stage, RepetitionStage.Matched)
    }
  }

  test("condition grouping names its estimand and requires an explicit participant scope") {
    // The old unscoped name reproduced eyesim issue #28's inverted contrast silently.
    assert(typeCheckErrors("eyes4s.plan.RepetitionRelations.conditionGroups").nonEmpty)
    assert(
      typeCheckErrors(
        "val r: eyes4s.plan.RepetitionRelations = eyes4s.plan.RepetitionRelations.referenceConditionGrouping"
      ).nonEmpty
    )
    def run(scope: ParticipantScope) = get(
      RepetitionPlan.of(
        layout,
        RepetitionRelations.referenceConditionGrouping(scope),
        MapSimilarityMethod.Cosine,
        Selection.All,
        FailurePolicy.RequireAll,
        grid,
        Trials(rows)
      )
    ).run
    val within = run(ParticipantScope.WithinParticipant)
    val across = run(ParticipantScope.AcrossParticipants)
    val pooled = run(ParticipantScope.Pooled)
    val scoped = Set(RepetitionRule.SameParticipant, RepetitionRule.DifferentParticipant)
    ParticipantScope.values.foreach { scope =>
      val r = RepetitionRelations.referenceConditionGrouping(scope)
      assertEquals(r.matched.filter(scoped), r.controls.filter(scoped))
      assertEquals(r.matched.filter(scoped), scope.rule.toVector)
    }
    assertEquals(within.matched.rows.size, 12)
    assert(within.matched.rows.forall(r => r.left.person == r.right.person))
    assert(within.controls.rows.forall(r => r.left.person == r.right.person))
    assertEquals(across.matched.rows.size, 24)
    assert(across.matched.rows.forall(r => r.left.person != r.right.person))
    assertEquals(pooled.matched.rows.size, 36)
    // Every scope puts same-stimulus pairs from other occasions among the CONTROLS:
    // this estimand is condition grouping, not reinstatement.
    Vector(within, pooled).foreach { result =>
      assert(
        result.controls.rows.exists(r =>
          r.left.person == r.right.person && r.left.stimulus == r.right.stimulus
        )
      )
    }
  }

  test(
    "finite registered relation vocabulary covers condition groups and alternative typed axes"
  ) {
    val conditions = get(
      RepetitionPlan.of(
        layout,
        RepetitionRelations.referenceConditionGrouping(ParticipantScope.Pooled),
        MapSimilarityMethod.Cosine,
        Selection.All,
        FailurePolicy.RequireAll,
        grid,
        Trials(rows)
      )
    )
    val restored = get(codec.parse(get(codec.encode(conditions)).noSpaces))
    assertEquals(restored.run.matched, conditions.run.matched)
    assertEquals(restored.run.matched.rows.size, 36)
    val relations = get(
      RepetitionRelations.of(
        Vector(
          RepetitionRule.DifferentParticipant,
          RepetitionRule.SameStimulus,
          RepetitionRule.SameOccasion
        ),
        Vector(
          RepetitionRule.DifferentParticipant,
          RepetitionRule.DifferentStimulus,
          RepetitionRule.SameOccasion
        )
      )
    )
    val p = get(
      RepetitionPlan.of(
        layout,
        relations,
        MapSimilarityMethod.Cosine,
        Selection.All,
        get(FailurePolicy.successfulOnly(1)),
        grid,
        Trials(rows)
      )
    )
    val result = get(codec.parse(get(codec.encode(p)).noSpaces)).run
    assertEquals(result.matched.rows.size, 12); assertEquals(result.controls.rows.size, 12)
    result.matched.rows.foreach(r =>
      assert(
        r.left.person != r.right.person && r.left.stimulus == r.right.stimulus && r.left.occasion == r.right.occasion
      )
    )
    assert(
      RepetitionLayout
        .of[Key, Int, String, Occasion](
          id("bad"),
          id("same"),
          Projection.named("person")(_.person),
          id("same"),
          Projection.named("stimulus")(_.stimulus),
          id("occasion"),
          Projection.named("occasion")(_.occasion)
        )
        .isLeft
    )
    val foreign = get(Grid.over(get(Frame.screen("other", 5, 3)), 5, 3))
    assert(
      RepetitionPlan
        .of(
          layout,
          relations,
          MapSimilarityMethod.Cosine,
          Selection.All,
          FailurePolicy.RequireAll,
          foreign,
          Trials(rows)
        )
        .isLeft
    )
  }
