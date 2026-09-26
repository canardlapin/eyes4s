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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.results.*
import io.circe.Json

class StampedManifestSuite extends munit.FunSuite:
  import ManifestFixtures.{
    studies,
    inputs,
    results,
    plan,
    input,
    planArtifact,
    inputArtifact,
    decoders
  }
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val completed = get(get(StampedStudy.prepare(plan, input, studies, inputs)).run)
  private val packed    = get(ResultManifest.stamped("result", results, completed))

  private def save(
      result: ResultArtifacts = packed,
      p: StoredArtifact = planArtifact,
      i: StoredArtifact = inputArtifact
  ) = get(
    SavedManifest.of(
      Vector(p, i, result.result) ++ result.payloads,
      Vector(
        ManifestRelation.PlanInput(p.name, i.name),
        ManifestRelation.ResultOf(result.result.name, p.name, i.name)
      ) ++ result.relations
    )
  )

  private def resolve(
      saved: SavedManifest,
      registry: ArtifactDecoders[StudyKey, Px] = decoders
  ) =
    ArtifactResolver.resolve(saved.address, saved.source, registry).left.map(_.toVector)

  test("packed and inline stamped results resolve and preserve their saved claims") {
    Vector(DensityStorage.Packed, DensityStorage.Inline).foreach { storage =>
      val written = get(ResultManifest.stamped("result", results, completed, storage))
      val loaded  = get(resolve(save(written))).results.head._2
      assert(loaded.stampClaim.nonEmpty)
      assertEquals(loaded.stampClaim.map(_.input.display), Some(completed.stamp.input.display))
      val encoded = get(loaded.encode)
      assertEquals(get(encoded.hcursor.downField("schema").get[Int]("version")), 2)
      assertEquals(
        get(encoded.hcursor.downField("value").downField("runStamp").get[String]("plan")),
        completed.stamp.plan.sha256.hex
      )
    }
  }

  test("well-formed forged plan and input claims are refused with both canonical operands") {
    val document = get(Documents.parse(packed.result.bytes))
    Vector("plan", "input").foreach { field =>
      val altered = document.mapObject(o =>
        o.add(
          "value",
          o("value").get.mapObject(v =>
            v.add(
              "runStamp",
              v("runStamp").get.mapObject(_.add(field, Json.fromString("0" * 64)))
            )
          )
        )
      )
      val artifact =
        get(StoredArtifact.document("result", ArtifactRole.StudyResult, altered, None))
      val errors = resolve(save(packed.copy(result = artifact))).left.toOption
        .getOrElse(fail("forgery accepted"))
      assert(errors.exists {
        case ResolveError.Relation(_, RelationMismatch.RunPlan(reported, current, changes)) =>
          field == "plan" && reported.hex == "0" * 64 && current == completed.stamp.plan.sha256 && changes.isEmpty
        case ResolveError.Relation(_, RelationMismatch.RunInput(reported, current)) =>
          field == "input" && reported.hex == "0" * 64 && current == completed.stamp.input.sha256
        case _ => false
      })
    }
  }

  test("changed input evidence cannot pass by retaining the legacy input identity") {
    val frame = get(Frame.screen("manifest-stamp", 10, 10))
    val clock = ClockId("manifest-stamp")
    val span  = get(Interval.of(clock, Instant.micros(0), Instant.micros(1000)))
    def evidence(dispersion: Double) =
      val fixation =
        get(Event.Fixation.of(span, Pt[Px](5, 5), dispersion, DispersionMethod.RmsRadius, 2))
      StudyInput(
        Trials(
          Vector(
            Trial(
              StudyKey("p", "a", "recall"),
              (),
              get(Scanpath.of(frame, clock, IArray(fixation)))
            )
          )
        )
      )
    val before = evidence(0.1)
    val after  = evidence(0.2)
    assertEquals(before.reference, after.reference)
    val p = StudyResultFixtures.cosinePlan(
      before,
      FailurePolicy.RequireAll,
      Vector(StudyEstimate.Binned[Px]())
    )
    val finished = get(get(StampedStudy.prepare(p, before, studies, inputs)).run)
    val written  = get(ResultManifest.stamped("result", results, finished))
    val saved    = save(
      written,
      get(StoredArtifact.plan("plan", studies, p)),
      get(StoredArtifact.input("input", inputs, after))
    )
    assert(resolve(saved).left.toOption.exists(_.exists {
      case ResolveError.Relation(_, RelationMismatch.RunInput(reported, current)) =>
        reported == finished.stamp.input.sha256 && current == get(
          inputs.input.digest(after)
        ).sha256
      case _ => false
    }))
  }

  test("canonical input encoding is delegated and a missing custom encoder fails closed") {
    var calls   = 0
    val counted = new ArtifactDecoders.Delegating(decoders):
      override def inputDigest(document: Json, value: StudyInput[StudyKey, Px]) =
        calls += 1
        super.inputDigest(document, value)
    assert(resolve(save(), counted).isRight)
    assertEquals(calls, 1)
    val legacy = new ArtifactDecoders[StudyKey, Px]:
      def plan(d: Json)   = decoders.plan(d)
      def input(d: Json)  = decoders.input(d)
      def ledger(d: Json) = decoders.ledger(d)
      def result(d: Json) = decoders.result(d)
      override def resultWithPayloads(d: Json, p: PayloadRef => Option[VerifiedPayload]) =
        decoders.resultWithPayloads(d, p)
      def recording(d: Json, p: PayloadRef => Option[VerifiedPayload]) =
        decoders.recording(d, p)
      def recordingInput(d: Json) = decoders.recordingInput(d)
      def temporalInput(
          d: Json,
          b: ArtifactRef[StudyInput[StudyKey, Px]] => Option[StudyInput[StudyKey, Px]]
      ) = decoders.temporalInput(d, b)
    assert(resolve(save(), legacy).left.toOption.exists(_.exists {
      case ResolveError.Decode(_, CodecError.Unsupported("runStamp.input", _)) => true
      case _                                                                   => false
    }))
  }

  test("legacy results are still explicitly unstamped") {
    val written = get(ResultManifest.packed("result", results, ManifestFixtures.result))
    assertEquals(get(resolve(save(written))).results.head._2.stampClaim, None)
  }

  Vector(DensityStorage.Inline, DensityStorage.Packed).foreach { storage =>
    test(s"$storage stamped reports retain the score schema and refuse altered estimates") {
      val written  = get(ResultManifest.stamped("result", results, completed, storage))
      val document = get(Documents.parse(written.result.bytes))
      val binding  = ReportBinding(
        get(ReportCodecs.digest(get(studies.codec.digest(plan)), "plan")),
        get(ReportCodecs.digest(get(inputs.input.digest(input)), "input")),
        get(ReportCodecs.digest(get(CanonicalDigest.document[Json](document)), "result")),
        None
      )
      val spec = get(
        ReportSpec.of(
          get(ReportId.of("stamped by item")),
          0,
          get(ReportSelection.of(Vector(Role.Difference, Role.Matched), Vector("value"))),
          groupBy = Vector(Grouping.ByLevel(LevelTerm.Layout(LayoutField.Item)))
        )
      )
      val reports = ReportCodecs.report(studies.keys)
      val honest  = get(
        Report.evaluate(
          spec,
          get(ReportSource.study(plan, input, completed.result, None, binding))
        )
      )
      def graph(report: Report[StudyKey]) =
        val base = save(written)
        get(
          SavedManifest.of(
            base.artifacts ++ Vector(
              get(StoredArtifact.reportSpec("spec", spec)),
              get(StoredArtifact.report("report", reports, report))
            ),
            base.manifest.relations :+ ManifestRelation.ReportOf(
              ManifestFixtures.name("report"),
              ManifestFixtures.name("spec"),
              written.result.name,
              inputArtifact.name,
              None
            )
          )
        )
      val registry = new ArtifactDecoders.Delegating(decoders.withReports(reports))
      val resolved = get(resolve(graph(honest), registry))
      assertEquals(resolved.report(ManifestFixtures.name("report")), Some(honest))
      val loaded = resolved.results.head._2
      assertEquals(loaded.stampClaim.map(_.plan.sha256), Some(completed.stamp.plan.sha256))
      assertEquals(loaded.stampClaim.map(_.input.sha256), Some(completed.stamp.input.sha256))
      assertEquals(
        get(loaded.scoreSchema(get(studies.parameters.encode(plan.parameters)))).ids,
        get(ScoreSchema.study(plan)).ids
      )
      assertEquals(get(loaded.encode), document)

      val first   = honest.cells.find(_.estimate.isPresent).getOrElse(fail("no estimate"))
      val altered = get(
        Report.reconstruct(
          honest.spec,
          honest.binding,
          honest.groups,
          honest.cells.map(c => c.copy(estimate = c.estimate.map(_ + 1000))),
          honest.contrasts,
          honest.accounting,
          honest.findings
        )
      )
      val errors = resolve(graph(altered), registry).left.toOption
        .getOrElse(fail("accepted altered report"))
      val cells = errors.collect {
        case ResolveError.Relation(
              _,
              RelationMismatch.ReportCell(group, role, component, stored, fresh)
            ) =>
          (group, role, component, stored, fresh)
      }
      assertEquals(cells.size, 1, errors)
      assertEquals(
        cells.head,
        (
          first.group.render,
          first.role.toString,
          first.component,
          altered
            .cell(first.group, first.role, first.component)
            .getOrElse(fail("missing cell"))
            .toString,
          first.toString
        )
      )
    }
  }

  test("a different decoded plan with the same description fails canonical manifest binding") {
    import StudyResultFixtures.{TwoComponent, TwoDifference}
    val original = StudyResultFixtures.twoComponentMethod
    val method   = new StudyMethod[Double, Px, TwoComponent, TwoDifference](
      original.id,
      original.name,
      _ => Vector.empty,
      original.execution,
      original.descriptor
    )(using original.mean, original.difference)
    val plans = new StudyCodec(
      StudyResultFixtures.twoComponentStudy.schema,
      studies.layout,
      studies.keys,
      method,
      StudyResultFixtures.twoComponentStudy.parameters
    )
    val resultCodec = plans.results(
      StudyResultFixtures.twoComponentScores,
      StudyResultFixtures.twoComponentDifferences
    )
    def make(gain: Double) = get(
      StudyPlan.of(
        input.reference,
        plans.layout,
        StudyResultFixtures.gridOver(input),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned[Px]()),
        FailurePolicy.RequireAll,
        method,
        gain
      )
    )
    val first  = make(1.0)
    val second = make(2.0)
    assertEquals(first.description, second.description)
    val bound    = get(StampedStudy.prepare(first, input, plans, inputs))
    val written  = get(ResultManifest.stamped("result", resultCodec, get(bound.run)))
    val registry = ArtifactDecoders.of(
      get(StudyRegistry.empty[StudyKey, Px].register(plans.registration)),
      get(StudyInputRegistry.empty[StudyKey, Px].register(inputs)),
      get(StudyResultRegistry.empty[StudyKey, Px].register(resultCodec.registration))
    )
    assert(
      resolve(save(written, get(StoredArtifact.plan("plan", plans, first))), registry).isRight
    )
    assert(
      resolve(
        save(written, get(StoredArtifact.plan("plan", plans, second))),
        registry
      ).left.toOption.exists(_.exists {
        case ResolveError.Relation(_, RelationMismatch.RunPlan(reported, current, changes)) =>
          reported == bound.stamp.plan.sha256 && current == get(
            plans.codec.digest(second)
          ).sha256 && changes.isEmpty
        case _ => false
      })
    )
  }
