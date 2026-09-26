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
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.pointcodecconsumer.PointSamplingFixture
import io.circe.{Json, Decoder}
import org.scalacheck.{Gen, Test}

object AdditionalRecipeCodecs:
  def get[E, A](v: Either[E, A]): A =
    v.fold(e => throw new IllegalStateException(e.toString), identity)
  def id(s: String) = get(DefinitionId.of(s, 1))
  case class Occasion(phase: String, repeat: Int)
  object Occasion:
    given KeyDigest[Occasion] = KeyDigest.derived[Occasion]
  case class Key(person: Int, stimulus: String, occasion: Occasion)
  object Key:
    given KeyDigest[Key] = KeyDigest.derived[Key]
    given Ordering[Key]  =
      Ordering.by(k => (k.person, k.stimulus, k.occasion.phase, k.occasion.repeat))
  private def field[A: Decoder](j: Json, k: String): Either[CodecError, A] =
    j.hcursor.get[A](k).left.map(e => CodecError.Field(k, j, e.message))
  private val keys = VersionedCodec.of[Key](id("example.repetition-key"))(k =>
    Json.obj(
      "person"   -> Json.fromInt(k.person),
      "stimulus" -> Json.fromString(k.stimulus),
      "phase"    -> Json.fromString(k.occasion.phase),
      "repeat"   -> Json.fromInt(k.occasion.repeat)
    )
  ) { j =>
    for
      p     <- field[Int](j, "person"); s   <- field[String](j, "stimulus");
      phase <- field[String](j, "phase"); r <- field[Int](j, "repeat")
    yield Key(p, s, Occasion(phase, r))
  }
  private val layout = get(
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
  val repetition = RepetitionPlanCodec.of[Key, Px](
    id("example.repetition-plan"),
    get(RepetitionRegistry.empty[Key].register(layout)),
    keys
  )
  val point = new PointSamplingCodec[StudyKey, Px](
    id("example.point-plan"),
    id("example.point-result"),
    StudyKey.layout(id("example.point-layout")),
    StudyCodecs.key(id("example.point-key"))
  )

class AdditionalRecipeCodecLawSuite extends munit.DisciplineSuite:
  import AdditionalRecipeCodecs.*
  private val repetitionFixture = get(repetition.parse(RepetitionPlanFixture.versionOne))
  private val pointFixture      = get(point.archive.parse(PointSamplingFixture.versionOne))
  private val repetitions       = for
    cap <- Gen.choose(1, 4); seed <- Gen.chooseNum(Long.MinValue, Long.MaxValue)
  yield get(
    RepetitionPlan.of(
      repetitionFixture.layout,
      repetitionFixture.relations,
      repetitionFixture.method,
      Selection.BottomK(get(PairLimit.of(cap)), Seed(seed), SampleId("law-controls")),
      repetitionFixture.policy,
      repetitionFixture.grid,
      repetitionFixture.trials
    )
  )
  private def pointWith(
      a: PointSamplingArchive[StudyKey, Px],
      queries: Vector[Instant],
      controls: PointControlSelection
  ): PointSamplingArchive[StudyKey, Px] =
    val p    = a.plan; val s = p.spec
    val spec = get(
      PointSamplingSpec.of(
        s.clock,
        queries,
        s.bins,
        s.normalization,
        s.lookup,
        s.endpoint,
        controls,
        s.controlPolicy,
        s.binPolicy
      )
    )
    PointSamplingArchive.run(PointSamplingPlan.of(p.layout, p.source, p.templates, spec))
  private val points = for reverse <- Gen.oneOf(true, false); enabled <- Gen.oneOf(true, false)
  yield pointWith(
    pointFixture,
    if reverse then pointFixture.plan.spec.queries.reverse else pointFixture.plan.spec.queries,
    if enabled then pointFixture.plan.spec.controls else PointControlSelection.Disabled
  )
  private def sameRepeat(a: RepetitionPlan[Key, Px], b: RepetitionPlan[Key, Px]) =
    a.planHash == b.planHash && a.inputHash == b.inputHash && a.run.matched == b.run.matched && a.run.controls == b.run.controls
  private def samePoint(
      a: PointSamplingArchive[StudyKey, Px],
      b: PointSamplingArchive[StudyKey, Px]
  ) = a.plan.planHash == b.plan.planHash && point.archive.encode(a) == point.archive.encode(b)
  checkAll(
    "all-occasion repetition plan",
    CodecLaws.roundTrip(repetition, repetitions, sameRepeat)
  )
  checkAll(
    "static point sampling archive",
    CodecLaws.roundTrip(point.archive, points, samePoint)
  )
  private val parameters =
    Test.Parameters.default.withMinSuccessfulTests(20).withInitialSeed(0x706f696e74L)
  private def mutant[A](codec: VersionedCodec[A])(f: A => A): VersionedCodec[A] =
    VersionedCodec.checked[A](codec.schema)(a =>
      codec.encode(a).map(_.hcursor.downField("value").focus.get)
    ) { raw =>
      codec
        .decode(
          Json.obj(
            "schema" -> Json.obj(
              "name"    -> Json.fromString(codec.schema.name),
              "version" -> Json.fromInt(codec.schema.version)
            ),
            "value" -> raw
          )
        )
        .map(f)
    }
  private def killed[A](codec: VersionedCodec[A], gen: Gen[A], same: (A, A) => Boolean) =
    CodecLaws
      .roundTrip(codec, gen, same)
      .all
      .properties
      .exists((_, prop) => !Test.check(parameters, prop).passed)
  test("round-trip laws reject changed repetition controls and dropped point query") {
    val changed = mutant(repetition)(p =>
      get(
        RepetitionPlan.of(
          p.layout,
          p.relations,
          p.method,
          Selection.All,
          p.policy,
          p.grid,
          p.trials
        )
      )
    )
    assert(killed(changed, repetitions, sameRepeat))
    val dropped = mutant(point.archive)(a =>
      pointWith(a, a.plan.spec.queries.drop(1), a.plan.spec.controls)
    )
    assert(killed(dropped, points, samePoint))
  }
