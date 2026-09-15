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

import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.examples.MatchedControlFixtures
import eyes4s.surface.EdgePolicy
import io.circe.Json
import scala.compiletime.testing.typeCheckErrors

class StudyCodecSuite extends munit.FunSuite:
  private val OracleTolerance               = 1e-12
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private def id(name: String, version: Int = 1): DefinitionId = get(
    DefinitionId.of(name, version)
  )
  private val frame       = get(Frame.screen("matched-control-display", 2, 2))
  private val grid        = get(Grid.over(frame, 2, 2))
  private val layout      = StudyKey.layout(id("participant-stimulus-phase"))
  private val method      = StudyMethod.cosine[Px](id("eyes4s.cosine"))
  private val persistence = new StudyCodec(
    id("eyes4s.study"),
    layout,
    StudyCodecs.key(id("study-key")),
    method,
    VersionedCodec.unit(id("unit"))
  )
  private val input = StudyInput(
    Trials(
      MatchedControlFixtures.fixations
        .groupBy(r => StudyKey(r.participant, r.image, r.phase))
        .toVector
        .sortBy(_._1)
        .map { case (key, rows) =>
          val clock = ClockId(s"fixation-trial:${KeyDigest[StudyKey].digest(key).render}")
          val fixes = rows.sortBy(_.ordinal).map { row =>
            get(
              Event.Fixation.withoutDispersion(
                get(
                  Interval.of(
                    clock,
                    Instant.micros(row.onsetMicros),
                    Instant.micros(row.onsetMicros + row.durationMicros)
                  )
                ),
                Pt[Px](row.x, row.y),
                row.sampleCount
              )
            )
          }
          Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))
        }
    )
  )
  private def plan(
      estimates: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned()),
      weight: Weight = Weight.Duration
  ) =
    get(
      StudyPlan.of(
        input.reference,
        layout,
        grid,
        "recall",
        "encode",
        weight,
        estimates,
        FailurePolicy.RequireAll,
        method,
        ()
      )
    )
  private def results(p: StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]) =
    get(p.run(input)).scales.map(scale =>
      get(scale.contrast).rows.map(row => row.key -> get(row.difference).value)
    )

  test(
    "saved study round-trips structurally and reproduces every independent contrast target"
  ) {
    val original = plan()
    val json     = get(persistence.codec.encode(original))
    val decoded  = get(persistence.codec.parse(json.noSpaces))
    assertEquals(decoded, original)
    assertEquals(decoded.diff(original), Vector.empty)
    assertEquals(get(persistence.codec.encode(decoded)), json)
    val direct   = results(original).head.toMap
    val reloaded = results(decoded).head.toMap
    MatchedControlFixtures.reductions.foreach { expected =>
      val parts = expected.id.split("/")
      val key   = StudyKey(parts(0), parts(1), parts(2))
      assertEqualsDouble(direct(key), expected.difference, OracleTolerance)
      assertEquals(reloaded(key), direct(key))
    }
    val result = get(decoded.run(input)).scales.head
    assertEquals(result.estimation.size, 12)
    assertEquals(result.excludedPhases, Vector.empty)
    assertEquals(
      get(result.contrast).rows.map(_.control.map(_.contributing)),
      Vector.fill(6)(Some(2))
    )
  }

  test("missing and wrong input artifacts are diagnosed before execution") {
    val p = plan()
    assertEquals(
      p.prerequisites(None),
      Vector(PlanError.MissingArtifact(input.reference.digest))
    )
    val wrong = StudyInput(Trials(input.trials.rows.reverse))
    assertEquals(
      p.run(wrong).left.toOption,
      Some(PlanError.ArtifactMismatch(input.reference.digest, wrong.reference.digest))
    )
  }

  test("structural diff names changed scientific choices") {
    val changed = plan(weight = Weight.Uniform)
    assertEquals(plan().diff(changed).map(_.field), Vector("weight"))
    assertNotEquals(plan(), changed)
  }

  test("each Gaussian scale is retained through persistence, including failed estimation") {
    val scales = Vector(
      StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate),
      StudyEstimate.Gaussian(get(Sigma.px(1.0)), EdgePolicy.Renormalise),
      StudyEstimate.Gaussian(get(Sigma.px(0.01)), EdgePolicy.Truncate)
    )
    val p      = get(persistence.codec.decode(get(persistence.codec.encode(plan(scales)))))
    val result = get(p.run(input))
    assertEquals(result.scales.map(_.estimate), scales)
    assert(result.scales.take(2).forall(_.estimation.forall(_._2.isRight)))
    assert(result.scales(2).estimation.forall(_._2.isLeft))
    assert(get(result.scales(2).contrast).rows.forall(_.difference.isLeft))
    assert(get(result.scales(2).contrast).rows.forall(_.control.exists(_.failed == 2)))
  }

  test(
    "registry executes its captured typed codec and refuses missing or duplicate definitions"
  ) {
    val registry = get(StudyRegistry.empty[StudyKey, Px].register(persistence.registration))
    val json     = get(persistence.codec.encode(plan()))
    val loaded   = get(registry.decode(json))
    assertEquals(loaded.description, plan().description)
    assertEquals(get(loaded.encode), json)
    assertEquals(get(loaded.run(input)).scales.size, 1)
    assertEquals(
      registry.register(persistence.registration).left.toOption,
      Some(CodecError.DuplicateMethod(method.id))
    )
    assertEquals(
      StudyRegistry.empty[StudyKey, Px].decode(json).left.toOption,
      Some(CodecError.MissingMethod(method.id))
    )
  }

  private def replaceValue(json: Json, name: String, value: Json): Json =
    json.mapObject(
      _.add("value", get(json.hcursor.get[Json]("value")).mapObject(_.add(name, value)))
    )

  test("unsupported old schemas, method versions, parameters and spatial units reject") {
    val json = get(persistence.codec.encode(plan()))
    val old  = json.mapObject(
      _.add(
        "schema",
        Json.obj("name" -> Json.fromString("eyes4s.study"), "version" -> Json.fromInt(2))
      )
    )
    assertEquals(
      persistence.codec.decode(old).left.toOption,
      Some(CodecError.Schema(persistence.schema, id("eyes4s.study", 2)))
    )
    val unknown = replaceValue(
      json,
      "method",
      Json.obj("name" -> Json.fromString("eyes4s.cosine"), "version" -> Json.fromInt(2))
    )
    assert(persistence.codec.decode(unknown).isLeft)
    assert(persistence.codec.decode(replaceValue(json, "parameters", Json.obj())).isLeft)
    val alteredFrame = get(json.hcursor.downField("value").get[Json]("frame"))
      .mapObject(_.add("unit", Json.fromString("deg")))
    assert(persistence.codec.decode(replaceValue(json, "frame", alteredFrame)).isLeft)
    assert(persistence.codec.decode(replaceValue(json, "nx", Json.fromInt(0))).isLeft)
    assert(
      persistence.codec.decode(replaceValue(json, "nx", Json.fromInt(Int.MaxValue))).isLeft
    )
    assert(persistence.codec.decode(replaceValue(json, "estimates", Json.arr())).isLeft)
    assert(persistence.codec.parse("not JSON").isLeft)
  }

  test("conditional map codecs preserve typed keys and reject all duplicate occurrences") {
    val codec = VersionedCodec.entries(
      id("keyed-values"),
      StudyCodecs.key(id("study-key")),
      VersionedCodec.string(id("string"))
    )
    val key     = StudyKey("s1", "a", "recall")
    val encoded = get(codec.encode(Map(key -> "result")))
    assertEquals(get(codec.decode(encoded)), Map(key -> "result"))
    val rows      = get(encoded.hcursor.get[Vector[Json]]("value"))
    val duplicate = encoded.mapObject(_.add("value", Json.arr((rows ++ rows)*)))
    assertEquals(
      codec.decode(duplicate).left.toOption,
      Some(CodecError.DuplicateKeys(codec.schema, Vector(0, 1)))
    )
    assert(
      typeCheckErrors(
        """import eyes4s.codec.*; import eyes4s.plan.*; def wrong(c: VersionedCodec[String]): VersionedCodec[StudyKey] = c"""
      ).nonEmpty
    )
  }

  test("Gaussian scales agree with an independent 60-digit closed-form oracle") {
    MultiscaleFixtures.scales.foreach { expected =>
      Vector(EdgePolicy.Truncate, EdgePolicy.Renormalise).foreach { edges =>
        val p       = plan(Vector(StudyEstimate.Gaussian(get(Sigma.px(expected.sigma)), edges)))
        val decoded = get(persistence.codec.decode(get(persistence.codec.encode(p))))
        val scale   = get(decoded.run(input)).scales.head
        scale.estimation.foreach { case (key, value) =>
          val label  = s"${key.participant}/${key.stimulus}/${key.phase}"
          val target = expected.masses.toMap.apply(label)
          val mass   = get(value)
          target.zipWithIndex.foreach { case (cell, index) =>
            assertEqualsDouble(mass.at(index), cell, OracleTolerance)
          }
        }
        get(scale.contrast).rows.foreach { row =>
          val label = s"${row.key.participant}/${row.key.stimulus}/${row.key.phase}"
          assertEqualsDouble(
            get(row.difference).value,
            expected.differences.toMap.apply(label),
            OracleTolerance
          )
        }
      }
    }
  }

  test("pinned version-one project retains its original meaning") {
    val standard = StudyCodecs.cosine[Px]
    val decoded  = get(standard.codec.parse(SavedStudyFixtures.versionOne))
    assertEquals(
      get(standard.codec.encode(decoded)),
      get(io.circe.parser.parse(SavedStudyFixtures.versionOne))
    )
    val result = get(decoded.run(input))
    get(result.scales.head.contrast).rows.zip(MatchedControlFixtures.reductions).foreach {
      case (row, expected) =>
        assertEqualsDouble(get(row.difference).value, expected.difference, OracleTolerance)
    }
  }
