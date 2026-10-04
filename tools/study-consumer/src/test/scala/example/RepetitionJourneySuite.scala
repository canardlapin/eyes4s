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

package example

import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.Tolerance
import eyes4s.plan.*
import eyes4s.surface.*
import io.circe.{Decoder, Json}

/** The repetition page from a fresh consumer, in two parts.
  *
  * `RepetitionPlan`: a within-participant reinstatement study over the
  * consumer's own key type, saved and reopened through its own registry, whose
  * matched and control edges equal the explicit `RepetitionDesign` composition
  * and whose similarities and contrasts equal cosines and means the consumer
  * computes from the map values itself.
  *
  * `PointSamplingPlan`: static templates sampled along focal scanpaths, against
  * the pinned values of `tools/r-parity/fixtures/point-sampling.json`
  * (`PointSamplingOracle`), with each sampled point and bin mean recomputed
  * explicitly from the scanpath and the template map.
  *
  * Runs on the JVM and Scala.js.
  */
class RepetitionJourneySuite extends munit.FunSuite:
  private val Oracle = Tolerance(absolute = 1e-12, relative = 0)

  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def id(name: String): DefinitionId    = get(DefinitionId.of(name, 1))
  private val runtime = if System.getProperty("java.vm.name") == "Scala.js" then "js" else "jvm"

  // ---------------------------------------------------------------- repetition

  final case class Viewing(person: Int, stimulus: String, occasion: Int)
  object Viewing:
    given KeyDigest[Viewing] = KeyDigest.derived[Viewing]
    given Ordering[Viewing]  = Ordering.by(k => (k.person, k.stimulus, k.occasion))

  private val person   = Projection.named[Viewing, Int]("person")(_.person)
  private val stimulus = Projection.named[Viewing, String]("stimulus")(_.stimulus)
  private val occasion = Projection.named[Viewing, Int]("occasion")(_.occasion)

  private def layout = get(
    RepetitionLayout.of[Viewing, Int, String, Int](
      id("example.viewing-layout"),
      id("example.person"),
      person,
      id("example.stimulus"),
      stimulus,
      id("example.occasion"),
      occasion
    )
  )

  private def field[A: Decoder](json: Json, name: String): Either[CodecError, A] =
    json.hcursor.get[A](name).left.map(e => CodecError.Field(name, json, e.message))

  private def keys = VersionedCodec.of[Viewing](id("example.viewing-key"))(k =>
    Json.obj(
      "person"   -> Json.fromInt(k.person),
      "stimulus" -> Json.fromString(k.stimulus),
      "occasion" -> Json.fromInt(k.occasion)
    )
  ) { json =>
    for
      p <- field[Int](json, "person")
      s <- field[String](json, "stimulus")
      o <- field[Int](json, "occasion")
    yield Viewing(p, s, o)
  }

  private def codec = RepetitionPlanCodec.of[Viewing, Px](
    id("example.viewing-plan"),
    get(RepetitionRegistry.empty[Viewing].register(layout)),
    keys
  )

  private val grid = get(Grid.over(get(Frame.screen("repetition-journey", 4, 3)), 4, 3))

  /** Two people view three images on three occasions; each map has one peak
    * that moves with person, image and occasion, on a flat floor.
    */
  private val viewings = for
    p <- Vector(1, 2)
    s <- Vector("a", "b", "c")
    o <- Vector(0, 1, 2)
  yield
    val peak   = (p * 5 + Vector("a", "b", "c").indexOf(s) * 3 + o) % 12
    val values = IArray.tabulate(12)(i => if i == peak then 4.0 else 1.0 + (i % 3) * 0.5)
    val map    = get(
      Surface
        .intensity(grid, values, Provenance.raw(ContentHash.of(values)))
        .flatMap(_.normalised)
    )
    Trial(Viewing(p, s, o), (), map)

  private val controls =
    Selection.BottomK(
      get(PairLimit.of(2)),
      Seed(Long.MinValue + 11),
      SampleId("journey-controls")
    )

  private def repetitionPlan = get(
    RepetitionPlan.of(
      layout,
      RepetitionRelations.withinParticipant,
      MapSimilarityMethod.Cosine,
      controls,
      FailurePolicy.RequireAll,
      grid,
      Trials(viewings)
    )
  )

  /** Cosine of two maps, from their cell values alone. */
  private def cosine(a: Mass[Px], b: Mass[Px]): Double =
    val dot = a.values.indices.map(i => a.values(i) * b.values(i)).sum
    dot / math.sqrt(a.values.map(v => v * v).sum * b.values.map(v => v * v).sum)

  test("RepetitionPlan equals the explicit RepetitionDesign and the consumer's own cosines") {
    val plan   = repetitionPlan
    val result = plan.run
    val direct = RepetitionDesign
      .withinParticipant(person, stimulus, occasion, controls)
      .evaluate(
        plan.trials,
        plan.inputHash,
        MapSimilarityMethod.Cosine.similarity[Px],
        plan.specification
      )
    assertEquals(result.matched, direct.matched)
    assertEquals(result.controls, direct.controls)

    val maps     = viewings.map(t => t.key -> t.value).toMap
    val expected = for
      x <- viewings
      y <- viewings
      if x.key.person == y.key.person && x.key.stimulus == y.key.stimulus &&
        x.key.occasion != y.key.occasion
    yield x.key -> y.key
    assertEquals(result.matched.rows.map(r => r.left -> r.right), expected)
    result.matched.rows.foreach { r =>
      val score = get(r.result)
      assert(Oracle.approxEquals(score.value, cosine(maps(r.left), maps(r.right))))
    }
    result.controls.rows.foreach { r =>
      assertEquals(r.left.person, r.right.person)
      assertNotEquals(r.left.stimulus, r.right.stimulus)
      assertNotEquals(r.left.occasion, r.right.occasion)
      assert(Oracle.approxEquals(get(r.result).value, cosine(maps(r.left), maps(r.right))))
    }
    assertEquals(result.controls.diagnostics.selectedPairCount, 36)

    // Each focal key's contrast: the mean matched cosine minus the mean control cosine.
    def meanOf(rows: Vector[(Viewing, Viewing)], key: Viewing): Double =
      val partners = rows.collect { case (l, r) if l == key => cosine(maps(l), maps(r)) }
      partners.sum / partners.size
    val matchedEdges = result.matched.rows.map(r => r.left -> r.right)
    val controlEdges = result.controls.rows.map(r => r.left -> r.right)
    val contrasts    = get(result.contrasts).rows
    assertEquals(contrasts.map(_.key), viewings.map(_.key))
    contrasts.foreach { row =>
      val explicit = meanOf(matchedEdges, row.key) - meanOf(controlEdges, row.key)
      assert(Oracle.approxEquals(get(row.difference).value, explicit), s"${row.key}")
    }
  }

  test("a saved RepetitionPlan reopens through the consumer's registry and reruns exactly") {
    val plan     = repetitionPlan
    val json     = get(codec.encode(plan))
    val reopened = get(codec.parse(json.noSpaces))
    assertEquals(get(codec.encode(reopened)), json)
    assertEquals((reopened.planHash, reopened.inputHash), (plan.planHash, plan.inputHash))
    val (a, b) = (plan.run, reopened.run)
    assertEquals(a.matched, b.matched)
    assertEquals(a.controls, b.controls)
    val differences = get(b.contrasts).rows.map(r => get(r.difference).value)
    assertEquals(get(a.contrasts).rows.map(r => get(r.difference).value), differences)
    // A larger cap keeps every edge of the smaller one.
    val wider = get(
      RepetitionPlan.of(
        layout,
        RepetitionRelations.withinParticipant,
        MapSimilarityMethod.Cosine,
        Selection.BottomK(
          get(PairLimit.of(3)),
          Seed(Long.MinValue + 11),
          SampleId("journey-controls")
        ),
        FailurePolicy.RequireAll,
        grid,
        Trials(viewings)
      )
    ).run
    val narrow = a.controls.rows.map(r => r.left -> r.right).toSet
    assert(narrow.subsetOf(wider.controls.rows.map(r => r.left -> r.right).toSet))
    val value = json.hcursor.downField("value").focus.getOrElse(fail("no value"))
    assert(
      codec
        .decode(
          json.mapObject(
            _.add("value", value.mapObject(_.add("method", Json.fromString("emd"))))
          )
        )
        .isLeft
    )
    println(
      "EYES4S_REPETITION_JOURNEY=" + Json
        .obj(
          "runtime"     -> Json.fromString(runtime),
          "plan"        -> json,
          "matched"     -> Json.fromInt(a.matched.rows.size),
          "controls"    -> Json.fromInt(a.controls.rows.size),
          "differences" -> Json.arr(differences.map(Json.fromDoubleOrNull)*)
        )
        .noSpaces
    )
  }

  // ---------------------------------------------------------------- point sampling

  private val clock  = ClockId("point-onsets")
  private val frame  = get(Frame.screen("point-map", 2, 2))
  private val pgrid  = get(Grid.over(frame, 2, 2))
  private val policy = FailurePolicy.SuccessfulOnly(get(MinimumSuccessful.of(1)))

  private val fixations = PointSamplingOracle.sources.map { s =>
    s.key -> s.onsets.indices.map(i => (s.onsets(i), Pt[Px](s.x(i), s.y(i)))).toVector
  }.toMap

  private val pointInput = StudyInput(Trials(PointSamplingOracle.sources.map { s =>
    val fixes = s.onsets.indices.map(i =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval.of(
              clock,
              Instant.micros(s.onsets(i)),
              Instant.micros(s.onsets(i) + s.durations(i))
            )
          ),
          Pt[Px](s.x(i), s.y(i)),
          1
        )
      )
    )
    Trial(
      StudyKey(s.stratum, s.matched, s.key),
      (),
      get(Scanpath.of(frame, clock, IArray.from(fixes)))
    )
  }))

  private val templateMaps = PointSamplingOracle.templates.map { t =>
    val values = IArray.from(t.values)
    t.key -> get(Surface.signed(pgrid, values, Provenance.raw(ContentHash.of(values))))
  }.toMap

  private val templates = Trials(PointSamplingOracle.templates.map { t =>
    Trial(StudyKey(t.stratum, t.key, "template-" + t.key), (), templateMaps(t.key))
  })

  private def pointPlan(controls: PointControlSelection) = PointSamplingPlan.of(
    StudyKey.layout(id("example.point-layout")),
    pointInput,
    templates,
    get(
      PointSamplingSpec.of(
        clock,
        PointSamplingOracle.queries.map(Instant.micros),
        Some(
          get(
            PointBins.of(
              clock,
              PointSamplingOracle.boundaries.map(Instant.micros),
              PointBinEndpoint.HalfOpen
            )
          )
        ),
        DensityNormalization.None,
        DensityLookupPolicy.NearestClampedRIndex,
        TrajectoryEndpoint.HoldLastOnset,
        controls,
        policy,
        policy
      )
    )
  )

  test("PointSamplingPlan equals the pinned values and the consumer's explicit lookups") {
    val oracleRows =
      PointSamplingOracle.results.filter(r => r.normalization == "none" && r.cap == 0)
    val result = pointPlan(PointControlSelection.Disabled).run
    assertEquals(result.rows.map(_.key.phase), oracleRows.map(_.key))
    result.rows.zip(oracleRows).foreach { (row, expected) =>
      row.points.zip(expected.values).foreach { (point, value) =>
        (point.value.toOption, value) match
          case (Some(a), Some(b)) =>
            assert(Oracle.approxEquals(a, b), s"${row.key} ${point.index}")
          case (a, b) => assertEquals(a, b, s"${row.key} ${point.index}")
        // Explicitly: the position of the last fixation whose onset is not after the
        // query, and the template's value at that position when it lies on the grid.
        val t    = point.time.toMicros
        val held = fixations(row.key.phase).filter(_._1 <= t).lastOption.map(_._2)
        assertEquals(point.point, held, s"${row.key} position at $t")
        val template = templateMaps(row.key.stimulus)
        held.flatMap(template.sampleAt).foreach { v =>
          assertEquals(point.value.toOption, Some(v), s"${row.key} explicit value at $t")
        }
      }
      row.bins.zip(expected.bins).foreach { (bin, value) =>
        val members  = bin.queries.flatMap(i => row.points(i).value.toOption)
        val explicit = Option.when(members.nonEmpty)(members.sum / members.size)
        assertEquals(bin.observed.result.toOption.isDefined, value.isDefined)
        (bin.observed.result.toOption, value, explicit) match
          case (Some(a), Some(b), Some(c)) =>
            assert(
              Oracle.approxEquals(a, b) && Oracle.approxEquals(a, c),
              s"${row.key} bin ${bin.index}"
            )
          case _ => assertEquals(value, None)
      }
    }
    assertEquals(result.unbinned, Vector(0, 4, 5, 6))
  }

  test(
    "exhaustive point controls match the pinned control means; a saved plan reruns exactly"
  ) {
    val oracleRows =
      PointSamplingOracle.results.filter(r => r.normalization == "none" && r.cap == 20)
    val plan   = pointPlan(PointControlSelection.Candidates(Selection.All))
    val result = plan.run
    result.rows.zip(oracleRows).foreach { (row, expected) =>
      val controls = row.points.map(_.control.flatMap(_.result.toOption))
      controls.zip(expected.controls.getOrElse(fail("no pinned controls"))).foreach {
        case (Some(a), Some(b)) => assert(Oracle.approxEquals(a, b), s"${row.key}")
        case (a, b)             => assertEquals(a, b, s"${row.key}")
      }
    }
    val persistence = new PointSamplingCodec[StudyKey, Px](
      id("example.point-plan"),
      id("example.point-result"),
      StudyKey.layout(id("example.point-layout")),
      StudyCodecs.key(id("example.point-key"))
    )
    val json     = get(persistence.plan.encode(plan))
    val reopened = get(persistence.plan.parse(json.noSpaces))
    val rerun    = reopened.run
    assertEquals(rerun.planHash, result.planHash)
    assertEquals(
      rerun.rows.map(_.points.map(_.value.toOption)),
      result.rows.map(_.points.map(_.value.toOption))
    )
    println(
      "EYES4S_POINT_JOURNEY=" + Json
        .obj(
          "runtime" -> Json.fromString(runtime),
          "plan"    -> Json.fromString(result.planHash.render),
          "values"  -> Json.arr(
            result.rows
              .flatMap(_.points.map(p => Json.fromDoubleOrNull(p.value.getOrElse(Double.NaN))))*
          )
        )
        .noSpaces
    )
  }
