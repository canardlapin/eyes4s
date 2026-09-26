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

package eyes4s.pointcodecconsumer

import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.*
import io.circe.Json

class PointSamplingCodecSuite extends munit.FunSuite:
  private def get[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)
  private def id(name: String)              = get(DefinitionId.of(name, 1))
  private val clock                         = ClockId("point-clock")
  private val frame                         = get(Frame.screen("point-codec", 2, 2))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private def layout                        = StudyKey.layout(id("example.point-layout"))
  private def codec                         = new PointSamplingCodec[StudyKey, Px](
    id("example.point-plan"),
    id("example.point-result"),
    layout,
    StudyCodecs.key(id("example.point-key"))
  )
  private val labels = Vector("A", "B", "C")
  private val path   = get(
    Scanpath.of(
      frame,
      clock,
      IArray.from(Vector(0L, 10000L, 20000L).zipWithIndex.map { (t, i) =>
        get(
          Event.Fixation.withoutDispersion(
            get(Interval.of(clock, Instant.micros(t), Instant.micros(t + 5000))),
            Pt[Px](if i == 0 then .5 else 1.5, .5),
            1
          )
        )
      })
    )
  )
  private val source = StudyInput(
    Trials(labels.map(s => Trial(StudyKey("p,\"λ\"\n", s, "source"), (), path)))
  )
  private val templates = Trials(labels.zipWithIndex.map { (s, i) =>
    val values = IArray(1.0 + i, 2.0, 4.0, 8.0)
    Trial(
      StudyKey("p,\"λ\"\n", s, "template"),
      (),
      get(Surface.signed(grid, values, Provenance.raw(ContentHash.of(values))))
    )
  })
  private val success   = FailurePolicy.SuccessfulOnly(get(MinimumSuccessful.of(1)))
  private val selection = Selection.BottomK(
    get(PairLimit.of(1)),
    Seed(Long.MinValue + 7),
    SampleId("persisted-points")
  )
  private def plan(
      queries: Vector[Long] = Vector(20000, 0, 10000, -1, 20001),
      bins: Boolean = true,
      controls: PointControlSelection = PointControlSelection.Candidates(selection),
      input: StudyInput[StudyKey, Px] = source,
      refs: Trials[StudyKey, Unit, Signed[Px]] = templates,
      queryClock: ClockId = clock
  ) =
    val b = if bins then
      Some(
        get(
          PointBins.of(
            queryClock,
            Vector(0, 10000, 20000).map(x => Instant.micros(x.toLong)),
            PointBinEndpoint.IncludeFinalEndpoint
          )
        )
      )
    else None
    val spec = get(
      PointSamplingSpec.of(
        queryClock,
        queries.map(Instant.micros),
        b,
        DensityNormalization.SampleZScore,
        DensityLookupPolicy.NearestClampedRIndex,
        TrajectoryEndpoint.OnsetRange,
        controls,
        success,
        success
      )
    )
    PointSamplingPlan.of(layout, input, refs, spec)
  private def replay(p: PointSamplingPlan[StudyKey, Px]): Json =
    val original = get(codec.archive.encode(PointSamplingArchive.run(p)))
    val reopened = get(codec.archive.parse(original.noSpaces))
    assertEquals(get(codec.archive.encode(reopened)), original)
    assertEquals(reopened.plan.planHash, p.planHash);
    assertEquals(reopened.plan.inputHash, p.inputHash)
    assertEquals(reopened.result.rows.map(_.key), p.source.trials.rows.map(_.key))
    original

  test(
    "fresh codec reopens complete recipe and replay-verified results with exact controls, Unicode keys and failures"
  ) {
    val p = plan(); val json = replay(p)
    assertEquals(json, get(io.circe.parser.parse(PointSamplingFixture.versionOne)))
    assertEquals(
      get(codec.archive.encode(get(codec.archive.parse(PointSamplingFixture.versionOne)))),
      json
    )
    val reopened = get(codec.plan.parse(get(codec.plan.encode(p)).noSpaces))
    assertEquals(reopened.spec.controls, PointControlSelection.Candidates(selection))
    assertEquals(
      reopened.run.rows.flatMap(_.controls.map(_.occurrence)),
      p.run.rows.flatMap(_.controls.map(_.occurrence))
    )
    assert(json.noSpaces.contains((Long.MinValue + 7).toString))
    assert(json.noSpaces.contains("trajectory")); assert(!json.noSpaces.contains("NaN"))
  }
  test(
    "disabled controls, absent bins, empty queries, empty sources and matching failures survive persistence"
  ) {
    replay(plan(bins = false, controls = PointControlSelection.Disabled))
    replay(plan(queries = Vector.empty))
    replay(plan(input = StudyInput(Trials(Vector.empty[Trial[StudyKey, Unit, Scanpath[Px]]]))))
    replay(plan(refs = Trials(Vector.empty[Trial[StudyKey, Unit, Signed[Px]]])))
    replay(plan(refs = Trials(templates.rows :+ templates.rows.head)))
    replay(plan(input = StudyInput(Trials(source.trials.rows :+ source.trials.rows.head))))
    replay(plan(queryClock = ClockId("wrong")))
  }
  test("microseconds above 2^53 persist as exact integers with explicit out-of-range results") {
    val p =
      plan(queries = Vector(9007199254740992L, 9007199254740993L, Long.MaxValue), bins = false)
    val json = replay(p)
    val q    = get(codec.archive.decode(json)).plan.spec.queries.map(_.toMicros)
    assertEquals(q, p.spec.queries.map(_.toMicros)); assert(q(0) != q(1))
  }
  test(
    "changed derived values, counts, keys, policies or provenance are rejected during replay"
  ) {
    val json                    = replay(plan())
    def change(f: Json => Json) =
      json.hcursor.downField("value").downField("result").withFocus(f).top.get
    val result    = json.hcursor.downField("value").downField("result").focus.get
    val rows      = result.hcursor.get[Vector[Json]]("rows").toOption.get
    val first     = rows.head
    val mutations = Vector(
      result.mapObject(_.add("inputHash", Json.fromString("changed"))),
      result.mapObject(_.add("unbinned", Json.arr())),
      result.mapObject(
        _.add(
          "rows",
          Json.arr((first.mapObject(_.add("eligibleControls", Json.fromInt(99))) +: rows.tail)*)
        )
      ),
      result.mapObject(
        _.add("rows", Json.arr((first.mapObject(_.add("points", Json.arr())) +: rows.tail)*))
      ),
      result.mapObject(
        _.add("rows", Json.arr((first.mapObject(_.add("key", Json.Null)) +: rows.tail)*))
      )
    )
    mutations.foreach(m => assert(codec.archive.decode(change(_ => m)).isLeft))
  }
  test(
    "unknown methods, stale hashes, changed geometry and unsorted bins cannot decode as valid plans"
  ) {
    val json                     = get(codec.plan.encode(plan()))
    def payload(f: Json => Json) = json.hcursor.downField("value").withFocus(f).top.get
    for (name, value) <- Vector(
        "method"        -> Json.fromString("arbitrary.callback"),
        "planHash"      -> Json.fromString("stale"),
        "normalization" -> Json.fromString("unknown")
      )
    do assert(codec.plan.decode(payload(_.mapObject(_.add(name, value)))).isLeft)
    val bad = json.hcursor
      .downField("value")
      .downField("bins")
      .downField("boundaries")
      .withFocus(_ => Json.arr(Json.fromString("1"), Json.fromString("0")))
      .top
      .get
    assert(codec.plan.decode(bad).isLeft)
    val other = new PointSamplingCodec[StudyKey, Px](
      id("example.point-plan"),
      id("example.point-result"),
      StudyKey.layout(id("other.layout")),
      StudyCodecs.key(id("example.point-key"))
    )
    assert(other.plan.decode(json).isLeft)
  }
