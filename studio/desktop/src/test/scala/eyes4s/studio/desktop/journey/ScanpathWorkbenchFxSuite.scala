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

package eyes4s.studio.desktop.journey

import eyes4s.studio.core.scanpath.ScanpathStudy
import eyes4s.studio.desktop.harness.{FxStage, SnapshotScale, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.scanpath.ScanpathWorkbench
import io.circe.{Json, parser}
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Same public controls for synthetic CI and opt-in private native/R pair evidence. */
class ScanpathWorkbenchFxSuite extends StudioFxSuite:
  override val munitTimeout: Duration = 20.minutes
  private final case class Tolerance(absolute: Double, relative: Double):
    def approxEquals(a: Double, b: Double): Boolean =
      a.isFinite && b.isFinite && math
        .abs(a - b) <= absolute + relative * math.max(math.abs(a), math.abs(b))
  private val ScoreTolerance                = Tolerance(absolute = 2e-12, relative = 2e-12)
  private def get[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)
  private def until(fx: FxStage)(condition: => Boolean): Unit =
    val end = System.nanoTime() + 180.seconds.toNanos
    while !runOnFx(condition) do
      if System.nanoTime() > end then fail("Scanpath controls did not settle")
      Thread.sleep(25)
    fx.awaitLayout()
  private def safe(p: Path): Path =
    val path         = p.toRealPath()
    var parent: Path = path
    while parent != null do
      require(
        !Files.exists(parent.resolve(".git")),
        "Private scanpath evidence must stay outside Git"
      )
      parent = parent.getParent
    path
  private def exercise(fx: FxStage, root: Path, expected: Option[Vector[Json]]): Json =
    val expectedQueries = Option.when(Files.exists(root.resolve("expected-queries.json"))) {
      get(
        get(parser.parse(Files.readString(root.resolve("expected-queries.json"))))
          .as[Vector[Json]]
      )
        .map(j => get(j.hcursor.get[String]("id")) -> j)
        .toMap
    }
    var toOpen = root.resolve("request.json")
    var toSave = root.resolve("workspace.json")
    val view   = runOnFx(ScanpathWorkbench(() => Some(toOpen), () => Some(toSave)))
    try
      fx.show(view.node)
      fx.robot.click(view.open)
      until(fx)(!view.busy && !view.run.isDisabled)
      val phases = runOnFx(view.phase.getItems.asScala.toVector)
      val runs   = phases.zipWithIndex.map { (phase, index) =>
        runOnFx(view.phase.setValue(phase))
        fx.robot.click(view.run)
        until(fx)(!view.busy && view.result.nonEmpty)
        val result  = runOnFx(view.result).get
        val pairs   = get(result.hcursor.get[Vector[Json]]("pairs"))
        val queries = get(result.hcursor.get[Vector[Json]]("queries"))
        expected.foreach { all =>
          val ids          = queries.map(q => get(q.hcursor.get[String]("id"))).toSet
          val wanted       = all.filter(p => ids(get(p.hcursor.get[String]("source"))))
          def key(j: Json) =
            Vector("source", "reference", "kind").map(k => get(j.hcursor.get[String](k)))
          val byKey = wanted.map(p => key(p) -> p).toMap
          assertEquals(pairs.size, wanted.size)
          assertEquals(pairs.map(key).toSet, byKey.keySet)
          pairs.foreach { pair =>
            val other = byKey(key(pair))
            assertEquals(
              pair.hcursor.downField("error").succeeded,
              other.hcursor.downField("error").succeeded
            )
            if !pair.hcursor.downField("error").succeeded then
              ScanpathStudy.components.foreach { c =>
                assert(
                  ScoreTolerance.approxEquals(
                    get(pair.hcursor.downField("values").get[Double](c)),
                    get(other.hcursor.downField("values").get[Double](c))
                  ),
                  s"Pair $c differs"
                )
              }
          }
        }
        expectedQueries.foreach { expected =>
          queries.foreach { q =>
            val want = expected(get(q.hcursor.get[String]("id")))
            assertEquals(
              q.hcursor.downField("values").focus.contains(Json.Null),
              want.hcursor.downField("values").focus.contains(Json.Null)
            )
            assertEquals(
              get(q.hcursor.get[Vector[Json]]("unavailable_pairs")).size,
              get(want.hcursor.get[Int]("unavailable_pairs"))
            )
            if !q.hcursor.downField("values").focus.contains(Json.Null) then
              for
                c   <- ScanpathStudy.components;
                key <- Vector("observed", "baseline", "difference")
              do
                assert(
                  ScoreTolerance.approxEquals(
                    get(q.hcursor.downField("values").downField(c).get[Double](key)),
                    get(want.hcursor.downField("values").downField(c).get[Double](key))
                  )
                )
          }
        }
        ScanpathStudy.components.foreach { c =>
          runOnFx(view.component.setValue(c))
          fx.awaitLayout()
          val texts = runOnFx(view.queryTexts)
          assertEquals(texts.size, queries.size)
          queries.zip(texts).foreach { (q, row) =>
            assertEquals(row.head, get(q.hcursor.get[String]("id")))
            val values = q.hcursor.downField("values").downField(c)
            Vector("observed", "baseline", "difference").zipWithIndex.foreach { (name, i) =>
              assertEquals(
                row(i + 3),
                values.get[Double](name).toOption.fold("unavailable")(ScanpathWorkbench.display)
              )
            }
          }
        }
        if sys.env.contains("EYES4S_PRIVATE_RUN") then
          fx.snapshot(StudioTheme.Light, List(SnapshotScale.X1))
            .foreach(p => Files.copy(p, root.resolve(s"phase-${index + 1}.png")))
        toSave = root.resolve(s"phase-${index + 1}-workspace.json")
        fx.robot.click(view.save)
        until(fx)(!view.busy && Files.exists(toSave))
        toOpen = toSave
        fx.robot.click(view.open)
        until(fx)(!view.busy && view.result.nonEmpty)
        assertEquals(runOnFx(view.result), Some(result))
        Json.obj(
          "phase"               -> Json.fromString(phase),
          "queries"             -> Json.fromInt(queries.size),
          "pairs"               -> Json.fromInt(pairs.size),
          "unavailable_queries" -> Json.fromInt(
            queries.count(_.hcursor.downField("values").focus.contains(Json.Null))
          )
        )
      }
      Json.obj(
        "status" -> Json.fromString("passed"),
        "route"  -> Json.fromString(
          "Open request; choose phase; Run MultiMatch; inspect all five component tables; save and verify reopened workspace"
        ),
        "runs" -> Json.arr(runs*)
      )
    finally runOnFx(view.dispose())

  fxStage.test("scanpath controls compute and reopen synthetic workspace") { fx =>
    val root   = Files.createTempDirectory("studio-scanpath-synthetic-")
    val stream = getClass.getResourceAsStream("/scanpath/request.json")
    try Files.copy(stream, root.resolve("request.json"))
    finally stream.close()
    val result = exercise(fx, root, None)
    assertEquals(get(result.hcursor.get[String]("status")), "passed")
  }
  sys.env.get("EYES4S_PRIVATE_RUN").foreach { directory =>
    fxStage.test("private scanpath controls agree with independently retained CLI pairs") {
      fx =>
        require(
          !sys.env.contains("CI") && !sys.env.contains("GITHUB_ACTIONS"),
          "Private UI disabled in CI"
        )
        require(StudioFxSuite.headless, "Private UI must be headless")
        val root = safe(Path.of(directory))
        Files.createDirectories(StudioFxSuite.snapshotRoot)
        require(
          safe(StudioFxSuite.snapshotRoot).startsWith(root),
          "Private screenshots must stay in the run"
        )
        val expected = Files
          .readAllLines(root.resolve("expected.jsonl"))
          .asScala
          .toVector
          .map(s => get(parser.parse(s)))
        val result = exercise(fx, root, Some(expected))
        Files.writeString(
          root.resolve("ui-result.json"),
          result.spaces2,
          StandardOpenOption.CREATE_NEW
        )
    }
  }
