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

import eyes4s.compare.ComparisonQuantum
import eyes4s.design.{FailurePolicy, PairQuantum, WorkQuanta}
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json
import scala.compiletime.testing.typeCheckErrors

class StampedStudySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val plans                             = StudyCodecs.cosine[Px]
  private val inputs                            = StudyInputCodecs.study[Px]
  private val results                           = StudyResultCodecs.cosine[Px]
  private val archives                          = new DensityArchiveCodec(results)
  private val input     = get(inputs.input.parse(StudyInputFixtures.inputVersionOne))
  private val plan      = get(plans.codec.parse(SavedStudyFixtures.versionOne))
  private val bound     = get(StampedStudy.prepare(plan, input, plans, inputs))
  private val completed = get(bound.run)

  test("pure and stepped completion bind the same result and canonical documents") {
    val stepped = get(
      Stepwise.complete(
        get(bound.work()),
        WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))
      )
    )
    assertEquals(
      get(results.codec.encode(stepped.result)),
      get(results.codec.encode(completed.result))
    )
    assertEquals(completed.stamp.check(stepped.stamp, Vector.empty), Right(()))
    assert(get(completed.checkAgainst(bound)) eq completed.result)
  }

  test("a completed envelope refuses a different prepared plan") {
    val changed = StudyResultFixtures.cosinePlan(input, FailurePolicy.RequireAll)
    val current = get(StampedStudy.prepare(changed, input, plans, inputs))
    assert(completed.checkAgainst(current).left.exists {
      case RunStampError.ChangedPlan(reported, actual, changes) =>
        reported.sameAs(completed.stamp.plan) && actual
          .sameAs(current.stamp.plan) && changes.nonEmpty
      case _ => false
    })
  }

  test("an arbitrary result cannot be attached with public constructors or copy") {
    assert(typeCheckErrors("""
      import eyes4s.codec.*
      import eyes4s.plan.*
      import eyes4s.kernel.Unit2D.Px
      import eyes4s.compare.Similarity
      import eyes4s.design.SignedDifference
      val stamp: RunStamp[StudyPlan[StudyKey,Px,Unit,Similarity,SignedDifference],StudyInput[StudyKey,Px]] = ???
      val result: StudyResult[StudyKey,Px,Similarity,SignedDifference] = ???
      new StampedStudyResult(stamp, result)
    """).exists(_.message.contains("cannot be accessed")))
    assert(typeCheckErrors("""
      import eyes4s.codec.*
      import eyes4s.plan.*
      import eyes4s.kernel.Unit2D.Px
      import eyes4s.compare.Similarity
      import eyes4s.design.SignedDifference
      val completed: StampedStudyResult[StudyKey,Px,Unit,Similarity,SignedDifference] = ???
      completed.copy()
    """).exists(_.message.contains("copy")))
  }

  test("legacy v1 is explicitly unstamped and retains its pinned bytes") {
    val old = get(archives.codec.parse(StudyResultFixtures.resultVersionOne))
    assertEquals(old.stampClaim, None)
    assertEquals(old.checkStamp(bound), Right(None))
    assertEquals(
      get(archives.codec.encode(old)).noSpaces,
      get(io.circe.parser.parse(StudyResultFixtures.resultVersionOne)).noSpaces
    )
  }

  test("all stamped storage forms use v2 and round-trip both canonical bindings") {
    DensityStorage.values.foreach { storage =>
      val bundle   = get(archives.encodeStamped(completed, storage))
      val document = get(archives.codec.encode(bundle.archive))
      assertEquals(get(document.hcursor.downField("schema").get[Int]("version")), 2)
      assertEquals(
        get(document.hcursor.downField("value").downField("runStamp").get[String]("plan")),
        completed.stamp.plan.sha256.hex
      )
      assertEquals(
        get(document.hcursor.downField("value").downField("runStamp").get[String]("input")),
        completed.stamp.input.sha256.hex
      )
      val opened = get(archives.codec.parse(document.noSpaces))
      assert(get(opened.checkStamp(bound)).nonEmpty)
      val recompute    = get(archives.checkedRecomputer(bound, opened))
      val materialized = get(
        opened.materialize(
          ref => bundle.chunks.find(_._2.ref == ref).map(_._2),
          Some(recompute)
        )
      )
      assertEquals(
        get(results.codec.encode(materialized)),
        get(results.codec.encode(completed.result))
      )
      assertEquals(get(archives.codec.encode(opened)), document)
    }
  }

  private def withClaim(document: Json, claim: Json): Json =
    document.mapObject(o => o.add("value", o("value").get.mapObject(_.add("runStamp", claim))))

  test("malformed claims and claims smuggled into v1 fail at the wire boundary") {
    val bundle   = get(archives.encodeStamped(completed, DensityStorage.Inline))
    val document = get(archives.codec.encode(bundle.archive))
    Vector(
      Json.Null,
      Json.obj(),
      Json.obj(
        "plan"  -> Json.fromString("bad"),
        "input" -> Json.fromString(completed.stamp.input.sha256.hex)
      )
    ).foreach { malformed =>
      assert(archives.codec.decode(withClaim(document, malformed)).isLeft)
    }
    val old = get(io.circe.parser.parse(StudyResultFixtures.resultVersionOne))
    assert(archives.codec.decode(withClaim(old, RunStampWire.write(completed.stamp))).isLeft)
  }

  test("a well-formed saved stamp remains an unverified claim until checked") {
    val bundle   = get(archives.encodeStamped(completed, DensityStorage.Recomputable))
    val document = get(archives.codec.encode(bundle.archive))
    val forged   =
      RunStampWire.write(completed.stamp).mapObject(_.add("plan", Json.fromString("0" * 64)))
    val opened = get(archives.codec.decode(withClaim(document, forged)))
    assert(opened.stampClaim.nonEmpty)
    assert(opened.checkStamp(bound).left.exists {
      case RunStampError.ChangedPlan(reported, actual, changes) =>
        reported.sha256.hex == "0" * 64 && actual.sameAs(bound.stamp.plan) && changes.isEmpty
      case _ => false
    })
    assert(archives.checkedRecomputer(bound, opened).isLeft)
    val key = completed.result.scales.head.estimation.head._1
    assert(archives.recomputer(plan, input, opened)(0, 0, key).isLeft)
  }
