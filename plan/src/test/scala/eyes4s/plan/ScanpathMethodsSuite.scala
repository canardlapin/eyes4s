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

package eyes4s.scanpathmethodconsumer

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import scala.compiletime.testing.typeCheckErrors

class ScanpathMethodsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame = get(Frame.screen("scanpath-methods", 100, 100))
  private val clock = ClockId("scanpath-methods")
  private def path(
      points: Vector[(Double, Double)],
      sourceClock: ClockId = clock
  ): Scanpath[Px] =
    val fixes = points.zipWithIndex.map { (point, index) =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval.of(
              sourceClock,
              Instant.micros(index * 1000L),
              Instant.micros(index * 1000L + 500)
            )
          ),
          Pt[Px](point._1, point._2),
          1
        )
      )
    }
    get(Scanpath.of(frame, sourceClock, IArray.from(fixes)))
  private val a = path(Vector((1, 1), (2, 1), (2, 3)))
  private val b = path(Vector((1, 2), (3, 2), (3, 4)))

  test("registered identities, component contracts and exact underlying MultiMatch agree") {
    assertEquals(ScanpathMethods.all, ScanpathMethodKind.values.toVector)
    ScanpathMethods.all.foreach(kind =>
      assertEquals(ScanpathMethods.resolve(kind.id), Some(kind))
    )
    val method = ScanpathMethods.multiMatch[Px]
    assertEquals(method.execution, ExecutionCapability.SynchronousWholeOperation)
    assert(method.properties(ComparisonProperty.Symmetric))
    get(method.compare(a, b)) match
      case ScanpathMethodOutput.MultiMatch(value) =>
        assertEquals(value, get(MultiMatch[Px].compare(a, b)))
        assertEquals(get(method.compare(b, a)).scores, get(method.compare(a, b)).scores)
      case other => fail(s"$other")
    assertEquals(
      method.componentNames,
      Vector("shape", "direction", "length", "position", "duration")
    )
  }

  test("baseline cardinality and alignment size refuse before expensive allocation") {
    val short = path(Vector((1, 1), (2, 2)))
    assert(ScanpathMethods.multiMatch[Px].compare(short, a).isRight)
    ScanpathMethods.baselineMultiMatch[Px].compare(short, a) match
      case Left(
            ScanpathComparisonError.Alignment(CompareError.TooShort("left scanpath", 2, 3))
          ) =>
        ()
      case other => fail(s"$other")
    val method = ScanpathMethod
      .MultiMatch[Px](MultiMatchCardinality.NativeTwoFixations, get(AlignmentCellLimit.of(3)))
    assertEquals(
      method.compare(a, b),
      Left(ScanpathComparisonError.AlignmentSize(3, 3, 4L, 3L))
    )
    assert(AlignmentCellLimit.of(0).isLeft)
    assert(typeCheckErrors("new eyes4s.plan.AlignmentCellLimit(0)").nonEmpty)
  }

  test(
    "overlap keeps repeated ordered queries and strict missing-policy failures with their ledger"
  ) {
    val queries = Vector(Instant.micros(1000), Instant.micros(-1), Instant.micros(1000))
    val spec    = get(
      FixationOverlapSpec.of(
        clock,
        queries,
        get(OverlapThreshold.of[Px](2)),
        FixationGroundDistance.Euclidean,
        TrajectoryEndpoint.HoldLastOnset,
        MissingOverlapPolicy.RequireComplete
      )
    )
    val method = ScanpathMethod.Overlap(spec)
    get(method.compare(a, b)) match
      case ScanpathMethodOutput.Overlap(value) =>
        assertEquals(value.rows.map(_.time), queries)
        assertEquals(value.requested, 3)
        assertEquals(value.contributing, 2)
        assertEquals(value.similarity, Left(FixationComparisonError.MissingSupport(3, 2)))
        assert(value.rows(1).distance.isLeft)
      case other => fail(s"$other")
    assert(
      FixationOverlapSpec
        .of(
          clock,
          Vector.empty,
          get(OverlapThreshold.of[Px](2)),
          FixationGroundDistance.Euclidean,
          TrajectoryEndpoint.OnsetRange,
          MissingOverlapPolicy.CountAsNonOverlap
        )
        .isLeft
    )
  }

  test("transport retains solver diagnostics, work guards and directed clock requirements") {
    val config =
      get(FixationTransportConfig.of[Px](100, 100, Span.millis(1), 0, 1, maximumWork = 9))
    val method = ScanpathMethod.Transport(config)
    assert(!method.properties(ComparisonProperty.Symmetric))
    assertEquals(method.componentNames, Vector("transport"))
    assertEquals(
      method.parameters.toMap.apply("maximumWork"),
      Vector(Provenance.Param.Text("9"))
    )
    method.compare(a, path(Vector((1, 1), (2, 1), (2, 3)), ClockId("independent"))) match
      case Left(ScanpathComparisonError.Fixations(FixationComparisonError.Clock(_))) => ()
      case other => fail(s"$other")
    val tiny =
      get(FixationTransportConfig.of[Px](100, 100, Span.millis(1), 0, 1, maximumWork = 8))
    ScanpathMethod.Transport(tiny).compare(a, b) match
      case Left(
            ScanpathComparisonError.Fixations(FixationComparisonError.WorkLimit(3, 3, 0, _, 8L))
          ) =>
        ()
      case other => fail(s"$other")
    val admitted = ScanpathMethod.Transport(
      get(FixationTransportConfig.of[Px](100, 100, Span.millis(1), 0, 1))
    )
    get(admitted.compare(a, b)) match
      case ScanpathMethodOutput.Transport(value) =>
        assert(value.iterations > 0)
        assert(value.marginalResidual <= 1e-10)
        assertEquals(value.leftCount, 3)
        assertEquals(value.rightCount, 3)
      case other => fail(s"$other")
  }

  test("Euclidean overlap uses the named exact fdlibm norm on both runtimes") {
    val left  = path(Vector((0, 0)))
    val right = path(Vector((24, 4)))
    val spec  = get(
      FixationOverlapSpec.of(
        clock,
        Vector(Instant.micros(0)),
        get(OverlapThreshold.of[Px](30.0)),
        FixationGroundDistance.Euclidean,
        TrajectoryEndpoint.OnsetRange,
        MissingOverlapPolicy.RequireComplete
      )
    )
    val method = ScanpathMethod.Overlap(spec)
    assertEquals(spec.distance.normConvention, FixationNormConvention.FdlibmHypotV1)
    get(method.compare(left, right)) match
      case ScanpathMethodOutput.Overlap(value) =>
        assertEquals(value.rows.head.distance, Right(24.331050121192877))
      case other => fail(s"$other")
  }
