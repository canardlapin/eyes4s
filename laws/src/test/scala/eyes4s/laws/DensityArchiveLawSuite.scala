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
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

import org.scalacheck.{Gen, Test}

/** Archive laws over small, independently constructed completed studies.
  *
  * | mutant              | injected fault                              | killed by |
  * |---------------------|---------------------------------------------|-----------|
  * | first-map-for-all   | reader returns the first map for every key  | storage preserves each map identity |
  * | v1-to-v2-identity   | ladder fails to add v2 storage representation | schema-ladder upcasting property |
  *
  * Mutation kills count only `Test.Failed`; a thrown property or exhausted
  * generator is not evidence that a law discriminates.
  */
class DensityArchiveLawSuite extends munit.DisciplineSuite:

  private type Result = StudyResult[StudyKey, Px, Similarity, SignedDifference]
  private final case class Case(
      plan: StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference],
      input: StudyInput[StudyKey, Px],
      result: Result
  )

  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def sure[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalStateException(error.toString), identity)

  private val frame        = get(Frame.screen("density-archive-laws", 2, 2))
  private val grid         = get(Grid.over(frame, 2, 2))
  private val resultCodec  = StudyResultCodecs.cosine[Px]
  private val archiveCodec = new DensityArchiveCodec(resultCodec)

  private def trial(
      key: StudyKey,
      points: Vector[(Double, Double)]
  ): Trial[StudyKey, Unit, Scanpath[Px]] =
    val clock = ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")
    val fixes = points.zipWithIndex.map { case ((x, y), index) =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval.of(
              clock,
              Instant.micros(index * 2000L),
              Instant.micros(index * 2000L + 1000L)
            )
          ),
          Pt[Px](x, y),
          index + 1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))

  private val points: Gen[Vector[(Double, Double)]] =
    Gen.oneOf(
      Vector((0.5, 0.5)),
      Vector((1.5, 0.5)),
      Vector((0.5, 1.5), (1.5, 1.5)),
      Vector((1.5, 1.5), (0.5, 1.5))
    )

  private def caseOf(
      p1Recall: Vector[(Double, Double)],
      p1Encode: Vector[(Double, Double)],
      p2Recall: Vector[(Double, Double)],
      p2Encode: Vector[(Double, Double)]
  ): Case =
    val input = StudyInput(
      Trials(
        Vector(
          trial(StudyKey("p1", "item", "recall"), p1Recall),
          trial(StudyKey("p1", "item", "encode"), p1Encode),
          trial(StudyKey("p2", "item", "recall"), p2Recall),
          trial(StudyKey("p2", "item", "encode"), p2Encode)
        )
      )
    )
    val plan = sure(
      StudyPlan.cosine[Px](
        input.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll
      )
    )
    Case(plan, input, sure(plan.run(input)))

  private val cases: Gen[Case] = for
    p1Recall <- points
    p1Encode <- points
    p2Recall <- points
    p2Encode <- points
  yield caseOf(p1Recall, p1Encode, p2Recall, p2Encode)

  private val distinctCase = caseOf(
    Vector((0.5, 0.5)),
    Vector((1.5, 0.5)),
    Vector((0.5, 1.5)),
    Vector((1.5, 1.5))
  )

  private def same(left: Result, right: Result): Boolean =
    StudyResultEquivalence.same(left, right)(
      (a, b) => a.value == b.value,
      (a, b) => a.value == b.value
    )

  private def access(
      value: Case,
      bundle: DensityArchiveBundle[StudyKey, Px, Unit, Similarity, SignedDifference]
  ): DensityArchiveLaws.Access[StudyKey, Px] =
    val chunks = bundle.chunks.map(_._2).map(chunk => chunk.ref -> chunk).toMap
    DensityArchiveLaws.Access(
      chunks.get,
      Some(archiveCodec.recomputer(value.plan, value.input, bundle.archive))
    )

  private val reader
      : DensityArchiveLaws.ViewReader[Case, StudyKey, Px, Unit, Similarity, SignedDifference] =
    (value, bundle, scale, key) =>
      val providers = access(value, bundle)
      bundle.archive.densityReader(providers.payloads, providers.recompute).density(scale, key)

  private val storages =
    Vector(DensityStorage.Inline, DensityStorage.Packed, DensityStorage.Recomputable)

  checkAll(
    "density archive inline",
    DensityArchiveLaws.roundTrip(
      archiveCodec,
      cases,
      _.result,
      access,
      DensityStorage.Inline,
      same
    )
  )
  checkAll(
    "density archive packed",
    DensityArchiveLaws.roundTrip(
      archiveCodec,
      cases,
      _.result,
      access,
      DensityStorage.Packed,
      same
    )
  )
  checkAll(
    "density archive recomputable",
    DensityArchiveLaws.roundTrip(
      archiveCodec,
      cases,
      _.result,
      access,
      DensityStorage.Recomputable,
      same
    )
  )
  checkAll(
    "density archive storage identity",
    DensityArchiveLaws.storageIndependentMaps(archiveCodec, cases, _.result, storages, reader)
  )

  private val archives
      : Gen[StudyResultArchive[StudyKey, Px, Unit, Similarity, SignedDifference]] =
    for
      value   <- cases
      storage <- Gen.oneOf(storages)
    yield sure(archiveCodec.encode(value.result, storage)).archive

  private def sameArchive(
      left: StudyResultArchive[StudyKey, Px, Unit, Similarity, SignedDifference],
      right: StudyResultArchive[StudyKey, Px, Unit, Similarity, SignedDifference]
  ): Boolean =
    archiveCodec.codec
      .encode(left)
      .flatMap(encoded => archiveCodec.codec.encode(right).map(_ == encoded))
      .getOrElse(false)

  checkAll(
    "density archive document",
    CodecLaws.roundTrip(archiveCodec.codec, archives, sameArchive)
  )

  checkAll(
    "density archive versions",
    SchemaLadderLaws.ladder(archiveCodec.ladder, archives, sameArchive)
  )

  private val mutationParameters =
    Test.Parameters.default.withMinSuccessfulTests(40).withInitialSeed(0x44454e53495459L)

  private def failedOnly(rules: org.typelevel.discipline.Laws#RuleSet): Vector[String] =
    rules.all.properties.collect {
      case (name, prop) if genuinelyFalsified(prop) => name.stripPrefix("densityArchive.maps.")
    }.toVector

  private def genuinelyFalsified(prop: org.scalacheck.Prop): Boolean =
    Test.check(mutationParameters, prop).status match
      case Test.Failed(_, _) => true
      case _                 => false

  private val firstMapForAll
      : DensityArchiveLaws.ViewReader[Case, StudyKey, Px, Unit, Similarity, SignedDifference] =
    (value, bundle, _, _) =>
      val first = value.result.scales.head.estimation.collectFirst { case (key, Right(_)) =>
        key
      }.get
      reader(value, bundle, 0, first)

  test("published map identity law rejects a reader that aliases every key to the first map") {
    val rules = DensityArchiveLaws.storageIndependentMaps(
      archiveCodec,
      Gen.const(distinctCase),
      _.result,
      storages,
      firstMapForAll
    )
    assertEquals(failedOnly(rules), Vector("storage preserves each map identity"))
  }

  test("published schema ladder laws reject an identity v1-to-v2 upcast") {
    assert(LadderMutants.passes(archiveCodec.ladder, archives, sameArchive))
    val mutant = LadderMutants.rebuilt(archiveCodec.ladder)(upcast = {
      case DensityArchiveDefinitions.studyResultV2 => (json: Json) => json
    })
    assert(LadderMutants.falsified(mutant, archives, sameArchive).nonEmpty)
  }
