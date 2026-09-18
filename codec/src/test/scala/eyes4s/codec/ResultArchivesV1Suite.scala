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

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** The frozen recording-result-v1 and temporal-result-v1 archives pin the
  * archived meaning of the pinned inputs' results, independently of the
  * encoder, and re-encode to the same JSON values on the JVM and Scala.js.
  */
class ResultArchivesV1Suite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private def parse(text: String): Json     = get(io.circe.parser.parse(text))

  private lazy val recording =
    get(
      ArchiveFixtures.recordingResults.codec.parse(
        ResultArchiveMirrors.recordingResultVersionOne
      )
    )
  private lazy val temporal =
    get(
      ArchiveFixtures.temporalResults.codec.parse(ResultArchiveMirrors.temporalResultVersionOne)
    )

  test(
    "frozen recording-result v1 fixes the plan, synchronization, stages, events and assignment"
  ) {
    val analysis = recording
    assertEquals(analysis.plan.description, ArchiveFixtures.recordingPlan.description)
    assertEquals(analysis.input.digest, "2c826dc41ae25e67")
    assertEquals(analysis.input.digest, InputPayloadFixtures.monocular.contentHash.render)
    // The synchronization is refitted from the plan's marks: t4 is rejected.
    val sync = analysis.synchronization
    assertEquals(sync.offset.toMicros, 1000010L)
    assertEquals(sync.usedMarks.map(_.id), Vector("t1", "t2", "t3"))
    assertEquals(
      sync.rejectedMarks.map(r => r.mark.id -> r.residual.toMicros),
      Vector("t4" -> 3742L)
    )
    // Angular and prepared stages: the 6 ms gap is interpolated in preparation only.
    assertEquals(analysis.angular.frame.id, FrameId("angular"))
    assertEquals(analysis.angular.clock, ClockId("display"))
    assertEquals(
      analysis.angular.samples.map(_.t.toMicros).toVector,
      Vector(1000010L, 1002010L, 1004011L, 1006010L, 1008010L, 1010010L, 1012010L, 1014010L)
    )
    assertEquals(analysis.angular.samples(2).gaze, Gaze.Blink[Unit2D.Deg]())
    assertEquals(
      analysis.prepared.samples.map(_.t).toVector,
      analysis.angular.samples.map(_.t).toVector
    )
    assertEquals(
      analysis.prepared.samples.map(_.lineage).toVector.slice(2, 4),
      Vector(SampleLineage.interpolated, SampleLineage.interpolated)
    )
    assertNotEquals(analysis.prepared.contentHash, analysis.angular.contentHash)
    // Two source-supported fixations over the prepared samples.
    val detection = analysis.detection
    assertEquals(detection.identity, DetectorIdentity.Algorithm(AlgorithmCards.idt))
    assertEquals(
      detection.eventSeries.support.map(r => r.from -> r.until),
      Vector(0 -> 5, 6 -> 8)
    )
    val fixations = detection.eventSeries.events.collect { case f: Event.Fixation[?] => f }
    assertEquals(fixations.map(_.sampleCount), Vector(5, 2))
    assertEquals(fixations.map(_.centre.x), Vector(0.48222735651099996, 1.074165338927112))
    fixations.foreach(f =>
      f.dispersionStatus match
        case DispersionStatus.Available(value, SummaryEvidence.SourceSupported(source, _)) =>
          assertEquals(value.method, DispersionMethod.BoundingBoxDiagonal)
          assertEquals(source, RecordingRef("synthetic-left-headfixed-500"))
        case other => fail(s"unexpected dispersion $other")
    )
    assertEquals(
      detection.labels.toVector,
      Vector.fill(5)(SampleClass.Fixation) ++ Vector(SampleClass.OffSurface) ++
        Vector.fill(2)(SampleClass.Fixation)
    )
    assertEquals(detection.provenance.inputs, analysis.prepared.contentHash)
    assertEquals(detection.report.totalSamples, 8)
    // One area; the off-screen sample is excluded.
    val assignment = analysis.assignment
    assertEquals(assignment.aoiSet.ids.map(_.value), Vector("image"))
    assertEquals(
      assignment.toVector(5),
      SampleMembership.Excluded(ExclusionReason.OffSurface)
    )
    assertEquals(assignment.report.aoiUnionTime.toMicros, 14000L)
    assertEquals(assignment.report.excludedTime.toMicros, 2000L)
    assert(assignment.accountingHolds)
    assertEquals(
      get(ArchiveFixtures.recordingResults.codec.encode(analysis)),
      parse(ResultArchiveMirrors.recordingResultVersionOne)
    )
  }

  test("frozen temporal-result v1 fixes the cells, occupancy ledgers and temporal failures") {
    val result = temporal
    val plan   = ConventionalPlanFixtures.temporalPlan
    assertEquals(result.description, plan.description)
    assertEquals(result.input, InputPayloadFixtures.temporal.reference)
    assertEquals(
      result.cells.map(c => c.repetition.name -> c.window.name),
      for
        r <- Vector("recall-encode", "retest-recall")
        w <- Vector("early", "middle", "late", "outside")
      yield r -> w
    )
    val missing = InputPayloadFixtures.temporal.study.trials.rows
      .map(_.key)
      .filterNot(InputPayloadFixtures.temporal.epochs.contains)
    assertEquals(missing.size, 1)
    result.cells.foreach { cell =>
      assertEquals(cell.occupancy.size, 18)
      assertEquals(
        cell.study.description,
        get(plan.repetitionPlan(cell.repetition)).description
      )
      assertEquals(cell.result.scales.size, 2)
      // The trial without an epoch fails in every cell, and its estimation
      // carries the same typed temporal failure at every scale.
      val failure = get(cell.occupancy.toMap.apply(missing.head).swap.left.map(_ => "occupied"))
      assertEquals(
        failure,
        TemporalStudyError.MissingEpoch(
          plan.base.layout.digest.digest(missing.head).render
        )
      )
      cell.result.scales.foreach(scale =>
        assertEquals(
          scale.estimation.toMap.apply(missing.head),
          Left(StudyFailure.Temporal(missing.head, failure))
        )
      )
      // Every density was estimated from its trial's occupancy measure.
      cell.result.scales.foreach(_.estimation.foreach {
        case (key, Right(mass)) =>
          val occupancy = get(cell.occupancy.toMap.apply(key).left.map(_.message))
          assertEquals(mass.provenance.inputs, occupancy.measure.provenance.inputs)
        case _ => ()
      })
    }
    // The anchor beyond JavaScript's exact integer range survives in every window.
    val beyond = InputPayloadFixtures.temporal.epochs.collectFirst {
      case (key, epoch) if epoch.anchor.toMicros > (1L << 53) => key -> epoch.anchor.toMicros
    }
    val (anchored, anchor) = get(beyond.toRight("no anchor beyond 2^53"))
    result.cells.foreach { cell =>
      val occupancy = get(cell.occupancy.toMap.apply(anchored).left.map(_.message))
      assertEquals(occupancy.interval.onset.toMicros, anchor + cell.window.window.from.toMicros)
      assertEquals(
        occupancy.observedMicros + occupancy.missingMicros,
        cell.window.window.width.toMicros
      )
    }
    // The coverage gap of s2/b/retest leaves missing time in its middle window.
    val gap    = StudyKey("s2", "b", "retest")
    val middle = get(
      result.cells
        .find(c => c.repetition.name == "retest-recall" && c.window.name == "middle")
        .toRight("cell")
    )
    assertEquals(
      get(middle.occupancy.toMap.apply(gap).left.map(_.message)).missingMicros,
      150000L
    )
    assertEquals(
      get(ArchiveFixtures.temporalResults.codec.encode(result)),
      parse(ResultArchiveMirrors.temporalResultVersionOne)
    )
  }

  test(
    "the recording archive re-derives its members: an altered derived member is refused by path"
  ) {
    val json  = parse(ResultArchiveMirrors.recordingResultVersionOne)
    val label = json.hcursor
      .downField("value")
      .downField("detection")
      .downField("labels")
      .downN(5)
      .withFocus(_ => Json.fromString("fixation"))
      .top
      .get
    assertEquals(
      ArchiveFixtures.recordingResults.codec.decode(label).left.toOption,
      Some(
        CodecError.Derived(
          "detection.labels[5]",
          Json.fromString("fixation"),
          Json.fromString("offSurface")
        )
      )
    )
    val offset = json.hcursor
      .downField("value")
      .downField("synchronization")
      .downField("offsetMicros")
      .withFocus(_ => Json.fromString("1000011"))
      .top
      .get
    assertEquals(
      ArchiveFixtures.recordingResults.codec.decode(offset).left.toOption,
      Some(
        CodecError.Derived(
          "synchronization.offsetMicros",
          Json.fromString("1000011"),
          Json.fromString("1000010")
        )
      )
    )
  }

  test("a declared fixation dispersion value is refused where it would be ignored") {
    val json   = parse(ResultArchiveMirrors.recordingResultVersionOne)
    val valued = json.hcursor
      .downField("value")
      .downField("detection")
      .downField("events")
      .downArray
      .downField("dispersion")
      .withFocus(_.mapObject(_.add("value", Json.fromDoubleOrNull(0.5))))
      .top
      .get
    ArchiveFixtures.recordingResults.codec.decode(valued).left.toOption match
      case Some(CodecError.Entry(path, CodecError.Field("value", _, reason))) =>
        assertEquals(path, "detection.events[0]")
        assertEquals(
          reason,
          "a source-supported dispersion carries its method only; its value is derived"
        )
      case other => fail(s"unexpected $other")
  }
