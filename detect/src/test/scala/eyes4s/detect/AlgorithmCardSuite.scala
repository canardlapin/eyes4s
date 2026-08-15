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

package eyes4s.detect

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

class AlgorithmCardSuite extends munit.FunSuite:

  private val clock = ClockId("algorithm-card")

  test("every shipped paper-named detector has a unique complete card") {
    val cards = AlgorithmCards.all

    assertEquals(cards.length, 3)
    assertEquals(cards.map(_.id.value).distinct.length, cards.length)
    cards.foreach { card =>
      assert(card.name.nonEmpty)
      assert(card.citations.nonEmpty)
      assert(card.assumptions.nonEmpty)
      assert(card.references.nonEmpty)
      assertEquals(card.detectorRef.name, card.id.value)
      assertEquals(card.detectorRef.version, card.version.render)
    }
  }

  test("I-VT and I-DT identify the canonical Salvucci-Goldberg publication") {
    Vector(
      AlgorithmCards.ivt -> "src/pymovements/events/detection/ivt.py",
      AlgorithmCards.idt -> "src/pymovements/events/detection/idt.py"
    ).foreach { case (card, referencePath) =>
      assertEquals(card.citations.map(_.doi.value), Vector("10.1145/355017.355028"))
      assert(
        card.references.contains(DesignatedReference.Publication(card.citations.head))
      )
      assert(
        card.references.contains(
          DesignatedReference.SourceImplementation(
            "https://github.com/aeye-lab/pymovements",
            "6753fdf8b81da40b890576dd6b369edb81243b06",
            referencePath,
            "MIT"
          )
        )
      )
      assert(card.deviations.contains(DetectorDeviation.HalfOpenEventIntervals))
      assert(card.deviations.contains(DetectorDeviation.SourceSupportIsRetained))
    }
  }

  test("Engbert-Kliegl pins both the paper and independent source revision") {
    val card = AlgorithmCards.engbertKliegl

    assertEquals(
      card.citations.map(_.doi.value),
      Vector("10.1016/S0042-6989(03)00084-1")
    )
    assert(
      card.references.exists {
        case DesignatedReference.SourceImplementation(_, revision, path, "GPL-3.0") =>
          revision == "a3eba6e9f1464c953c81fbd87944ba7678c2cf64" &&
          path == "EngbertMicrosaccadeToolbox/microsac_detection.py"
        case _ => false
      }
    )
    assertEquals(card.execution, DetectorExecution.StreamingAfterWholeRecordingPreparation)
    assert(card.deviations.contains(DetectorDeviation.BinocularOverlapNotImplemented))
    assert(card.deviations.contains(DetectorDeviation.ThresholdEstimationIsSeparate))
    assert(card.deviations.contains(DetectorDeviation.PublicationMedianSquareThreshold))
    assert(card.deviations.contains(DetectorDeviation.ToolboxBoundaryVelocitiesOmitted))
    assert(card.deviations.contains(DetectorDeviation.InclusiveCandidatePeakVelocity))
  }

  test("runnable detectors cannot be obtained without their exact card") {
    val ivt = Detectors.ivt(
      IvtThreshold.of(Velocity.degPerSecond(30.0).toOption.get).toOption.get,
      MinimumEventDuration.of(Span.millis(20)).toOption.get,
      clock
    )
    val idt = Detectors.idt[Deg](
      Extent.square[Deg](1.0).toOption.get,
      MinimumEventDuration.of(Span.millis(20)).toOption.get,
      clock
    )
    val ek = Detectors.engbertKliegl(
      EkThresholds.of(30.0, 30.0).toOption.get,
      EkMinimumSamples.of(3).toOption.get,
      clock
    )

    assertEquals(ivt.card, AlgorithmCards.ivt)
    assertEquals(idt.card, AlgorithmCards.idt)
    assertEquals(ek.card, AlgorithmCards.engbertKliegl)
    assertEquals(
      ivt.configuration,
      Vector(
        "velocityThresholdDegPerSecond" -> Provenance.Param.Num(30.0),
        "minimumEventDurationMicros"    -> Provenance.Param.Num(20000.0)
      )
    )
    assertEquals(
      idt.configuration,
      Vector(
        "extentWidth"                -> Provenance.Param.Num(1.0),
        "extentHeight"               -> Provenance.Param.Num(1.0),
        "minimumEventDurationMicros" -> Provenance.Param.Num(20000.0)
      )
    )
    assertEquals(
      ek.configuration,
      Vector(
        "etaXDegPerSecond" -> Provenance.Param.Num(30.0),
        "etaYDegPerSecond" -> Provenance.Param.Num(30.0),
        "minimumSamples"   -> Provenance.Param.Num(3.0)
      )
    )
  }

  test("metadata constructors reject malformed local invariants") {
    assertEquals(
      AlgorithmId.from("  "),
      Left(AlgorithmMetadataError.EmptyAlgorithmId("  "))
    )
    assertEquals(
      Doi.from("https://doi.org/10.1/example"),
      Left(AlgorithmMetadataError.InvalidDoi("https://doi.org/10.1/example"))
    )
    assertEquals(
      AlgorithmVersion.of(1, -1, 0),
      Left(AlgorithmMetadataError.InvalidVersion(1, -1, 0))
    )
    val id      = AlgorithmId.from("test.detector").toOption.get
    val version = AlgorithmVersion.of(1, 0, 0).toOption.get
    assertEquals(
      AlgorithmCard.of(
        id,
        version,
        "test",
        Vector.empty,
        Vector(DetectorAssumption.StrictlyIncreasingTimestamps),
        Vector.empty,
        DetectorExecution.Streaming,
        Vector(DesignatedReference.Publication(AlgorithmCards.ivt.citations.head))
      ),
      Left(AlgorithmMetadataError.NoCitations(id))
    )
  }

end AlgorithmCardSuite
