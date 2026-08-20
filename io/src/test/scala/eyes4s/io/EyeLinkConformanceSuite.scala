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

package eyes4s.io

import eyes4s.core.Gaze
import eyes4s.kernel.Bounds
import eyes4s.kernel.ClockId
import eyes4s.kernel.Frame
import eyes4s.kernel.FrameId
import eyes4s.kernel.Unit2D
import eyes4s.kernel.YAxis

private[io] object EyeLinkPortableConformanceExpected:
  val semanticDigests: Map[String, String] = Map(
    "synthetic-binocular-remote-2000" -> "678b21c8e2606b90a79c069141119638d685d0858e191de73605c996105c40ea",
    "synthetic-events-messages" -> "590538961f6776dafa898fb2adcf3a9bdfa6cc7c8f6c0ba2e261a6ad68b96b7e",
    "synthetic-failsafe-corrupt" -> "8a74e3b0b28162fb107f2845fd4f6f465b1340914cdac700f7fe5ead5935e645",
    "synthetic-left-headfixed-1000" -> "79377121949034d1831e3c306f04df94a179b1606e428f35c2074e9c4adc7a0d",
    "synthetic-multiblock" -> "5913f5b7db027292155a82c46136d62397ea5381d1d4c1d5f18e54aede41011f",
    "synthetic-right-headfixed-500" -> "fd65532b333228d129eb276b07ec69a7b3f1569c6bcd7d98113a11afa3041a75"
  )

  val renderTsv: String =
    """schema	fixture	eyes4s_sha256	asc_oracle	asc_status	asc_sha256	edf_oracle	edf_status	edf_sha256	metamorphic_assertions	limitation
      |eyes4s-eyelink-portable-conformance-v1	synthetic-binocular-remote-2000	678b21c8e2606b90a79c069141119638d685d0858e191de73605c996105c40ea	none-available	missing-external		none-available	missing-external		6	no independent ASC oracle; licensed EDF oracle unavailable
      |eyes4s-eyelink-portable-conformance-v1	synthetic-events-messages	590538961f6776dafa898fb2adcf3a9bdfa6cc7c8f6c0ba2e261a6ad68b96b7e	CRAN-eyelinker-0.2.2	compared-pass	7c8852f0d8d2a0b4a1dabea4eb6b16d36fb81780b9285fc3ba974281198d3d04	none-available	missing-external		6	ASC oracle compared; licensed EDF oracle unavailable
      |eyes4s-eyelink-portable-conformance-v1	synthetic-failsafe-corrupt	8a74e3b0b28162fb107f2845fd4f6f465b1340914cdac700f7fe5ead5935e645	none-available	missing-external		none-available	missing-external		6	no independent ASC oracle; licensed EDF oracle unavailable
      |eyes4s-eyelink-portable-conformance-v1	synthetic-left-headfixed-1000	79377121949034d1831e3c306f04df94a179b1606e428f35c2074e9c4adc7a0d	none-available	missing-external		none-available	missing-external		6	no independent ASC oracle; licensed EDF oracle unavailable
      |eyes4s-eyelink-portable-conformance-v1	synthetic-multiblock	5913f5b7db027292155a82c46136d62397ea5381d1d4c1d5f18e54aede41011f	none-available	missing-external		none-available	missing-external		6	no independent ASC oracle; licensed EDF oracle unavailable
      |eyes4s-eyelink-portable-conformance-v1	synthetic-right-headfixed-500	fd65532b333228d129eb276b07ec69a7b3f1569c6bcd7d98113a11afa3041a75	none-available	missing-external		none-available	missing-external		6	no independent ASC oracle; licensed EDF oracle unavailable
      |""".stripMargin

class EyeLinkConformanceSuite extends munit.FunSuite:
  private val defaultFrame =
    Frame.screen("conformance-surface", 1000, 1000).fold(error => fail(error.message), identity)

  private def config(frame: Frame[Unit2D.Px]): EyeLinkAscSessionConfig =
    EyeLinkAscSessionConfig
      .of(
        frame,
        ClockId("conformance-tracker"),
        ConversionEvidencePolicy.Exploratory,
        AscBlinkReconciliationPolicy.ConfirmedNativeIntervals,
        AscUnspecifiedPupilPolicy.RejectMeasuredValues
      )
      .fold(error => fail(error.message), identity)

  private def ascii(value: String): IArray[Byte] =
    IArray.from(value.toVector.map(_.toByte))

  private def asciiString(value: IArray[Byte]): String =
    value.iterator.map(byte => (byte & 0xff).toChar).mkString

  private def materialize(
      id: String,
      input: IArray[Byte],
      frame: Frame[Unit2D.Px] = defaultFrame,
      chunks: Option[Vector[IArray[Byte]]] = None
  ): EyeLinkAscSessionMaterialization =
    val settings = AscStreamSettings
      .of(s"$id.asc", 16384, 7)
      .fold(
        error => fail(error.message),
        identity
      )
    val framing = chunks match
      case Some(values) => EyeLinkAscFraming.machine(settings).runAll(values)
      case None         => EyeLinkAscFraming.whole(settings, input)
    EyeLinkAscSessions.materialize(
      EyeLinkAscOrigin.Unidentified(Sha256.ofBytes(input)),
      config(frame),
      framing
    )

  private def fact(path: String, value: String): EyeLinkConformanceFact =
    EyeLinkConformanceFact
      .create(path, EyeLinkConformanceValue.value(value))
      .fold(error => fail(error.message), identity)

  private def manifest(
      fixture: String,
      artifact: String,
      values: (String, String)*
  ): EyeLinkConformanceManifest =
    val digest  = Sha256.ofUtf8(values.mkString("|"))
    val operand = EyeLinkConformanceOperand
      .create(fixture, artifact, digest)
      .fold(error => fail(error.message), identity)
    EyeLinkConformanceManifest
      .create(operand, values.toVector.map((fact _).tupled))
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

  private def semantic(
      fixture: String,
      artifact: String,
      result: EyeLinkAscSessionMaterialization
  ): EyeLinkConformanceManifest =
    EyeLinkConformanceManifest
      .fromCanonical(
        fixture,
        artifact,
        EyeLinkAscCanonicalManifest.from(result),
        semanticPath
      )
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

  private def semanticPath(path: String): Boolean =
    val sourceLocation =
      path == "source.origin" || path.startsWith("source.") || path.startsWith("line.") ||
        path.endsWith(".source-line") || path.endsWith(".declared-line") ||
        path.startsWith("diagnostic.") || path.contains(".diagnostic.") ||
        path.contains(".lexical.") || path.contains(".requirement.") ||
        path.contains(".outcome.")
    !sourceLocation

  private def chunk(input: IArray[Byte], sizes: Vector[Int]): Vector[IArray[Byte]] =
    val bytes  = input.toVector
    val output = Vector.newBuilder[IArray[Byte]]
    var offset = 0
    var index  = 0
    while offset < bytes.length do
      val size = sizes(index % sizes.length)
      output += IArray.from(bytes.slice(offset, math.min(offset + size, bytes.length)))
      offset += size
      index += 1
    output.result()

  private val portableFixtures =
    EyeLinkAscParityFixtures.all.filter(_.id.startsWith("synthetic-"))

  test("exact comparison names both operands and the differing field") {
    val left   = manifest("fixture-one", "eyes4s", "sample.0.x" -> "10")
    val right  = manifest("fixture-one", "oracle", "sample.0.x" -> "11")
    val errors = EyeLinkConformance
      .compare(left, right)
      .fold(_.toVector, _ => fail("expected exact mismatch"))

    assertEquals(errors.length, 1)
    assert(errors.head.message.contains("fixture='fixture-one'"))
    assert(errors.head.message.contains("left='eyes4s"))
    assert(errors.head.message.contains("right='oracle"))
    assert(errors.head.message.contains("field='sample.0.x'"))
    assert(errors.head.message.contains("tolerance='exact'"))
  }

  test("a named decimal tolerance is explicit and rejects non-decimal values") {
    val tolerance = EyeLinkFieldTolerance
      .decimal(
        "tracker-quantisation",
        BigDecimal("0.001"),
        BigDecimal(0),
        "tracker-ms",
        "independent reader prints three decimals"
      )
      .fold(error => fail(error.message), identity)
    val left  = manifest("fixture-one", "eyes4s", "sample.0.time" -> "1.0004")
    val right = manifest("fixture-one", "oracle", "sample.0.time" -> "1")
    assert(EyeLinkConformance.compare(left, right, Map("sample.0.time" -> tolerance)).isRight)

    val textLeft  = manifest("fixture-one", "eyes4s", "sample.0.eye" -> "L")
    val textRight = manifest("fixture-one", "oracle", "sample.0.eye" -> "L")
    val mismatch  = EyeLinkConformance
      .compare(textLeft, textRight, Map("sample.0.eye" -> tolerance))
      .fold(_.head, _ => fail("numeric tolerance on text must fail"))
    assert(mismatch.message.contains("non-decimal"))
  }

  test("missing, omitted, and absent fields cannot masquerade as equality") {
    val missing = EyeLinkConformanceValue
      .missing("native dot")
      .fold(error => fail(error.message), identity)
    val omitted = EyeLinkConformanceValue
      .omitted("oracle does not expose field")
      .fold(error => fail(error.message), identity)
    val operandA = EyeLinkConformanceOperand
      .create("fixture-one", "eyes4s", Sha256.ofUtf8("a"))
      .fold(error => fail(error.message), identity)
    val operandB = EyeLinkConformanceOperand
      .create("fixture-one", "oracle", Sha256.ofUtf8("b"))
      .fold(error => fail(error.message), identity)
    val leftFact = EyeLinkConformanceFact
      .create("sample.0.pupil", missing)
      .fold(error => fail(error.message), identity)
    val rightFact = EyeLinkConformanceFact
      .create("sample.0.pupil", omitted)
      .fold(error => fail(error.message), identity)
    val left = EyeLinkConformanceManifest
      .create(operandA, Vector(leftFact))
      .fold(errors => fail(errors.head.message), identity)
    val right = EyeLinkConformanceManifest
      .create(operandB, Vector(rightFact))
      .fold(errors => fail(errors.head.message), identity)

    assert(EyeLinkConformance.compare(left, right).isLeft)
    assert(
      EyeLinkConformance
        .compare(left, manifest("fixture-one", "other", "sample.0.x" -> "1"))
        .isLeft
    )
    val emptyOperand = EyeLinkConformanceOperand
      .create("fixture-one", "empty", Sha256.ofUtf8("empty"))
      .fold(error => fail(error.message), identity)
    assert(EyeLinkConformanceManifest.create(emptyOperand, Vector.empty).isLeft)
  }

  portableFixtures.foreach { fixture =>
    test(s"${fixture.id}: emits a nonempty portable semantic manifest") {
      val projected = semantic(fixture.id, "eyes4s", materialize(fixture.id, fixture.input))
      assert(projected.facts.nonEmpty)
      assertEquals(projected.operand.fixtureId, fixture.id)
      assert(projected.byPath.keys.exists(_.startsWith("report.")))
    }
  }

  test("newline representation leaves the scientific projection unchanged") {
    val fixture = EyeLinkAscParityFixtures.syntheticLeftHeadfixed1000
    val lf      = asciiString(fixture.input)
    val crlf    = ascii(lf.replace("\n", "\r\n"))
    val left    = semantic(fixture.id, "lf", materialize(fixture.id, fixture.input))
    val right   = semantic(fixture.id, "crlf", materialize(fixture.id, crlf))
    assert(EyeLinkConformance.compare(left, right).isRight)
  }

  test("legal structural whitespace leaves the scientific projection unchanged") {
    val fixture = EyeLinkAscParityFixtures.syntheticLeftHeadfixed1000
    val tabs    = asciiString(fixture.input)
    val spaces  = ascii(tabs.replace("\t", "   "))
    val left    = semantic(fixture.id, "tabs", materialize(fixture.id, fixture.input))
    val right   = semantic(fixture.id, "spaces", materialize(fixture.id, spaces))
    assert(EyeLinkConformance.compare(left, right).isRight)
  }

  test("arbitrary byte repartitioning preserves the complete canonical result") {
    val fixture = EyeLinkAscParityFixtures.syntheticEventsMessages
    val whole   = EyeLinkAscCanonicalManifest.from(materialize(fixture.id, fixture.input))
    val pieces  = EyeLinkAscCanonicalManifest.from(
      materialize(
        fixture.id,
        fixture.input,
        chunks = Some(chunk(fixture.input, Vector(1, 2, 7, 3, 31)))
      )
    )
    assertEquals(pieces.canonical, whole.canonical)
  }

  test("sample and event conversion subsets preserve the records they declare") {
    val combined = ascii(
      "START 100 LEFT SAMPLES EVENTS\n" +
        "PUPIL AREA\n" +
        "SAMPLES GAZE LEFT RATE 1000\n" +
        "EVENTS GAZE LEFT RATE 1000\n" +
        "100 10 20 30\n" +
        "EFIX L 100 101 1 10 20 30\n" +
        "END 102\n"
    )
    val samples = ascii(
      "START 100 LEFT SAMPLES\nPUPIL AREA\nSAMPLES GAZE LEFT RATE 1000\n" +
        "100 10 20 30\nEND 102\n"
    )
    val events = ascii(
      "START 100 LEFT EVENTS\nEVENTS GAZE LEFT RATE 1000\n" +
        "EFIX L 100 101 1 10 20 30\nEND 102\n"
    )
    val combinedResult = materialize("conversion-combined", combined)
    val sampleResult   = materialize("conversion-samples", samples)
    val eventResult    = materialize("conversion-events", events)

    assertEquals(
      combinedResult.raw.sampleRows.flatMap(_.sample).map(_.timestamp),
      sampleResult.raw.sampleRows.flatMap(_.sample).map(_.timestamp)
    )
    assertEquals(
      combinedResult.raw.nativeRows.flatMap(_.record).collect {
        case AscParsedNativeRecord.Event(value) => value.onset
      },
      eventResult.raw.nativeRows.flatMap(_.record).collect {
        case AscParsedNativeRecord.Event(value) => value.onset
      }
    )
  }

  test("translating GAZE samples with their frame preserves frame-relative positions") {
    val baseBounds = Bounds
      .of[Unit2D.Px](0, 0, 1000, 1000)
      .fold(error => fail(error.message), identity)
    val movedBounds = Bounds
      .of[Unit2D.Px](10, 20, 1010, 1020)
      .fold(error => fail(error.message), identity)
    val baseFrame  = Frame.of(FrameId("base"), baseBounds, YAxis.Down)
    val movedFrame = Frame.of(FrameId("moved"), movedBounds, YAxis.Down)
    val base       = ascii(
      "START 0 LEFT SAMPLES\nPUPIL AREA\nSAMPLES GAZE LEFT RATE 1000\n" +
        "0 100 200 30\n1 101 201 31\nEND 2\n"
    )
    val moved = ascii(
      "START 0 LEFT SAMPLES\nPUPIL AREA\nSAMPLES GAZE LEFT RATE 1000\n" +
        "0 110 220 30\n1 111 221 31\nEND 2\n"
    )

    def relative(
        value: EyeLinkAscSessionMaterialization,
        bounds: Bounds[Unit2D.Px]
    ): Vector[(Double, Double)] =
      val session = value.trusted.fold(errors => fail(errors.head.message), identity)
      session.recordingBlocks.flatMap { block =>
        block.recording match
          case EyeLinkAscRecordingArtifact.Monocular(recording) =>
            recording.samples.toVector.flatMap(_.gaze match
              case Gaze.Tracked(point, _) =>
                Some(point.x - bounds.xMin -> (point.y - bounds.yMin))
              case _ => None)
          case EyeLinkAscRecordingArtifact.Binocular(_) => fail("expected monocular fixture")
      }

    assertEquals(
      relative(materialize("base", base, baseFrame), baseBounds),
      Vector(100.0 -> 200.0, 101.0 -> 201.0)
    )
    assertEquals(
      relative(materialize("moved", moved, movedFrame), movedBounds),
      Vector(100.0 -> 200.0, 101.0 -> 201.0)
    )
  }

  test("concatenating complete blocks preserves block-local samples and native events") {
    val first =
      "START 0 LEFT SAMPLES\nPUPIL AREA\nSAMPLES GAZE LEFT RATE 1000\n0 10 20 30\nEND 1\n"
    val second =
      "START 10 LEFT EVENTS\nEVENTS GAZE LEFT RATE 1000\nEFIX L 10 11 1 40 50 60\nEND 12\n"
    val each = Vector(materialize("first", ascii(first)), materialize("second", ascii(second)))
    val joined = materialize("joined", ascii(first + second))

    val separateSamples = each.flatMap(_.raw.sampleRows.flatMap(_.sample).map(_.timestamp))
    val joinedSamples   = joined.raw.sampleRows.flatMap(_.sample).map(_.timestamp)
    val separateEvents  = each.flatMap(_.raw.nativeRows.flatMap(_.record).collect {
      case AscParsedNativeRecord.Event(value) => value.onset
    })
    val joinedEvents = joined.raw.nativeRows.flatMap(_.record).collect {
      case AscParsedNativeRecord.Event(value) => value.onset
    }
    assertEquals(joinedSamples, separateSamples)
    assertEquals(joinedEvents, separateEvents)
    assertEquals(joined.report.recordingBlocks, 2)
  }

  test("unsupported coordinate transformations are typed and cannot pass") {
    val result = EyeLinkMetamorphicSupport.assess(
      EyeLinkMetamorphicTransformation.GazeTranslation,
      "raw-fixture",
      AscCoordinateMode.RawPupil
    )
    result match
      case unsupported: EyeLinkMetamorphicSupport.Unsupported =>
        assert(unsupported.message.contains("fixture='raw-fixture'"))
        assert(unsupported.message.contains("coordinate-mode='RawPupil'"))
      case EyeLinkMetamorphicSupport.Supported(_) => fail("RAW translation must not pass")
  }

  test("portable summary cannot imply licensed EDF certification") {
    val oracleDigest = Sha256
      .fromHex(
        "eyelinker-oracle",
        "7c8852f0d8d2a0b4a1dabea4eb6b16d36fb81780b9285fc3ba974281198d3d04"
      )
      .fold(error => fail(error.message), identity)
    val rows = portableFixtures.map { fixture =>
      val projected = semantic(fixture.id, "eyes4s", materialize(fixture.id, fixture.input))
      val hasOracle = fixture.id == "synthetic-events-messages"
      EyeLinkPortableConformanceRow
        .create(
          fixture.id,
          projected.scientificDigest,
          if hasOracle then "CRAN-eyelinker-0.2.2" else "none-available",
          if hasOracle then EyeLinkConformanceStatus.ComparedPass
          else EyeLinkConformanceStatus.MissingExternal,
          Option.when(hasOracle)(oracleDigest),
          "none-available",
          EyeLinkConformanceStatus.MissingExternal,
          None,
          6,
          if hasOracle then "ASC oracle compared; licensed EDF oracle unavailable"
          else "no independent ASC oracle; licensed EDF oracle unavailable"
        )
        .fold(error => fail(error.message), identity)
    }
    val summary = EyeLinkPortableConformanceSummary
      .create(rows)
      .fold(errors => fail(errors.head.message), identity)

    rows.foreach { row =>
      assertEquals(
        row.eyes4sDigest.hex,
        EyeLinkPortableConformanceExpected.semanticDigests.getOrElse(
          row.fixtureId,
          fail(s"missing expected semantic digest fixture=${row.fixtureId}")
        )
      )
    }
    assert(summary.availableComparisonsPassed)
    assert(!summary.ascOracleCoverageComplete)
    assert(!summary.vendorEdfCertified)
    assert(summary.renderTsv.contains("missing-external"))
    assertEquals(summary.renderTsv, EyeLinkPortableConformanceExpected.renderTsv)
  }

  test("a failed EDF comparison makes the available comparison summary fail") {
    val row = EyeLinkPortableConformanceRow
      .create(
        "edf-failure",
        Sha256.ofUtf8("eyes4s"),
        "asc-not-run",
        EyeLinkConformanceStatus.MissingExternal,
        None,
        "edf-api",
        EyeLinkConformanceStatus.ComparedFail,
        Some(Sha256.ofUtf8("edf-api-manifest")),
        0,
        "deliberate regression fixture"
      )
      .fold(error => fail(error.message), identity)
    val summary = EyeLinkPortableConformanceSummary
      .create(Vector(row))
      .fold(errors => fail(errors.head.message), identity)

    assert(!summary.availableComparisonsPassed)
    assert(!summary.vendorEdfCertified)
  }

end EyeLinkConformanceSuite
