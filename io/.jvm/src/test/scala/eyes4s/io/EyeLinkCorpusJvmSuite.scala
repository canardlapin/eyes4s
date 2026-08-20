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

import java.nio.charset.StandardCharsets

class EyeLinkCorpusJvmSuite extends munit.FunSuite:

  private val ResourceRoot = "/eyes4s/io/eyelink/corpus/"

  private def resourceBytes(name: String): Array[Byte] =
    val stream = Option(getClass.getResourceAsStream(ResourceRoot + name))
      .getOrElse(fail(s"missing corpus resource=$name"))
    try stream.readAllBytes()
    finally stream.close()

  private lazy val manifestText: String =
    new String(resourceBytes("manifest.tsv"), StandardCharsets.UTF_8)

  private lazy val manifest: EyeLinkCorpusManifest =
    EyeLinkCorpusManifest
      .parseTsv("classpath:manifest.tsv", manifestText)
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

  private def lexicalLines(fixture: EyeLinkCorpusFixture): Vector[AscLineResult] =
    val bytes = resourceBytes(
      fixture.localPath.getOrElse(fail(s"fixture=${fixture.id} has no path"))
    )
    val text   = new String(bytes, StandardCharsets.US_ASCII)
    val values = text.stripSuffix("\n").split("\n", -1).toVector
    var offset = 0L
    values.zipWithIndex.map { case (value, index) =>
      val lineBytes = IArray.from(value.getBytes(StandardCharsets.US_ASCII))
      val line      = AscSourceLine
        .of(
          fixture.id,
          index + 1L,
          offset,
          lineBytes,
          terminator = AscLineTerminator.LineFeed
        )
        .fold(error => fail(error.message), identity)
      offset += lineBytes.length.toLong + 1L
      EyeLinkAscLexer.parse(line)
    }

  test("checked-in manifest is canonical and every included digest matches exact bytes") {
    assertEquals(manifest.renderTsv, manifestText)
    assertEquals(manifest.corpusVersion, "2026.08.15.3")
    assertEquals(manifest.supportContractVersion, EyeLinkSupport.contractVersion)
    assertEquals(manifest.publiclyLoadable.length, 6)

    manifest.publiclyLoadable.foreach { fixture =>
      val bytes  = resourceBytes(fixture.localPath.getOrElse(fail("missing public path")))
      val actual = Sha256.ofBytes(IArray.from(bytes))
      assertEquals(actual, fixture.ascDigest.getOrElse(fail("missing ASC digest")), fixture.id)
      val firstLine = new String(bytes, StandardCharsets.US_ASCII).lines().findFirst()
      assert(firstLine.isPresent, fixture.id)
      assert(firstLine.get().contains("no human"), fixture.id)
    }
  }

  test(
    "public court includes only generated data and retains real acquisition slots as plans"
  ) {
    assert(!manifest.containsPrivateBytes)
    assert(manifest.publiclyLoadable.forall(_.kind == EyeLinkCorpusKind.Synthetic))
    assert(manifest.publiclyLoadable.forall(_.privacy == EyeLinkCorpusPrivacy.NoHumanData))
    assert(manifest.publiclyLoadable.forall(_.conversionReceipt.isEmpty))

    val planned = manifest.fixtures.filter(
      _.availability == EyeLinkCorpusAvailability.Planned
    )
    assertEquals(planned.length, 4)
    assert(planned.exists(_.kind == EyeLinkCorpusKind.RedistributableReal))
    assertEquals(planned.count(_.kind == EyeLinkCorpusKind.PrivateRealPair), 3)
    assert(planned.forall(_.localPath.isEmpty))
    assert(planned.forall(_.ascDigest.isEmpty))
    assert(planned.forall(_.conversionReceipt.isEmpty))
  }

  test(
    "every required capability and dimension pair is represented but explicitly unvalidated"
  ) {
    assertEquals(manifest.coverage.missingRequiredCapabilities, Vector.empty)
    assertEquals(manifest.coverage.unrepresentedDimensionPairs, Vector.empty)
    assertEquals(
      manifest.coverage.unvalidatedRequiredCapabilities.map(_.id).sorted,
      EyeLinkSupport.required.map(_.id).sorted
    )
    assertEquals(
      manifest.coverage.unvalidatedDimensionPairs.length,
      manifest.coverage.dimensionPairs.length
    )
    assert(
      manifest.coverage.capabilities.forall(
        _.status != EyeLinkCorpusCoverageStatus.RealEvidenceAvailable
      )
    )
  }

  test("synthetic bytes are line-accounted and exercise the declared sample layouts") {
    val expectedSamples = Map(
      "synthetic-binocular-remote-2000" -> 2,
      "synthetic-events-messages"       -> 1,
      "synthetic-failsafe-corrupt"      -> 2,
      "synthetic-left-headfixed-1000"   -> 2,
      "synthetic-multiblock"            -> 6,
      "synthetic-right-headfixed-500"   -> 2
    )

    manifest.publiclyLoadable.foreach { fixture =>
      val lexical    = lexicalLines(fixture)
      val blocks     = EyeLinkAscBlocks.machine.runAll(lexical)
      val blockLines = blocks.collect { case AscBlockEmission.Line(value) => value }
      val samples    = EyeLinkAscSamples.machine
        .runAll(blocks)
        .collect { case AscSampleEmission.Parsed(value) if value.sample.nonEmpty => value }

      assertEquals(blockLines.length, lexical.length, fixture.id)
      assertEquals(samples.length, expectedSamples(fixture.id), fixture.id)
      assertEquals(
        blockLines.map(_.line.source.number),
        (1L to lexical.length.toLong).toVector,
        fixture.id
      )
    }
  }

end EyeLinkCorpusJvmSuite
