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

class ScientificValidationArtifactSuite extends munit.FunSuite:

  private val sourceRevision = "894fe38bad944bd94680de444f873f2b0d020d72"
  private val detectorOracle =
    """{"$schema":"eyes4s.detector-conformance.v1","fixture":"shared-test"}"""
  private val engbertOracle =
    """{"schema":"eyes4s.engbert-kernels.v1","fixture":"shared-test"}"""

  private val build = ValidationBuildIdentity
    .workingTree("0.1.0-SNAPSHOT", sourceRevision)
    .fold(error => fail(error.message), identity)

  private val oracles = ValidationOracleInputs
    .fromCanonicalFiles(detectorOracle, engbertOracle)
    .fold(error => fail(error.message), identity)

  private def artifact(platform: ValidationPlatform): ScientificValidationArtifact =
    ScientificValidationArtifact
      .generate(build, platform, oracles)
      .fold(error => fail(error.message), identity)

  test("SHA-256 uses canonical UTF-8 and agrees with published vectors") {
    assertEquals(
      ArtifactDigest.sha256Utf8("").render,
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
    assertEquals(
      ArtifactDigest.sha256Utf8("abc").render,
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
  }

  test("successful artifacts expose every evidence boundary without parsing test output") {
    val report = artifact(ValidationPlatform.Jvm)

    assertEquals(report.schemaVersion, ScientificValidationArtifact.CurrentSchemaVersion)
    assertEquals(report.algorithmCards.length, 3)
    assertEquals(report.configurations.length, 3)
    assertEquals(report.fixtures.length, 4)
    assertEquals(report.metamorphicLaws.length, 5)
    assertEquals(report.inputDigests.length, 4)
    assertEquals(report.warnings.length, 3)
    assert(report.syntheticMetrics.perClass.nonEmpty)
    assert(report.inputDigests.forall(_.digest.render.length == 64))
    assertEquals(
      report.inputDigests.take(2).map(_.name),
      Vector(
        "tools/detector-conformance/reference.json",
        "tools/engbert-kernels/reference.json"
      )
    )
  }

  test("JVM and Scala.js artifacts share one scientific summary digest") {
    val jvm = artifact(ValidationPlatform.Jvm)
    val js  = artifact(ValidationPlatform.ScalaJs)

    assertEquals(jvm.scientificDigest, js.scientificDigest)
    assertEquals(
      jvm.scientificDigest.render,
      "e698f655280b4101de863673262cbb152b361f6344e64c2052e792e55ade1ebf"
    )
    assertNotEquals(jvm.canonicalJson, js.canonicalJson)
    assert(jvm.canonicalJson.contains("\"platform\":\"jvm\""))
    assert(js.canonicalJson.contains("\"platform\":\"scala-js\""))
  }

  test("canonical JSON is deterministic and contains no non-JSON numeric sentinels") {
    val first  = artifact(ValidationPlatform.Jvm).canonicalJson
    val second = artifact(ValidationPlatform.Jvm).canonicalJson

    assertEquals(first, second)
    assert(first.startsWith("{\"schema_version\":"))
    assert(!first.contains("NaN"))
    assert(!first.contains("Infinity"))
  }

  test("build identity distinguishes release source from an honest working tree") {
    val release = ValidationBuildIdentity
      .release("0.1.0", sourceRevision)
      .fold(error => fail(error.message), identity)
    val working = ValidationBuildIdentity
      .workingTree("0.1.0-SNAPSHOT", sourceRevision)
      .fold(error => fail(error.message), identity)

    assertEquals(release.sourceState, ValidationSourceState.Release)
    assertEquals(release.dirty, false)
    assertEquals(working.sourceState, ValidationSourceState.WorkingTree)
    assertEquals(working.dirty, true)
  }

  test("build identity rejects empty or non-SHA source operands") {
    assertEquals(
      ValidationBuildIdentity.release("", sourceRevision),
      Left(ValidationArtifactError.EmptyBuildField("libraryVersion", ""))
    )
    assertEquals(
      ValidationBuildIdentity.release("0.1.0", "revision"),
      Left(ValidationArtifactError.InvalidSourceRevision("sourceRevision", "revision"))
    )
    assertEquals(
      ValidationBuildIdentity.release("vérsion", sourceRevision),
      Left(
        ValidationArtifactError.InvalidBuildField(
          "libraryVersion",
          "vérsion",
          "ASCII letters, digits, '.', '+', or '-' without surrounding whitespace"
        )
      )
    )
  }

  test("oracle inputs reject empty, wrong, and swapped scientific documents") {
    assertEquals(
      ValidationOracleInputs.fromCanonicalFiles("", engbertOracle),
      Left(
        ValidationArtifactError.EmptyOracleContent(
          "tools/detector-conformance/reference.json",
          0
        )
      )
    )
    assertEquals(
      ValidationOracleInputs.fromCanonicalFiles(detectorOracle, detectorOracle),
      Left(
        ValidationArtifactError.MissingOracleSchema(
          "tools/engbert-kernels/reference.json",
          "eyes4s.engbert-kernels.v1"
        )
      )
    )
  }

  test("changing one oracle document changes its input and scientific digests") {
    val changed = ValidationOracleInputs
      .fromCanonicalFiles(detectorOracle + " ", engbertOracle)
      .fold(error => fail(error.message), identity)
    val baseline = artifact(ValidationPlatform.Jvm)
    val mutation = ScientificValidationArtifact
      .generate(build, ValidationPlatform.Jvm, changed)
      .fold(error => fail(error.message), identity)

    assertNotEquals(
      baseline.inputDigests.head.digest,
      mutation.inputDigests.head.digest
    )
    assertNotEquals(baseline.scientificDigest, mutation.scientificDigest)
  }

end ScientificValidationArtifactSuite
