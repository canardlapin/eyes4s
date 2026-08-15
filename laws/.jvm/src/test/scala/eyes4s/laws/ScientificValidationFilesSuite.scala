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

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest

class ScientificValidationFilesSuite extends munit.FunSuite:

  private val detectorRelative = Paths.get("tools/detector-conformance/reference.json")
  private val engbertRelative  = Paths.get("tools/engbert-kernels/reference.json")
  private val artifactRelative = Paths.get("validation/eyes4s-validation-0.1.0.json")

  private val root: Path =
    Iterator
      .iterate(Paths.get(sys.props("user.dir")).toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .find(path =>
        Files.isRegularFile(path.resolve(detectorRelative)) &&
          Files.isRegularFile(path.resolve(engbertRelative)) &&
          Files.isRegularFile(path.resolve(artifactRelative))
      )
      .getOrElse(fail(s"repository root not found from user.dir=${sys.props("user.dir")}"))

  private val detectorPath = root.resolve(detectorRelative)
  private val engbertPath  = root.resolve(engbertRelative)
  private val artifactPath = root.resolve(artifactRelative)

  private val detectorContents = Files.readString(detectorPath, StandardCharsets.UTF_8)
  private val engbertContents  = Files.readString(engbertPath, StandardCharsets.UTF_8)
  private val checkedArtifact  = Files.readString(artifactPath, StandardCharsets.UTF_8)

  private val oracles = ValidationOracleInputs
    .fromCanonicalFiles(detectorContents, engbertContents)
    .fold(error => fail(error.message), identity)

  private val canonicalDocument = checkedArtifact.stripSuffix("\n")
  private val buildPrefix       =
    raw"""^\{"schema_version":"([^"]+)","build":\{"library_version":"([0-9A-Za-z.+-]+)","source_revision":"([0-9a-f]{40})","source_state":"(release|working-tree)","dirty":(true|false)\},"platform":"jvm","scientific_digest_sha256":"([0-9a-f]{64})","scientific_evidence":""".r

  private val buildMatch = buildPrefix
    .findPrefixMatchOf(canonicalDocument)
    .getOrElse(
      fail(s"validation artifact does not have the canonical typed prefix: $artifactPath")
    )

  private val libraryVersion = buildMatch.group(2)
  private val sourceRevision = buildMatch.group(3)
  private val sourceState    = buildMatch.group(4)
  private val renderedDirty  = buildMatch.group(5).toBoolean
  private val renderedDigest = buildMatch.group(6)

  private val build =
    val parsed = sourceState match
      case "release"      => ValidationBuildIdentity.release(libraryVersion, sourceRevision)
      case "working-tree" =>
        ValidationBuildIdentity.workingTree(libraryVersion, sourceRevision)
      case other => fail(s"unrecognised validation source_state=$other")
    parsed.fold(error => fail(error.message), identity)

  private val generated = ScientificValidationArtifact
    .generate(build, ValidationPlatform.Jvm, oracles)
    .fold(error => fail(error.message), identity)

  test("typed oracle identities equal independent SHA-256 of the repository files") {
    assertEquals(
      oracles.detectorConformanceDigest.render,
      jdkSha256(Files.readAllBytes(detectorPath))
    )
    assertEquals(
      oracles.engbertKernelsDigest.render,
      jdkSha256(Files.readAllBytes(engbertPath))
    )
    assertEquals(
      oracles.detectorConformanceDigest.render,
      "7582beb4de3650a6631362c91849cdaae6bd394dc821d0ef5fd2b394290892b7"
    )
    assertEquals(
      oracles.engbertKernelsDigest.render,
      "923b6019cd2c1d956a4c8be701386c83a21bcfd96a926b2b61751034d2bf5271"
    )
  }

  test("checked validation JSON is the exact canonical generator output") {
    assertEquals(buildMatch.group(1), ScientificValidationArtifact.CurrentSchemaVersion)
    assertEquals(renderedDirty, build.dirty)
    assertEquals(renderedDigest, generated.scientificDigest.render)
    assertEquals(checkedArtifact, generated.canonicalJson + "\n")
  }

  test("a release artifact names an existing source commit rather than itself") {
    if build.sourceState == ValidationSourceState.Release then
      val process = new ProcessBuilder(
        "git",
        "cat-file",
        "-e",
        s"${build.sourceRevision}^{commit}"
      ).directory(root.toFile).start()
      assertEquals(
        process.waitFor(),
        0,
        s"missing release source revision ${build.sourceRevision}"
      )
    else assertEquals(build.sourceState, ValidationSourceState.WorkingTree)
  }

  private def jdkSha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

end ScientificValidationFilesSuite
