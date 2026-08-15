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

class GenerateScientificValidationArtifactSuite extends munit.FunSuite:

  test("release writer binds canonical output to a clean current commit") {
    val fixture = repositoryFixture()
    val output  = fixture.root.resolve("validation/report.json")

    val result = GenerateScientificValidationArtifact
      .generate(Vector("0.1.0", fixture.revision, output.toString), fixture.root)
      .fold(message => fail(message), identity)
    val document = Files.readString(output, StandardCharsets.UTF_8)

    assertEquals(result.sourceRevision, fixture.revision)
    assertEquals(result.output, output)
    assert(document.endsWith("\n"))
    assert(document.contains(s"\"source_revision\":\"${fixture.revision}\""))
    assert(document.contains("\"source_state\":\"release\""))
    assert(document.contains("\"dirty\":false"))
    assert(document.contains(s"\"scientific_digest_sha256\":\"${result.scientificDigest}\""))
  }

  test("release writer rejects a source revision other than current HEAD") {
    val fixture = repositoryFixture()
    val other   = "0" * 40

    val result = GenerateScientificValidationArtifact.generate(
      Vector("0.1.0", other, fixture.root.resolve("report.json").toString),
      fixture.root
    )

    assert(result.left.exists(_.contains("is not current HEAD")))
    assert(!Files.exists(fixture.root.resolve("report.json")))
  }

  test("release writer rejects tracked scientific inputs changed after the source commit") {
    val fixture = repositoryFixture()
    Files.writeString(
      fixture.root.resolve("tools/engbert-kernels/reference.json"),
      "{}\n",
      StandardCharsets.UTF_8
    )

    val result = GenerateScientificValidationArtifact.generate(
      Vector("0.1.0", fixture.revision, fixture.root.resolve("report.json").toString),
      fixture.root
    )

    assert(result.left.exists(_.contains("tracked worktree differs")))
    assert(!Files.exists(fixture.root.resolve("report.json")))
  }

  test("release writer rejects untracked source that could change generated evidence") {
    val fixture = repositoryFixture()
    val source  = fixture.root.resolve("laws/src/main/scala/Untracked.scala")
    Files.createDirectories(source.getParent)
    Files.writeString(source, "object Untracked\n", StandardCharsets.UTF_8)

    val result = GenerateScientificValidationArtifact.generate(
      Vector("0.1.0", fixture.revision, fixture.root.resolve("report.json").toString),
      fixture.root
    )

    assert(result.left.exists(_.contains("untracked source could change")))
    assert(!Files.exists(fixture.root.resolve("report.json")))
  }

  test("release writer rejects missing operands before writing") {
    val fixture = repositoryFixture()
    val result  = GenerateScientificValidationArtifact.generate(Vector("0.1.0"), fixture.root)

    assert(result.left.exists(_.contains("expected arguments")))
  }

  private final case class RepositoryFixture(root: Path, revision: String)

  private def repositoryFixture(): RepositoryFixture =
    val sourceRoot = repositoryRoot()
    val root       = Files.createTempDirectory("eyes4s-validation-writer-")
    copyOracle(sourceRoot, root, "tools/detector-conformance/reference.json")
    copyOracle(sourceRoot, root, "tools/engbert-kernels/reference.json")
    git(root, "init", "-q")
    git(root, "config", "user.name", "eyes4s test")
    git(root, "config", "user.email", "eyes4s-test@example.invalid")
    git(root, "add", "tools")
    git(root, "commit", "-q", "-m", "fixture")
    RepositoryFixture(root, git(root, "rev-parse", "HEAD").trim)

  private def repositoryRoot(): Path =
    Iterator
      .iterate(Path.of(sys.props("user.dir")).toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .find(path => Files.isRegularFile(path.resolve("tools/engbert-kernels/reference.json")))
      .getOrElse(fail(s"repository root not found from ${sys.props("user.dir")}"))

  private def copyOracle(sourceRoot: Path, targetRoot: Path, relative: String): Unit =
    val target = targetRoot.resolve(relative)
    Files.createDirectories(target.getParent)
    val _ = Files.copy(sourceRoot.resolve(relative), target)

  private def git(root: Path, arguments: String*): String =
    val process = new ProcessBuilder(("git" +: arguments)*).directory(root.toFile).start()
    val output  = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val error   = new String(process.getErrorStream.readAllBytes(), StandardCharsets.UTF_8)
    assertEquals(process.waitFor(), 0, s"git ${arguments.mkString(" ")} failed: $error")
    output

end GenerateScientificValidationArtifactSuite
