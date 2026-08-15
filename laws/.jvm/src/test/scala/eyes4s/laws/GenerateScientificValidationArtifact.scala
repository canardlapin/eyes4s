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
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import scala.util.control.NonFatal

/** Repository-only writer for the checked scientific-validation artifact.
  *
  * The writer deliberately lives in the JVM test source set: it is release
  * tooling, not published library API. It refuses to mint a release artifact
  * unless the requested source revision is the current clean `HEAD`, and it
  * verifies both oracle files against that commit before writing atomically.
  */
private[laws] object GenerateScientificValidationArtifact:

  private val detectorRelative = "tools/detector-conformance/reference.json"
  private val engbertRelative  = "tools/engbert-kernels/reference.json"
  private val futureDriftSuite =
    "laws/.jvm/src/test/scala/eyes4s/laws/ScientificValidationFilesSuite.scala"

  def main(arguments: Array[String]): Unit =
    generate(arguments.toVector, Paths.get(sys.props("user.dir"))) match
      case Left(message) =>
        Console.err.println(s"scientific validation artifact not written: $message")
        System.exit(2)
      case Right(result) =>
        println(
          s"wrote ${result.output} for ${result.sourceRevision} " +
            s"with scientific digest ${result.scientificDigest}"
        )

  private[laws] final case class GenerationResult(
      output: Path,
      sourceRevision: String,
      scientificDigest: String
  )

  private[laws] def generate(
      arguments: Vector[String],
      workingDirectory: Path
  ): Either[String, GenerationResult] =
    arguments match
      case Vector(libraryVersion, sourceRevision, outputArgument) =>
        for
          root             <- repositoryRoot(workingDirectory)
          _                <- requireCleanSource(root, sourceRevision)
          detectorContents <- committedOracle(root, sourceRevision, detectorRelative)
          engbertContents  <- committedOracle(root, sourceRevision, engbertRelative)
          build            <- ValidationBuildIdentity
            .release(libraryVersion, sourceRevision)
            .left
            .map(_.message)
          oracles <- ValidationOracleInputs
            .fromCanonicalFiles(detectorContents, engbertContents)
            .left
            .map(_.message)
          artifact <- ScientificValidationArtifact
            .generate(build, ValidationPlatform.Jvm, oracles)
            .left
            .map(_.message)
          output <- resolveOutput(root, outputArgument)
          _      <- writeAtomically(output, artifact.canonicalJson + "\n")
        yield GenerationResult(output, sourceRevision, artifact.scientificDigest.render)
      case _ =>
        Left(
          "expected arguments: <library-version> <40-character-source-revision> <output-path>"
        )

  private def repositoryRoot(start: Path): Either[String, Path] =
    Iterator
      .iterate(start.toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .find(path => Files.exists(path.resolve(".git")))
      .toRight(s"no Git repository found from ${start.toAbsolutePath.normalize}")

  private def requireCleanSource(root: Path, sourceRevision: String): Either[String, Unit] =
    for
      headBytes <- git(root, "rev-parse", "HEAD")
      head = new String(headBytes, StandardCharsets.UTF_8).trim
      _ <- Either.cond(
        head == sourceRevision,
        (),
        s"requested source revision $sourceRevision is not current HEAD $head"
      )
      _ <- gitQuiet(root, "diff", "--quiet", "--").left
        .map(message =>
          s"tracked worktree differs from source revision $sourceRevision: $message"
        )
      _ <- gitQuiet(root, "diff", "--cached", "--quiet", "HEAD", "--").left
        .map(message => s"index differs from source revision $sourceRevision: $message")
      untrackedBytes <- git(root, "ls-files", "--others", "--exclude-standard", "-z")
      untrackedSources = new String(untrackedBytes, StandardCharsets.UTF_8)
        .split('\u0000')
        .toVector
        .filter(path => sourceExtension(path) && path != futureDriftSuite)
      _ <- Either.cond(
        untrackedSources.isEmpty,
        (),
        s"untracked source could change the generated evidence: ${untrackedSources.mkString(", ")}"
      )
    yield ()

  private def committedOracle(
      root: Path,
      sourceRevision: String,
      relative: String
  ): Either[String, String] =
    val currentPath = root.resolve(relative)
    for
      current   <- readBytes(currentPath)
      committed <- git(root, "show", s"$sourceRevision:$relative")
      _         <- Either.cond(
        java.util.Arrays.equals(current, committed),
        (),
        s"oracle $relative does not equal its bytes in source revision $sourceRevision"
      )
    yield new String(committed, StandardCharsets.UTF_8)

  private def sourceExtension(path: String): Boolean =
    path.endsWith(".scala") || path.endsWith(".java") || path.endsWith(".sbt") ||
      path.endsWith(".sc")

  private def resolveOutput(root: Path, argument: String): Either[String, Path] =
    val candidate = Paths.get(argument)
    val output    =
      if candidate.isAbsolute then candidate.normalize
      else root.resolve(candidate).normalize
    Either.cond(
      output.getFileName != null,
      output,
      s"output path has no file name: $output"
    )

  private def readBytes(path: Path): Either[String, Array[Byte]] =
    try Right(Files.readAllBytes(path))
    catch case NonFatal(error) => Left(s"cannot read $path: ${error.getMessage}")

  private def writeAtomically(path: Path, contents: String): Either[String, Unit] =
    val parent = Option(path.getParent).getOrElse(Paths.get(".").toAbsolutePath.normalize)
    var temporary: Option[Path] = None
    try
      Files.createDirectories(parent)
      val temp = Files.createTempFile(parent, ".eyes4s-validation-", ".tmp")
      temporary = Some(temp)
      Files.writeString(temp, contents, StandardCharsets.UTF_8)
      try
        Files.move(
          temp,
          path,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      catch
        case _: AtomicMoveNotSupportedException =>
          Files.move(temp, path, StandardCopyOption.REPLACE_EXISTING)
      temporary = None
      Right(())
    catch case NonFatal(error) => Left(s"cannot write $path: ${error.getMessage}")
    finally temporary.foreach(path => Files.deleteIfExists(path))

  private def git(root: Path, arguments: String*): Either[String, Array[Byte]] =
    runGit(root, arguments.toVector).flatMap { case (exitCode, output) =>
      Either.cond(
        exitCode == 0,
        output,
        s"git ${arguments.mkString(" ")} failed ($exitCode): " +
          new String(output, StandardCharsets.UTF_8).trim
      )
    }

  private def gitQuiet(root: Path, arguments: String*): Either[String, Unit] =
    runGit(root, arguments.toVector).flatMap { case (exitCode, output) =>
      Either.cond(
        exitCode == 0,
        (),
        Option(new String(output, StandardCharsets.UTF_8).trim)
          .filter(_.nonEmpty)
          .getOrElse(s"git ${arguments.mkString(" ")} exited $exitCode")
      )
    }

  private def runGit(
      root: Path,
      arguments: Vector[String]
  ): Either[String, (Int, Array[Byte])] =
    try
      val process = new ProcessBuilder(("git" +: arguments)*).directory(root.toFile).start()
      val standardOutput = process.getInputStream.readAllBytes()
      val standardError  = process.getErrorStream.readAllBytes()
      val exitCode       = process.waitFor()
      val output         =
        if standardError.isEmpty then standardOutput
        else standardOutput ++ standardError
      Right((exitCode, output))
    catch case NonFatal(error) => Left(s"cannot run git in $root: ${error.getMessage}")

end GenerateScientificValidationArtifact
