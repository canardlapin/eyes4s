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

package eyes4s.studio.desktop.report

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.core.rolling.RollingFileAppender
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The studio log's file and its bounds (ticket S1.12): it rolls at a size,
  * keeps a capped total, and when it cannot be written says why without a
  * path and keeps warnings on standard error. No JavaFX.
  */
class StudioLogSuite extends munit.FunSuite:

  private def context: LoggerContext = LoggerFactory.getILoggerFactory match
    case c: LoggerContext => c
    case other            => fail(s"SLF4J is bound to ${other.getClass.getName}")

  private def appenders: Vector[String] =
    context
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .iteratorForAppenders
      .asScala
      .map(_.getName)
      .toVector

  override def afterEach(context: AfterEach): Unit =
    this.context.reset()

  private def files(dir: Path): Vector[Path] =
    Files.list(dir).iterator.asScala.filter(Files.isRegularFile(_)).toVector

  private def await(what: String)(ok: => Boolean): Unit =
    val deadline = System.nanoTime + 10_000_000_000L
    while !ok && System.nanoTime < deadline do Thread.sleep(20)
    assert(ok, s"timed out waiting for $what")

  test("the default log rolls daily and at 5 MB, keeping 14 days and 50 MB") {
    assertEquals(StudioLog.Limits.default, StudioLog.Limits("5MB", 14, "50MB"))
    val dir = Files.createTempDirectory("eyes-studio-logs")
    StudioLog.open(dir).fold(e => fail(e.message), identity)
    val appender = context
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .getAppender("studio-file")
    assert(appender.isInstanceOf[RollingFileAppender[?]], s"$appender")
  }

  test("the log rolls at its size and keeps its total under the cap") {
    val dir = Files.createTempDirectory("eyes-studio-logs")
    val log = StudioLog
      .open(dir, StudioLog.Limits("2KB", 14, "8KB"))
      .fold(e => fail(e.message), identity)
    val logger = log.logger("eyes4s.studio")
    val line   = "x" * 100
    (1 to 600).foreach(i => logger.error(s"$i $line"))
    // About 60 KB were written: the live file rolled at 2 KB, and the rolled
    // files are removed in the background until the total is within the cap
    // (Logback's remover may remove every rolled file, so none is required).
    assert(Files.size(log.file) < 4 * 1024, Files.size(log.file).toString)
    await("the total cap")(files(dir).map(Files.size).sum <= (8 + 2 + 1) * 1024)
    assert(
      files(dir).forall(_.getFileName.toString.startsWith("eyes-studio.")),
      files(dir).toString
    )
  }

  test("an unwritable directory is refused by kind, without a path, and keeps stderr") {
    val blocker = Files.createTempFile("eyes-studio-not-a-dir", ".txt")
    val refused = StudioLog.open(blocker.resolve("logs"))
    refused match
      case Right(_)                             => fail("opened a log under a file")
      case Left(LogError.Unwritable(_, reason)) =>
        assert(!reason.contains("/"), reason)
        assert(!reason.contains(sys.props("user.name")), reason)
      case Left(other) => fail(s"$other")
    // A directory that exists but cannot be written falls back to stderr.
    val locked = Files.createTempDirectory("eyes-studio-locked")
    assume(locked.toFile.setWritable(false), "cannot make a directory read-only here")
    try
      assert(StudioLog.open(locked).isLeft)
      assertEquals(appenders, Vector("studio-stderr"))
    finally locked.toFile.setWritable(true): Unit
  }
