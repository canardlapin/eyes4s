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

import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, LoggerContext}
import ch.qos.logback.core.FileAppender
import eyes4s.studio.app.report.LogState
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Files, Path, Paths}
import scala.util.Try

/** Why the studio log could not be opened. Each case names the path. */
enum LogError derives CanEqual:
  /** SLF4J is not bound to Logback, so the file appender cannot be set. */
  case NotLogback(factory: String)
  case Unwritable(directory: Path, reason: String)

  def message: String = this match
    case NotLogback(f) => s"SLF4J is bound to $f, not Logback; the studio log is not written."
    case Unwritable(d, why) =>
      s"The log directory ${StudioLog.shown(d)} cannot be written: $why"

/** The studio's log file (ticket S1.12): every SLF4J logger of the desktop
  * writes to `eyes-studio.log` in the platform log directory
  * (`~/Library/Logs/Eyes Studio` on macOS).
  */
final class StudioLog private (val file: Path):
  def logger(name: String): Logger = LoggerFactory.getLogger(name)

  /** The log file as a dialog shows it, the home directory as `~`. */
  def state: LogState = LogState.Written(StudioLog.shown(file))

object StudioLog:
  val FileName: String = "eyes-studio.log"
  val Pattern: String  = "%d{ISO8601} %-5level [%thread] %logger - %msg%n"

  /** The platform log directory: `~/Library/Logs/Eyes Studio` on macOS,
    * `~/.eyes4s-studio/logs` elsewhere.
    */
  def defaultDirectory: Path =
    val home = Paths.get(sys.props.getOrElse("user.home", "."))
    if sys.props.getOrElse("os.name", "").toLowerCase.contains("mac") then
      home.resolve("Library").resolve("Logs").resolve("Eyes Studio")
    else home.resolve(".eyes4s-studio").resolve("logs")

  /** `path` with the user's home directory shown as `~`. */
  def shown(path: Path): String =
    val home = Paths.get(sys.props.getOrElse("user.home", "/nonexistent")).toAbsolutePath
    val abs  = path.toAbsolutePath
    if abs.startsWith(home) then "~/" + home.relativize(abs).toString else abs.toString

  /** Route the root logger to `directory/eyes-studio.log`, replacing any
    * earlier configuration; the file is appended to.
    */
  def open(directory: Path = defaultDirectory): Either[LogError, StudioLog] =
    LoggerFactory.getILoggerFactory match
      case context: LoggerContext =>
        Try(Files.createDirectories(directory)).toEither.left
          .map(e => LogError.Unwritable(directory, e.toString))
          .flatMap { _ =>
            val file = directory.resolve(FileName)
            Try {
              context.reset()
              val encoder = PatternLayoutEncoder()
              encoder.setContext(context)
              encoder.setPattern(Pattern)
              encoder.start()
              val appender = FileAppender[ILoggingEvent]()
              appender.setContext(context)
              appender.setName("studio-file")
              appender.setFile(file.toString)
              appender.setAppend(true)
              appender.setEncoder(encoder)
              appender.start()
              val root = context.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
              root.setLevel(Level.INFO)
              root.addAppender(appender)
              if !appender.isStarted then throw IllegalStateException("appender did not start")
              new StudioLog(file)
            }.toEither.left.map(e => LogError.Unwritable(directory, e.toString))
          }
      case other => Left(LogError.NotLogback(other.getClass.getName))
