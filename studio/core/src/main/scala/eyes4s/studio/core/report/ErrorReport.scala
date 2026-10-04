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

package eyes4s.studio.core.report

import eyes4s.studio.core.backend.ProtocolVersion

/** Where an unexpected failure happened (ticket S1.12). */
enum FailureOrigin derives CanEqual:
  /** An uncaught exception on the UI thread (the JavaFX application thread). */
  case UiThread(thread: String)

  /** A job's execution failed with a defect, not a refusal; `effect` names
    * the kind of effect that was being performed ("Submit").
    */
  case Job(effect: String)

  /** An uncaught exception on another thread. */
  case Background(thread: String)

  def render: String = this match
    case UiThread(t)   => s"UI thread ($t)"
    case Job(effect)   => s"job ($effect)"
    case Background(t) => s"background thread ($t)"

/** One stack frame: code location only. */
final case class StackLine(
    declaringClass: String,
    method: String,
    file: Option[String],
    line: Option[Int]
) derives CanEqual:
  def render: String =
    val where = (file, line) match
      case (Some(f), Some(n)) => s"$f:$n"
      case (Some(f), None)    => f
      case _                  => "unknown source"
    s"$declaringClass.$method($where)"

/** A throwable reduced to what a report may carry: each exception's class
  * and code locations, through its causes. Messages are left out because
  * they can hold data values (a participant, a coordinate, a file path).
  * `omittedFrames` counts the frames left out: past [[FailureTrace.MaxFrames]]
  * or without a source file.
  */
final case class FailureTrace(
    exception: String,
    frames: Vector[StackLine],
    omittedFrames: Int,
    cause: Option[FailureTrace]
) derives CanEqual:
  def lines: Vector[String] =
    Vector(exception) ++ frames.map("  at " + _.render) ++
      Option.when(omittedFrames > 0)(s"  … $omittedFrames more frames") ++
      cause.toVector.flatMap(c => ("Caused by " + c.lines.head) +: c.lines.tail)

object FailureTrace:
  /** Frames kept per exception, and causes followed. */
  val MaxFrames: Int = 48
  val MaxCauses: Int = 8

  def of(error: Throwable): FailureTrace = of(error, MaxCauses, Set.empty)

  /** The last segment of a path or URL ("main.js", "AppModel.scala"). */
  private def fileName(file: String): String =
    file.split("[/\\\\]").lastOption.filter(_.nonEmpty).getOrElse(file)

  private def of(error: Throwable, causes: Int, seen: Set[Throwable]): FailureTrace =
    // Only frames with a source file are code locations: on Scala.js the
    // message itself arrives as file-less pseudo-frames. A file is kept by
    // name, never by its path.
    val all = Option(error.getStackTrace)
      .fold(Vector.empty[StackTraceElement])(_.toVector)
      .flatMap(f => Option(f.getFileName).filter(_.nonEmpty).map(f -> fileName(_)))
    val frames = all.take(MaxFrames).map { (f, file) =>
      StackLine(
        f.getClassName,
        f.getMethodName,
        Some(file),
        Option.when(f.getLineNumber >= 0)(f.getLineNumber)
      )
    }
    val next    = Option(error.getCause).filter(c => causes > 0 && !seen(c) && (c ne error))
    val dropped = Option(error.getStackTrace).fold(0)(_.length) - all.size
    FailureTrace(
      error.getClass.getName,
      frames,
      all.size - frames.size + dropped,
      next.map(of(_, causes - 1, seen + error))
    )

/** The build and platform an error report describes. `runtime` holds
  * platform facts in order ("Java" → "25", "OS" → "Mac OS X 14.3 aarch64").
  */
final case class BuildFacts(
    version: String,
    commit: Option[String],
    modified: Boolean,
    protocol: ProtocolVersion,
    runtime: Vector[(String, String)]
) derives CanEqual

/** A copyable error report (ticket S1.12): versions, commit, where the
  * failure happened, its stack and the open project's scientific digest, and
  * nothing else. No participant data, no data values, no file paths and no
  * exception messages: the digest identifies the project without revealing
  * it.
  */
final case class ErrorBundle(
    at: String,
    build: BuildFacts,
    origin: FailureOrigin,
    trace: FailureTrace,
    project: Option[String]
) derives CanEqual:

  def render: String =
    val commit = build.commit.fold("unknown commit")(c =>
      s"commit $c" + (if build.modified then " (modified)" else "")
    )
    val protocol = s"protocol ${build.protocol.major}.${build.protocol.minor}"
    val runtime  = build.runtime.map((k, v) => s"$k $v")
    (Vector(
      ErrorBundle.Heading,
      s"Eyes Studio ${build.version} · $commit · $protocol",
      runtime.mkString(" · "),
      s"When: $at",
      s"Where: ${origin.render}",
      s"Project: ${project.getOrElse("none open")}",
      "",
      "Failure:"
    ) ++ trace.lines ++ Vector("", ErrorBundle.Footer)).mkString("\n")

object ErrorBundle:
  val Heading: String = "Eyes Studio error report"
  val Footer: String  =
    "This report holds no participant data, data values, file paths or exception messages."
