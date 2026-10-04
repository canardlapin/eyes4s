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

/** The error report value (ticket S1.12): a throwable reduced to classes and
  * code locations, never its messages, and the copyable bundle around it.
  */
class ErrorReportSuite extends munit.FunSuite:

  /** Data values a failure message might carry. */
  private val secrets =
    Vector("P17", "ret_07", "1148.5", "/Users/someone/study", "fixations.csv")
  private val message = "P17 ret_07 at (1148.5, 456.5) in /Users/someone/study/fixations.csv"

  private def thrown(): Throwable =
    try throw IllegalStateException(message, IllegalArgumentException(message))
    catch case e: Throwable => e

  private val facts = BuildFacts(
    "0.1.0",
    Some("0123456789abcdef0123456789abcdef01234567"),
    true,
    ProtocolVersion(1, 8),
    Vector("Java" -> "25", "OS" -> "Mac OS X 14.3 aarch64")
  )

  test("a trace keeps each exception's class and frames and drops its message") {
    val t = FailureTrace.of(thrown())
    assertEquals(t.exception, "java.lang.IllegalStateException")
    assertEquals(t.cause.map(_.exception), Some("java.lang.IllegalArgumentException"))
    assert(
      t.frames.exists(_.declaringClass.startsWith("eyes4s.studio.core.report.ErrorReportSuite"))
    )
    assert(t.frames.forall(f => f.file.forall(!_.contains("/"))), t.frames)
    val text = t.lines.mkString("\n")
    secrets.foreach(s => assert(!text.contains(s), s"trace holds $s"))
  }

  test("a frame without a source file is left out, and a file is kept by name only") {
    val e = RuntimeException(message)
    e.setStackTrace(
      Array(
        StackTraceElement("<jscode>", message, null, -1),
        StackTraceElement("a.B", "run", "/Users/someone/study/main.js", 12),
        StackTraceElement("a.C", "call", "C.scala", 3)
      )
    )
    val t = FailureTrace.of(e)
    assertEquals(
      t.frames,
      Vector(
        StackLine("a.B", "run", Some("main.js"), Some(12)),
        StackLine("a.C", "call", Some("C.scala"), Some(3))
      )
    )
    assertEquals(t.omittedFrames, 1)
    secrets.foreach(s => assert(!t.lines.mkString("\n").contains(s), s))
  }

  test("frames past the limit are counted, and a cause cycle ends") {
    val deep = RuntimeException("deep")
    deep.setStackTrace(
      Array.tabulate(FailureTrace.MaxFrames + 5)(i =>
        StackTraceElement("a.B", s"m$i", "B.scala", i)
      )
    )
    val t = FailureTrace.of(deep)
    assertEquals(t.frames.size, FailureTrace.MaxFrames)
    assertEquals(t.omittedFrames, 5)
    assert(t.lines.contains("  … 5 more frames"))
    val a = RuntimeException("a")
    val b = RuntimeException("b", a)
    a.initCause(b)
    val chain =
      Iterator.iterate(Option(FailureTrace.of(a)))(_.flatMap(_.cause)).takeWhile(_.isDefined)
    assert(chain.size <= FailureTrace.MaxCauses + 1)
  }

  test("the bundle names build, origin, project digest and stack, and no data value") {
    val digest = "sha256:" + "ab" * 32
    val bundle = ErrorBundle(
      "2026-10-04T12:00:00Z",
      facts,
      FailureOrigin.UiThread("JavaFX Application Thread"),
      FailureTrace.of(thrown()),
      Some(digest)
    )
    val text = bundle.render
    assert(text.startsWith(ErrorBundle.Heading), text)
    assert(text.endsWith(ErrorBundle.Footer), text)
    assert(
      text.contains(
        "Eyes Studio 0.1.0 · commit 0123456789abcdef0123456789abcdef01234567 (modified) · protocol 1.8"
      ),
      text
    )
    assert(text.contains("Java 25 · OS Mac OS X 14.3 aarch64"), text)
    assert(text.contains("Where: UI thread (JavaFX Application Thread)"), text)
    assert(text.contains(s"Project: $digest"), text)
    assert(text.contains("Caused by java.lang.IllegalArgumentException"), text)
    secrets.foreach(s => assert(!text.contains(s), s"bundle holds $s"))
    val none = bundle.copy(project = None, build = facts.copy(commit = None, modified = false))
    assert(none.render.contains("Project: none open"))
    assert(none.render.contains("unknown commit"))
    assertEquals(FailureOrigin.Job("Submit").render, "job (Submit)")
  }
