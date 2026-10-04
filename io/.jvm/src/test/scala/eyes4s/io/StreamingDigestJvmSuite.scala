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

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

/** The study-result digest runs in bounded memory (bead
  * bd-01M441B59K01XN3EVVCRMQVHDV): in a fresh JVM whose heap holds a
  * synthetic result of 14.4 million density values but not its encoded
  * archive, the digest completes, while encoding the whole archive, which is
  * what the digest did before, runs out of memory. Both children run the same
  * harness over this test's classpath; the digest is deterministic across
  * processes.
  */
class StreamingDigestJvmSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(20, "min")

  /** The heap of each child: above the result, below its encoded archive. */
  private val Heap = "-Xmx384m"

  private lazy val classpath: String =
    val stream = Option(getClass.getResourceAsStream("/eyes4s/fresh-process/classpath.txt"))
      .getOrElse(fail("the build did not generate the fresh-process classpath resource"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()

  private val java = Paths.get(System.getProperty("java.home"), "bin", "java").toString

  private final case class Run(status: Int, output: String):
    def receipt: Option[String] =
      output.linesIterator.find(_.startsWith(StreamingDigestHarness.marker))

  private def launch(mode: String): Run =
    val log     = Files.createTempFile("streaming-digest-", ".log")
    val command = Vector(
      java,
      Heap,
      "-XX:+UseSerialGC",
      "-cp",
      classpath,
      "eyes4s.io.StreamingDigestHarness",
      mode
    )
    val process = new ProcessBuilder(command*)
      .redirectErrorStream(true)
      .redirectOutput(log.toFile)
      .start()
    try
      if !process.waitFor(10, TimeUnit.MINUTES) then
        process.destroyForcibly()
        val _ = process.waitFor(30, TimeUnit.SECONDS)
        fail(s"the harness did not finish within 10 minutes:\n${Files.readString(log)}")
      Run(process.exitValue, Files.readString(log))
    finally
      val _ = Files.deleteIfExists(log)

  test("the digest of a result too large to encode in the heap is computed, and repeatable") {
    val first = launch("streamed")
    assertEquals(first.status, 0, first.output)
    val receipt = first.receipt.getOrElse(fail(s"no receipt:\n${first.output}"))
    assert(receipt.endsWith(" values=14400000"), receipt)
    assert(
      receipt.matches(s"${StreamingDigestHarness.marker}[0-9a-f]{64} values=\\d+"),
      receipt
    )
    val again = launch("streamed")
    assertEquals(again.receipt, Some(receipt), again.output)
  }

  test("encoding the same result whole does not fit in that heap") {
    val whole = launch("whole")
    assertNotEquals(whole.status, 0, whole.output)
    assert(whole.output.contains("OutOfMemoryError"), whole.output.take(2000))
    assertEquals(whole.receipt, None)
  }
