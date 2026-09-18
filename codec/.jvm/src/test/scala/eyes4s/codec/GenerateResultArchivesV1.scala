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

package eyes4s.codec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** Repository-only writer of the pinned recording and temporal result
  * archives, kept in the JVM test source set rather than published:
  * `recording-result-v1.json` (the I-DT plan whose provenance is exactly
  * recording-input-v1's evidence, run on its monocular channels) and
  * `temporal-result-v1.json` (the pinned temporal-study-v1 plan run on
  * temporal-study-input-v1), both as the UTF-8 of the pretty-printed document
  * `StoredArtifact` stores, and the Scala source of their compact portable
  * mirrors (`ResultArchiveMirrors`), split into string constants a class file
  * can hold. `ResultArchivesV1JvmSuite` checks that the resources are exactly
  * this writer's output and that the mirrors carry the same JSON values.
  *
  * {{{
  * sbt "codecJVM/Test/runMain eyes4s.codec.GenerateResultArchivesV1 codec/src/test/resources/eyes4s codec/src/test/scala/eyes4s/codec/ResultArchiveMirrors.scala"
  * }}}
  */
private[codec] object GenerateResultArchivesV1:
  val recordingFile = "recording-result-v1.json"
  val temporalFile  = "temporal-result-v1.json"

  private def message[A](value: Either[CodecError, A]): Either[String, A] =
    value.left.map(_.message)

  /** The two archives' pretty-printed documents, by file name. */
  def documents: Either[String, Vector[(String, io.circe.Json)]] =
    message(
      for
        recording <- ArchiveFixtures.recordingResults.codec.encode(
          ArchiveFixtures.recordingAnalysis
        )
        temporal <- ArchiveFixtures.temporalResults.codec.encode(ArchiveFixtures.temporalResult)
      yield Vector(recordingFile -> recording, temporalFile -> temporal)
    )

  /** The resource bytes this writer produces, by file name. */
  def generated: Either[String, Vector[(String, IArray[Byte])]] =
    documents.flatMap(
      _.foldLeft[Either[String, Vector[(String, IArray[Byte])]]](Right(Vector.empty)) {
        case (acc, (file, json)) =>
          acc.flatMap(done =>
            message(Documents.utf8(json.spaces2)).map(bytes => done :+ (file -> bytes))
          )
      }
    )

  /** Split compact JSON after commas into pieces below the class-file
    * constant limit, so no piece starts or ends inside a quoted run.
    */
  private def pieces(text: String, limit: Int = 48000): Vector[String] =
    if text.length <= limit then Vector(text)
    else
      val cut = text.lastIndexOf(',', limit) + 1
      require(cut > 0, "no comma to split at")
      text.take(cut) +: pieces(text.drop(cut), limit)

  private def constant(name: String, text: String): String =
    val parts = pieces(text).map(p => s"    \"\"\"$p\"\"\"")
    s"""  val $name: String = Vector(
${parts.mkString(",\n")}
  ).mkString
"""

  /** The Scala source of the portable compact mirrors. */
  def mirrors: Either[String, String] =
    documents.map { docs =>
      val byFile = docs.toMap
      val header =
        """/*
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

package eyes4s.codec

/** Compact mirrors of src/test/resources/eyes4s/recording-result-v1.json and
  * temporal-result-v1.json for portable JVM/JS tests, written by
  * `GenerateResultArchivesV1` and checked against the resources by
  * `ResultArchivesV1JvmSuite`. Do not edit by hand.
  */
object ResultArchiveMirrors:
"""
      header + constant("recordingResultVersionOne", byFile(recordingFile).noSpaces) + "\n" +
        constant("temporalResultVersionOne", byFile(temporalFile).noSpaces)
    }

  def main(arguments: Array[String]): Unit =
    val (resources, mirror) = arguments.toVector match
      case Vector(r, m) => (Paths.get(r), Paths.get(m))
      case other        => sys.error(s"usage: <resource directory> <mirror source>, got $other")
    val written = for
      files  <- generated
      source <- mirrors
    yield
      files.foreach((file, bytes) =>
        Files.write(resources.resolve(file), Array.tabulate(bytes.length)(bytes(_)))
      )
      Files.writeString(mirror, source, StandardCharsets.UTF_8)
      files.map(_._1) :+ mirror.toString
    written.fold(e => sys.error(e), paths => println(paths.mkString("wrote ", ", ", "")))
