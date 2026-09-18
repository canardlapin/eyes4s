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

import eyes4s.kernel.Unit2D.Px

import java.nio.file.{Files, Paths}

/** Repository-only writer of `manifest-v1.json`, kept in the JVM test source
  * set rather than published. `ManifestV1JvmSuite` checks that [[written]]
  * reproduces the frozen resource byte for byte, so the fixture cannot drift
  * from what the writer produces.
  *
  * {{{
  * sbt "codecJVM/Test/runMain eyes4s.codec.GenerateManifestV1 <output path>"
  * }}}
  */
private[codec] object GenerateManifestV1:
  /** Entry name to pinned resource file; the layout is this fixture's choice. */
  val files: Vector[(String, String)] = Vector(
    "plan"   -> "study-v1.json",
    "input"  -> "study-input-v1.json",
    "ledger" -> "admission-ledger-v1.json",
    "result" -> "study-result-v1.json"
  )

  def resource(name: String): Either[String, IArray[Byte]] =
    Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .toRight(s"missing resource $name")
      .map { stream =>
        try IArray.unsafeFromArray(stream.readAllBytes())
        finally stream.close()
      }

  /** The manifest over the four pinned files, with the plan-input and
    * result-of relations. The pinned ledger records a refused import and is
    * carried without a ledger-of relation.
    */
  def written: Either[String, SavedManifest] = for
    plan   <- resource("study-v1.json")
    input  <- resource("study-input-v1.json")
    ledger <- resource("admission-ledger-v1.json")
    result <- resource("study-result-v1.json")
    value  <- StudyInputCodecs
      .study[Px]
      .input
      .parse(new String(Array.tabulate(input.length)(input(_)), "UTF-8"))
      .left
      .map(_.message)
    saved <- (for
      p <- StoredArtifact.bytes("plan", ArtifactRole.StudyPlan, plan, None)
      i <- StoredArtifact.bytes("input", ArtifactRole.StudyInput, input, Some(value.hash))
      l <- StoredArtifact.bytes("ledger", ArtifactRole.AdmissionLedger, ledger, None)
      r <- StoredArtifact.bytes("result", ArtifactRole.StudyResult, result, None)
      s <- SavedManifest.of(
        Vector(p, i, l, r),
        Vector(
          ManifestRelation.PlanInput(p.name, i.name),
          ManifestRelation.ResultOf(r.name, p.name, i.name)
        )
      )
    yield s).left.map(_.message)
  yield saved

  def main(arguments: Array[String]): Unit =
    (for
      path  <- arguments.headOption.toRight("usage: GenerateManifestV1 <output path>")
      saved <- written
    yield Files.write(
      Paths.get(path),
      Array.tabulate(saved.bytes.length)(saved.bytes(_))
    )) match
      case Left(message) =>
        Console.err.println(s"manifest-v1 not written: $message")
        System.exit(2)
      case Right(path) => println(s"wrote $path")
