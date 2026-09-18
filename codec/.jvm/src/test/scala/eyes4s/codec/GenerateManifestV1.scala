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

/** Repository-only writer of `admission-ledger-complete-v1.json` and
  * `manifest-v1.json`, kept in the JVM test source set rather than published.
  * `ManifestV1JvmSuite` checks that both resources are exactly what this
  * writer produces, so the fixtures cannot drift from it.
  *
  * {{{
  * sbt "codecJVM/Test/runMain eyes4s.codec.GenerateManifestV1 <output directory>"
  * }}}
  */
private[codec] object GenerateManifestV1:
  val completeLedgerFile = "admission-ledger-complete-v1.json"

  /** Entry name to pinned resource file; the layout is this fixture's choice. */
  val files: Vector[(String, String)] = Vector(
    "plan"           -> "study-v1.json",
    "input"          -> "study-input-v1.json",
    "ledger"         -> completeLedgerFile,
    "refused-ledger" -> "admission-ledger-v1.json",
    "result"         -> "study-result-v1.json"
  )

  def resource(name: String): Either[String, IArray[Byte]] =
    Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .toRight(s"missing resource $name")
      .map { stream =>
        try IArray.unsafeFromArray(stream.readAllBytes())
        finally stream.close()
      }

  /** The complete admission ledger of study-input-v1, as the writer stores it. */
  def completeLedger: Either[String, IArray[Byte]] =
    StoredArtifact
      .ledger("ledger", StudyInputCodecs.study[Px], ManifestFixtures.ledger)
      .map(_.bytes)
      .left
      .map(_.message)

  /** The manifest over the pinned files as `read` supplies them: plan-input,
    * ledger-of for the complete ledger and result-of. The refused ledger is
    * carried as evidence without a relation.
    */
  def written(read: String => Either[String, IArray[Byte]]): Either[String, SavedManifest] =
    for
      bytes <- files.traverseEither((entry, file) => read(file).map(entry -> _))
      table = bytes.toMap
      input <- StudyInputCodecs
        .study[Px]
        .input
        .parse(new String(Array.tabulate(table("input").length)(table("input")(_)), "UTF-8"))
        .left
        .map(_.message)
      saved <- (for
        p <- StoredArtifact.bytes("plan", ArtifactRole.StudyPlan, table("plan"), None)
        i <- StoredArtifact.bytes(
          "input",
          ArtifactRole.StudyInput,
          table("input"),
          Some(input.hash)
        )
        l <- StoredArtifact.bytes("ledger", ArtifactRole.AdmissionLedger, table("ledger"), None)
        f <- StoredArtifact
          .bytes("refused-ledger", ArtifactRole.AdmissionLedger, table("refused-ledger"), None)
        r <- StoredArtifact.bytes("result", ArtifactRole.StudyResult, table("result"), None)
        s <- SavedManifest.of(
          Vector(p, i, l, f, r),
          Vector(
            ManifestRelation.PlanInput(p.name, i.name),
            ManifestRelation.LedgerOf(l.name, i.name),
            ManifestRelation.ResultOf(r.name, p.name, i.name)
          )
        )
      yield s).left.map(_.message)
    yield saved

  extension [A](values: Vector[A])
    private def traverseEither[B](f: A => Either[String, B]): Either[String, Vector[B]] =
      values.foldLeft[Either[String, Vector[B]]](Right(Vector.empty))((acc, a) =>
        acc.flatMap(bs => f(a).map(bs :+ _))
      )

  def main(arguments: Array[String]): Unit =
    (for
      directory <- arguments.headOption.toRight("usage: GenerateManifestV1 <output directory>")
      ledger    <- completeLedger
      saved     <- written(file =>
        if file == completeLedgerFile then Right(ledger) else resource(file)
      )
    yield
      val out = Paths.get(directory)
      Files.write(out.resolve(completeLedgerFile), Array.tabulate(ledger.length)(ledger(_)))
      Files.write(
        out.resolve("manifest-v1.json"),
        Array.tabulate(saved.bytes.length)(saved.bytes(_))
      )
      saved.address.hex
    ) match
      case Left(message) =>
        Console.err.println(s"manifest-v1 not written: $message")
        System.exit(2)
      case Right(address) => println(s"wrote manifest-v1 with address $address")
