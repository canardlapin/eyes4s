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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

import java.nio.file.{Files, Path}

/** The JVM file adapter over a directory the test lays out: end-to-end
  * resolution, and missing, changed and escaping files named by entry.
  */
class ArtifactFilesSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A           = e.fold(x => fail(s"$x"), identity)
  private def array(bytes: IArray[Byte]): Array[Byte] = Array.tabulate(bytes.length)(bytes(_))

  private val input    = StudyInputFixtures.matchedControl
  private val frame    = get(Frame.screen("matched-control-display", 2, 2))
  private val grid     = get(Grid.over(frame, 2, 2))
  private val decoders = get(ArtifactDecoders.study[Px])
  private val plan     = get(
    StudyPlan.cosine[Px](
      input.reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned()),
      FailurePolicy.RequireAll
    )
  )
  private val result = get(plan.run(input))

  /** Every matched-control record admitted: the ledger of this input. */
  private val ledger = get(
    AdmissionLedger.of(
      SourceRef
        .of("matched-control.csv", StudyInputFixtures.header, StudyInputFixtures.records),
      StudyInputFixtures.header,
      eyes4s.examples.MatchedControlFixtures.fixations.zipWithIndex.map { case (row, index) =>
        SourceRecord(
          index + 2,
          Disposition.Admitted(StudyKey(row.participant, row.image, row.phase), row.ordinal)
        )
      },
      AdmissionOutcome.Complete
    )
  )

  private def saved(ledgerName: String): SavedManifest = get(
    for
      p <- StoredArtifact.plan("plan.json", StudyCodecs.cosine[Px], plan)
      i <- StoredArtifact.input("input.json", StudyInputCodecs.study[Px], input)
      r <- StoredArtifact.result("result.json", StudyResultCodecs.cosine[Px], result)
      l <- StoredArtifact.document(
        ledgerName,
        ArtifactRole.AdmissionLedger,
        get(StudyInputCodecs.study[Px].ledger.encode(ledger)),
        None
      )
      s <- SavedManifest.of(
        Vector(p, i, r, l),
        Vector(
          ManifestRelation.PlanInput(p.name, i.name),
          ManifestRelation.ResultOf(r.name, p.name, i.name),
          ManifestRelation.LedgerOf(l.name, i.name)
        )
      )
    yield s
  )

  private def written(value: SavedManifest): Path =
    val root = Files.createTempDirectory("eyes4s-manifest")
    Files.write(root.resolve("manifest.json"), array(value.bytes))
    value.artifacts.foreach { a =>
      val path = root.resolve(a.name.value).normalize
      if path.startsWith(root) then Files.write(path, array(a.bytes)): Unit
    }
    root

  private def resolve(value: SavedManifest, root: Path) =
    ArtifactFiles
      .resolve[IO, StudyKey, Px](
        value.address,
        ArtifactFiles.inDirectory(root, "manifest.json"),
        decoders
      )
      .unsafeRunSync()
      .left
      .map(_.toVector)

  test("a saved study written to a directory resolves through the file adapter") {
    val value    = saved("ledger.json")
    val resolved = get(resolve(value, written(value)))
    assertEquals(resolved.inputs.map(_._2.reference.digest), Vector("cebe7474ab5c2aec"))
    assertEquals(
      resolved.results.map(_._2.encode),
      Vector(StudyResultCodecs.cosine[Px].codec.encode(result))
    )
    assertEquals(resolved.ledgers.map(_._2.outcome), Vector(AdmissionOutcome.Complete))
  }

  test(
    "missing and changed files are named by entry; a name leaving the directory is not read"
  ) {
    val value = saved("ledger.json")
    val root  = written(value)
    Files.delete(root.resolve("plan.json"))
    val changed = Files.readAllBytes(root.resolve("result.json"))
    changed(10) = (changed(10) ^ 1).toByte
    Files.write(root.resolve("result.json"), changed)
    val errors = get(resolve(value, root).swap)
    assertEquals(errors.head, ResolveError.Missing(get(ArtifactName.of("plan.json"))))
    assert(errors(1) match
      case ResolveError.Digest(name, _, _) => name.value == "result.json"
      case _                               => false)

    val escaping = saved("../ledger.json")
    val refused  = get(resolve(escaping, written(escaping)).swap)
    assert(refused match
      case Vector(ResolveError.Unreadable(name, reason)) =>
        name.value == "../ledger.json" && reason.contains("outside")
      case _ => false)
    assertEquals(
      resolve(value, Files.createTempDirectory("eyes4s-empty")),
      Left(Vector(ResolveError.MissingManifest(value.address)))
    )
  }
