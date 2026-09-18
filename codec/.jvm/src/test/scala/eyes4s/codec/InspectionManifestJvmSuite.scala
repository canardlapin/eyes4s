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
import eyes4s.plan.*

/** The pinned manifest-v1, resolved from its resource files, drills down from
  * a contrast row of its result to the CSV records its complete ledger names;
  * a resolution refusal is a coded diagnostic naming the entry.
  */
class InspectionManifestJvmSuite extends munit.FunSuite:
  import CodecDiagnostics.given

  private def get[E, A](e: Either[E, A]): A     = e.fold(error => fail(s"$error"), identity)
  private def name(value: String): ArtifactName = get(ArtifactName.of(value))
  private def resource(file: String)            = get(GenerateManifestV1.resource(file))
  private val address = get(ByteDigest.parse(ManifestV1Fixtures.address))

  private def source(entries: Map[ArtifactName, IArray[Byte]]) = ByteSource.inMemory(
    Map(address -> resource("manifest-v1.json")),
    entries
  )
  private val files =
    GenerateManifestV1.files.map((entry, file) => name(entry) -> resource(file)).toMap

  test("resolved manifest-v1 drills down from a contrast row to its CSV record numbers") {
    val resolved = get(
      ArtifactResolver
        .resolve(address, source(files), get(ArtifactDecoders.study[Px]))
        .left
        .map(_.toVector)
    )
    val input   = get(resolved.input(name("input")).toRight("input"))
    val ledger  = get(resolved.ledger(name("ledger")).toRight("ledger"))
    val loaded  = get(resolved.result(name("result")).toRight("result"))
    val sources = get(StudySources.of(input, ledger))
    val view    = get(ResultInspection.study(loaded.result, sources, ScoreSchema.undescribed))
    val focal   = StudyKey("s2", "c", "recall")
    val row     = get(view.contrastRow(ResultRef.ContrastRow(0, focal)))
    val control = get(view.reduction(get(row.control.toRight("control"))))
    assertEquals(
      control.contributors,
      Vector(StudyKey("s2", "a", "encode"), StudyKey("s2", "b", "encode"))
    )
    val records =
      control.members.map(m => get(view.trial(get(view.pair(m.pair)).reference)).records)
    assertEquals(records, Vector(Vector(14, 15, 16, 17), Vector(18, 19, 20, 21)))
    assertEquals(get(sources.fixation(focal, 3)), FixationSource(focal, 3, 49, 3))
    assertEquals(sources.source.map(_.records.digest), Some(ledger.source.records.digest))
    assertEquals(view.failures, Vector.empty)
  }

  test("a corrupted entry is refused with a coded diagnostic naming the entry") {
    val bytes     = files(name("ledger"))
    val corrupted = files.updated(name("ledger"), bytes.updated(10, (bytes(10) ^ 1).toByte))
    val refused   =
      ArtifactResolver.resolve(address, source(corrupted), get(ArtifactDecoders.study[Px]))
    val errors      = refused.left.map(_.toVector).swap.getOrElse(fail("expected a refusal"))
    val diagnostics = errors.map(Diagnostic.of(_))
    assertEquals(diagnostics.map(_.code.render), Vector("resolve.digest"))
    assertEquals(diagnostics.map(_.subject), Vector(Vector(Locus.Entry("ledger"))))
  }
