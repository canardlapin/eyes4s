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

package eyes4s.studio.core.real

import eyes4s.codec.ByteDigest
import eyes4s.studio.core.artifacts.*
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import java.nio.charset.StandardCharsets.UTF_8

/** Tiny actual CSV execution keeps graph verification portable on JVM and JS. */
class NativeArtifactPackageSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val run                               = RunId(1)
  private val fixations                         =
    "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n" +
      "P01,Encoding,enc_01,1,1,0,100,600,300,1\n" +
      "P01,Encoding,enc_02,1,1,0,100,620,320,1\n" +
      "P01,Retrieval,ret_01,1,1,0,100,604,305,1\n"
  private val inventory =
    "participant,phase,trial,occurrence,item,response,display_kind,image_file\n" +
      "P01,Encoding,enc_01,1,item-a,,image,item-a.png\n" +
      "P01,Encoding,enc_02,1,item-b,,image,item-b.png\n" +
      "P01,Retrieval,ret_01,1,item-a,Remembered,image,item-a.png\n"
  private lazy val prepared =
    val sample   = get(StoryMoments.t2)
    val original = sample.datasets.last
    val byRole   = Map(SourceRole.Fixations -> fixations, SourceRole.Trials -> inventory)
    val sources  = get(
      Sources.of(
        original.sources.entries.map(s =>
          s.copy(bytes = ByteDigest.sha256(IArray.from(byRole(s.role).getBytes(UTF_8))))
        )
      )
    )
    val spec   = original.copy(sources = sources)
    val assets = get(
      AssetRegistry.of(spec.id, sources.trials.get.bytes, spec.geometry.screen, Vector.empty)
    )
    val admitted = get(RealAdmission.admit(spec, fixations, inventory, assets))
    val recipe   = sample.analyses.last.recipe.copy(
      grid = get(GridSize.of(8, 8)),
      scales = get(ScaleSet.of(Vector(get(Sigma.of(1)), get(Sigma.of(2)))))
    )
    get(RealPrepared.of(AnalysisRevision(1), spec.id, recipe, admitted))
  private lazy val result = get(prepared.work.run)
  private lazy val held   =
    RealStudyBackend.RealRun(prepared, result, RealStudyBackend.RunOrigin.Computed)
  private lazy val exported = get(
    NativeArtifacts.build(run, held, NativeArtifactBudget.Default)
  )

  test("native closure verifies both after production and a copied byte readback") {
    val copy   = exported.files.map((name, data) => name -> data.map(identity))
    val loaded = get(NativeArtifactPackage.verify(copy, exported.budget))
    assertEquals(loaded.facts, exported.facts)
    assertEquals(loaded.manifestAddress, exported.manifestAddress)
    assertEquals(loaded.archive.index.archive, CoreBinding.Bound(loaded.facts.result))
    assertEquals(
      loaded.facts.source,
      SemanticIdentity.fromCore(prepared.admitted.evidence.source.records)
    )
    assertNotEquals(
      loaded.manifest.entry(NativeArtifactPackage.ResultName).get.sha256,
      loaded.facts.result.sha256
    )
    assertNotEquals(loaded.manifestAddress, loaded.facts.result.sha256)
  }

  test("corruption, incomplete closure and substituted canonical facts refuse") {
    val corrupted = exported.files.map { (name, data) =>
      if name == NativeArtifactPackage.InputName then
        name    -> data.updated(data.size - 1, (data.last ^ 1).toByte)
      else name -> data
    }
    assert(NativeArtifactPackage.verify(corrupted).isLeft)
    assert(
      NativeArtifactPackage
        .verify(exported.files.filterNot(_._1 == NativeArtifactPackage.InputName))
        .isLeft
    )
    val altered = exported.files.map { (name, data) =>
      if name != NativeArtifactPackage.FactsName then name -> data
      else
        val json    = get(io.circe.parser.parse(String(data.toArray, UTF_8)))
        val changed = json.hcursor
          .downField("value")
          .downField("source")
          .withFocus(_ => io.circe.Json.fromString("abcdef1234567890"))
          .top
          .get
        name -> changed.noSpaces.getBytes(UTF_8).toVector
    }
    assert(NativeArtifactPackage.verify(altered).isLeft)
  }

  test("named density, row, entry and total-byte budgets refuse explicitly") {
    val cells   = get(NativeArtifactBudget.of(maxDensityCells = 1))
    val rows    = get(NativeArtifactBudget.of(maxRows = 1))
    val entries = get(NativeArtifactBudget.of(maxEntries = 1))
    val bytes   = get(NativeArtifactBudget.of(maxTotalBytes = 1))
    Vector(cells, rows, entries, bytes).foreach(budget =>
      assert(NativeArtifacts.build(run, held, budget).isLeft)
    )
    assert(NativeArtifactPackage.verify(exported.files, bytes).isLeft)
    assert(NativeArtifactBudget.of(maxRows = 0).isLeft)
  }

  test("an actually empty admitted input is refused without inventing a source frame") {
    val emptyCsv = fixations.linesIterator.take(1).mkString("", "", "\n")
    val spec     = prepared.admitted.spec
      .copy(sources = get(Sources.of(prepared.admitted.spec.sources.entries.map { source =>
        if source.role == SourceRole.Fixations then
          source.copy(bytes = ByteDigest.sha256(IArray.from(emptyCsv.getBytes(UTF_8))))
        else source
      })))
    val assets = get(
      AssetRegistry.of(
        spec.id,
        spec.sources.trials.get.bytes,
        spec.geometry.screen,
        Vector.empty
      )
    )
    val admission = get(RealAdmission.admit(spec, emptyCsv, inventory, assets))
    assertEquals(admission.input.trials.size, 0)
    val emptyWork =
      get(RealPrepared.of(AnalysisRevision(1), spec.id, prepared.recipe, admission))
    val emptyResult = get(emptyWork.work.run)
    val completion  =
      RealStudyBackend.RealRun(emptyWork, emptyResult, RealStudyBackend.RunOrigin.Computed)
    val refused =
      NativeArtifacts.build(run, completion, NativeArtifactBudget.Default).left.toOption.get
    assertEquals(
      refused,
      NativeArtifactError.Package(
        run,
        "recipe verification",
        "Empty admitted input has no native source frame."
      )
    )
  }

  test(
    "recipe and canonical result substitution refuse independently of stored entry byte hashes"
  ) {
    def changed(field: String, replace: io.circe.Json => io.circe.Json) = exported.files.map {
      (name, data) =>
        if name != NativeArtifactPackage.FactsName then name -> data
        else
          val json    = get(io.circe.parser.parse(String(data.toArray, UTF_8)))
          val updated =
            json.hcursor.downField("value").downField(field).withFocus(replace).top.get
          name -> updated.noSpaces.getBytes(UTF_8).toVector
    }
    val otherResult = changed("result", _ => io.circe.Json.fromString("f" * 64))
    assert(NativeArtifactPackage.verify(otherResult).isLeft)
    val otherRecipe = changed(
      "recipeSnapshot",
      recipe =>
        recipe.hcursor
          .downField("grid")
          .downField("columns")
          .withFocus(_ => io.circe.Json.fromInt(16))
          .top
          .get
    )
    assert(NativeArtifactPackage.verify(otherRecipe).isLeft)
  }
