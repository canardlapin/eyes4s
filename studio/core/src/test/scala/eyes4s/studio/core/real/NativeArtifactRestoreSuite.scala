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
class NativeArtifactRestoreSuite extends munit.FunSuite:
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
          s.copy(
            bytes = ByteDigest.sha256(IArray.from(byRole(s.role).getBytes(UTF_8))),
            semantic = None
          )
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

  private lazy val ref = RunRef(
    run,
    prepared.revision,
    prepared.dataset,
    RunLifecycle.Completed,
    CoreBinding.Bound(exported.facts.result)
  )
  private lazy val revision = get(StoryMoments.t2).analyses.last.copy(
    id = prepared.revision,
    dataset = prepared.dataset,
    plan = exported.facts.stamp.plan,
    recipe = prepared.recipe.copy(input = Some(exported.facts.source))
  )
  private lazy val restored = get(
    NativeArtifactRestore.of(exported, ref, revision, prepared.admitted.spec)
  )

  test(
    "materialization retains the checked stored result, plan and input without executing the study"
  ) {
    assert(restored.result eq exported.checked.result)
    assert(restored.prepared.plan eq exported.checked.plan)
    assert(restored.prepared.admitted.input eq exported.checked.input)
    assert(restored.prepared.admitted.evidence eq exported.checked.ledger)
    assertEquals(restored.prepared.summary, prepared.summary)
    assertEquals(restored.prepared.admitted.ledger, prepared.admitted.ledger)
    assertEquals(restored.prepared.counts.pairRowsPerScale, prepared.counts.pairRowsPerScale)
    assertEquals(restored.origin, RealStudyBackend.RunOrigin.Restored(exported.manifestAddress))
    val liveRows     = get(RealResults.of(run, held)).queryRows
    val archivedRows = get(RealResults.of(run, restored)).queryRows
    assertEquals(archivedRows, liveRows)
  }

  test(
    "raw text remains absent until explicitly supplied and checked against the native ledger"
  ) {
    assertEquals(restored.prepared.admitted.sourceText, None)
    val live   = get(RealTrialViews.of(prepared))
    val stored = get(RealTrialViews.of(restored.prepared))
    val key    = RealResults.key(prepared.admitted.input.trials.rows.head.key)
    assertEquals(stored.fixations(key), live.fixations(key))
    assert(stored.sourceRecords(1, 3).left.toOption.exists(_.message.contains("no verbatim")))
    assertEquals(stored.sourceRecords(fixations, 1, 3), live.sourceRecords(1, 3))
    assert(stored.sourceRecords(fixations.replace("600,300", "601,300"), 1, 3).isLeft)
  }

  test(
    "scope, unpublished bindings, recipe, source declarations and screen disagreement refuse"
  ) {
    assert(
      NativeArtifactRestore
        .of(exported, ref.copy(id = RunId(99)), revision, prepared.admitted.spec)
        .isLeft
    )
    assert(
      NativeArtifactRestore
        .of(exported, ref.copy(state = RunLifecycle.Running), revision, prepared.admitted.spec)
        .isLeft
    )
    assert(
      NativeArtifactRestore
        .of(exported, ref.copy(archive = CoreBinding.unbound), revision, prepared.admitted.spec)
        .isLeft
    )
    assert(
      NativeArtifactRestore
        .of(exported, ref, revision.copy(plan = CoreBinding.unbound), prepared.admitted.spec)
        .isLeft
    )
    assert(
      NativeArtifactRestore
        .of(
          exported,
          ref,
          revision.copy(recipe = revision.recipe.copy(input = None)),
          prepared.admitted.spec
        )
        .isLeft
    )
    assert(
      NativeArtifactRestore
        .of(
          exported,
          ref,
          revision.copy(recipe = revision.recipe.copy(grid = get(GridSize.of(16, 8)))),
          prepared.admitted.spec
        )
        .isLeft
    )
    val spec   = prepared.admitted.spec
    val screen = get(ScreenSize.of(spec.geometry.screen.width + 1, spec.geometry.screen.height))
    val geometry = get(Geometry.of(screen, spec.geometry.image, spec.geometry.pixelsPerDegree))
    assert(
      NativeArtifactRestore.of(exported, ref, revision, spec.copy(geometry = geometry)).isLeft
    )
    val sources = get(
      Sources.of(
        spec.sources.entries.map(s =>
          if s.role == SourceRole.Fixations then
            s.copy(semantic = Some(get(SemanticIdentity.of("0123456789abcdef"))))
          else s
        )
      )
    )
    assert(
      NativeArtifactRestore.of(exported, ref, revision, spec.copy(sources = sources)).isLeft
    )
    val renamed = get(
      Sources.of(
        spec.sources.entries.map(s =>
          if s.role == SourceRole.Fixations then
            s.copy(path = get(SourcePath.of("renamed.csv")))
          else s
        )
      )
    )
    assert(
      NativeArtifactRestore.of(exported, ref, revision, spec.copy(sources = renamed)).isLeft
    )
  }
