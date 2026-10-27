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
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import java.nio.charset.StandardCharsets.UTF_8

class RealReportsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val run                               = RunId(1)
  private val fixations                         =
    "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n" +
      "P01,Encoding,enc_01,1,1,0,100,600,300,1\n" +
      "P01,Encoding,enc_02,1,1,0,100,620,320,1\n" +
      "P01,Retrieval,ret_01,1,1,0,100,604,305,1\n"
  private val inventory =
    "participant,phase,trial,occurrence,item,response,display_kind,image_file\n" +
      "P01,Encoding,enc_01,1,item-a,reference-only,image,item-a.png\n" +
      "P01,Encoding,enc_02,1,item-b,reference-only,image,item-b.png\n" +
      "P01,Retrieval,ret_01,1,item-a,observed,image,item-a.png\n"
  private lazy val prepared                                     = preparedWith(inventory)
  private def preparedWith(inventoryRows: String): RealPrepared =
    val sample   = get(StoryMoments.t2)
    val original = sample.datasets.last
    val byRole   = Map(SourceRole.Fixations -> fixations, SourceRole.Trials -> inventoryRows)
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
    val admitted = get(RealAdmission.admit(spec, fixations, inventoryRows, assets))
    val recipe   = sample.analyses.last.recipe.copy(
      grid = get(GridSize.of(8, 8)),
      scales = get(ScaleSet.of(Vector(get(Sigma.of(1)), get(Sigma.of(2)))))
    )
    get(RealPrepared.of(AnalysisRevision(1), spec.id, recipe, admitted))
  private def report(data: RealPrepared): ReportView =
    val result = get(data.work.run)
    val held   = RealStudyBackend.RealRun(data, result, RealStudyBackend.RunOrigin.Computed)
    val spec   = get(
      ReportingSpec.of(
        get(ReportingId.of("response-groups")),
        "Responses",
        Some(get(Covariate.of("response"))),
        Vector.empty,
        None,
        ReportingWeight.ParticipantMeans,
        None
      )
    )
    get(RealReports.evaluate(run, spec, 0, held))

  test("category levels use focal declarations, without reference-only levels") {
    val view = report(prepared)
    assertEquals(view.cells.flatMap(_.group.map(_.label)).toSet, Set("observed"))
    assert(view.cells.filter(_.role == ReportRole.Difference).forall(_.estimate.nonEmpty))
    assertEquals(view.participants.map(_.participant).distinct, Vector("P01"))
  }

  test("unadmitted focal category remains declared with its empty-group absence") {
    val data =
      preparedWith(inventory + "P01,Retrieval,ret_02,1,never-studied,declared-empty,unknown,\n")
    val view   = report(data)
    val groups = view.cells.filter(_.role == ReportRole.Difference)
    assertEquals(groups.flatMap(_.group.map(_.label)).toSet, Set("observed", "declared-empty"))
    val empty = groups.find(_.group.exists(_.label == "declared-empty")).get
    assertEquals(empty.estimate, None)
    assertEquals(empty.absence, Some(ReportAbsence.EmptyGroup))
    assert(
      data.admitted.evidence.inventory.get.trials.exists(e =>
        e.identity.trial == "ret_02" && e.disposition != eyes4s.plan.TrialDisposition.Admitted
      )
    )
  }
