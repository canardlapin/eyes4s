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

/** Actual authored CSV declares a five-second trial whose final fixation ends at 400ms. */
object TrialDurationNativeFixture:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new AssertionError(e.toString), identity)
  val trial    = TrialKey("P17", Phase.Encoding, "enc_03", 1)
  val revision = AnalysisRevision(4)
  val fixes    =
    "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n" +
      "P17,Encoding,enc_03,1,1,100,300,600,300,1\n" +
      "P17,Encoding,enc_04,1,1,200,100,620,320,1\n" +
      "P17,Retrieval,ret_07,1,1,50,100,604,305,1\n"
  def trials(cell: String) =
    "participant,phase,trial,occurrence,item,response,display_kind,image_file,elapsed\n" +
      s"P17,Encoding,enc_03,1,item-a,,blank,,$cell\n" +
      s"P17,Encoding,enc_04,1,item-b,,blank,,$cell\n" +
      s"P17,Retrieval,ret_07,1,item-a,Remembered,blank,,$cell\n"
  def spec(cell: Option[String]): DatasetRevisionSpec =
    val original = get(StoryMoments.t2).datasets.last
    val texts    =
      Map(SourceRole.Fixations -> fixes, SourceRole.Trials -> trials(cell.getOrElse("")))
    val sources = get(
      Sources.of(
        original.sources.entries.map(s =>
          s.copy(
            bytes = ByteDigest.sha256(IArray.from(texts(s.role).getBytes(UTF_8))),
            semantic = None
          )
        )
      )
    )
    val duration = cell.map(_ =>
      get(InventoryDurationColumn.of(get(ColumnName.of("elapsed")), TimeUnit.Seconds))
    )
    original.copy(
      sources = sources,
      inventory = original.inventory.map(m => get(InventoryMapping.withDuration(m, duration)))
    )

  def prepared(cell: Option[String]): RealPrepared =
    val dataset = spec(cell)
    val assets  = get(
      AssetRegistry.of(
        dataset.id,
        dataset.sources.trials.get.bytes,
        dataset.geometry.screen,
        Vector.empty
      )
    )
    val admitted = get(RealAdmission.admit(dataset, fixes, trials(cell.getOrElse("")), assets))
    val recipe   = get(StoryMoments.t2).analyses.last.recipe.copy(
      input = None,
      grid = get(GridSize.of(8, 8)),
      scales = get(ScaleSet.of(Vector(get(Sigma.of(1)))))
    )
    get(RealPrepared.of(revision, dataset.id, recipe, admitted))

  def served(cell: Option[String] = Some("5")): TrialFixations =
    get(get(RealTrialViews.of(prepared(cell))).fixations(trial))
