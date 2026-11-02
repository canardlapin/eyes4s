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

package eyes4s.studio.desktop.importing

import eyes4s.studio.core.document.*
import eyes4s.studio.core.backend.{DatasetRevision, AnalysisRevision}
import eyes4s.studio.core.artifacts.LocationNativePackage
import eyes4s.codec.ByteDigest
import java.nio.charset.StandardCharsets.UTF_8

/** Exact five-file FIGRIM witness shared by desktop spatial workflow tests. */
private[desktop] object SpatialFixture:
  lazy val texts: Map[SourceRole, String] = Map(
    SourceRole.LocationTrials -> "trial_id,observer_slot,image,category,phase,sdt,image_index,image_family\ne1,1,a.jpg,c,enc,1,1,a.jpg\nr1,1,a.jpg,c,rec,2,1,a.jpg\nc1,1,b.jpg,c,enc,1,2,b.jpg\n",
    SourceRole.Locations     -> "trial_id\tx_px\ty_px\ne1\t0\t0\nr1\t0\t0\nc1\t900\t900\n",
    SourceRole.LocationPairs -> "pair_id\tfocal_id\tencoding_id\tcontrol_id\np1\tr1\te1\tc1\n",
    SourceRole.PhaseLedger -> "image_index\timage\tcategory\tobserver_slot\trecording_label\tinitials\tsource_trial\tphase\tslot_state\tgaze_state\tsdt\tresponse_state\tsource_locations\tadmitted_locations\timage_family\n1\ta.jpg\tc\t1\tlabel\tAB\t1\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\ta.jpg\n1\ta.jpg\tc\t1\tlabel\tAB\t1\trec\tmetadata_present\tlocations\t2\trecorded\t1\t1\ta.jpg\n2\tb.jpg\tc\t1\tlabel\tAB\t2\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\tb.jpg\n1\ta.jpg\tc\t2\tlabel\tAB\t\trec2\tmetadata_present\tmissing_phase\t\tabsent\t0\t0\ta.jpg\n",
    SourceRole.LocationLedger -> "image_index\tobserver_slot\tphase\tordinal\traw_x\traw_y\trounded_x\trounded_y\tadmitted\tx_px\ty_px\traw_zero_x\traw_zero_y\treason\n1\t1\tenc\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\tadmitted\n1\t1\trec\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\tadmitted\n2\t1\tenc\t1\t901\t901\t901\t901\t1\t900\t900\t900\t900\tadmitted\n"
  )

  /** Declared spatial analysis for shell layout and keyboard tests. */
  lazy val document: StudioDocument =
    def get[E, A](value: Either[E, A]): A =
      value.fold(e => throw new AssertionError(e.toString), identity)
    val files   = texts.toVector.map((role, text) => role -> text.getBytes(UTF_8).toIndexedSeq)
    val sources = get(
      LocationBundleSources.of(
        files.map((role, bytes) =>
          Source(
            role,
            get(SourcePath.of(s"${role.ordinal}.txt")),
            ByteDigest.sha256(IArray.from(bytes)),
            None
          )
        )
      )
    )
    val admitted = get(
      LocationNativePackage.admit(
        LocationDatasetRevision(
          DatasetRevision(1),
          None,
          sources,
          LocationProfile.FigrimV1,
          LocationAdmissionDecision.Pending
        ),
        files
      )
    )
    val analysis = LocationAnalysisRevision(
      AnalysisRevision(1),
      admitted.dataset.id,
      CoreBinding.Bound(admitted.planCanonical),
      LocationRecipe(LocationProfile.FigrimV1, CoreBinding.Bound(admitted.inputCanonical)),
      get(RevisionName.of("Spatial fixture")),
      "Declared spatial analysis"
    )
    get(
      StudioDocument.of(
        Vector(admitted.dataset),
        Vector(analysis),
        None,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
