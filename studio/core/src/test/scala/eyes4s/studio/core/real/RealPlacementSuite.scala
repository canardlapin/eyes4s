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
import eyes4s.kernel.{Pt, Unit2D}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{GoldenAssets, StoryMoments}
import java.nio.charset.StandardCharsets.UTF_8

class RealPlacementSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private lazy val original                     = get(
    get(StoryMoments.t2).dataset(StoryMoments.r3).toRight("no dataset")
  )
  private lazy val declared  = get(GoldenAssets.rows).head
  private lazy val trial     = declared.trial
  private lazy val fixations =
    "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n" +
      s"${trial.participant},${trial.phase.label},${trial.trial},${trial.occurrence},1,0,100,260,120,1\n" +
      s"${trial.participant},${trial.phase.label},${trial.trial},${trial.occurrence},2,210,100,-40.5,500,1\n"
  private lazy val inventory =
    "participant,phase,trial,occurrence,item,response,display_kind,image_file\n" +
      s"${trial.participant},${trial.phase.label},${trial.trial},${trial.occurrence},${declared.item},Remembered,${declared.kind},${declared.file}\n"
  private lazy val bytes = IArray.from(fixations.getBytes(UTF_8))
  private lazy val spec  =
    val texts   = Map(SourceRole.Fixations -> fixations, SourceRole.Trials -> inventory)
    val sources = get(
      Sources.of(
        original.sources.entries.map(s =>
          s.copy(bytes = ByteDigest.sha256(IArray.from(texts(s.role).getBytes(UTF_8))))
        )
      )
    )
    original.copy(sources = sources)
  private def corrected = spec.copy(admission =
    spec.admission.copy(corrections =
      Vector(
        CorrectionRule(
          CorrectionTarget.Trial(trial),
          CoordinateCorrection.Translate(get(Offset.of(500, 0)))
        )
      )
    )
  )
  private def admit(dataset: DatasetRevisionSpec) =
    RealAdmission.admit(dataset, fixations, inventory, get(GoldenAssets.registry(dataset)))

  test(
    "real placement uses native raw, corrected, local and angular coordinates without rewriting bytes"
  ) {
    val preview = get(RealPlacement.place(corrected, bytes))
    assertEquals(
      preview.records.map(r => (r.record, r.rawX, r.rawY)),
      Vector((1, 260.0, 120.0), (2, -40.5, 500.0))
    )
    assertEquals(
      preview.records.map(r => (r.correctedX, r.correctedY)),
      Vector((760.0, 120.0), (459.5, 500.0))
    )
    assertEquals(
      preview.records.map(r => (r.imageX, r.imageY)),
      Vector((312.0, -36.0), (11.5, 344.0))
    )
    assertEquals(preview.records.map(_.rule), Vector(Some(0), Some(0)))
    assertEquals(
      preview.records.map(_.placement),
      Vector(RecordPlacement.OutsideWindow, RecordPlacement.Inside)
    )
    assert(preview.records.forall(_.degrees.nonEmpty))
    assertEquals(preview.records.size + preview.unplaced.size, 2)
    assertEquals(ByteDigest.sha256(bytes), corrected.sources.fixations.get.bytes)
    val admitted = get(admit(corrected))
    assertEquals(
      admitted.input.trials.rows.head.value.fixations.iterator.map(_.centre).toVector,
      preview.records.map(r => Pt[Unit2D.Px](r.correctedX, r.correctedY))
    )
  }

  test("corrections apply before off-screen admission, including quarantine policy") {
    val quarantine =
      spec.copy(admission = spec.admission.copy(offScreen = OffScreenChoice.QuarantineTrial))
    val before = get(admit(quarantine))
    assert(before.ledger.head.disposition.isInstanceOf[TrialDisposition.Quarantined])
    val repaired = corrected.copy(admission =
      corrected.admission.copy(offScreen = OffScreenChoice.QuarantineTrial)
    )
    val after = get(admit(repaired))
    assertEquals(after.ledger.head.disposition, TrialDisposition.Admitted)
    assertEquals(after.input.trials.rows.head.value.fixations.length, 2)
  }

  // S5.5 recheck on the real admission path (slice r10 of S3.7): under
  // ExcludeRecord eyes4s admits the off-screen record and lists it in the
  // ledger as outside the frame (studies leave it out of every map); its
  // trial stays admitted and the source bytes are never rewritten.
  test("ExcludeRecord lists the off-screen record natively and keeps its trial admitted") {
    val exclude =
      spec.copy(admission = spec.admission.copy(offScreen = OffScreenChoice.ExcludeRecord))
    val admitted = get(admit(exclude))
    assertEquals(admitted.ledger.head.disposition, TrialDisposition.Admitted)
    assertEquals(
      admitted.input.trials.rows.head.value.fixations.iterator.map(_.centre).toVector,
      Vector(Pt[Unit2D.Px](260.0, 120.0), Pt[Unit2D.Px](-40.5, 500.0))
    )
    assertEquals(
      admitted.ledger.head.outsideFrame.map(o => (o.record, o.x, o.y)),
      // The same record the placement preview and source records call 2.
      Vector((2, -40.5, 500.0))
    )
    val preview = get(RealPlacement.place(exclude, bytes))
    assertEquals(
      preview.records.map(r => (r.record, r.placement)),
      Vector((1, RecordPlacement.OutsideWindow), (2, RecordPlacement.OutsideScreen))
    )
    assertEquals(ByteDigest.sha256(bytes), exclude.sources.fixations.get.bytes)
  }

  test(
    "conflicting correction rules quarantine natively and preserve every unplaced source record"
  ) {
    val conflict = spec.copy(admission =
      spec.admission.copy(corrections =
        Vector(
          CorrectionRule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipX),
          CorrectionRule(CorrectionTarget.Trial(trial), CoordinateCorrection.FlipY)
        )
      )
    )
    val admitted = get(admit(conflict))
    assert(admitted.ledger.head.disposition.isInstanceOf[TrialDisposition.Quarantined])
    val preview = get(RealPlacement.place(conflict, bytes))
    assertEquals(preview.records.size, 0)
    assertEquals(preview.trials.map(t => (t.trial, t.records)), Vector((trial, 0)))
    assertEquals(preview.unplaced.map(_.record), Vector(1, 2))
    assertEquals(preview.records.size + preview.unplaced.size, 2)
    assert(preview.unplaced.forall(_.reason.toLowerCase.contains("correction")))
  }

  test("trial correction scopes require a real declared inventory identity and item") {
    val unknown = trial.copy(occurrence = trial.occurrence + 1)
    val missing = spec.copy(admission =
      spec.admission.copy(corrections =
        Vector(CorrectionRule(CorrectionTarget.Trial(unknown), CoordinateCorrection.FlipX))
      )
    )
    val error =
      admit(missing).swap.toOption.getOrElse(fail("invented correction target was admitted"))
    assert(
      error.message.contains("not declared") && error.message.contains(unknown.trial),
      error.message
    )
  }
