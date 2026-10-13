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

package eyes4s.studio.desktop.runtime

import cats.effect.IO
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.real.{RealAdmission, RealStudyBackend}
import java.nio.charset.StandardCharsets.UTF_8

/** Native service recheck of correction order, admission policy and exact source text. */
class NativeAdmissionPolicySuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A        = value.fold(e => fail(s"$e"), identity)
  private def ok[E, A](value: IO[Either[E, A]]): IO[A] = value.map(get)
  private val trial = TrialKey("P1", Phase.Encoding, "enc_1", 1)

  for
    policy    <- Vector(OffScreenChoice.ExcludeRecord, OffScreenChoice.QuarantineTrial)
    corrected <- Vector(false, true)
  do
    test(
      s"native $policy with correction=$corrected agrees with direct admission and preserves every source line"
    ) {
      val original = NativeScienceHarness.fixture()
      val lines    =
        String(Array.from(original.bytes(SourceRole.Fixations)), UTF_8).linesIterator.toVector
      val first = lines(1).split(",", -1).toVector.updated(5, "-40.5").updated(6, "500")
      val text  = lines.updated(1, first.mkString(",")).mkString("", "\n", "\n")
      val bytes =
        original.bytes.updated(SourceRole.Fixations, IArray.from(text.getBytes(UTF_8)))
      val data = original.data.copy(
        decision = AdmissionDecision.Admitted(
          Some(eyes4s.plan.AdmissionDecision.ReviewExclusions),
          CoreBinding.unbound,
          CoreBinding.unbound
        ),
        sources = get(
          Sources.of(
            original.data.sources.entries.map(s =>
              s.copy(bytes = ByteDigest.sha256(bytes(s.role)))
            )
          )
        ),
        admission = original.data.admission.copy(
          offScreen = policy,
          corrections = Option
            .when(corrected)(
              CorrectionRule(
                CorrectionTarget.Trial(trial),
                CoordinateCorrection.Translate(get(Offset.of(500, 0)))
              )
            )
            .toVector
        )
      )
      val registry = get(
        AssetRegistry.fromInventory(data, bytes(SourceRole.Trials), Vector.empty, Vector.empty)
      )
      val doc      = original.document
      val document = get(
        StudioDocument.of(
          Vector(data),
          doc.analyses,
          doc.draft,
          doc.runs,
          doc.reporting,
          doc.figures,
          doc.presentation,
          doc.jobs
        )
      )
      val fixture = NativeScienceHarness.Fixture(document, data, bytes, registry)
      val direct  = get(
        RealAdmission.admit(
          data,
          text,
          String(Array.from(bytes(SourceRole.Trials)), UTF_8),
          registry
        )
      )
      val revision = document.analyses.head.id
      RealStudyBackend.resource[IO](document, fixture.sources).use { backend =>
        for
          admission <- ok(backend.admission(data.id))
          ledger    <- ok(backend.ledger(data.id, get(PageRequest.of(0, 100))))
          fixations <- backend.trialFixations(revision, trial)
          records   <- ok(backend.sourceRecords(revision, 1, 100))
          after     <- fixture.sources.bytes(data, data.sources.fixations.get)
        yield
          assertEquals(admission, direct.summary)
          assertEquals(ledger.entries, direct.ledger)
          assertEquals(records.rows.map(_.line), text.linesIterator.drop(1).toVector)
          assertEquals(records.rows.map(_.record), (1 to (lines.size - 1)).toVector)
          assertEquals(ByteDigest.sha256(after.get), data.sources.fixations.get.bytes)
          val entry = direct.ledger.find(_.trial == trial).get
          if policy == OffScreenChoice.QuarantineTrial && !corrected then
            assert(entry.disposition.isInstanceOf[TrialDisposition.Quarantined])
            assertEquals(
              fixations.left.toOption.map(_.code),
              Some("studio-backend.unavailable")
            )
          else
            assertEquals(entry.disposition, TrialDisposition.Admitted)
            val native   = get(fixations)
            val expected = direct.input.trials.rows
              .find(t =>
                t.key.participant == trial.participant && t.key.phase == trial.phase.label && t.key.trial == trial.trial
              )
              .get
              .value
            assertEquals(
              native.fixations.map(f => (f.screenX, f.screenY)),
              expected.fixations.iterator.map(f => (f.centre.x, f.centre.y)).toVector
            )
            assertEquals(native.fixations.head.screenX, if corrected then 459.5 else -40.5)
            assertEquals(
              entry.outsideFrame.map(_.record),
              if corrected then Vector.empty else Vector(1)
            )
      }
    }
