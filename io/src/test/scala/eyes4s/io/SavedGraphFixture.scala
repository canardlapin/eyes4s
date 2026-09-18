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

import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.examples.MatchedControlFixtures
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** A saved matched-control study (plan, input, complete ledger, result) for
  * the io adapters; the ledger's artifact name is the test's choice.
  */
object SavedGraphFixture:
  private def get[E, A](e: Either[E, A]): A =
    e.fold(x => throw new IllegalStateException(s"$x"), identity)

  val input: StudyInput[StudyKey, Px] = StudyInputFixtures.matchedControl
  private val frame                   = get(Frame.screen("matched-control-display", 2, 2))
  private val grid                    = get(Grid.over(frame, 2, 2))
  val decoders: ArtifactDecoders[StudyKey, Px] = get(ArtifactDecoders.study[Px])
  val plan                                     = get(
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
  val result = get(plan.run(input))

  /** Every matched-control record admitted: the ledger of this input. */
  val ledger: AdmissionLedger[StudyKey] = get(
    AdmissionLedger.of(
      SourceRef
        .of("matched-control.csv", StudyInputFixtures.header, StudyInputFixtures.records),
      StudyInputFixtures.header,
      MatchedControlFixtures.fixations.zipWithIndex.map { case (row, index) =>
        SourceRecord(
          index + 2,
          Disposition.Admitted(StudyKey(row.participant, row.image, row.phase), row.ordinal)
        )
      },
      AdmissionOutcome.Complete
    )
  )

  def saved(ledgerName: String = "ledger.json"): SavedManifest = get(
    for
      p <- StoredArtifact.plan("plan.json", StudyCodecs.cosine[Px], plan)
      i <- StoredArtifact.input("input.json", StudyInputCodecs.study[Px], input)
      l <- StoredArtifact.ledger(ledgerName, StudyInputCodecs.study[Px], ledger)
      r <- StoredArtifact.result("result.json", StudyResultCodecs.cosine[Px], result)
      s <- SavedManifest.of(
        Vector(p, i, l, r),
        Vector(
          ManifestRelation.PlanInput(p.name, i.name),
          ManifestRelation.LedgerOf(l.name, i.name),
          ManifestRelation.ResultOf(r.name, p.name, i.name)
        )
      )
    yield s
  )
