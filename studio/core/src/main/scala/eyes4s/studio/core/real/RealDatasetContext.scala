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

import eyes4s.kernel.{Frame, Unit2D}
import eyes4s.plan.{AdmissionLedger, StudyInput, TrialKey as CoreKey}
import eyes4s.studio.core.backend.{AdmissionSummary, LedgerEntry}
import eyes4s.studio.core.document.DatasetRevisionSpec

/** Exact native data needed to read a completed study. A stored ledger has parsed
  * source fields, but cannot supply verbatim CSV or current stimulus availability.
  */
sealed trait RealDatasetContext:
  def ledger: Vector[LedgerEntry]
  def screen: Frame[Unit2D.Px]
  def input: StudyInput[CoreKey, Unit2D.Px]
  def evidence: AdmissionLedger[CoreKey]
  def spec: DatasetRevisionSpec
  def sourceText: Option[String]

/** Actual host admission retains its observed summary and exact fixation text. */
final case class AdmittedDataset(
    summary: AdmissionSummary,
    ledger: Vector[LedgerEntry],
    screen: Frame[Unit2D.Px],
    input: StudyInput[CoreKey, Unit2D.Px],
    evidence: AdmissionLedger[CoreKey],
    spec: DatasetRevisionSpec,
    fixations: String
) extends RealDatasetContext:
  def sourceText: Option[String] = Some(fixations)

/** Constructed from a checked native snapshot, without invented source text or
  * image-presence summary. Those host observations remain separate reads.
  */
private[real] final class ArchivedDatasetContext private[real] (
    val ledger: Vector[LedgerEntry],
    val screen: Frame[Unit2D.Px],
    val input: StudyInput[CoreKey, Unit2D.Px],
    val evidence: AdmissionLedger[CoreKey],
    val spec: DatasetRevisionSpec
) extends RealDatasetContext:
  val sourceText: Option[String] = None
