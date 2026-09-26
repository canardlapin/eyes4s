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

import eyes4s.plan.*

/** The pinned version-3 admission ledger: a trial-keyed admission joined to
  * its trial inventory. src/test/resources/eyes4s/admission-ledger-v3.json is
  * its pretty-printed encoding and [[LedgerV3Mirrors]] its compact mirror.
  *
  * It has one trial of each disposition (admitted, quarantined with rejected
  * records, quarantined by an item conflict with the inventory, no fixations
  * and absent), a trial only the fixation table names, every attribute kind
  * (an integer beyond 2^53 among them) and record attributes.
  */
object LedgerV3Fixtures:
  private def get[E, A](e: Either[E, A]): A =
    e.fold(x => throw new AssertionError(s"$x"), identity)

  private def id(trial: String, phase: String = "Encoding", participant: String = "P1") =
    get(TrialIdentity.of(participant, phase, trial, TrialOccurrence.first))
  private def key(identity: TrialIdentity, item: String) = get(identity.withItem(item))

  private val e1 = id("e1")
  private val e2 = id("e2")
  private val r3 = id("r3", "Retrieval")
  private val r4 = id("r4", "Retrieval")
  private val r5 = id("r5", "Retrieval")
  private val x9 = id("x9", participant = "P2")

  val header: Vector[String] =
    Vector(
      "participant",
      "phase",
      "trial",
      "ordinal",
      "x",
      "y",
      "onset",
      "duration",
      "n",
      "item"
    )
  val rows: Vector[Vector[String]] = Vector(
    Vector("P1", "Encoding", "e1", "1", "3", "4", "0", "100", "50", "beach"),
    Vector("P1", "Encoding", "e1", "2", "5", "4", "150", "100", "50", "beach"),
    Vector("P1", "Encoding", "e2", "1", "3", "4", "0", "100", "0", "dog"),
    Vector("P1", "Retrieval", "r3", "1", "3", "4", "0", "100", "0", "beach"),
    Vector("P1", "Retrieval", "r3", "2", "3", "4", "150", "100", "50", "beach"),
    Vector("P2", "Encoding", "x9", "1", "3", "4", "0", "100", "50", "tower"),
    Vector("P1", "Retrieval", "r4", "1", "3", "4", "0", "100", "50", "lake")
  )
  val inventoryHeader: Vector[String] =
    Vector("participant", "phase", "trial", "item", "response", "count", "weight", "kind")
  val inventoryRows: Vector[Vector[String]] = Vector(
    Vector("P1", "Encoding", "e1", "beach", "", "9007199254740993", "0.5", "image"),
    Vector("P1", "Encoding", "e2", "dog", "", "-3", "", "image"),
    Vector("P1", "Retrieval", "r3", "beach", "Remembered", "", "1.25", "cross"),
    Vector("P1", "Retrieval", "r4", "bridge", "Forgotten", "", "", "cross"),
    Vector("P1", "Retrieval", "r5", "desert", "Forgotten", "", "", "cross")
  )

  private def attributes(values: (String, AttributeValue)*) = get(
    Attributes.of(values.toVector)
  )
  private def row(
      response: AttributeValue,
      count: AttributeValue,
      weight: AttributeValue,
      kind: String
  ) =
    attributes(
      "response" -> response,
      "count"    -> count,
      "weight"   -> weight,
      "kind"     -> AttributeValue.Text(kind)
    )

  private val missing     = AttributeValue.Blank
  private val zeroSamples = AdmissionReason.Number("n", "0", "a positive integer")

  def ledger: AdmissionLedger[TrialKey] =
    val base = get(
      AdmissionLedger.decide(
        SourceRef.of("fixations.csv", header, rows),
        header,
        Vector(
          SourceRecord(2, Disposition.Admitted(key(e1, "beach"), 1)),
          SourceRecord(3, Disposition.Admitted(key(e1, "beach"), 2)),
          SourceRecord(4, Disposition.Rejected(rows(2), Some(key(e2, "dog")), zeroSamples)),
          SourceRecord(5, Disposition.Rejected(rows(3), Some(key(r3, "beach")), zeroSamples)),
          SourceRecord(
            6,
            Disposition.Rejected(
              rows(4),
              Some(key(r3, "beach")),
              AdmissionReason.Quarantined(Vector(5, 6), QuarantineCause.RejectedRecords)
            )
          ),
          SourceRecord(
            7,
            Disposition.Rejected(
              rows(5),
              Some(key(x9, "tower")),
              AdmissionReason.Quarantined(
                Vector(7),
                QuarantineCause.NotInInventory("P2", "Encoding", "x9", 1)
              )
            )
          ),
          SourceRecord(
            8,
            Disposition.Rejected(
              rows(6),
              Some(key(r4, "bridge")),
              AdmissionReason.Quarantined(
                Vector(8),
                QuarantineCause.InventoryItemConflict("bridge", Vector("lake"))
              )
            )
          )
        ),
        AdmissionDecision.ReviewExclusions,
        AdmissionPolicy.default[TrialKey],
        Vector.empty
      )
    )
    val inventory = get(
      InventoryLedger.of(
        SourceRef.of("trials.csv", inventoryHeader, inventoryRows),
        inventoryHeader,
        Vector(
          InventoryTrial(
            e1,
            Vector(2),
            Some("beach"),
            row(
              missing,
              AttributeValue.Integer(9007199254740993L),
              AttributeValue.Number(0.5),
              "image"
            ),
            Vector("beach"),
            Vector(2, 3),
            TrialDisposition.Admitted
          ),
          InventoryTrial(
            e2,
            Vector(3),
            Some("dog"),
            row(missing, AttributeValue.Integer(-3L), missing, "image"),
            Vector("dog"),
            Vector(4),
            TrialDisposition.NoFixations
          ),
          InventoryTrial(
            r3,
            Vector(4),
            Some("beach"),
            row(
              AttributeValue.Text("Remembered"),
              missing,
              AttributeValue.Number(1.25),
              "cross"
            ),
            Vector("beach"),
            Vector(5, 6),
            TrialDisposition.Quarantined(QuarantineCause.RejectedRecords)
          ),
          InventoryTrial(
            r4,
            Vector(5),
            Some("bridge"),
            row(AttributeValue.Text("Forgotten"), missing, missing, "cross"),
            Vector("lake"),
            Vector(8),
            TrialDisposition.Quarantined(
              QuarantineCause.InventoryItemConflict("bridge", Vector("lake"))
            )
          ),
          InventoryTrial(
            r5,
            Vector(6),
            Some("desert"),
            row(AttributeValue.Text("Forgotten"), missing, missing, "cross"),
            Vector.empty,
            Vector.empty,
            TrialDisposition.Absent
          )
        ),
        Vector(UnlistedTrial(x9, Vector("tower"), Vector(7))),
        Vector(
          RecordAttributes(2, attributes("pupil" -> AttributeValue.Number(3.25))),
          RecordAttributes(3, attributes("pupil" -> AttributeValue.Blank))
        )
      )
    )
    get(base.withInventory(inventory, TrialIdentity.of, _.item))
