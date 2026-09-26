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
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import scala.compiletime.testing.typeCheckErrors

class LedgerReverificationSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("replay-frame", 100, 100))
  private val columns                           = get(
    SourceFixationColumns.of(
      "n",
      "x",
      "y",
      "onset",
      "duration",
      SampleCountRule.PositiveColumn("samples")
    )
  )
  private val spec = get(
    ImportSpec.of(
      SourceKeyColumns.Study("participant", "item", "phase"),
      columns,
      frame,
      SourceTimeUnit.Milliseconds,
      AdmissionPolicy.default[StudyKey],
      AdmissionDecision.ReviewExclusions
    )
  )
  private val header   = "participant,item,phase,n,x,y,onset,duration,samples\n"
  private val csv      = header + "p,i,encode,0,10,20,0,100,10\nbad\nwrong\n"
  private val original = get(SourceAdmission.read("fixations.csv", csv, spec))
  private def verify(
      text: String = csv,
      ledger: AdmissionLedger[StudyKey] = original.ledger,
      input: StudyInput[StudyKey, Px] = original.accepted
  ) =
    LedgerReverification.verify("relocated.csv", text, spec, ledger, input)
  private def records(base: AdmissionLedger[StudyKey], rows: Vector[SourceRecord[StudyKey]]) =
    get(
      AdmissionLedger.of(
        base.source,
        base.header,
        rows,
        base.outcome,
        base.policy,
        base.outsideFrame
      )
    )

  test("source replay binds the input and every rejected record, ignoring display relocation") {
    val verified = get(verify())
    assertEquals(verified.input.hash, original.accepted.hash)
    assertEquals(verified.spec.digest, spec.digest)
    assertEquals(verified.ledger, original.ledger)
    assert(verified.admitted.nonEmpty)
    assert(verify(csv.replace("\n", "\r\n")).isRight)
    assert(verify(csv.replace("wrong", "different")).left.toOption.exists {
      case LedgerVerificationError.SourceChanged(_, _, causes) =>
        causes.values(IdentityChange.Records)
      case _ => false
    })
  }

  test("standalone rejection deletion passes pure binding and is refused by source replay") {
    val forged = records(original.ledger, original.ledger.records.filterNot(_.record == 4))
    assertEquals(forged.checkAgainst(original.accepted), Right(()))
    assert(
      get(
        StudyInputCodecs
          .study[Px]
          .ledger
          .decode(get(StudyInputCodecs.study[Px].ledger.encode(forged)))
      ) == forged
    )
    assert(verify(ledger = forged).left.toOption.exists {
      case LedgerVerificationError.LedgerMismatch(_, LedgerComponent.Records, _, _) => true
      case _                                                                        => false
    })
  }

  test("dropping a rejected row and rewriting its quarantine scope still fails replay") {
    val text =
      header + "p,i,encode,0,10,20,bad,100,10\np,i,encode,1,10,20,200,100,10\np,i,encode,2,10,20,400,100,10\n"
    val admitted = get(SourceAdmission.read("q.csv", text, spec))
    val kept     = admitted.ledger.records.filterNot(_.record == 2).map {
      case SourceRecord(
            n,
            Disposition.Rejected(raw, key, AdmissionReason.Quarantined(_, cause))
          ) =>
        SourceRecord(
          n,
          Disposition.Rejected(raw, key, AdmissionReason.Quarantined(Vector(3, 4), cause))
        )
      case row => row
    }
    val forged = records(admitted.ledger, kept)
    assertEquals(forged.checkAgainst(admitted.accepted), Right(()))
    assert(LedgerReverification.verify("q.csv", text, spec, forged, admitted.accepted).isLeft)
  }

  private val inventoryText =
    "participant,phase,trial,item,response\np,encode,t,i,yes\np,encode,u,j,no\n"
  private val inventorySpec = get(
    InventoryImportSpec.of(
      "participant",
      "phase",
      "trial",
      item = Some("item"),
      attributes = Vector(AttributeColumn("response", AttributeKind.Text))
    )
  )
  private val inventorySource = get(
    SourceAdmission.inventorySource("trials.csv", inventoryText, inventorySpec)
  )
  private val trialSpec = get(
    ImportSpec.of(
      SourceKeyColumns.Trial("participant", "phase", "trial", None, None),
      columns,
      frame,
      SourceTimeUnit.Milliseconds,
      AdmissionPolicy.default[TrialKey],
      AdmissionDecision.ReviewExclusions,
      inventory = Some(SourceInventory(inventorySpec, inventorySource.identity.get))
    )
  )
  private val trialText =
    "participant,phase,trial,n,x,y,onset,duration,samples\np,encode,t,0,10,20,0,100,10\nbad\n"
  private val trialAdmission = get(
    SourceAdmission.read(
      "fixations.csv",
      trialText,
      trialSpec,
      Some("trials.csv" -> inventoryText)
    )
  )
  private val inv = trialAdmission.ledger.inventory.get
  private def forgedInventory(
      attrs: Vector[AttributeColumn] = inv.attributeColumns,
      trials: Vector[InventoryTrial] = inv.trials,
      counts: SampleCountRule = inv.sampleCounts
  ) =
    val changed = get(
      InventoryLedger.of(
        inv.source,
        inv.header,
        attrs,
        trials,
        inv.unlisted,
        inv.recordAttributeColumns,
        inv.recordAttributes,
        counts
      )
    )
    get(trialAdmission.ledger.withInventory(changed, TrialIdentity.of, _.item))
  private def verifyInventory(ledger: AdmissionLedger[TrialKey]) =
    LedgerReverification.verify(
      "fixations.csv",
      trialText,
      trialSpec,
      ledger,
      trialAdmission.accepted,
      Some("relocated-inventory.csv" -> inventoryText)
    )
  private def pureAccepts(ledger: AdmissionLedger[TrialKey]): Unit =
    assertEquals(ledger.checkAgainst(trialAdmission.accepted), Right(()))
    val codec = StudyInputCodecs.trial[Px].ledger
    assertEquals(get(codec.decode(get(codec.encode(ledger)))), ledger)

  test("sample-count rule and positive column forgeries pass pure checks but fail replay") {
    assert(verifyInventory(trialAdmission.ledger).isRight)
    Vector(
      SampleCountRule.PositiveColumn("absent"),
      SampleCountRule.DerivedFromDuration(get(Hz(100)))
    ).foreach { rule =>
      val forged = forgedInventory(counts = rule)
      pureAccepts(forged)
      assert(verifyInventory(forged).left.toOption.exists {
        case LedgerVerificationError.LedgerMismatch(_, LedgerComponent.SampleCounts, _, _) =>
          true
        case _ => false
      })
    }
  }

  test(
    "inventory attribute declaration absent from the header passes pure checks but fails replay"
  ) {
    val changed = inv.trials.map(t =>
      get(
        InventoryTrial.of(
          t.identity,
          t.rows,
          t.inventoryItem,
          get(Attributes.of(Vector("forged" -> t.attributes.get("response").get))),
          t.recordItems,
          t.records,
          t.disposition
        )
      )
    )
    val forged = forgedInventory(
      attrs = Vector(AttributeColumn("forged", AttributeKind.Text)),
      trials = changed
    )
    pureAccepts(forged)
    assert(verifyInventory(forged).left.toOption.exists {
      case LedgerVerificationError
            .LedgerMismatch(_, LedgerComponent.InventoryAttributes, _, _) =>
        true
      case _ => false
    })
  }

  test(
    "a keyless rejection assigned to an arbitrary inventory trial passes pure checks but fails replay"
  ) {
    val changed = inv.trials.map(t =>
      if t.identity.trial == "u" then
        get(
          InventoryTrial.of(
            t.identity,
            t.rows,
            t.inventoryItem,
            t.attributes,
            t.recordItems,
            Vector(3),
            TrialDisposition.NoFixations
          )
        )
      else t
    )
    val forged = forgedInventory(trials = changed)
    pureAccepts(forged)
    assert(verifyInventory(forged).left.toOption.exists {
      case LedgerVerificationError.LedgerMismatch(_, LedgerComponent.InventoryTrials, _, _) =>
        true
      case _ => false
    })
  }

  test("legacy unspecified sources and changed input cannot acquire verification") {
    val base   = original.ledger
    val legacy = get(
      AdmissionLedger.of(
        SourceRef(base.source.label, base.source.records),
        base.header,
        base.records,
        base.outcome,
        base.policy,
        base.outsideFrame
      )
    )
    assert(verify(ledger = legacy).left.toOption.exists {
      case LedgerVerificationError.LegacyUnverified(_) => true
      case _                                           => false
    })
    val different =
      get(SourceAdmission.read("different", csv.replace(",10,20,", ",11,20,"), spec)).accepted
    assert(verify(input = different).left.toOption.exists {
      case LedgerVerificationError.InputMismatch(_, _, _) => true
      case _                                              => false
    })
  }

  test("replay verification preserves refusal instead of admitting the accepted subset") {
    val required = get(
      ImportSpec.of(
        spec.keys,
        spec.columns,
        spec.frame,
        spec.timeUnit,
        spec.policy,
        AdmissionDecision.RequireComplete
      )
    )
    val imported = get(SourceAdmission.read("refused.csv", csv, required))
    assertEquals(imported.ledger.outcome, AdmissionOutcome.Refused)
    assertEquals(imported.accepted.trials.rows.size, 1)
    assertEquals(imported.admitted, None)
    val verified = get(
      LedgerReverification.verify(
        "refused.csv",
        csv,
        required,
        imported.ledger,
        imported.accepted
      )
    )
    assertEquals(verified.input.hash, imported.accepted.hash)
    assertEquals(verified.admitted, None)
  }

  test("malformed source bytes refuse replay with the source-labelled import error") {
    assert(verify(text = header + "\"unterminated").left.toOption.exists {
      case LedgerVerificationError.Import(
            SourceAdmissionError.Import("relocated.csv", FixationImportError.Csv(_))
          ) =>
        true
      case _ => false
    })
  }

  test("verification evidence has no public constructor") {
    val errors = typeCheckErrors("""
      import eyes4s.io.*
      import eyes4s.plan.*
      import eyes4s.kernel.Unit2D.Px
      def fake(s: ImportSpec[StudyKey, Px], l: AdmissionLedger[StudyKey], i: StudyInput[StudyKey, Px]) =
        new VerifiedAdmission(s, l, i)
    """)
    assert(errors.exists(_.message.contains("constructor")), errors.toString)
  }
