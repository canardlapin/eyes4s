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

class SourceAdmissionSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("source-frame", 100, 100))
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
  private val csv =
    "participant,item,phase,n,x,y,onset,duration,samples\np,i,encode,0,10,20,0,100,10\nbad\n"

  test("admission keeps rejected records in the declared source and ledger v4") {
    val imported = get(SourceAdmission.read("a.csv", csv, spec))
    assertEquals(imported.spec.digest, spec.digest)
    assertEquals(imported.accepted.trials.rows.map(_.key), Vector(StudyKey("p", "i", "encode")))
    assertEquals(imported.ledger.records.size, 2)
    assertEquals(imported.ledger.rejected.size, 1)
    assertEquals(imported.ledger.version, 4)
    assertEquals(get(SourceAdmission.source("a.csv", csv, spec)), imported.ledger.source)
    val codec = StudyInputCodecs.study[Px].ledger
    assertEquals(get(codec.decode(get(codec.encode(imported.ledger)))), imported.ledger)
  }
  test("re-encoding CSV changes bytes while preserving decoded source identity") {
    val a = get(SourceAdmission.source("a", csv, spec))
    val b = get(SourceAdmission.source("b", csv.replace("\n", "\r\n"), spec))
    assertEquals(a.identity, b.identity)
    assertNotEquals(Sha256.ofUtf8(csv), Sha256.ofUtf8(csv.replace("\n", "\r\n")))
  }
  test("a persisted custom reader is refused, never reported as replayed") {
    val custom = get(
      ImportSpec.of[StudyKey, Px](
        SourceKeyColumns.Custom(
          get(DefinitionId.of("custom.reader", 1)),
          get(DefinitionId.of("custom.clock", 1)),
          Vector("p")
        ),
        columns,
        frame,
        SourceTimeUnit.Seconds,
        AdmissionPolicy.default[StudyKey],
        AdmissionDecision.RequireComplete
      )
    )
    val saved =
      get(ImportSpecCodec.study[Px].decode(get(ImportSpecCodec.study[Px].encode(custom))))
    assert(SourceAdmission.read("custom.csv", csv, saved).left.toOption.exists {
      case SourceAdmissionError.UnsupportedReplay("custom.csv", _, _) => true
      case _                                                          => false
    })
  }

  test("trial inventory admission binds both sources and preserves ledger v4 attributes") {
    val inventoryText = "participant,phase,trial,item,response\np,encode,t,i,yes\n"
    val invSpec       = get(
      InventoryImportSpec.of(
        "participant",
        "phase",
        "trial",
        item = Some("item"),
        attributes = Vector(AttributeColumn("response", AttributeKind.Text))
      )
    )
    val invSource = get(SourceAdmission.inventorySource("trials.csv", inventoryText, invSpec))
    val trialSpec = get(
      ImportSpec.of(
        SourceKeyColumns.Trial("participant", "phase", "trial", None, None),
        columns,
        frame,
        SourceTimeUnit.Milliseconds,
        AdmissionPolicy.default[TrialKey],
        AdmissionDecision.ReviewExclusions,
        inventory = Some(SourceInventory(invSpec, invSource.identity.get))
      )
    )
    val contents =
      "participant,phase,trial,n,x,y,onset,duration,samples\np,encode,t,0,10,20,0,100,10\n"
    val admitted = get(
      SourceAdmission.read(
        "fixations.csv",
        contents,
        trialSpec,
        Some("trials.csv" -> inventoryText)
      )
    )
    assertEquals(admitted.ledger.inventory.map(_.source), Some(invSource))
    assert(admitted.admitted.nonEmpty)
    val codec = StudyInputCodecs.trial[Px].ledger
    assertEquals(get(codec.decode(get(codec.encode(admitted.ledger)))), admitted.ledger)
    val savedSpec =
      get(ImportSpecCodec.trial[Px].decode(get(ImportSpecCodec.trial[Px].encode(trialSpec))))
    assertEquals(savedSpec.digest, trialSpec.digest)
    assert(
      SourceAdmission
        .read(
          "fixations.csv",
          contents,
          savedSpec,
          Some("trials.csv" -> inventoryText.replace("yes", "no"))
        )
        .isLeft
    )
  }
