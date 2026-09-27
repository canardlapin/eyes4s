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
import eyes4s.design.{Trial, Trials}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class ManifestReverificationSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def name(s: String): ArtifactName     = get(ArtifactName.of(s))
  private def bytes(s: String): IArray[Byte]    =
    IArray.from(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
  private val frame   = get(Frame.screen("manifest-replay", 100, 100))
  private val columns = get(
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
    "participant,item,phase,n,x,y,onset,duration,samples\np,i,encode,0,10,20,0,100,10\nbad\nwrong\n"
  private val admission      = get(SourceAdmission.read("original.csv", csv, spec))
  private val sourceRelation = ManifestRelation.LedgerSource(
    name("ledger"),
    name("sources/fix.csv"),
    name("spec"),
    LedgerSourceRole.Primary
  )
  private val inputRelation = ManifestRelation.LedgerOf(name("ledger"), name("input"))
  private val decoders      = get(ArtifactDecoders.study[Px])
  private def saved(
      text: String = csv,
      ledger: AdmissionLedger[StudyKey] = admission.ledger,
      input: StudyInput[StudyKey, Px] = admission.accepted,
      linked: Boolean = true
  ): SavedManifest = get(
    SavedManifest.of(
      Vector(
        get(
          StoredArtifact.sourceFile("sources/fix.csv", SourceFormat.FixationCsv, bytes(text))
        ),
        get(StoredArtifact.importSpec("spec", ImportSpecCodec.study[Px], spec)),
        get(StoredArtifact.ledger("ledger", StudyInputCodecs.study[Px], ledger)),
        get(StoredArtifact.input("input", StudyInputCodecs.study[Px], input))
      ),
      if linked then Vector(sourceRelation, inputRelation) else Vector(inputRelation)
    )
  )
  private def resolve(value: SavedManifest) = get(
    ArtifactResolver.resolve(value.address, value.source, decoders)
  )

  test("saved sources resolve then replay into private verification evidence") {
    val restored = resolve(saved())
    val verified = get(ManifestReverification.verify(restored, name("ledger")))
    assertEquals(verified.ledger, admission.ledger)
    assertEquals(verified.spec.digest, spec.digest)
    assertEquals(verified.admitted.map(_.hash), admission.admitted.map(_.hash))
    assert(
      ManifestReverification
        .verify(resolve(saved(text = csv.replace("\n", "\r\n"))), name("ledger"))
        .isRight
    )
  }

  test("replacing source bytes and their byte digest still fails semantic replay") {
    val restored = resolve(saved(text = csv.replace("wrong", "other")))
    assert(ManifestReverification.verify(restored, name("ledger")).left.toOption.exists {
      case LedgerVerificationError.SourceChanged(_, _, causes) =>
        causes.values(IdentityChange.Records)
      case _ => false
    })
  }

  test("a pure-accepted rejection deletion remains refused after manifest roundtrip") {
    val old    = admission.ledger
    val forged = get(
      AdmissionLedger.of(
        old.source,
        old.header,
        old.records.dropRight(1),
        old.outcome,
        old.policy,
        old.outsideFrame
      )
    )
    assertEquals(forged.checkAgainst(admission.accepted), Right(()))
    val restored = resolve(saved(ledger = forged))
    assert(ManifestReverification.verify(restored, name("ledger")).left.toOption.exists {
      case LedgerVerificationError.LedgerMismatch(_, LedgerComponent.Records, _, _) => true
      case _                                                                        => false
    })
  }

  test("same-hash fabricated dispersion cannot gain verification through a manifest") {
    val row      = admission.accepted.trials.rows.head
    val fixation = row.value.first
    val fake     = get(
      Event.Fixation.of(
        fixation.span,
        fixation.centre,
        42.0,
        DispersionMethod.BoundingBoxWidth,
        fixation.sampleCount
      )
    )
    val path  = get(Scanpath.of(row.value.frame, row.value.clock, IArray(fake)))
    val input = StudyInput(Trials(Vector(Trial(row.key, (), path))))
    assertEquals(input.hash, admission.accepted.hash)
    val restored = resolve(saved(input = input))
    assert(ManifestReverification.verify(restored, name("ledger")).left.toOption.exists {
      case LedgerVerificationError
            .InputEvidenceMismatch(_, InputComponent.Fixation, Some(0), Some(0), _, _) =>
        true
      case _ => false
    })
  }

  test("source-less graphs and unknown ledgers remain readable but cannot be verified") {
    val restored = resolve(saved(linked = false))
    assertEquals(
      ManifestReverification.verify(restored, name("ledger")),
      Left(LedgerVerificationError.ManifestBinding("ledger", "primary source", 0))
    )
    assertEquals(
      ManifestReverification.verify(restored, name("missing")),
      Left(LedgerVerificationError.ManifestBinding("missing", "ledger", 0))
    )
    val noInput = get(SavedManifest.of(saved().artifacts, Vector(sourceRelation)))
    assertEquals(
      ManifestReverification.verify(resolve(noInput), name("ledger")),
      Left(LedgerVerificationError.ManifestBinding("ledger", "ledger-of", 0))
    )
  }

  test(
    "inventory replay requires both source roles and binds inventory bytes and description"
  ) {
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
    val text =
      "participant,phase,trial,n,x,y,onset,duration,samples\np,encode,t,0,10,20,0,100,10\n"
    val admitted =
      get(SourceAdmission.read("fix.csv", text, trialSpec, Some("trials.csv" -> inventoryText)))
    val inputs = StudyInputCodecs.trial[Px]
    val ds     = ArtifactDecoders
      .of(
        StudyRegistry.empty[TrialKey, Px],
        get(StudyInputRegistry.empty[TrialKey, Px].register(inputs)),
        StudyResultRegistry.empty[TrialKey, Px]
      )
      .withImportSpecs(ImportSpecCodec.trial[Px])
    val artifacts = Vector(
      get(StoredArtifact.sourceFile("sources/fix.csv", SourceFormat.FixationCsv, bytes(text))),
      get(
        StoredArtifact
          .sourceFile("trials.csv", SourceFormat.TrialInventoryCsv, bytes(inventoryText))
      ),
      get(StoredArtifact.importSpec("spec", ImportSpecCodec.trial[Px], trialSpec)),
      get(StoredArtifact.inventoryImportSpec("inv-spec", invSpec)),
      get(StoredArtifact.ledger("ledger", inputs, admitted.ledger)),
      get(StoredArtifact.input("input", inputs, admitted.accepted))
    )
    val inventoryRelation = ManifestRelation.LedgerSource(
      name("ledger"),
      name("trials.csv"),
      name("inv-spec"),
      LedgerSourceRole.TrialInventory
    )
    val relations = Vector(sourceRelation, inventoryRelation, inputRelation)
    def resolved(as: Vector[StoredArtifact], rs: Vector[ManifestRelation] = relations) =
      val archive = get(SavedManifest.of(as, rs))
      ArtifactResolver.resolve(archive.address, archive.source, ds)
    val result = get(resolved(artifacts))
    assertEquals(result.inventorySpec(name("inv-spec")), Some(invSpec))
    assert(ManifestReverification.verify(result, name("ledger")).isRight)
    assert(resolved(artifacts, relations.filterNot(_ == inventoryRelation)).isLeft)
    val changed = get(
      StoredArtifact.sourceFile(
        "trials.csv",
        SourceFormat.TrialInventoryCsv,
        bytes(inventoryText.replace("yes", "no"))
      )
    )
    assert(
      ManifestReverification
        .verify(get(resolved(artifacts.updated(1, changed))), name("ledger"))
        .isLeft
    )
    val wrong =
      get(InventoryImportSpec.of("participant", "phase", "trial", item = Some("item")))
    assert(
      resolved(
        artifacts.updated(3, get(StoredArtifact.inventoryImportSpec("inv-spec", wrong)))
      ).isLeft
    )
  }
