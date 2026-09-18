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

import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

/** What content verification does and does not establish about a ledger's
  * exclusions, pinned as current behaviour.
  *
  * A ledger's rejected records are bound by nothing the resolver can
  * re-derive: the source digest is carried, not recomputed (admitted
  * records keep no raw fields), and `LedgerOf` checks only that the admitted
  * records address the input. A forger who drops rejected records and
  * re-declares the manifest's digests consistently is therefore refused only
  * when the result breaks a ledger invariant. Exclusions can be verified
  * only by re-importing the source file, an io-level check deferred to G1.
  */
class LedgerForgerySuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A     = e.fold(error => fail(s"$error"), identity)
  private def name(value: String): ArtifactName = get(ArtifactName.of(value))
  private val inputs                            = StudyInputCodecs.study[Px]
  private val decoders                          = get(ArtifactDecoders.study[Px])

  private def rejected(ledger: AdmissionLedger[StudyKey]): Vector[Int] =
    ledger.records.filterNot(_.isAdmitted).map(_.record)

  /** The pinned refused ledger with its records edited as JSON. */
  private def edited(change: Vector[Json] => Vector[Json]): Json =
    get(io.circe.parser.parse(StudyInputFixtures.ledgerVersionOne)).hcursor
      .downField("value")
      .downField("records")
      .withFocus(records => Json.arr(change(records.asArray.toVector.flatten)*))
      .top
      .get

  private def record(json: Json): Int = get(json.hcursor.get[Int]("record"))

  private def rescoped(scope: Vector[Int])(json: Json): Json =
    json.hcursor
      .downField("reason")
      .withFocus(reason =>
        if reason.hcursor.get[String]("kind").contains("quarantined") then
          reason.mapObject(_.add("records", Json.arr(scope.map(Json.fromInt)*)))
        else reason
      )
      .top
      .getOrElse(json) // an admitted record has no reason

  /** Resolve one ledger (and, when related, its input) from memory. */
  private def resolve(ledger: IArray[Byte], related: Boolean) =
    val saved = get(
      for
        l <- StoredArtifact.bytes("ledger", ArtifactRole.AdmissionLedger, ledger, None)
        i <- StoredArtifact.input("input", inputs, ManifestFixtures.input)
        s <- SavedManifest.of(
          Vector(l, i),
          if related then Vector(ManifestRelation.LedgerOf(l.name, i.name)) else Vector.empty
        )
      yield s
    )
    ArtifactResolver.resolve(saved.address, saved.source, decoders).left.map(_.toVector)

  private def utf8(json: Json): IArray[Byte] =
    get(Utf8.encode(json.noSpaces).left.map(i => s"at $i"))

  test("dropping the time rejection that a quarantine scope names breaks the scope invariant") {
    val dropped = edited(_.filterNot(record(_) == 2))
    assertEquals(
      inputs.ledger.decode(dropped).left.toOption,
      Some(CodecError.Admission(AdmissionError.QuarantineScope(3, Vector(2, 3, 4, 5))))
    )
  }

  test("pinned: the same rejection dropped with the scope rewritten resolves cleanly") {
    val original = get(inputs.ledger.parse(StudyInputFixtures.ledgerVersionOne))
    val forged   = edited(_.filterNot(record(_) == 2).map(rescoped(Vector(3, 4, 5))))
    val resolved = get(resolve(utf8(forged), related = false))
    val ledger   = get(resolved.ledger(name("ledger")).toRight("ledger"))
    assertEquals(rejected(original), Vector(2, 3, 4, 5))
    assertEquals(rejected(ledger), Vector(3, 4, 5))
    // The carried source digest still names the original import: only a
    // re-import of the source file could show that record 2 is missing.
    assertEquals(ledger.source, original.source)
    assertEquals(ledger.outcome, AdmissionOutcome.Refused)
  }

  test("pinned: a standalone rejection dropped from a related ledger resolves cleanly") {
    // Two extra source rows rejected on their own, reviewed as exclusions.
    val extra = Vector(
      Vector("s3", "a", "encode", "0", "x", "0.5", "0", "100000", "100"),
      Vector("s3", "a", "encode", "1", "0.5")
    )
    val source = SourceRef.of(
      "matched-control.csv",
      StudyInputFixtures.header,
      StudyInputFixtures.records ++ extra
    )
    val complete = ManifestFixtures.ledger
    val reviewed = get(
      AdmissionLedger.of(
        source,
        StudyInputFixtures.header,
        complete.records ++ Vector(
          SourceRecord(
            50,
            Disposition.Rejected(
              extra(0),
              Some(StudyKey("s3", "a", "encode")),
              AdmissionReason.Number("x_px", "x", "a finite number")
            )
          ),
          SourceRecord(51, Disposition.Rejected(extra(1), None, AdmissionReason.Width(9, 5)))
        ),
        AdmissionOutcome.ReviewedExclusions
      )
    )
    val stored = get(inputs.ledger.encode(reviewed))
    assertEquals(
      get(resolve(utf8(stored), related = true)).ledger(name("ledger")).map(rejected),
      Some(Vector(50, 51))
    )
    // Record 51 dropped: the outcome stays consistent, the admitted records
    // still address every input trial, and nothing binds the rejected rows.
    val forged = stored.hcursor
      .downField("value")
      .downField("records")
      .withFocus(records =>
        Json.arr(records.asArray.toVector.flatten.filterNot(record(_) == 51)*)
      )
      .top
      .get
    val resolved = get(resolve(utf8(forged), related = true))
    val ledger   = get(resolved.ledger(name("ledger")).toRight("ledger"))
    assertEquals(rejected(ledger), Vector(50))
    assertEquals(ledger.source, source)
    assertEquals(ledger.checkAgainst(ManifestFixtures.input), Right(()))
  }
