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
import eyes4s.kernel.Unit2D

/** Replay one ledger from an already byte-verified manifest. Resolution by
  * itself does not grant this evidence. Legacy/source-less graphs are readable
  * but refused here. The synchronous replay has no cancellation guarantee.
  */
object ManifestReverification:
  def verify[K, U <: Unit2D](
      resolved: ResolvedManifest[K, U],
      ledgerName: ArtifactName
  ): Either[LedgerVerificationError, VerifiedAdmission[K, U]] =
    def one[A](kind: String, values: Vector[A]): Either[LedgerVerificationError, A] =
      values match
        case Vector(value) => Right(value)
        case _             =>
          Left(LedgerVerificationError.ManifestBinding(ledgerName.value, kind, values.size))
    val sources = resolved.manifest.relations.collect {
      case r @ ManifestRelation.LedgerSource(ledger, _, _, _) if ledger == ledgerName => r
    }
    for
      ledger    <- one("ledger", resolved.ledger(ledgerName).toVector)
      primary   <- one("primary source", sources.filter(_.role == LedgerSourceRole.Primary))
      spec      <- one("import spec", resolved.importSpec(primary.importSpec).toVector)
      contents  <- one("source file", resolved.sourceFile(primary.sourceFile).toVector)
      inputName <- one(
        "ledger-of",
        resolved.manifest.relations.collect {
          case ManifestRelation.LedgerOf(ledger, input) if ledger == ledgerName => input
        }
      )
      input     <- one("input", resolved.input(inputName).toVector)
      inventory <-
        if spec.inventory.isEmpty then Right(None)
        else
          for
            source <- one(
              "inventory source",
              sources.filter(_.role == LedgerSourceRole.TrialInventory)
            )
            text <- one(
              "inventory source file",
              resolved.sourceFile(source.sourceFile).toVector
            )
          yield Some(source.sourceFile.value -> text)
      verified <- LedgerReverification.verify(
        primary.sourceFile.value,
        contents,
        spec,
        ledger,
        input,
        inventory
      )
    yield verified
