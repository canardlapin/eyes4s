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

package eyes4s.laws

import eyes4s.codec.*
import eyes4s.kernel.*
import eyes4s.plan.*

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Published laws for storage-aware density archives.
  *
  * The caller supplies the storage providers and recomputer belonging to its
  * saved plan/input. The archive itself never owns effects or a payload cache.
  */
trait DensityArchiveLaws extends Laws:

  final case class Access[K, U <: Unit2D](
      payloads: PayloadRef => Option[VerifiedPayload],
      recompute: Option[(Int, K) => Either[DensityError[K], Mass[U]]]
  )

  type ViewReader[C, K, U <: Unit2D, P, S, D] =
    (C, DensityArchiveBundle[K, U, P, S, D], Int, K) => Either[DensityError[K], DensityView[U]]

  /** Encoding and materializing recovers the checked completed result. */
  def roundTrip[C, K, U <: Unit2D, P, S, D](
      codec: DensityArchiveCodec[K, U, P, S, D],
      cases: Gen[C],
      result: C => StudyResult[K, U, S, D],
      access: (C, DensityArchiveBundle[K, U, P, S, D]) => Access[K, U],
      storage: DensityStorage,
      equivalent: (StudyResult[K, U, S, D], StudyResult[K, U, S, D]) => Boolean
  ): RuleSet =
    new SimpleRuleSet(
      "densityArchive",
      "archive materializes to the checked result" -> forAll(cases) { value =>
        codec.encode(result(value), storage).exists { bundle =>
          val providers = access(value, bundle)
          bundle.archive.materialize(providers.payloads, providers.recompute).exists {
            restored =>
              equivalent(result(value), restored)
          }
        }
      }
    )

  /** Every readable map has the same storage-independent digest as its source
    * map under every storage policy. The view supplies the checked grid, copied
    * cells and provenance needed to rebuild a mass before hashing; the law does
    * not depend on chunk placement or the view's equality implementation.
    */
  def storageIndependentMaps[C, K, U <: Unit2D, P, S, D](
      codec: DensityArchiveCodec[K, U, P, S, D],
      cases: Gen[C],
      result: C => StudyResult[K, U, S, D],
      storages: Vector[DensityStorage],
      read: ViewReader[C, K, U, P, S, D]
  ): RuleSet =
    new SimpleRuleSet(
      "densityArchive.maps",
      "storage preserves each map identity" -> forAll(cases) { value =>
        val expected = result(value).scales.zipWithIndex.flatMap { (scale, scaleIndex) =>
          scale.estimation.collect { case (key, Right(mass)) => (scaleIndex, key, mass) }
        }
        storages.forall { storage =>
          codec.encode(result(value), storage).exists { bundle =>
            expected.forall { case (scale, key, mass) =>
              read(value, bundle, scale, key).exists { actual =>
                Surface.mass(actual.geometry.grid, actual.cells, actual.provenance) match
                  case Left(_)         => false
                  case Right(restored) =>
                    DensityDigest
                      .of(mass, result(value).description)
                      .flatMap { expected =>
                        DensityDigest
                          .of(restored, result(value).description)
                          .map(expected.sameAs)
                      }
                      .getOrElse(false)
              }
            }
          }
        }
      }
    )

end DensityArchiveLaws

object DensityArchiveLaws extends DensityArchiveLaws
