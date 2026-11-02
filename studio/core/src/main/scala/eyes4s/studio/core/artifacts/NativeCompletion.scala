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

package eyes4s.studio.core.artifacts

import eyes4s.codec.ArtifactName
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.runs.RunArchive

/** One storage and publication boundary, retaining each scientific kind's verifier. */
enum NativeCompletionFacts derives CanEqual:
  case Timed(value: NativeBindingFacts)
  case Locations(value: LocationBindingFacts)
  def run: RunId = this match
    case Timed(value)     => value.run
    case Locations(value) => value.run

  def stamp: eyes4s.studio.core.execution.ExecutionStamp = this match
    case Timed(value)     => value.stamp
    case Locations(value) =>
      eyes4s.studio.core.execution.LocationRunStamp(
        value.analysis.id,
        value.dataset.id,
        value.analysis.plan,
        value.analysis.recipe.input
      )

enum NativeCompletion:
  case Timed(value: NativeArtifactPackage)
  case Locations(value: LocationNativePackage)

  def facts: NativeCompletionFacts = this match
    case Timed(value)     => NativeCompletionFacts.Timed(value.facts)
    case Locations(value) => NativeCompletionFacts.Locations(value.facts)
  def archive: RunArchive = this match
    case Timed(value)     => value.archive
    case Locations(value) => value.archive
  def files: Vector[(ArtifactName, IndexedSeq[Byte])] = this match
    case Timed(value)     => value.files
    case Locations(value) => value.files
  def budget: NativeArtifactBudget = this match
    case Timed(value)     => value.budget
    case Locations(value) => value.budget
  def bytes(name: ArtifactName): Option[IndexedSeq[Byte]] = files.collectFirst {
    case (`name`, data) => data
  }

object NativeCompletion:
  def marked(names: Vector[ArtifactName]): Boolean =
    names.contains(NativeArtifactPackage.FactsName) || names.contains(
      LocationNativePackage.FactsName
    ) || names.contains(LocationNativePackage.ResultName)

  def verify(
      files: Vector[(ArtifactName, IndexedSeq[Byte])],
      budget: NativeArtifactBudget = NativeArtifactBudget.Default
  ): Either[NativeArtifactError, NativeCompletion] =
    if files.exists((name, _) =>
        name == LocationNativePackage.FactsName || name == LocationNativePackage.ResultName
      )
    then LocationNativePackage.verify(files, budget).map(Locations.apply)
    else NativeArtifactPackage.verify(files, budget).map(Timed.apply)
