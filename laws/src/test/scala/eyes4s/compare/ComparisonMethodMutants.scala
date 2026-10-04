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

package eyes4s.compare

import eyes4s.kernel.*

/** A package-local constructor makes a test method possible without adding it
  * to the production registry. Its distance scores are symmetric, within the
  * declared scale, and described correctly, but are not higher-is-closer.
  */
object ComparisonMethodMutants:
  val distanceLike: MapSimilarityMethod =
    new MapSimilarityMethod.SymmetricMethod("DistanceLikeMutant"):
      def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
        val distance = Distribution.totalVariation[U]
        new SymmetricCompare[Mass[U], Similarity]:
          val info: MeasureInfo = distance.info.copy(scale = MeasureScale.DistanceLike)
          def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
            distance.compare(a, b).flatMap(d => Similarity.computed(info.name, d.value))
