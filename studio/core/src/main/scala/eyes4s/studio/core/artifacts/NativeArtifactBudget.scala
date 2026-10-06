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

/** Whole-package limits are explicit because RunStore persists complete archives. */
final case class NativeArtifactBudget private (
    maxDensityCells: Long,
    maxRows: Long,
    maxEntries: Int,
    maxTotalBytes: Long
) derives CanEqual

object NativeArtifactBudget:
  val Default: NativeArtifactBudget =
    new NativeArtifactBudget(20_000_000L, 1_000_000L, 512, 256L * 1024 * 1024)
  def of(
      maxDensityCells: Long = Default.maxDensityCells,
      maxRows: Long = Default.maxRows,
      maxEntries: Int = Default.maxEntries,
      maxTotalBytes: Long = Default.maxTotalBytes
  ): Either[NativeArtifactError, NativeArtifactBudget] =
    Vector(
      "density cells" -> BigInt(maxDensityCells),
      "rows"          -> BigInt(maxRows),
      "entries"       -> BigInt(maxEntries),
      "total bytes"   -> BigInt(maxTotalBytes)
    )
      .collectFirst {
        case (field, value) if value <= 0 => NativeArtifactError.InvalidBudget(field, value)
      }
      .toLeft(new NativeArtifactBudget(maxDensityCells, maxRows, maxEntries, maxTotalBytes))
