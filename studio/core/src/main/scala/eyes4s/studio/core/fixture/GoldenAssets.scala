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

package eyes4s.studio.core.fixture

import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.{AssetFile, AssetRef, AssetRegistry, DisplayRow}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.document.DatasetRevisionSpec

/** A stimulus file of fixtures/studio-golden: its asset and byte length. */
final case class GoldenStimulus(asset: AssetRef, length: Long) derives CanEqual

/** The displays and stimuli of fixtures/studio-golden (ticket S2.10), read
  * at build time into [[GoldenInventory]]: trials.csv's display columns and
  * the name, SHA-256 and length of every file in stimuli/.
  */
object GoldenAssets:

  /** Every trial's display columns, in inventory order. */
  val rows: Either[String, Vector[DisplayRow]] =
    GoldenInventory.displays.linesIterator.toVector.traverse { line =>
      line.split("\t", -1).toList match
        case p :: phase :: trial :: occurrence :: item :: kind :: file :: Nil =>
          occurrence.toIntOption
            .toRight(s"display line '$line': bad occurrence")
            .map(o => DisplayRow(TrialKey(p, Phase(phase), trial, o), item, kind, file))
        case _ => Left(s"display line '$line' does not have 7 fields")
    }

  /** Every stimulus file present, by name. */
  val stimuli: Either[String, Vector[GoldenStimulus]] =
    GoldenInventory.stimuli.linesIterator.toVector.traverse { line =>
      line.split("\t", -1).toList match
        case name :: hex :: length :: Nil =>
          for
            file   <- AssetFile.of(name).leftMap(_.message)
            digest <- ByteDigest.parse(hex).leftMap(_.message)
            bytes  <- length.toLongOption.toRight(s"stimulus line '$line': bad length")
          yield GoldenStimulus(AssetRef(file, digest), bytes)
        case _ => Left(s"stimulus line '$line' does not have 3 fields")
    }

  /** The registry of a dataset revision imported from fixtures/studio-golden,
    * with every stimulus present stored: 257 of 259 images present,
    * forest-044.png and kitchen-081.png missing.
    */
  def registry(dataset: DatasetRevisionSpec): Either[String, AssetRegistry] =
    for
      rs       <- rows
      stored   <- stimuli
      registry <- AssetRegistry.fromRows(dataset, rs, stored.map(_.asset)).leftMap(_.message)
    yield registry
