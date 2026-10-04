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

import eyes4s.design.KeyDigest
import eyes4s.kernel.*
import eyes4s.plan.*

/** The execution cursor may invoke only the known, bounded evidence instances.
  * Inspect reference identity before reading any user-defined method, including
  * equality, unit labels and the import description's lazy digest.
  */
private[io] object LedgerExecutionEvidence:
  enum Unsupported derives CanEqual:
    case KeyColumns, KeyDigest, UnitLabel

  def qualify[K, U <: Unit2D](
      spec: ImportSpec[K, U]
  ): Either[Unsupported, Unit] =
    val keys = spec.keys match
      case SourceKeyColumns.Custom(_, _, _) => Left(Unsupported.KeyColumns)
      case SourceKeyColumns.Study(_, _, _)  =>
        Either.cond(spec.keyDigest eq summon[KeyDigest[StudyKey]], (), Unsupported.KeyDigest)
      case SourceKeyColumns.Trial(_, _, _, _, _) =>
        Either.cond(spec.keyDigest eq summon[KeyDigest[TrialKey]], (), Unsupported.KeyDigest)
    keys.flatMap { _ =>
      val unit = spec.unit
      Either.cond(
        (unit eq summon[UnitLabel[Unit2D.Px]]) ||
          (unit eq summon[UnitLabel[Unit2D.Deg]]) ||
          (unit eq summon[UnitLabel[Unit2D.Norm]]) ||
          (unit eq summon[UnitLabel[Unit2D.Mm]]),
        (),
        Unsupported.UnitLabel
      )
    }
