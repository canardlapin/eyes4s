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
import eyes4s.kernel.ClockId
import eyes4s.plan.*

/** Key readers whose participant is the study layout's.
  *
  * A participant-scoped correction rule is resolved twice: when the table is
  * admitted (through the key reader's participant) and when coordinate
  * provenance names the rule that moved a trial (through the plan layout's
  * participant). The built-in readers ([[FixationKeyReader.study]] and
  * [[FixationKeyReader.trial]]) and layouts both read the key's
  * `participant`. For a custom key, build the reader here, so both resolve
  * through the one projection the layout declares.
  */
object LayoutKeys:
  def reader[K: Ordering](layout: StudyLayout[K], columns: Vector[String])(
      read: Map[String, String] => Either[String, K],
      clock: K => ClockId
  ): Either[FixationImportError, FixationKeyReader[K]] =
    given KeyDigest[K] = layout.digest
    FixationKeyReader.withParticipant(columns)(read, clock, layout.participant.apply)
