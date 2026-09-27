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

package eyes4s.studio.core.importing

/** Trial key studies for S5.3, shaped like fixtures/studio-golden (studio-core
  * reads no files, so they are generated): 24 participants, each with
  * enc_01–enc_20 then ret_01–ret_20, 960 trials, occurrence 1 everywhere.
  *
  * The `blocks` variant is the board's occurrence case: for P01–P19, the
  * trials enc_19 and enc_20 are relabelled enc_01 and enc_02 in a second
  * block (Block 2), so 19 × 2 = 38 keys (participant, phase, trial) name two
  * presentations each, and 922 distinct keys remain.
  */
object KeyStudies:

  val participants: Vector[String] = (1 to 24).toVector.map(i => f"P$i%02d")

  /** Records a trial has in the fixation table. */
  val recordsPerTrial: Int = 3

  /** One study trial: participant, phase, trial label, block. */
  final case class StudyTrial(participant: String, phase: String, trial: String, block: Int)

  private def label(prefix: String, n: Int): String = f"${prefix}_$n%02d"

  def trials(blocks: Boolean): Vector[StudyTrial] =
    participants.flatMap { p =>
      val relabel = blocks && p <= "P19"
      val enc     = (1 to 20).map { n =>
        if relabel && n >= 19 then StudyTrial(p, "Encoding", label("enc", n - 18), 2)
        else StudyTrial(p, "Encoding", label("enc", n), 1)
      }
      val ret = (1 to 20).map(n => StudyTrial(p, "Retrieval", label("ret", n), 1))
      enc ++ ret
    }

  /** The 38 keys of the `blocks` variant that name two presentations. */
  val repeatedKeys: Vector[KeyLabel] =
    participants
      .filter(_ <= "P19")
      .flatMap(p =>
        Vector(KeyLabel(p, "Encoding", "enc_01"), KeyLabel(p, "Encoding", "enc_02"))
      )

  /** A fixation table with a `Block` column (the board's header names). */
  def fixations(blocks: Boolean): String =
    val header = "Subject,Phase,TrialID,Block,FixNum,FixX,FixY,FixStart,FixDur,NSamples"
    val rows   = trials(blocks).flatMap { t =>
      (1 to recordsPerTrial).map(o =>
        s"${t.participant},${t.phase},${t.trial},${t.block},$o,960.0,540.0,${o * 300},200,100"
      )
    }
    (header +: rows).mkString("", "\n", "\n")

  /** The trial inventory of the same trials, with a `Block` column. */
  def inventory(blocks: Boolean): String =
    val header = "participant,phase,trial,Block,item"
    val rows   = trials(blocks).map(t =>
      s"${t.participant},${t.phase},${t.trial},${t.block},item-${t.participant}-${t.trial}"
    )
    (header +: rows).mkString("", "\n", "\n")
