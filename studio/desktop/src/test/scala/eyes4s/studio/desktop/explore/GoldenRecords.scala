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

package eyes4s.studio.desktop.explore

import eyes4s.studio.app.explore.*
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, StudioRef}
import eyes4s.studio.desktop.trial.GoldenTrials

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

/** fixtures/studio-golden/fixations.csv behind the source records port, as
  * a fake backend would serve it until the wire carries it: each data record
  * with its verbatim line (lines end at LF only, as core's RecordLines
  * counts them), its image-frame position (the window at (448, 156)), its
  * degrees from the window's centre at the board's declared 35 px/°, x right
  * and y up, and where it falls. Test scaffolding, not studio code.
  */
object GoldenRecords:

  private val window      = (448.0, 156.0, 1472.0, 924.0)
  private val screen      = (0.0, 0.0, 1920.0, 1080.0)
  private val pxPerDegree = 35.0

  /** The data lines, the header excluded, verbatim. */
  lazy val lines: Vector[String] =
    val text = String(Files.readAllBytes(GoldenTrials.golden.resolve("fixations.csv")), UTF_8)
    text.split("\n", -1).toVector.drop(1).filter(_.nonEmpty)

  private def inside(box: (Double, Double, Double, Double), x: Double, y: Double) =
    x >= box._1 && x < box._3 && y >= box._2 && y < box._4

  /** Data record `n` (from 1). */
  def row(n: Int): SourceRecordRow =
    val line   = lines(n - 1)
    val f      = line.split(",", -1)
    val phase  = if f(1) == "Encoding" then Phase.Encoding else Phase.Retrieval
    val trial  = TrialKey(f(0), phase, f(2), f(3).toInt)
    val index  = FixationIndex.of(f(4).toInt).toOption.get
    val record = RecordNumber.of(n).toOption.get
    val x      = f(5).toDouble
    val y      = f(6).toDouble
    val ix     = x - window._1
    val iy     = y - window._2
    val place  =
      if inside(window, x, y) then RecordPlace.Inside
      else if inside(screen, x, y) then RecordPlace.Outside
      else RecordPlace.OffScreen
    SourceRecordRow(
      record,
      StudioRef.SourceRecord(trial, Some(index), SourceRole.Fixations, record),
      Some(index),
      trial,
      f(4).toInt,
      f(7).toDouble,
      f(8).toDouble,
      f(5),
      f(6),
      Some(FramePosition(ix, iy)),
      Some(FramePosition((ix - 512.0) / pxPerDegree, (384.0 - iy) / pxPerDegree)),
      f.lift(9).flatMap(_.toIntOption),
      place,
      line
    )

  /** The port over the golden records; pages answer at once. */
  val source: SourceRecordsSource = (_, from, size, done) =>
    val total = lines.size
    val to    = math.min(total, from + size)
    done(
      Right(
        BackendAnswer.Answered(
          SourceRecordPage(from, total, (from until to).map(i => row(i + 1)).toVector)
        )
      )
    )
