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

package eyes4s.studio.desktop.trial

import eyes4s.studio.app.compare.{
  ContentError,
  ContentFixation,
  TrialContent,
  TrialContentSource
}

import eyes4s.studio.core.assets.{AssetRegistry, TrialDisplay}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.document.ScreenSize
import eyes4s.studio.core.fixture.{GoldenAssets, StoryMoments}
import eyes4s.studio.core.selection.FixationIndex
import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.viz.trial.TrialFixation

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The golden fixture (fixtures/studio-golden) as the trial view reads it:
  * displays through the asset registry of story moment t2's dataset, stimuli
  * from `stimuli/`, and fixations from `fixations.csv`.
  *
  * Window membership uses the fixture's stated rule (half-open image frame
  * `448 <= x < 1472`, `156 <= y < 924`, README), standing in for the eyes4s
  * admission report until UI-A supplies it.
  */
object GoldenTrials:

  private def right[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  val buildRoot: Path = Paths.get(
    String(
      getClass.getClassLoader
        .getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
        .readAllBytes,
      UTF_8
    ).trim
  )

  val golden: Path  = buildRoot.resolve("fixtures/studio-golden")
  val stimuli: Path = golden.resolve("stimuli")
  val bundle: Path  = buildRoot.resolve("fixtures/studio-bundles/example.eyes")

  lazy val registry: AssetRegistry =
    val dataset = right(StoryMoments.t2).datasets.last
    right(GoldenAssets.registry(dataset))

  def screen: ScreenSize = registry.screen

  def key(participant: String, trial: String): TrialKey =
    val phase = if trial.startsWith("enc") then Phase.Encoding else Phase.Retrieval
    TrialKey(participant, phase, trial, 1)

  def display(participant: String, trial: String): TrialDisplay =
    val k = key(participant, trial)
    registry.display(k).getOrElse(sys.error(s"no display for ${k.label}"))

  private lazy val records: Vector[Array[String]] =
    Files
      .readAllLines(golden.resolve("fixations.csv"), UTF_8)
      .asScala
      .toVector
      .drop(1)
      .map(_.split(",", -1))

  /** The fixations of one trial as Compare's content port carries them,
    * with their onsets, in record order.
    */
  def contentFixations(participant: String, trial: String): Vector[ContentFixation] =
    val onsets = records.filter(r => r(0) == participant && r(2) == trial).map(_(7).toInt)
    val k      = key(participant, trial)
    fixations(participant, trial).zip(onsets).map { (f, onset) =>
      ContentFixation(k, f.index, f.screenX, f.screenY, onset, f.durationMs, f.placement)
    }

  /** fixtures/studio-golden behind Compare's trial content port: the
    * registry's display and the trial's fixations, for any analysis revision.
    */
  val contentSource: TrialContentSource = (_, key, done) =>
    done(
      registry
        .display(key)
        .toRight(ContentError.NotServed(key))
        .map(d => TrialContent(d, screen, contentFixations(key.participant, key.trial)))
    )

  /** The fixations of one trial, in record order. */
  def fixations(participant: String, trial: String): Vector[TrialFixation] =
    val k = key(participant, trial)
    records
      .filter(r => r(0) == participant && r(2) == trial)
      .map { r =>
        val x      = r(5).toDouble
        val y      = r(6).toDouble
        val inside = x >= 448 && x < 1472 && y >= 156 && y < 924
        TrialFixation
          .of(
            k,
            FixationIndex.of(r(4).toInt).fold(e => sys.error(e.message), identity),
            x,
            y,
            r(8).toInt,
            if inside then MapPlacement.InMap
            else MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)
          )
          .fold(e => sys.error(e.message), identity)
      }
