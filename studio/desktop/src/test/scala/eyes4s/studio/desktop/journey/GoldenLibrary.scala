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

package eyes4s.studio.desktop.journey

import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.plan.{
  AttributeColumn,
  AttributeKind,
  QuarantineCause,
  SampleCountRule,
  TrialDisposition,
  WindowTally
}
import eyes4s.studio.core.document.{ColumnRole, InventoryMapping}
import eyes4s.studio.core.fixture.{GoldenCsv, StoryMoments}

/** One trial's fixations against the analysis window, as eyes4s tallies them. */
final case class TrialWindow(fixations: Int, outside: Int, outsideShare: Option[Double])
    derives CanEqual

/** The focus fixation as eyes4s reads it: its record (from 1, the header
  * excluded), its screen, image-frame and angular positions, its onset and
  * duration in ms.
  */
final case class FocusFixation(
    record: Int,
    screen: (Double, Double),
    image: (Double, Double),
    degrees: (Double, Double),
    onsetMs: Double,
    durationMs: Double
) derives CanEqual

/** The resolved design as eyes4s's pairing makes it: the retrieval queries
  * requested, those not admitted, those with no admitted matched reference,
  * the eligible ones; the Cartesian candidate pairs per scale (480 × 480,
  * `candidatePairCount`); and the eligible pairs per scale, matched and
  * control (same participant, encoding, other item, admitted).
  */
final case class LibraryDesign(
    requested: Int,
    notAdmitted: Int,
    noMatch: Int,
    eligible: Int,
    candidatePairsPerScale: Long,
    pairsPerScale: Long,
    controlsPerQuery: Map[Int, Int]
) derives CanEqual

/** What eyes4s itself makes of fixtures/studio-golden (ticket S10.1's direct
  * library run): the inventory admission, record counts, the analysis
  * window's tallies, and the focus trials.
  */
final case class LibraryFacts(
    inventoryTrials: Int,
    admitted: Int,
    quarantined: Map[String, Int],
    noFixations: Int,
    absent: Int,
    fixationRecords: Int,
    outsideWindowRecords: Int,
    outsideWindowTrials: Int,
    outsideScreenRecords: Int,
    admittedRecords: Int,
    outsideDurationShare: Double,
    items: Int,
    imagesFound: Int,
    missingImages: Vector[String],
    design: LibraryDesign,
    allOutside: Vector[(String, String, Int)],
    ret07: TrialWindow,
    enc03: TrialWindow,
    focus: FocusFixation
) derives CanEqual

/** eyes4s run directly on the golden tables, with nothing of Studio's but the
  * story's r3 column mapping and the fixture README's declared geometry
  * (screen 1920×1080, image frame 1024×768 at (448, 156), 35 px/°): eyes4s-io
  * admits the fixation table against the trial inventory, and eyes4s's
  * `WindowTally` and kernel frames place each admitted fixation. JVM test
  * scope only.
  */
object GoldenLibrary:

  private def get[E, A](what: String)(e: Either[E, A]): A =
    e.fold(x => throw IllegalStateException(s"$what: $x"), identity)

  private val screen: Frame[Unit2D.Px] = get("screen")(Frame.screen("screen", 1920, 1080))

  private val window: Subframe[Unit2D.Px] = get("window")(
    Bounds
      .of[Unit2D.Px](448.0, 156.0, 448.0 + 1024.0, 156.0 + 768.0)
      .flatMap(Subframe.of(screen, FrameId("image"), _))
  )

  private val degrees: Warp[Unit2D.Px, Unit2D.Deg] = get("degrees")(
    LinearAngularScale
      .of(screen, 35.0)
      .flatMap(_.on(window))
      .flatMap(_.angular(FrameId("image/degrees")))
  )

  private val mapping: InventoryMapping =
    get("mapping")(StoryMoments.inventory(occurrence = true))

  private def name(role: ColumnRole): Option[String] = mapping.column(role).map(_.value)

  private val trialColumns = get("trial columns")(
    TrialColumns.of(
      name(ColumnRole.Participant).get,
      name(ColumnRole.Phase).get,
      name(ColumnRole.Trial).get,
      name(ColumnRole.Occurrence)
    )
  )

  private val inventoryColumns = get("inventory columns")(
    TrialInventoryColumns.of(
      trialColumns,
      name(ColumnRole.Item),
      name(ColumnRole.Response).map(AttributeColumn(_, AttributeKind.Text)).toVector ++
        mapping.coreAttributes
    )
  )

  private val table = get("table")(
    FixationTable.of(
      trialColumns,
      "ordinal",
      "x",
      "y",
      TimeColumns("onset_ms", "duration_ms", TimestampUnit.Milliseconds),
      SampleCountRule.PositiveColumn("sample_count")
    )
  )

  private def cause(c: QuarantineCause): String = c match
    case QuarantineCause.DuplicateOrdinals => "duplicate-ordinals"
    case QuarantineCause.RejectedRecords   => "rejected-records"
    case QuarantineCause.Overlap(_, _, _)  => "overlap"
    case other                             => other.toString

  private def designOf(trials: Vector[eyes4s.plan.InventoryTrial]): LibraryDesign =
    import eyes4s.design.{
      DirectedPairSchedule,
      Pairing,
      Projection,
      Relation,
      Trial,
      Trials,
      pair
    }
    def key(t: eyes4s.plan.InventoryTrial) =
      s"${t.identity.participant}|${t.identity.phase}|${t.identity.trial}"
    val byKey     = trials.map(t => key(t) -> t).toMap
    val queries   = trials.filter(_.identity.phase == "Retrieval")
    val encodings = trials.filter(_.identity.phase == "Encoding")
    def admitted(ts: Vector[eyes4s.plan.InventoryTrial]) =
      ts.filter(_.disposition == TrialDisposition.Admitted)
    def collection(ts: Vector[eyes4s.plan.InventoryTrial]) =
      Trials(ts.map(t => Trial(key(t), (), ())))
    val participant =
      Projection.named[String, String]("participant")(k => byKey(k).identity.participant)
    val item    = Projection.named[String, Option[String]]("item")(k => byKey(k).inventoryItem)
    val matched = pair(
      collection(admitted(queries)),
      collection(admitted(encodings)),
      Pairing.between[String, String].sameOn(participant, participant).sameOn(item, item).all
    )
    val eligible = admitted(queries).filterNot(q => matched.unmatchedLeft.contains(key(q)))
    val controls = pair(
      collection(eligible),
      collection(admitted(encodings)),
      Pairing
        .between[String, String]
        .sameOn(participant, participant)
        .differentOn(item, item)
        .all
    )
    val candidates = get("candidates")(
      DirectedPairSchedule.exhaustive(
        queries.map(key),
        encodings.map(key),
        Relation.all[String, String]
      )
    )
    LibraryDesign(
      requested = queries.size,
      notAdmitted = queries.size - admitted(queries).size,
      noMatch = matched.unmatchedLeft.size,
      eligible = eligible.size,
      candidatePairsPerScale = candidates.candidatePairCount,
      pairsPerScale = matched.eligiblePairCount + controls.eligiblePairCount,
      controlsPerQuery = controls.pairs
        .groupMapReduce(_._1.key)(_ => 1)(_ + _)
        .values
        .groupMapReduce(identity)(_ => 1)(_ + _)
    )

  /** The facts, computed once. */
  lazy val facts: LibraryFacts =
    val joined = get("admission")(
      FixationCsv.admitInventory(
        GoldenCsv.fixations,
        table,
        get("inventory")(TrialInventory.read(GoldenCsv.trials, inventoryColumns)),
        screen
      )
    )
    val trials                                = joined.trials
    def count(p: TrialDisposition => Boolean) = trials.count(t => p(t.disposition))
    val quarantined                           = trials
      .map(_.disposition)
      .collect { case TrialDisposition.Quarantined(c) => cause(c) }
      .groupMapReduce(identity)(_ => 1)(_ + _)
    val rows    = joined.fixations.accepted.rows
    val tallies = rows.map { row =>
      row.key -> get(s"tally ${row.key}")(WindowTally.window(window, row.value))
    }
    def trialOf(participant: String, trial: String) =
      rows
        .find(r => r.key.participant == participant && r.key.trial == trial)
        .getOrElse(throw IllegalStateException(s"no admitted $participant $trial"))
    def windowOf(participant: String, trial: String): TrialWindow =
      val tally = get("tally")(WindowTally.window(window, trialOf(participant, trial).value))
      TrialWindow(tally.total, tally.outsideWindow, tally.outsideWindowShare)
    // The focus fixation: P17 enc_03's sixth, and its record in file order.
    val sixth  = trialOf("P17", "enc_03").value.fixations(5)
    val image  = get("image")(window.enter(sixth.centre).toRight("outside the screen"))
    val angle  = get("degrees")(degrees(image).toRight("no degrees"))
    val lines  = GoldenCsv.fixations.linesIterator.toVector.drop(1)
    val record = lines.indexWhere(_.startsWith("P17,Encoding,enc_03,1,6,")) + 1
    // Each item's image in the fixture's stimuli folder, present or not.
    val stimuli = FixtureDoc.root.resolve("fixtures/studio-golden/stimuli")
    val images  = trials
      .flatMap(_.inventoryItem)
      .distinct
      .sorted
      .partition(i => java.nio.file.Files.isRegularFile(stimuli.resolve(s"$i.png")))
    LibraryFacts(
      inventoryTrials = trials.size,
      admitted = count(_ == TrialDisposition.Admitted),
      quarantined = quarantined,
      noFixations = count(_ == TrialDisposition.NoFixations),
      absent = count(_ == TrialDisposition.Absent),
      fixationRecords = trials.map(_.records.size).sum + joined.unlisted.size,
      outsideWindowRecords = tallies.map(_._2.outsideWindow).sum,
      outsideWindowTrials = tallies.count(_._2.outsideWindow > 0),
      outsideScreenRecords = tallies.map(_._2.outsideScreen).sum,
      admittedRecords = tallies.map(_._2.total).sum,
      outsideDurationShare = tallies.map(_._2.outsideWindowDuration.toMillis).sum /
        tallies.map(_._2.totalDuration.toMillis).sum,
      items = trials.flatMap(_.inventoryItem).distinct.size,
      imagesFound = images._1.size,
      missingImages = images._2,
      design = designOf(trials),
      allOutside = tallies
        .collect { case (k, t) if t.allOutside => (k.participant, k.trial, t.total) }
        .sortBy(x => (x._1, x._2)),
      ret07 = windowOf("P17", "ret_07"),
      enc03 = windowOf("P17", "enc_03"),
      focus = FocusFixation(
        record,
        (sixth.centre.x, sixth.centre.y),
        (image.x, image.y),
        (angle.x, angle.y),
        sixth.span.onset.toMillis,
        sixth.span.duration.toMillis
      )
    )
