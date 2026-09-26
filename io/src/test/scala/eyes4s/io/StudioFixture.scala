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

/** A small deterministic stand-in for the Eyes Studio mock study (seed
  * 20260925): 24 participants, each encoding 20 items and retrieving them in
  * another order, as a fixation table and a trial inventory.
  *
  * It reproduces the Studio fixture's design and counts, not its random
  * positions or scores. The acceptance suite reads it only through
  * [[StudioFixture.Source]], so it can be pointed at the canonical
  * `fixtures/studio-golden/{fixations,trials}.csv` without changing its
  * assertions.
  */
object StudioFixture:
  /** What the acceptance suite reads: the fixation table and the inventory
    * of trials by participant and trial label.
    */
  final case class Source(fixations: String, inventory: Vector[(String, String)])

  val Seed = 20260925L

  private val categories = Vector(
    "beach",
    "forest",
    "kitchen",
    "street",
    "garden",
    "office",
    "dog",
    "bridge",
    "market",
    "tower",
    "harbor",
    "library",
    "desert",
    "church",
    "station",
    "field",
    "cafe",
    "museum",
    "canyon",
    "bakery",
    "stadium",
    "lake",
    "alley",
    "farm"
  )
  private val pool =
    Vector.tabulate(240)(i => f"${categories(i % 24)}-${(i * 37) % 900 + 7}%03d")
  private val p17Items = Vector(
    "street-112",
    "dog-077",
    "office-203",
    "kitchen-017",
    "forest-008",
    "garden-054",
    "beach-042",
    "bridge-019",
    "market-066",
    "tower-150",
    "harbor-231",
    "library-388",
    "desert-305",
    "church-417",
    "station-529",
    "field-613",
    "cafe-702",
    "museum-814",
    "canyon-126",
    "bakery-240"
  )
  val participants: Vector[String] = Vector.tabulate(24)(i => f"P${i + 1}%02d")

  def items(participant: Int): Vector[String] =
    if participants(participant) == "P17" then p17Items
    else Vector.tabulate(20)(j => pool((participant * 10 + j) % 240))

  /** Encoding order differs from retrieval order: item k is encoded here. */
  def encodingTrial(k: Int): String  = f"enc_${(k * 7) % 20 + 1}%02d"
  def retrievalTrial(k: Int): String = f"ret_${k + 1}%02d"

  enum Cause:
    case Overlap, DuplicateOrdinals, RejectedRecords, NoFixations

  /** Trials quarantined by the importer, with the Studio cause. */
  val quarantined: Map[(String, String), Cause] = Map(
    ("P03", "enc_11") -> Cause.Overlap,
    ("P08", "enc_04") -> Cause.NoFixations,
    ("P11", "enc_16") -> Cause.DuplicateOrdinals,
    ("P14", "enc_02") -> Cause.RejectedRecords,
    ("P21", "enc_09") -> Cause.Overlap,
    ("P02", "ret_05") -> Cause.NoFixations,
    ("P04", "ret_12") -> Cause.Overlap,
    ("P06", "ret_03") -> Cause.NoFixations,
    ("P09", "ret_18") -> Cause.DuplicateOrdinals,
    ("P12", "ret_07") -> Cause.Overlap,
    ("P13", "ret_15") -> Cause.NoFixations,
    ("P16", "ret_01") -> Cause.DuplicateOrdinals,
    ("P19", "ret_10") -> Cause.Overlap,
    ("P20", "ret_06") -> Cause.NoFixations,
    ("P22", "ret_13") -> Cause.RejectedRecords,
    ("P23", "ret_19") -> Cause.Overlap,
    ("P24", "ret_08") -> Cause.DuplicateOrdinals
  )

  /** Trials in the inventory without any fixation record. */
  val absent: Set[(String, String)] = Set(
    ("P07", "enc_13"),
    ("P10", "enc_18"),
    ("P15", "enc_05"),
    ("P18", "enc_12"),
    ("P01", "ret_17"),
    ("P17", "ret_09")
  )

  /** Retrieval trials whose fixations all fall outside the image. */
  val offImage: Set[(String, String)] =
    Set(("P05", "ret_04"), ("P05", "ret_11"), ("P05", "ret_16"))

  val focusQuery   = ("P17", "ret_07")
  val focusMatched = ("P17", "enc_03")

  private final case class Trial(
      participant: String,
      phase: String,
      trial: String,
      item: String
  ):
    def id: (String, String) = (participant, trial)

  private val trials: Vector[Trial] = participants.indices.toVector.flatMap { pi =>
    val p = participants(pi)
    items(pi).zipWithIndex.flatMap { (item, k) =>
      Vector(
        Trial(p, "encoding", encodingTrial(k), item),
        Trial(p, "retrieval", retrievalTrial(k), item)
      )
    }
  }

  val inventory: Vector[(String, String)] = trials.map(_.id)

  /** Records per trial: 12, 11 for the off-image queries, 13 for the focus
    * match, and one more for enough other trials to make 11,520.
    */
  private val recorded                           = trials.filterNot(t => absent.contains(t.id))
  private val counts: Map[(String, String), Int] =
    val pinned = offImage.map(_ -> 11).toMap + (focusMatched -> 13) + (focusQuery -> 12)
    val base   = recorded.map(t => t.id -> pinned.getOrElse(t.id, 12)).toMap
    val free   = recorded.map(_.id).filterNot(pinned.contains).sorted
    val short  = 11520 - base.values.sum
    val extra = Iterator.iterate(0)(i => (i + 37) % free.size).take(free.size).toVector.distinct
    base ++ extra.take(short).map(i => free(i) -> (base(free(i)) + 1))

  /** Fixations outside the image: all of the off-image queries, one each in
    * the focus pair, and 508 more in 404 admitted trials (104 with two).
    */
  private val outside: Map[(String, String), Int] =
    val pinned = offImage.map(id => id -> 11).toMap + (focusMatched -> 1) + (focusQuery -> 1)
    val candidates = recorded
      .map(_.id)
      .filterNot(id => pinned.contains(id) || quarantined.contains(id))
      .sorted
    val order =
      Iterator
        .iterate(0)(i => (i + 53) % candidates.size)
        .take(candidates.size)
        .toVector
        .distinct
    pinned ++ order
      .take(404)
      .zipWithIndex
      .map((i, n) => candidates(i) -> (if n < 104 then 2 else 1))

  /** The fixation table, in trial order. */
  lazy val fixations: String =
    val rng    = new scala.util.Random(Seed)
    val header = Vector(
      "participant",
      "phase",
      "trial",
      "occurrence",
      "item",
      "fixation",
      "x_px",
      "y_px",
      "onset_ms",
      "duration_ms",
      "samples"
    )
    val rows = recorded.flatMap { t =>
      val n         = counts(t.id)
      val out       = outside.getOrElse(t.id, 0)
      val cause     = quarantined.get(t.id)
      val durations = Vector.tabulate(n) { i =>
        if t.id == focusQuery then (if i == n - 1 then 100 else 218)
        else if t.id == focusMatched then (if i == n - 1 then 90 else 243)
        else 200
      }
      var onset = 0
      Vector.tabulate(n) { i =>
        val inImage = i < n - out
        val x       = if inImage then 468 + rng.nextInt(980) else 20 + rng.nextInt(380)
        val y       = if inImage then 176 + rng.nextInt(720) else 20 + rng.nextInt(1040)
        val start   = cause match
          case Some(Cause.Overlap) if i == 1 => onset - durations(0) + 10
          case _                             => onset
        onset = start + durations(i) + 50
        val ordinal  = if cause.contains(Cause.DuplicateOrdinals) && i == 1 then 0 else i
        val duration =
          cause match
            case Some(Cause.RejectedRecords | Cause.NoFixations) if i == 0 => "-5"
            case _ => durations(i).toString
        Vector(
          t.participant,
          t.phase,
          t.trial,
          "1",
          t.item,
          ordinal.toString,
          x.toString,
          y.toString,
          start.toString,
          duration,
          "10"
        )
      }
    }
    Rfc4180.encode(header +: rows)

  def source: Source = Source(fixations, inventory)
