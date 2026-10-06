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
import eyes4s.studio.core.backend.*
import io.circe.{ACursor, Codec, Decoder, DecodingFailure, Json}

/** One control reference of the focus query and its 2° score. */
final case class ControlScore(trial: String, item: String, score: Double) derives CanEqual

/** One retrieval query of `docs/studio/fixture/fixture.json`, as written there. */
final case class MockQuery(
    participant: String,
    trial: String,
    item: String,
    matchTrial: String,
    response: Response,
    controls: Option[Int],
    status: String,
    reason: Option[String],
    m: Option[Vector[Double]],
    b: Option[Vector[Double]],
    d: Option[Vector[Double]],
    controlScores2deg: Vector[ControlScore]
) derives CanEqual:
  def key: TrialKey        = MockStudy.key(participant, trial)
  def matchedKey: TrialKey = MockStudy.key(participant, matchTrial)

// Illustrative fixture metadata. These rounded means are not result protocol
// values: native reports and fake reports both read their retained query rows.
/** One reporting group's means of M (matched), B (control) and D = M − B. */
final case class GroupMeans(label: Response, n: Int, m: Double, b: Double, d: Double)
    derives CanEqual,
      Codec.AsObject

final case class ScoreMeans(m: Double, b: Double, d: Double, dByScale: Vector[Double])
    derives CanEqual,
      Codec.AsObject

final case class ParticipantSummary(
    participant: String,
    requested: Int,
    contributing: Int,
    failed: Int,
    noMatch: Int,
    notAdmitted: Int,
    all: ScoreMeans,
    groups: Vector[GroupMeans]
) derives CanEqual,
      Codec.AsObject

/** Grand means of one reporting group over participant means. `attribute`
  * names the inventory attribute the groups split on.
  */
final case class GroupSummary(
    attribute: String,
    label: Response,
    n: Int,
    d: Double,
    dByScale: Vector[Double]
) derives CanEqual,
      Codec.AsObject

/** The illustrative summary metadata of fixture.json, including rounded
  * display examples. ResultSummary serves only its scale-free run facts.
  */
final case class MockSummary(
    inventoryTrials: Int,
    admitted: Int,
    quarantined: Int,
    absent: Int,
    quarantineBySlug: Vector[(String, Int)],
    fixationRecords: Int,
    itemsInPool: Int,
    imagesFound: Int,
    imagesMissing: Int,
    missingImages: Vector[MissingImage],
    contrasts: QueryContrasts,
    pairRowsPerScale: Long,
    pairRowsAllScales: Long,
    pairRowsRev5: Long,
    scales: Vector[String],
    grandD: Double,
    grandDByScale: Vector[Double],
    groups: Vector[GroupSummary],
    pairedN: Int,
    groupNRange: (Int, Int),
    outsideWindowRecords: Int,
    outsideWindowTrials: Int,
    eligibleQueries: Int,
    candidatePairsPerScale: Long,
    /** Admitted retrieval and encoding trials: the candidate population
      * (eyes4s candidatePairCount = focalTrials × referenceTrials).
      */
    focalTrials: Int,
    referenceTrials: Int,
    datasetHistory: Map[String, String],
    runs: Vector[(String, String)],
    participants: Vector[ParticipantSummary]
) derives CanEqual

/** docs/studio/fixture/fixture.json, decoded, and fixtures/studio-golden's
  * inventory and admitted scanpaths. All are embedded at build time
  * (project/StudioFixture.scala), so they load without file I/O on every
  * platform.
  */
final case class MockStudy(
    queries: Vector[MockQuery],
    summary: MockSummary,
    inventory: Vector[LedgerEntry],
    scanpaths: Map[TrialKey, Vector[ScanpathRecord]]
) derives CanEqual:

  /** A query's control references: every admitted encoding trial of another
    * item from its participant, in inventory order, with each one's item;
    * none when the pool disagrees with the query's served control count.
    */
  def controls(q: MockQuery): Option[Vector[(TrialKey, String)]] =
    val pool = inventory.collect {
      case e
          if e.trial.participant == q.participant && e.trial.phase == Phase.Encoding &&
            e.disposition == TrialDisposition.Admitted && e.item != q.item =>
        (e.trial, e.item)
    }
    Option.when(q.controls.contains(pool.size))(pool)

object MockStudy:

  /** Every fixture trial has occurrence 1 (FIXTURE.md). */
  val Occurrence: Int = 1

  /** The inventory attribute the reporting groups split on. */
  val GroupAttribute: String = "response"

  /** The fixture names encoding trials `enc_NN` and retrieval trials `ret_NN`. */
  def phaseOf(trial: String): Phase =
    if trial.startsWith("enc_") then Phase.Encoding else Phase.Retrieval

  def key(participant: String, trial: String): TrialKey =
    TrialKey(participant, phaseOf(trial), trial, Occurrence)

  /** The exact text of fixture.json. */
  def fixtureText: String = FixtureJson.text

  lazy val load: Either[String, MockStudy] =
    assemble(FixtureJson.text, GoldenInventory.trials, GoldenInventory.scanpaths)

  /** The study from fixture.json's text, the generated inventory lines and
    * the generated scanpath lines; refuses the first part it cannot read,
    * naming it.
    */
  private[fixture] def assemble(
      fixture: String,
      inventoryLines: String,
      scanpathLines: String
  ): Either[String, MockStudy] =
    for
      json      <- io.circe.parser.parse(fixture).leftMap(_.message)
      queries   <- decodeQueries(json)
      inventory <- parseInventory(inventoryLines)
      scanpaths <- FakeNavigator.parseScanpaths(scanpathLines)
      labels = inventory.flatMap(_.response).distinct
      summary <- decodeSummary(json.hcursor.downField("summary"), labels).leftMap(_.getMessage)
      _       <- queries.traverse_ { q =>
        val lengths = Vector(q.m, q.b, q.d).flatten.map(_.size)
        Either.cond(
          lengths.forall(_ == summary.scales.size),
          (),
          s"${q.participant} ${q.trial} has scores for ${lengths.mkString("/")} scales, " +
            s"not ${summary.scales.size}"
        )
      }
    yield MockStudy(queries, summary, inventory, scanpaths)

  // -------------------------------------------------------------------------

  private def decodeQueries(json: Json): Either[String, Vector[MockQuery]] =
    json.hcursor
      .downField("participants")
      .as[Vector[Json]]
      .flatMap(_.flatTraverse { p =>
        val c = p.hcursor
        for
          id  <- c.get[String]("id")
          qs  <- c.get[Vector[Json]]("queries")
          out <- qs.traverse { q =>
            val h = q.hcursor
            for
              trial    <- h.get[String]("trial")
              item     <- h.get[String]("item")
              matched  <- h.get[String]("match")
              response <- h.get[Response]("response")
              controls <- h.get[Option[Int]]("controls")
              status   <- h.get[String]("status")
              reason   <- h.get[Option[String]]("reason")
              m        <- h.get[Option[Vector[Double]]]("M")
              b        <- h.get[Option[Vector[Double]]]("B")
              d        <- h.get[Option[Vector[Double]]]("D")
              scores   <- h
                .get[Option[Vector[Json]]]("control_scores_2deg")
                .flatMap(_.getOrElse(Vector.empty).traverse { s =>
                  val sc = s.hcursor
                  (sc.get[String]("trial"), sc.get[String]("item"), sc.get[Double]("cos"))
                    .mapN(ControlScore.apply)
                })
            yield MockQuery(
              id,
              trial,
              item,
              matched,
              response,
              controls,
              status,
              reason,
              m,
              b,
              d,
              scores
            )
          }
        yield out
      })
      .leftMap(_.getMessage)

  private def groupMeans(c: ACursor, label: Response): Decoder.Result[GroupMeans] =
    val g = c.downField(label.label)
    (g.get[Int]("n"), g.get[Double]("M"), g.get[Double]("B"), g.get[Double]("D"))
      .mapN(GroupMeans(label, _, _, _, _))

  private def participant(
      json: Json,
      labels: Vector[Response]
  ): Decoder.Result[ParticipantSummary] =
    val c   = json.hcursor
    val all = c.downField("all")
    (
      c.get[String]("id"),
      c.get[Int]("requested"),
      c.get[Int]("contributing"),
      c.get[Int]("failed"),
      c.get[Int]("no_match"),
      c.get[Int]("not_admitted"),
      (
        all.get[Double]("M"),
        all.get[Double]("B"),
        all.get[Double]("D"),
        all.get[Vector[Double]]("D_by_scale")
      ).mapN(ScoreMeans.apply),
      labels.traverse(groupMeans(c, _))
    ).mapN(ParticipantSummary.apply)

  private def decodeSummary(c: ACursor, labels: Vector[Response]): Decoder.Result[MockSummary] =
    val qc = c.downField("query_contrasts")
    for
      causes  <- c.get[Map[String, Int]]("quarantine_by_cause").map(_.toVector.sortBy(_._1))
      missing <- c
        .get[Map[String, Json]]("missing_images")
        .flatMap(_.toVector.sortBy(_._1).traverse { case (item, j) =>
          (j.hcursor.get[Vector[String]]("participants"), j.hcursor.get[Int]("encoding_trials"))
            .mapN(MissingImage(item, _, _))
        })
      contrasts <- (
        qc.get[Int]("requested"),
        qc.get[Int]("query_not_admitted"),
        qc.get[Int]("no_match"),
        qc.get[Int]("failed"),
        qc.get[Int]("contributing")
      ).mapN(QueryContrasts.apply)
      participants <- c
        .get[Vector[Json]]("participants")
        .flatMap(_.traverse(participant(_, labels)))
      range <- c.get[Vector[Int]]("paired_group_n_range").flatMap {
        case Vector(lo, hi) => Right((lo, hi))
        case other          => Left(DecodingFailure(s"range $other is not a pair", c.history))
      }
      groups <- labels.traverse { l =>
        (
          c.get[Int](s"n_${l.label}"),
          c.get[Double](s"grand_D_${l.label}"),
          c.get[Vector[Double]](s"grand_D_by_scale_${l.label}")
        ).mapN(GroupSummary(GroupAttribute, l, _, _, _))
      }
      runs   <- c.get[Map[String, String]]("runs").map(_.toVector.sortBy(_._1))
      counts <- (
        c.get[Int]("inventory_trials"),
        c.get[Int]("admitted"),
        c.get[Int]("quarantined"),
        c.get[Int]("absent"),
        c.get[Int]("fixation_records"),
        c.get[Int]("items_in_pool"),
        c.get[Int]("images_found"),
        c.get[Int]("images_missing")
      ).tupled
      rows <- (
        c.get[Long]("pair_rows_per_scale"),
        c.get[Long]("pair_rows_all_scales"),
        c.get[Long]("pair_rows_rev5"),
        c.get[Vector[String]]("scales"),
        c.get[Long]("candidate_pairs_cartesian_per_scale"),
        c.get[Int]("eligible_queries")
      ).tupled
      population <- (
        c.get[Int]("candidate_focal_admitted"),
        c.get[Int]("candidate_reference_admitted")
      ).tupled
      rest <- (
        c.get[Double]("grand_D_all"),
        c.get[Vector[Double]]("grand_D_by_scale"),
        c.get[Int]("n_paired"),
        c.get[Int]("outside_window_records"),
        c.get[Int]("outside_window_trials"),
        c.get[Map[String, String]]("dataset_history")
      ).tupled
    yield
      val (inv, adm, qua, abs, recs, items, found, miss)        = counts
      val (perScale, all, rev5, scales, cartesian, eligible)    = rows
      val (grand, byScale, paired, outRecs, outTrials, history) = rest
      MockSummary(
        inv,
        adm,
        qua,
        abs,
        causes,
        recs,
        items,
        found,
        miss,
        missing,
        contrasts,
        perScale,
        all,
        rev5,
        scales,
        grand,
        byScale,
        groups,
        paired,
        range,
        outRecs,
        outTrials,
        eligible,
        cartesian,
        population._1,
        population._2,
        history,
        runs,
        participants
      )

  private def disposition(
      line: String,
      fields: List[String]
  ): Either[String, TrialDisposition] =
    fields match
      case List("admitted")                    => Right(TrialDisposition.Admitted)
      case List("absent")                      => Right(TrialDisposition.Absent)
      case List("no-fixations")                => Right(TrialDisposition.NoFixations)
      case List("quarantine.rejected-records") =>
        Right(TrialDisposition.Quarantined(QuarantineCause.RejectedRecords))
      case List("quarantine.duplicate-ordinals") =>
        Right(TrialDisposition.Quarantined(QuarantineCause.DuplicateOrdinals))
      case List("quarantine.overlap", index, previous, current) =>
        index.toIntOption
          .map(i => TrialDisposition.Quarantined(QuarantineCause.Overlap(i, previous, current)))
          .toRight(s"inventory line '$line': bad overlap index $index")
      case other => Left(s"inventory line '$line': unknown status ${other.mkString(" ")}")

  private def outsideFrame(line: String, field: String): Either[String, Vector[OutsideFrame]] =
    if field.isEmpty then Right(Vector.empty)
    else
      field.split('|').toVector.traverse { entry =>
        entry.split('@').toList match
          case List(n, x, y) =>
            (n.toIntOption, x.toDoubleOption, y.toDoubleOption)
              .mapN(OutsideFrame(_, _, _, "screen"))
              .toRight(s"inventory line '$line': bad outside-frame record $entry")
          case _ => Left(s"inventory line '$line': bad outside-frame record $entry")
      }

  private def parseInventory(text: String): Either[String, Vector[LedgerEntry]] =
    text.linesIterator.toVector.traverse { line =>
      line.split("\t", -1).toList match
        case p :: phase :: trial :: occurrence :: item :: response :: outside :: status =>
          for
            occ  <- occurrence.toIntOption.toRight(s"inventory line '$line': bad occurrence")
            disp <- disposition(line, status)
            off  <- outsideFrame(line, outside)
          yield LedgerEntry(
            TrialKey(p, Phase(phase), trial, occ),
            item,
            Option.when(response.nonEmpty)(Response(response)),
            disp,
            off
          )
        case _ => Left(s"inventory line '$line' has too few fields")
    }
