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
import io.circe.{ACursor, Decoder, Json}

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

/** The summary block of fixture.json that the backend serves. */
final case class MockSummary(
    inventoryTrials: Int,
    admitted: Int,
    quarantined: Int,
    absent: Int,
    quarantineByCause: Vector[QuarantineCount],
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
    remembered: GroupSummary,
    forgotten: GroupSummary,
    pairedN: Int,
    groupNRange: (Int, Int),
    outsideWindowRecords: Int,
    outsideWindowTrials: Int,
    eligibleQueries: Int,
    candidatePairsPerScale: Long,
    datasetHistory: Map[String, String],
    runs: Vector[(String, String)],
    participants: Vector[ParticipantSummary]
) derives CanEqual

/** docs/studio/fixture/fixture.json, decoded, and fixtures/studio-golden's
  * inventory. Both are embedded at build time (project/StudioFixture.scala),
  * so they load without file I/O on every platform.
  */
final case class MockStudy(
    queries: Vector[MockQuery],
    summary: MockSummary,
    inventory: Vector[LedgerEntry]
) derives CanEqual

object MockStudy:

  /** Every fixture trial has occurrence 1 (FIXTURE.md). */
  val Occurrence: Int = 1

  def phaseOf(trial: String): Phase =
    if trial.startsWith("enc_") then Phase.Encoding else Phase.Retrieval

  def key(participant: String, trial: String): TrialKey =
    TrialKey(participant, phaseOf(trial), trial, Occurrence)

  /** The exact text of fixture.json. */
  def fixtureText: String = FixtureJson.text

  lazy val load: Either[String, MockStudy] =
    for
      json      <- io.circe.parser.parse(FixtureJson.text).leftMap(_.message)
      queries   <- decodeQueries(json)
      summary   <- decodeSummary(json.hcursor.downField("summary")).leftMap(_.getMessage)
      inventory <- parseInventory(GoldenInventory.trials)
      _         <- queries.traverse_ { q =>
        val lengths = Vector(q.m, q.b, q.d).flatten.map(_.size)
        Either.cond(
          lengths.forall(_ == summary.scales.size),
          (),
          s"${q.participant} ${q.trial} has scores for ${lengths.mkString("/")} scales, " +
            s"not ${summary.scales.size}"
        )
      }
    yield MockStudy(queries, summary, inventory)

  // -------------------------------------------------------------------------

  private given Decoder[Response] = Decoder[String].emap {
    case "Remembered" => Right(Response.Remembered)
    case "Forgotten"  => Right(Response.Forgotten)
    case other        => Left(s"unknown response $other")
  }

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

  private def groupMeans(c: ACursor): Decoder.Result[GroupMeans] =
    (c.get[Int]("n"), c.get[Double]("M"), c.get[Double]("B"), c.get[Double]("D"))
      .mapN(GroupMeans.apply)

  private def participant(json: Json): Decoder.Result[ParticipantSummary] =
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
      groupMeans(c.downField("Remembered")),
      groupMeans(c.downField("Forgotten"))
    ).mapN(ParticipantSummary.apply)

  private def decodeSummary(c: ACursor): Decoder.Result[MockSummary] =
    val qc = c.downField("query_contrasts")
    for
      causes <- c
        .get[Map[String, Int]]("quarantine_by_cause")
        .flatMap(_.toVector.sortBy(_._1).traverse { case (slug, n) =>
          QuarantineCause
            .fromSlug(slug)
            .map(QuarantineCount(_, n))
            .toRight(io.circe.DecodingFailure(s"unknown quarantine cause $slug", c.history))
        })
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
      participants <- c.get[Vector[Json]]("participants").flatMap(_.traverse(participant))
      range        <- c.get[Vector[Int]]("paired_group_n_range").flatMap {
        case Vector(lo, hi) => Right((lo, hi))
        case other => Left(io.circe.DecodingFailure(s"range $other is not a pair", c.history))
      }
      runs    <- c.get[Map[String, String]]("runs").map(_.toVector.sortBy(_._1))
      nR      <- c.get[Int]("n_Remembered")
      nF      <- c.get[Int]("n_Forgotten")
      dR      <- c.get[Double]("grand_D_Remembered")
      dF      <- c.get[Double]("grand_D_Forgotten")
      sR      <- c.get[Vector[Double]]("grand_D_by_scale_Remembered")
      sF      <- c.get[Vector[Double]]("grand_D_by_scale_Forgotten")
      summary <- (
        c.get[Int]("inventory_trials"),
        c.get[Int]("admitted"),
        c.get[Int]("quarantined"),
        c.get[Int]("absent"),
        c.get[Int]("fixation_records"),
        c.get[Int]("items_in_pool"),
        c.get[Int]("images_found"),
        c.get[Int]("images_missing"),
        c.get[Long]("pair_rows_per_scale"),
        c.get[Long]("pair_rows_all_scales"),
        c.get[Long]("pair_rows_rev5"),
        c.get[Vector[String]]("scales")
      ).tupled.flatMap {
        case (inv, adm, qua, abs, recs, items, found, miss, perScale, all, rev5, scales) =>
          (
            c.get[Double]("grand_D_all"),
            c.get[Vector[Double]]("grand_D_by_scale"),
            c.get[Int]("n_paired"),
            c.get[Int]("outside_window_records"),
            c.get[Int]("outside_window_trials"),
            c.get[Int]("eligible_queries"),
            c.get[Long]("candidate_pairs_cartesian_per_scale"),
            c.get[Map[String, String]]("dataset_history")
          ).mapN { (grand, byScale, paired, outRecs, outTrials, eligible, cartesian, history) =>
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
              GroupSummary(nR, dR, sR),
              GroupSummary(nF, dF, sF),
              paired,
              range,
              outRecs,
              outTrials,
              eligible,
              cartesian,
              history,
              runs,
              participants
            )
          }
      }
    yield summary

  private def parseInventory(text: String): Either[String, Vector[LedgerEntry]] =
    text.linesIterator.toVector.traverse { line =>
      line.split(",", -1).toVector match
        case Vector(p, phase, trial, occurrence, item, response, status) =>
          for
            ph <- phase match
              case "Encoding"  => Right(Phase.Encoding)
              case "Retrieval" => Right(Phase.Retrieval)
              case other       => Left(s"inventory line '$line': unknown phase $other")
            occ  <- occurrence.toIntOption.toRight(s"inventory line '$line': bad occurrence")
            resp <- response match
              case ""           => Right(None)
              case "Remembered" => Right(Some(Response.Remembered))
              case "Forgotten"  => Right(Some(Response.Forgotten))
              case other        => Left(s"inventory line '$line': unknown response $other")
            disp <- status match
              case "admitted" => Right(TrialDisposition.Admitted)
              case "absent"   => Right(TrialDisposition.Absent)
              case slug       =>
                QuarantineCause
                  .fromSlug(slug)
                  .map(TrialDisposition.Quarantined(_))
                  .toRight(s"inventory line '$line': unknown status $slug")
          yield LedgerEntry(TrialKey(p, ph, trial, occ), item, resp, disp)
        case _ => Left(s"inventory line '$line' does not have 7 fields")
    }
