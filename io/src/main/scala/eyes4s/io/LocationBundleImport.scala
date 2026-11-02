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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** Exact text artifacts emitted by the FIGRIM location-only adapter. */
final case class LocationBundleText(
    trialsCsv: String,
    locationsTsv: String,
    pairsTsv: String,
    phaseLedgerTsv: String,
    locationLedgerTsv: String
) derives CanEqual:
  lazy val sourceDigest: Sha256 = Sha256.ofUtf8(
    Vector(
      "trials.csv",
      trialsCsv,
      "locations.tsv",
      locationsTsv,
      "pairs.tsv",
      pairsTsv,
      "phase-ledger.tsv",
      phaseLedgerTsv,
      "location-ledger.tsv",
      locationLedgerTsv
    ).map(value => s"${value.length}:$value").mkString
  )

final case class LocationImportProfile(
    id: String,
    frame: Frame[Px],
    columns: Int,
    rows: Int,
    duplicateFamilies: Map[String, String]
) derives CanEqual

object LocationImportProfile:
  private val duplicates = Vector(
    "sun_bnjxztsvnercygxn.jpg" -> "sun_bdwttbytrbnqyqsk.jpg",
    "sun_awjucfvtjllxrtkv.jpg" -> "sun_abpqxslcljhrwmck.jpg",
    "sun_byoonkqxghujgvkh.jpg" -> "sun_bjitfqyiepkgfkks.jpg",
    "sun_bwrlzawyknljbhcb.jpg" -> "sun_auwrraazjwdcjcjg.jpg",
    "sun_boskvwzgsvsyuhll.jpg" -> "sun_bgdykfpjgudqpzlu.jpg",
    "sun_bzagimnxconenicn.jpg" -> "sun_bsccnfecifucnavf.jpg"
  )
  val figrim: LocationImportProfile =
    val frame   = Frame.screen("figrim-image-pixels", 1000, 1000).toOption.get
    val aliases =
      duplicates.flatMap((a, b) => Vector(a -> Vector(a, b).min, b -> Vector(a, b).min)).toMap
    LocationImportProfile("figrim-locations-v1", frame, 10, 10, aliases)

final case class LocationTrialMetadata(
    id: LocationTrialId,
    imageIndex: Int,
    observerSlot: String,
    image: String,
    imageFamily: String,
    category: String,
    phase: String,
    sdt: String
) derives CanEqual

final case class LocationPhaseLedger(
    imageIndex: Int,
    observerSlot: String,
    phase: String,
    image: String,
    category: String,
    recordingLabel: String,
    initials: String,
    sourceTrial: String,
    slotState: String,
    gazeState: String,
    sdt: String,
    responseState: String,
    sourceLocations: Int,
    admittedLocations: Int,
    imageFamily: String
) derives CanEqual

final case class LocationLedgerEntry(
    imageIndex: Int,
    observerSlot: String,
    phase: String,
    ordinal: Long,
    rawX: Double,
    rawY: Double,
    roundedX: Double,
    roundedY: Double,
    rawZeroX: Double,
    rawZeroY: Double,
    admitted: Boolean,
    position: Option[Pt[Px]],
    reason: String
) derives CanEqual

final case class UnpairedLocationRetrieval(
    trial: Option[LocationTrialId],
    imageIndex: Int,
    observerSlot: String,
    phase: String,
    reason: String
) derives CanEqual

final class LocationImport private[io] (
    val input: LocationInput,
    val queries: Vector[LocationComparisonQuery],
    val profile: LocationImportProfile,
    val sourceDigest: Sha256,
    val trialMetadata: Vector[LocationTrialMetadata],
    val phaseLedger: Vector[LocationPhaseLedger],
    val locationLedger: Vector[LocationLedgerEntry],
    val unpairedRetrievals: Vector[UnpairedLocationRetrieval]
)

/** Strict, effect-free reader for the already-extracted FIGRIM location bundle.
  * It is deliberately not a MATLAB importer.
  */
object LocationBundleImport:
  private val trialHeader = Vector(
    "trial_id",
    "observer_slot",
    "image",
    "category",
    "phase",
    "sdt",
    "image_index",
    "image_family"
  )
  private val pointHeader = Vector("trial_id", "x_px", "y_px")
  private val pairHeader  = Vector("pair_id", "focal_id", "encoding_id", "control_id")
  private val phaseHeader = Vector(
    "image_index",
    "image",
    "category",
    "observer_slot",
    "recording_label",
    "initials",
    "source_trial",
    "phase",
    "slot_state",
    "gaze_state",
    "sdt",
    "response_state",
    "source_locations",
    "admitted_locations",
    "image_family"
  )
  private val ledgerHeader = Vector(
    "image_index",
    "observer_slot",
    "phase",
    "ordinal",
    "raw_x",
    "raw_y",
    "rounded_x",
    "rounded_y",
    "admitted",
    "x_px",
    "y_px",
    "raw_zero_x",
    "raw_zero_y",
    "reason"
  )

  def read(text: LocationBundleText): Either[LocationImportError, LocationImport] =
    for
      trials <- table("trials.csv", text.trialsCsv, ',', trialHeader)
      points <- table("locations.tsv", text.locationsTsv, '\t', pointHeader)
      pairs  <- table("pairs.tsv", text.pairsTsv, '\t', pairHeader)
      phases <- typedTable("phase-ledger.tsv", text.phaseLedgerTsv, '\t', phaseHeader)(
        parsePhase
      )
      ledger <- typedTable("location-ledger.tsv", text.locationLedgerTsv, '\t', ledgerHeader)(
        parseLedgerRow
      )
      result <- assemble(text.sourceDigest, trials, points, pairs, phases, ledger)
    yield result

  private def assemble(
      digest: Sha256,
      rows: Vector[Map[String, String]],
      pointRows: Vector[Map[String, String]],
      pairRows: Vector[Map[String, String]],
      phases: Vector[LocationPhaseLedger],
      ledger: Vector[LocationLedgerEntry]
  ): Either[LocationImportError, LocationImport] =
    val byPhase       = ledgerByPhase(ledger)
    val pointsByTrial = pointRows.groupMap(_("trial_id"))(identity)
    for
      metadata <- parseMetadata(rows)
      _        <- validateImages(phases)
      _        <- validateLedger(phases, ledger, byPhase)
      _        <- validateMetadata(metadata, phases)
      _        <- validatePoints(metadata, pointsByTrial, byPhase)
      input    <- makeInput(metadata, byPhase)
      queries  <- makeQueries(metadata, pairRows)
    yield new LocationImport(
      input,
      queries,
      LocationImportProfile.figrim,
      digest,
      metadata,
      phases,
      ledger,
      unpaired(metadata, phases, queries)
    )

  private def table(
      name: String,
      text: String,
      delimiter: Char,
      header: Vector[String]
  ): Either[LocationImportError, Vector[Map[String, String]]] =
    typedTable(name, text, delimiter, header)(row => Right(row))

  /** Convert one page at a time, without retaining a map for every ledger row. */
  private def typedTable[A](
      name: String,
      text: String,
      delimiter: Char,
      header: Vector[String]
  )(
      decode: Map[String, String] => Either[LocationImportError, A]
  ): Either[LocationImportError, Vector[A]] =
    val values = Vector.newBuilder[A]
    var cursor = Option(CsvCursor.start(text))
    var record = 0
    while cursor.nonEmpty do
      cursor.get.advance(1024, delimiter) match
        case Left(error) => return Left(LocationImportError.Csv(name, error.message))
        case Right(page) =>
          var index = 0
          while index < page.rows.size do
            val row = page.rows(index)
            record += 1
            if record == 1 then
              if row != header then return Left(LocationImportError.Header(name, header, row))
            else
              if row.size != header.size then
                return Left(LocationImportError.Width(name, record, header.size, row.size))
              decode(header.zip(row).toMap) match
                case Left(error)  => return Left(error)
                case Right(value) => values += value
            index += 1
          cursor = page.next
    if record == 0 then Left(LocationImportError.Header(name, header, Vector.empty))
    else Right(values.result())

  private def parseMetadata(
      rows: Vector[Map[String, String]]
  ): Either[LocationImportError, Vector[LocationTrialMetadata]] =
    rows
      .foldLeft[Either[LocationImportError, Vector[LocationTrialMetadata]]](
        Right(Vector.empty)
      ) { (acc, r) =>
        acc.flatMap { values =>
          for
            id <- LocationTrialId
              .of(r("trial_id"))
              .left
              .map(e => LocationImportError.Plan("trial_id", e.message))
            index <- integer("trials.csv", r, "image_index")
          yield LocationTrialMetadata(
            id,
            index,
            r("observer_slot"),
            r("image"),
            r("image_family"),
            r("category"),
            r("phase"),
            r("sdt")
          ) +: values
        }
      }
      .map(_.reverse)
      .flatMap { values =>
        Either.cond(
          values.map(_.id).distinct.size == values.size,
          values,
          LocationImportError.Duplicate("trials.csv", "trial_id")
        )
      }

  // Scala's Either does not support postfix extraction; keeping numeric parsing explicit avoids coercing source text.
  private def integer(
      file: String,
      row: Map[String, String],
      field: String
  ): Either[LocationImportError, Int] =
    row(field).toIntOption.toRight(LocationImportError.Number(file, field, row(field)))

  private def parsePhase(
      r: Map[String, String]
  ): Either[LocationImportError, LocationPhaseLedger] =
    for
      imageIndex <- integer("phase-ledger.tsv", r, "image_index")
      source     <- integer("phase-ledger.tsv", r, "source_locations")
      admitted   <- integer("phase-ledger.tsv", r, "admitted_locations")
    yield LocationPhaseLedger(
      imageIndex,
      r("observer_slot"),
      r("phase"),
      r("image"),
      r("category"),
      r("recording_label"),
      r("initials"),
      r("source_trial"),
      r("slot_state"),
      r("gaze_state"),
      r("sdt"),
      r("response_state"),
      source,
      admitted,
      r("image_family")
    )

  private def parseLedgerRow(
      r: Map[String, String]
  ): Either[LocationImportError, LocationLedgerEntry] =
    for
      image   <- integer("location-ledger.tsv", r, "image_index")
      ordinal <- r("ordinal").toLongOption.toRight(
        LocationImportError.Number("location-ledger.tsv", "ordinal", r("ordinal"))
      )
      rawX     <- number(r, "raw_x")
      rawY     <- number(r, "raw_y")
      rx       <- number(r, "rounded_x")
      ry       <- number(r, "rounded_y")
      zeroX    <- number(r, "raw_zero_x")
      zeroY    <- number(r, "raw_zero_y")
      admitted <- r("admitted") match
        case "0"   => Right(false)
        case "1"   => Right(true)
        case other => Left(LocationImportError.Number("location-ledger.tsv", "admitted", other))
      position <-
        if admitted then
          for x <- number(r, "x_px"); y <- number(r, "y_px") yield Some(Pt[Px](x, y))
        else
          Either.cond(
            r("x_px").isEmpty && r("y_px").isEmpty,
            None,
            LocationImportError.LedgerMismatch(image, r("observer_slot"), r("phase"))
          )
    yield LocationLedgerEntry(
      image,
      r("observer_slot"),
      r("phase"),
      ordinal,
      rawX,
      rawY,
      rx,
      ry,
      zeroX,
      zeroY,
      admitted,
      position,
      r("reason")
    )

  private def number(
      r: Map[String, String],
      field: String
  ): Either[LocationImportError, Double] =
    val raw   = r(field)
    val value = raw.toDoubleOption.orElse(raw.toLowerCase match
      case "nan"                => Some(Double.NaN)
      case "inf" | "infinity"   => Some(Double.PositiveInfinity)
      case "-inf" | "-infinity" => Some(Double.NegativeInfinity)
      case _                    => None)
    value.toRight(LocationImportError.Number("location-ledger.tsv", field, raw))

  private def validateImages(
      phases: Vector[LocationPhaseLedger]
  ): Either[LocationImportError, Unit] =
    val byIndex      = phases.groupBy(_.imageIndex)
    val byName       = phases.groupBy(_.image)
    val inconsistent = byIndex.toVector.sortBy(_._1).find { (_, rows) =>
      rows.map(p => (p.image, p.category)).distinct.size != 1 ||
      rows.exists(p => p.image.trim.isEmpty || p.category.trim.isEmpty)
    }
    inconsistent match
      case Some((index, rows)) =>
        Left(
          LocationImportError.ImageIdentity(
            index,
            rows.map(_.image).distinct,
            "one nonempty filename and category required"
          )
        )
      case None =>
        byName.toVector
          .sortBy(_._1)
          .find((_, rows) => rows.map(_.imageIndex).distinct.size != 1) match
          case Some((name, rows)) =>
            Left(
              LocationImportError.ImageIdentity(
                rows.head.imageIndex,
                Vector(name),
                s"filename names several indices: ${rows.map(_.imageIndex).distinct.sorted.mkString(", ")}"
              )
            )
          case None => Right(())

  private def validPhase(p: LocationPhaseLedger, images: Set[String]): Boolean =
    val code     = p.sdt.toIntOption
    val response =
      if p.sdt.isEmpty then p.responseState == "absent"
      else
        code.exists { value =>
          p.responseState == (if value == -1 then "missing_sentinel"
                              else if Set(1, 2, 3, 4)(value) then "recorded"
                              else "unknown_code")
        }
    val slot = p.slotState match
      case "metadata_present" => true
      case "empty_slot"       =>
        p.recordingLabel.isEmpty && p.initials.isEmpty &&
        p.sourceTrial.isEmpty && p.sdt.isEmpty && p.sourceLocations == 0 &&
        p.gazeState == "absent_fixation_struct"
      case _ => false
    Set("enc", "rec", "rec2")(p.phase) && p.imageIndex > 0 &&
    p.observerSlot.toIntOption.exists(n =>
      n > 0 && n.toString == p.observerSlot
    ) && p.sourceLocations >= 0 &&
    p.admittedLocations >= 0 && p.admittedLocations <= p.sourceLocations &&
    p.imageFamily == LocationImportProfile.figrim.duplicateFamilies
      .get(p.image)
      .filter(images.contains)
      .getOrElse(p.image) &&
    Set("locations", "empty_phase", "missing_phase", "absent_fixation_struct")(p.gazeState) &&
    ((p.gazeState == "locations") == (p.sourceLocations > 0)) && response && slot

  private def validateLedger(
      phases: Vector[LocationPhaseLedger],
      ledger: Vector[LocationLedgerEntry],
      byPhase: Map[(Int, String, String), Vector[LocationLedgerEntry]]
  ): Either[LocationImportError, Unit] =
    val keys   = phases.map(p => (p.imageIndex, p.observerSlot, p.phase))
    val keySet = keys.toSet
    val images = phases.map(_.image).toSet
    if keys.distinct.size != keys.size then
      Left(LocationImportError.Duplicate("phase-ledger.tsv", "image_index/observer_slot/phase"))
    else
      phases
        .find(p => !validPhase(p, images))
        .map(p =>
          Left(
            LocationImportError.PhaseState(
              p.imageIndex,
              p.observerSlot,
              p.phase,
              s"gaze=${p.gazeState}, response=${p.responseState}, counts=${p.sourceLocations}/${p.admittedLocations}"
            )
          )
        )
        .getOrElse {
          ledger
            .find(x => !keySet((x.imageIndex, x.observerSlot, x.phase)))
            .map(x =>
              Left(LocationImportError.OrphanLedger(x.imageIndex, x.observerSlot, x.phase))
            )
            .getOrElse {
              phases.foldLeft[Either[LocationImportError, Unit]](Right(())) { (acc, p) =>
                acc.flatMap { _ =>
                  val entries =
                    byPhase.getOrElse((p.imageIndex, p.observerSlot, p.phase), Vector.empty)
                  val valid  = entries.count(_.admitted)
                  val raster = entries.forall { x =>
                    val rounded = matlabRound(x.rawX) -> matlabRound(x.rawY)
                    val should  =
                      x.rawX.isFinite && x.rawY.isFinite && rounded._1 >= 1 && rounded._1 <= 1000 && rounded._2 >= 1 && rounded._2 <= 1000
                    same(x.roundedX, rounded._1) && same(x.roundedY, rounded._2) && same(
                      x.rawZeroX,
                      x.rawX - 1
                    ) && same(
                      x.rawZeroY,
                      x.rawY - 1
                    ) && x.admitted == should && (if should then
                                                    x.position.contains(
                                                      Pt[Px](rounded._1 - 1, rounded._2 - 1)
                                                    ) && x.reason == "admitted"
                                                  else
                                                    x.position.isEmpty && x.reason == (if !x.rawX.isFinite || !x.rawY.isFinite
                                                                                       then
                                                                                         "nonfinite"
                                                                                       else
                                                                                         "outside_pixel_bounds"))
                  }
                  Either.cond(
                    entries.map(_.ordinal).sorted == (1 to p.sourceLocations)
                      .map(_.toLong)
                      .toVector && entries.size == p.sourceLocations && valid == p.admittedLocations && raster,
                    (),
                    LocationImportError.LedgerMismatch(p.imageIndex, p.observerSlot, p.phase)
                  )
                }
              }
            }
        }

  private def validatePoints(
      metadata: Vector[LocationTrialMetadata],
      rows: Map[String, Vector[Map[String, String]]],
      byPhase: Map[(Int, String, String), Vector[LocationLedgerEntry]]
  ): Either[LocationImportError, Unit] =
    val ids = metadata.map(_.id.value).toSet
    if rows.keysIterator.exists(id => !ids(id)) then
      Left(LocationImportError.UnknownTrial(rows.keysIterator.find(id => !ids(id)).get))
    else
      metadata.foldLeft[Either[LocationImportError, Unit]](Right(())) { (acc, m) =>
        acc.flatMap { _ =>
          val expected = byPhase
            .getOrElse((m.imageIndex, m.observerSlot, m.phase), Vector.empty)
            .filter(_.admitted)
            .flatMap(_.position)
            .map(p => p.x -> p.y)
            .sorted
          val actual = rows
            .getOrElse(m.id.value, Vector.empty)
            .foldLeft[Either[LocationImportError, Vector[(Double, Double)]]](
              Right(Vector.empty)
            )((a, r) =>
              for
                xs <- a;
                x  <- r("x_px").toDoubleOption
                  .toRight(LocationImportError.Number("locations.tsv", "x_px", r("x_px")));
                y <- r("y_px").toDoubleOption
                  .toRight(LocationImportError.Number("locations.tsv", "y_px", r("y_px")))
              yield xs :+ (x -> y)
            )
            .map(_.sorted)
          actual.flatMap(a =>
            Either.cond(
              a == expected && a.nonEmpty,
              (),
              LocationImportError.PointMismatch(m.id.value)
            )
          )
        }
      }

  private def validateMetadata(
      metadata: Vector[LocationTrialMetadata],
      phases: Vector[LocationPhaseLedger]
  ): Either[LocationImportError, Map[(Int, String, String), LocationPhaseLedger]] =
    val admitted = phases.filter(_.admittedLocations > 0)
    val keys     = metadata.map(m => (m.imageIndex, m.observerSlot, m.phase))
    val phaseMap = phases.map(p => (p.imageIndex, p.observerSlot, p.phase) -> p).toMap
    Either.cond(
      keys.distinct.size == keys.size && keys.toSet == admitted
        .map(p => (p.imageIndex, p.observerSlot, p.phase))
        .toSet && metadata.forall { m =>
        phaseMap
          .get((m.imageIndex, m.observerSlot, m.phase))
          .exists(p =>
            p.image == m.image && p.category == m.category && p.sdt == m.sdt && p.imageFamily == m.imageFamily
          )
      },
      phaseMap,
      LocationImportError.MetadataMismatch
    )

  private def makeInput(
      metadata: Vector[LocationTrialMetadata],
      byPhase: Map[(Int, String, String), Vector[LocationLedgerEntry]]
  ): Either[LocationImportError, LocationInput] = for
    trials <- metadata.foldLeft[Either[LocationImportError, Vector[LocationTrial]]](
      Right(Vector.empty)
    ) { (acc, m) =>
      for
        xs     <- acc
        source <- LocationSourceId
          .of(s"figrim:${m.imageIndex}:${m.observerSlot}:${m.phase}")
          .left
          .map(e => LocationImportError.Plan("source", e.message))
        observations = byPhase
          .getOrElse((m.imageIndex, m.observerSlot, m.phase), Vector.empty)
          .filter(_.admitted)
          .sortBy(_.ordinal)
          .flatMap(x => x.position.map(p => LocationObservation(source, x.ordinal, p)))
        trial <- LocationTrial
          .of(m.id, observations)
          .left
          .map(e => LocationImportError.Plan(m.id.value, e.message))
      yield xs :+ trial
    }
    result <- LocationInput
      .of(LocationImportProfile.figrim.frame, trials)
      .left
      .map(e => LocationImportError.Plan("input", e.message))
  yield result

  private def makeQueries(
      metadata: Vector[LocationTrialMetadata],
      rows: Vector[Map[String, String]]
  ): Either[LocationImportError, Vector[LocationComparisonQuery]] =
    val byId      = metadata.map(m => m.id.value -> m).toMap
    val encByItem =
      metadata.filter(_.phase == "enc").groupMap(m => (m.observerSlot, m.image))(identity)
    val controls = metadata
      .filter(_.phase == "enc")
      .groupMap(m => (m.observerSlot, m.category))(identity)
      .view
      .mapValues(_.sortBy(m => (m.image, m.id.value)))
      .toMap
    def find(id: String) = byId.get(id).toRight(LocationImportError.UnknownTrial(id))
    def controlFor(focal: LocationTrialMetadata) = controls
      .getOrElse((focal.observerSlot, focal.category), Vector.empty)
      .find(_.imageFamily != focal.imageFamily)
    if rows.map(_("pair_id")).distinct.size != rows.size then
      Left(LocationImportError.Duplicate("pairs.tsv", "pair_id"))
    else
      rows
        .foldLeft[Either[LocationImportError, Vector[LocationComparisonQuery]]](
          Right(Vector.empty)
        ) { (acc, r) =>
          for
            xs <- acc; f <- find(r("focal_id")); m <- find(r("encoding_id"));
            c  <- find(r("control_id")); expected = controlFor(f)
            _  <- Either.cond(
              f.phase != "enc" && m.phase == "enc" && c.phase == "enc" && f.observerSlot == m.observerSlot && f.observerSlot == c.observerSlot && f.category == m.category && f.category == c.category && f.image == m.image && f.imageFamily != c.imageFamily && expected
                .contains(c),
              (),
              LocationImportError.PairProfile(r("pair_id"))
            )
            q <- LocationComparisonQuery
              .of(r("pair_id"), f.id, m.id, c.id)
              .left
              .map(e => LocationImportError.Plan(r("pair_id"), e.message))
          yield xs :+ q
        }
        .flatMap { queries =>
          val eligible = metadata
            .filter(f =>
              f.phase != "enc" && encByItem.contains((f.observerSlot, f.image)) && controlFor(
                f
              ).nonEmpty
            )
            .map(_.id)
            .toSet
          Either.cond(
            queries.map(_.focal).distinct.size == queries.size && queries
              .map(_.focal)
              .toSet == eligible,
            queries,
            LocationImportError.WorkloadMismatch
          )
        }

  private def ledgerByPhase(ledger: Vector[LocationLedgerEntry]) =
    ledger.groupMap(x => (x.imageIndex, x.observerSlot, x.phase))(identity)
  private def matlabRound(value: Double): Double =
    java.lang.Math.copySign(math.floor(math.abs(value) + .5), value)
  private def same(left: Double, right: Double): Boolean =
    left == right || left.isNaN && right.isNaN
  private def unpaired(
      metadata: Vector[LocationTrialMetadata],
      phases: Vector[LocationPhaseLedger],
      queries: Vector[LocationComparisonQuery]
  ) =
    val paired  = queries.map(_.focal.value).toSet
    val encoded = metadata.filter(_.phase == "enc").map(m => (m.observerSlot, m.image)).toSet
    metadata.filter(m => m.phase != "enc" && !paired(m.id.value)).map { m =>
      val encoding = encoded((m.observerSlot, m.image))
      UnpairedLocationRetrieval(
        Some(m.id),
        m.imageIndex,
        m.observerSlot,
        m.phase,
        if !encoding then "missing_encoding" else "no_eligible_control"
      )
    } ++ phases
      .filter(p => p.phase != "enc" && p.admittedLocations == 0)
      .map(p =>
        UnpairedLocationRetrieval(
          None,
          p.imageIndex,
          p.observerSlot,
          p.phase,
          "no_admitted_locations"
        )
      )

enum LocationImportError derives CanEqual:
  case Csv(file: String, detail: String);
  case Header(file: String, expected: Vector[String], actual: Vector[String]);
  case Width(file: String, record: Int, expected: Int, actual: Int);
  case Number(file: String, field: String, value: String);
  case Duplicate(file: String, key: String);
  case OrphanLedger(imageIndex: Int, observerSlot: String, phase: String);
  case LedgerMismatch(imageIndex: Int, observerSlot: String, phase: String);
  case PhaseState(imageIndex: Int, observerSlot: String, phase: String, state: String);
  case MetadataMismatch; case PointMismatch(trialId: String);
  case UnknownTrial(trialId: String); case PairProfile(pairId: String); case WorkloadMismatch;
  case Plan(operand: String, detail: String)
  case ImageIdentity(imageIndex: Int, images: Vector[String], detail: String)
  def message: String = this match
    case ImageIdentity(index, images, detail) => s"image_index=$index images=$images: $detail"
    case Csv(f, d)                            => s"file='$f' has malformed RFC-4180 text: $d";
    case Header(f, e, a)                      => s"file='$f' header=$a expected=$e";
    case Width(f, r, e, a)                    => s"file='$f' record=$r width=$a expected=$e";
    case Number(f, n, v)       => s"file='$f' field='$n' value='$v' is not numeric";
    case Duplicate(f, k)       => s"file='$f' has duplicate $k";
    case OrphanLedger(i, s, p) =>
      s"location ledger image_index=$i observer_slot='$s' phase='$p' has no phase ledger";
    case LedgerMismatch(i, s, p) =>
      s"ledger mismatch for image_index=$i observer_slot='$s' phase='$p'";
    case PhaseState(i, s, p, state) =>
      s"phase ledger image_index=$i observer_slot='$s' phase='$p' has invalid state '$state'";
    case MetadataMismatch =>
      "trials.csv does not exactly match the nonempty admitted phase ledger";
    case PointMismatch(t) =>
      s"locations.tsv does not exactly match admitted ledger points for trial_id='$t'";
    case UnknownTrial(t)  => s"pairs.tsv names unknown trial_id='$t'";
    case PairProfile(p)   => s"pair_id='$p' violates the FIGRIM pairing profile";
    case WorkloadMismatch =>
      "pairs.tsv does not contain exactly one row for every eligible retrieval focal";
    case Plan(o, d) => s"location plan operand='$o' was refused: $d"

object LocationImportError:
  /** The family is enrolled by the io catalog owner; keeping the instance beside
    * its enum prevents applications from having to know importer internals.
    */
  given diagnose: Diagnose[LocationImportError, Nothing] =
    Diagnose.derived(LocationImportDiagnostics.family)(_.message)

object LocationImportDiagnostics:
  val family: DiagnosticFamily = DiagnosticFamily.error("location-import")(
    "Csv",
    "Header",
    "Width",
    "Number",
    "Duplicate",
    "OrphanLedger",
    "LedgerMismatch",
    "PhaseState",
    "MetadataMismatch",
    "PointMismatch",
    "UnknownTrial",
    "PairProfile",
    "WorkloadMismatch",
    "Plan",
    "ImageIdentity"
  )
