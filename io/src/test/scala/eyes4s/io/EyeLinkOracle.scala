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

import eyes4s.plan.{Diagnose, Locus}
import cats.data.NonEmptyVector

/** Independent implementation that produced an EyeLink oracle. */
enum EyeLinkOracleKind(val token: String) derives CanEqual:
  case EdfAccessApi extends EyeLinkOracleKind("edf-access-api")
  case Eyelinker    extends EyeLinkOracleKind("eyelinker")
  case VendorAsc    extends EyeLinkOracleKind("vendor-asc-reader")

/** Input representation read by an independent implementation. */
enum EyeLinkOracleInput(val token: String) derives CanEqual:
  case Edf extends EyeLinkOracleInput("edf")
  case Asc extends EyeLinkOracleInput("asc")

/** Ordering guarantee retained by an independent implementation. */
enum EyeLinkOracleOrdering(val token: String) derives CanEqual:
  case Complete    extends EyeLinkOracleOrdering("complete")
  case WithinKind  extends EyeLinkOracleOrdering("within-kind")
  case Unavailable extends EyeLinkOracleOrdering("unavailable")

/** Whether an independently observed field has a value. */
enum EyeLinkOraclePresence(val token: String) derives CanEqual:
  case Value   extends EyeLinkOraclePresence("value")
  case Missing extends EyeLinkOraclePresence("missing")
  case Omitted extends EyeLinkOraclePresence("omitted")

/** Digest-pinned identity and invocation of an independent oracle tool. */
final class EyeLinkOracleDescriptor private (
    val oracleId: String,
    val fixtureId: String,
    val kind: EyeLinkOracleKind,
    val inputKind: EyeLinkOracleInput,
    val ordering: EyeLinkOracleOrdering,
    val toolName: String,
    val toolVersion: String,
    val toolDigest: Sha256,
    val adapterDigest: Sha256,
    val inputDigest: Sha256,
    val invocation: String,
    val converterReceiptDigest: Option[Sha256]
):
  override def toString: String = s"EyeLinkOracleDescriptor($oracleId,$fixtureId,$kind)"

object EyeLinkOracleDescriptor:
  private val IdPattern = "[a-z0-9][a-z0-9-]*".r

  def create(
      oracleId: String,
      fixtureId: String,
      kind: EyeLinkOracleKind,
      inputKind: EyeLinkOracleInput,
      ordering: EyeLinkOracleOrdering,
      toolName: String,
      toolVersion: String,
      toolDigest: Sha256,
      adapterDigest: Sha256,
      inputDigest: Sha256,
      invocation: String,
      converterReceiptDigest: Option[Sha256]
  ): Either[EyeLinkOracleError, EyeLinkOracleDescriptor] =
    val normalizedOracle     = oracleId.trim
    val normalizedFixture    = fixtureId.trim
    val normalizedTool       = toolName.trim
    val normalizedVersion    = toolVersion.trim
    val normalizedInvocation = invocation.trim

    def invalid(field: String, value: String, expected: String) =
      Left(EyeLinkOracleError.InvalidDescriptor(normalizedOracle, field, value, expected))

    if !IdPattern.matches(normalizedOracle) then
      invalid("oracle_id", normalizedOracle, "[a-z0-9][a-z0-9-]*")
    else if !IdPattern.matches(normalizedFixture) then
      invalid("fixture_id", normalizedFixture, "[a-z0-9][a-z0-9-]*")
    else if normalizedTool.isEmpty then invalid("tool_name", normalizedTool, "nonblank")
    else if normalizedVersion.isEmpty then
      invalid("tool_version", normalizedVersion, "nonblank")
    else if normalizedInvocation.isEmpty then
      invalid("invocation", normalizedInvocation, "nonblank exact argument vector")
    else if kind == EyeLinkOracleKind.EdfAccessApi && inputKind != EyeLinkOracleInput.Edf then
      invalid("input_kind", inputKind.token, "edf for edf-access-api")
    else if kind != EyeLinkOracleKind.EdfAccessApi && inputKind != EyeLinkOracleInput.Asc then
      invalid("input_kind", inputKind.token, "asc for ASC readers")
    else if inputKind == EyeLinkOracleInput.Edf && converterReceiptDigest.nonEmpty then
      invalid("converter_receipt_digest", "present", "absent for direct EDF input")
    else
      Right(
        new EyeLinkOracleDescriptor(
          normalizedOracle,
          normalizedFixture,
          kind,
          inputKind,
          ordering,
          normalizedTool,
          normalizedVersion,
          toolDigest,
          adapterDigest,
          inputDigest,
          normalizedInvocation,
          converterReceiptDigest
        )
      )

end EyeLinkOracleDescriptor

/** One atomic independently observed field.
  *
  * `Missing` means the independent implementation represented a native missing
  * value. `Omitted` means that implementation could not expose the field. The
  * distinction prevents a lossy reference reader from silently certifying data
  * it never retained.
  */
final class EyeLinkOracleFact private (
    val recordOrdinal: Int,
    val fieldOrdinal: Int,
    val recordKind: String,
    val block: Option[Int],
    val sourceOrder: Option[Long],
    val fieldPath: String,
    val presence: EyeLinkOraclePresence,
    val value: String,
    val detail: String
):
  override def toString: String =
    s"EyeLinkOracleFact($recordOrdinal,$fieldOrdinal,$recordKind,$fieldPath,$presence)"

object EyeLinkOracleFact:
  private val NamePattern = "[a-z][a-z0-9_.-]*".r

  def create(
      recordOrdinal: Int,
      fieldOrdinal: Int,
      recordKind: String,
      block: Option[Int],
      sourceOrder: Option[Long],
      fieldPath: String,
      presence: EyeLinkOraclePresence,
      value: String,
      detail: String
  ): Either[EyeLinkOracleError, EyeLinkOracleFact] =
    val normalizedKind   = recordKind.trim
    val normalizedPath   = fieldPath.trim
    val normalizedDetail = detail.trim

    def invalid(field: String, actual: String, expected: String) =
      Left(
        EyeLinkOracleError.InvalidFact(
          recordOrdinal,
          fieldOrdinal,
          field,
          actual,
          expected
        )
      )

    if recordOrdinal <= 0 then
      invalid("record_ordinal", recordOrdinal.toString, "positive integer")
    else if fieldOrdinal <= 0 then
      invalid("field_ordinal", fieldOrdinal.toString, "positive integer")
    else if !NamePattern.matches(normalizedKind) then
      invalid("record_kind", normalizedKind, "[a-z][a-z0-9_.-]*")
    else if block.exists(_ <= 0) then
      invalid("block", block.mkString, "absent or positive integer")
    else if sourceOrder.exists(_ <= 0L) then
      invalid("source_order", sourceOrder.mkString, "absent or positive integer")
    else if !NamePattern.matches(normalizedPath) then
      invalid("field_path", normalizedPath, "[a-z][a-z0-9_.-]*")
    else if presence == EyeLinkOraclePresence.Value && normalizedDetail.nonEmpty then
      invalid("detail", normalizedDetail, "blank when presence=value")
    else if presence != EyeLinkOraclePresence.Value && value.nonEmpty then
      invalid("value", value, "blank when presence is missing or omitted")
    else if presence != EyeLinkOraclePresence.Value && normalizedDetail.isEmpty then
      invalid("detail", normalizedDetail, "nonblank omission or missing-value reason")
    else
      Right(
        new EyeLinkOracleFact(
          recordOrdinal,
          fieldOrdinal,
          normalizedKind,
          block,
          sourceOrder,
          normalizedPath,
          presence,
          value,
          normalizedDetail
        )
      )

end EyeLinkOracleFact

/** Canonical, deterministic result emitted by an independent EyeLink reader. */
final class EyeLinkOracleManifest private (
    val descriptor: EyeLinkOracleDescriptor,
    val facts: Vector[EyeLinkOracleFact]
):
  lazy val scientificDigest: Sha256 = Sha256.ofUtf8(renderTsv)

  lazy val records: Vector[Vector[EyeLinkOracleFact]] =
    facts.groupBy(_.recordOrdinal).toVector.sortBy(_._1).map(_._2.sortBy(_.fieldOrdinal))

  def renderTsv: String = EyeLinkOracleManifest.render(this)

object EyeLinkOracleManifest:
  val schemaVersion: Int = 1

  val columns: Vector[String] = Vector(
    "record_ordinal",
    "field_ordinal",
    "record_kind",
    "block",
    "source_order",
    "field_path",
    "presence",
    "value",
    "detail"
  )

  def create(
      descriptor: EyeLinkOracleDescriptor,
      facts: Vector[EyeLinkOracleFact]
  ): Either[NonEmptyVector[EyeLinkOracleError], EyeLinkOracleManifest] =
    val errors = structuralErrors(descriptor, facts)
    NonEmptyVector.fromVector(errors) match
      case Some(values) => Left(values)
      case None         => Right(new EyeLinkOracleManifest(descriptor, facts))

  def parseTsv(
      source: String,
      input: String
  ): Either[NonEmptyVector[EyeLinkOracleError], EyeLinkOracleManifest] =
    val lines = input.replace("\r\n", "\n").split("\n", -1).toVector
    lines match
      case preamble +: header +: rows =>
        parsePreamble(source, preamble).flatMap { descriptor =>
          if header.split("\t", -1).toVector != columns then
            Left(
              NonEmptyVector.one(
                EyeLinkOracleError.InvalidHeader(
                  source,
                  2,
                  columns.mkString("\t"),
                  header
                )
              )
            )
          else parseRows(source, descriptor, rows)
        }
      case _ =>
        Left(NonEmptyVector.one(EyeLinkOracleError.InvalidPreamble(source, input.take(160))))

  private def parsePreamble(
      source: String,
      value: String
  ): Either[NonEmptyVector[EyeLinkOracleError], EyeLinkOracleDescriptor] =
    value.split("\t", -1).toVector match
      case Vector(
            "# eyes4s-eyelink-oracle",
            schema,
            oracleRaw,
            fixtureRaw,
            kindRaw,
            inputRaw,
            orderingRaw,
            toolRaw,
            versionRaw,
            toolDigestRaw,
            adapterDigestRaw,
            inputDigestRaw,
            invocationRaw,
            receiptDigestRaw
          ) if schema == schemaVersion.toString =>
        val parsed = for
          oracleId  <- decode(source, 1, "oracle_id", oracleRaw)
          fixtureId <- decode(source, 1, "fixture_id", fixtureRaw)
          kind      <- enumValue(source, 1, "kind", kindRaw, EyeLinkOracleKind.values)(_.token)
          inputKind <- enumValue(source, 1, "input_kind", inputRaw, EyeLinkOracleInput.values)(
            _.token
          )
          ordering <- enumValue(
            source,
            1,
            "ordering",
            orderingRaw,
            EyeLinkOracleOrdering.values
          )(_.token)
          toolName      <- decode(source, 1, "tool_name", toolRaw)
          toolVersion   <- decode(source, 1, "tool_version", versionRaw)
          toolDigest    <- digest(source, 1, "tool_digest", toolDigestRaw)
          adapterDigest <- digest(source, 1, "adapter_digest", adapterDigestRaw)
          inputDigest   <- digest(source, 1, "input_digest", inputDigestRaw)
          invocation    <- decode(source, 1, "invocation", invocationRaw)
          receiptDigest <- optionalDigest(
            source,
            1,
            "converter_receipt_digest",
            receiptDigestRaw
          )
          descriptor <- EyeLinkOracleDescriptor.create(
            oracleId,
            fixtureId,
            kind,
            inputKind,
            ordering,
            toolName,
            toolVersion,
            toolDigest,
            adapterDigest,
            inputDigest,
            invocation,
            receiptDigest
          )
        yield descriptor
        parsed.left.map(NonEmptyVector.one)
      case _ => Left(NonEmptyVector.one(EyeLinkOracleError.InvalidPreamble(source, value)))

  private def parseRows(
      source: String,
      descriptor: EyeLinkOracleDescriptor,
      rows: Vector[String]
  ): Either[NonEmptyVector[EyeLinkOracleError], EyeLinkOracleManifest] =
    val parsed = rows.zipWithIndex.collect {
      case (row, index) if row.nonEmpty && !row.startsWith("#") =>
        parseRow(source, index + 3, row)
    }
    val rowErrors = parsed.collect { case Left(error) => error }
    val facts     = parsed.collect { case Right(fact) => fact }
    NonEmptyVector.fromVector(rowErrors) match
      case Some(errors) => Left(errors)
      case None         => create(descriptor, facts)

  private def parseRow(
      source: String,
      line: Int,
      row: String
  ): Either[EyeLinkOracleError, EyeLinkOracleFact] =
    row.split("\t", -1).toVector match
      case Vector(
            recordRaw,
            fieldRaw,
            kindRaw,
            blockRaw,
            orderRaw,
            pathRaw,
            presenceRaw,
            valueRaw,
            detailRaw
          ) =>
        for
          record   <- positiveInt(source, line, "record_ordinal", recordRaw)
          field    <- positiveInt(source, line, "field_ordinal", fieldRaw)
          kind     <- decode(source, line, "record_kind", kindRaw)
          block    <- optionalPositiveInt(source, line, "block", blockRaw)
          order    <- optionalPositiveLong(source, line, "source_order", orderRaw)
          path     <- decode(source, line, "field_path", pathRaw)
          presence <- enumValue(
            source,
            line,
            "presence",
            presenceRaw,
            EyeLinkOraclePresence.values
          )(_.token)
          value  <- decode(source, line, "value", valueRaw)
          detail <- decode(source, line, "detail", detailRaw)
          fact   <- EyeLinkOracleFact.create(
            record,
            field,
            kind,
            block,
            order,
            path,
            presence,
            value,
            detail
          )
        yield fact
      case values =>
        Left(EyeLinkOracleError.WrongFieldCount(source, line, columns.length, values.length))

  private def structuralErrors(
      descriptor: EyeLinkOracleDescriptor,
      facts: Vector[EyeLinkOracleFact]
  ): Vector[EyeLinkOracleError] =
    if facts.isEmpty then Vector(EyeLinkOracleError.EmptyManifest(descriptor.oracleId))
    else
      val byRecord        = facts.groupBy(_.recordOrdinal).toVector.sortBy(_._1)
      val recordOrdinals  = byRecord.map(_._1)
      val expectedRecords = (1 to byRecord.length).toVector
      val recordErrors    =
        if recordOrdinals == expectedRecords then Vector.empty
        else
          Vector(
            EyeLinkOracleError.NonContiguousRecords(
              descriptor.oracleId,
              expectedRecords,
              recordOrdinals
            )
          )
      val perRecordErrors = byRecord.flatMap { case (record, values) =>
        val ordered        = values.sortBy(_.fieldOrdinal)
        val fieldOrdinals  = ordered.map(_.fieldOrdinal)
        val expectedFields = (1 to ordered.length).toVector
        val kinds          = ordered.map(_.recordKind).distinct
        val blocks         = ordered.map(_.block).distinct
        val sourceOrders   = ordered.map(_.sourceOrder).distinct
        val duplicatePaths = ordered
          .groupBy(_.fieldPath)
          .collect {
            case (path, duplicates) if duplicates.size > 1 => path
          }
          .toVector
          .sorted
        Vector(
          Option.when(fieldOrdinals != expectedFields)(
            EyeLinkOracleError.NonContiguousFields(
              descriptor.oracleId,
              record,
              expectedFields,
              fieldOrdinals
            )
          ),
          Option.when(kinds.length != 1)(
            EyeLinkOracleError.InconsistentRecordMetadata(
              descriptor.oracleId,
              record,
              "record_kind",
              kinds
            )
          ),
          Option.when(blocks.length != 1)(
            EyeLinkOracleError.InconsistentRecordMetadata(
              descriptor.oracleId,
              record,
              "block",
              blocks.map(_.fold("absent")(_.toString))
            )
          ),
          Option.when(sourceOrders.length != 1)(
            EyeLinkOracleError.InconsistentRecordMetadata(
              descriptor.oracleId,
              record,
              "source_order",
              sourceOrders.map(_.fold("absent")(_.toString))
            )
          ),
          Option.when(duplicatePaths.nonEmpty)(
            EyeLinkOracleError.DuplicateFieldPath(
              descriptor.oracleId,
              record,
              duplicatePaths
            )
          )
        ).flatten
      }
      val orderedRecords = byRecord.flatMap(_._2.headOption)
      val orderingErrors = descriptor.ordering match
        case EyeLinkOracleOrdering.Complete =>
          val actual   = orderedRecords.map(_.sourceOrder)
          val concrete = actual.flatten
          if concrete.length != orderedRecords.length then
            Vector(EyeLinkOracleError.OrderingConflict(descriptor.oracleId, "complete", actual))
          else if concrete != concrete.sorted || concrete.distinct.length != concrete.length
          then
            Vector(
              EyeLinkOracleError
                .OrderingConflict(descriptor.oracleId, "strictly increasing", actual)
            )
          else Vector.empty
        case EyeLinkOracleOrdering.WithinKind =>
          orderedRecords.groupBy(_.recordKind).toVector.flatMap { case (kind, records) =>
            val actual   = records.map(_.sourceOrder)
            val concrete = actual.flatten
            Option.when(
              concrete.length != records.length || concrete != concrete.sorted ||
                concrete.distinct.length != concrete.length
            )(
              EyeLinkOracleError.OrderingConflict(
                descriptor.oracleId,
                s"strictly increasing within record kind=$kind",
                actual
              )
            )
          }
        case EyeLinkOracleOrdering.Unavailable =>
          val present                  = orderedRecords.flatMap(_.sourceOrder)
          val recordsMissingDisclosure = byRecord.collect {
            case (record, values)
                if !values.exists(fact =>
                  fact.fieldPath == "oracle.source-order" &&
                    fact.presence == EyeLinkOraclePresence.Omitted
                ) =>
              record
          }
          Vector(
            Option.when(present.nonEmpty)(
              EyeLinkOracleError.OrderingConflict(
                descriptor.oracleId,
                "no source order when ordering=unavailable",
                orderedRecords.map(_.sourceOrder)
              )
            ),
            Option.when(recordsMissingDisclosure.nonEmpty)(
              EyeLinkOracleError.MissingOrderingDisclosure(
                descriptor.oracleId,
                recordsMissingDisclosure
              )
            )
          ).flatten
      recordErrors ++ perRecordErrors ++ orderingErrors

  private def render(manifest: EyeLinkOracleManifest): String =
    val d        = manifest.descriptor
    val preamble = Vector(
      "# eyes4s-eyelink-oracle",
      schemaVersion.toString,
      encode(d.oracleId),
      encode(d.fixtureId),
      d.kind.token,
      d.inputKind.token,
      d.ordering.token,
      encode(d.toolName),
      encode(d.toolVersion),
      d.toolDigest.hex,
      d.adapterDigest.hex,
      d.inputDigest.hex,
      encode(d.invocation),
      d.converterReceiptDigest.map(_.hex).getOrElse("")
    ).mkString("\t")
    val rows = manifest.facts
      .sortBy(fact => (fact.recordOrdinal, fact.fieldOrdinal))
      .map { fact =>
        Vector(
          fact.recordOrdinal.toString,
          fact.fieldOrdinal.toString,
          encode(fact.recordKind),
          fact.block.fold("")(_.toString),
          fact.sourceOrder.fold("")(_.toString),
          encode(fact.fieldPath),
          fact.presence.token,
          encode(fact.value),
          encode(fact.detail)
        ).mkString("\t")
      }
    (preamble +: columns.mkString("\t") +: rows).mkString("\n") + "\n"

  private def enumValue[A](
      source: String,
      line: Int,
      field: String,
      raw: String,
      values: Array[A]
  )(token: A => String): Either[EyeLinkOracleError, A] =
    decode(source, line, field, raw).flatMap { decoded =>
      values
        .find(value => token(value) == decoded)
        .toRight(
          EyeLinkOracleError.InvalidValue(
            source,
            line,
            field,
            decoded,
            values.map(token).mkString(",")
          )
        )
    }

  private def positiveInt(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkOracleError, Int] =
    raw.toIntOption
      .filter(_ > 0)
      .toRight(
        EyeLinkOracleError.InvalidValue(source, line, field, raw, "positive integer")
      )

  private def optionalPositiveInt(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkOracleError, Option[Int]] =
    if raw.isEmpty then Right(None) else positiveInt(source, line, field, raw).map(Some(_))

  private def optionalPositiveLong(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkOracleError, Option[Long]] =
    if raw.isEmpty then Right(None)
    else
      raw.toLongOption
        .filter(_ > 0L)
        .map(Some(_))
        .toRight(
          EyeLinkOracleError.InvalidValue(source, line, field, raw, "positive integer")
        )

  private def digest(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkOracleError, Sha256] =
    decode(source, line, field, raw).flatMap(value =>
      Sha256
        .fromHex(s"$source:$line:$field", value)
        .left
        .map(error => EyeLinkOracleError.InvalidDigest(source, line, field, error.message))
    )

  private def optionalDigest(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkOracleError, Option[Sha256]] =
    if raw.isEmpty then Right(None) else digest(source, line, field, raw).map(Some(_))

  private def decode(
      source: String,
      line: Int,
      field: String,
      raw: String
  ): Either[EyeLinkOracleError, String] =
    val output                            = new java.lang.StringBuilder(raw.length)
    var index                             = 0
    var error: Option[EyeLinkOracleError] = None
    while index < raw.length && error.isEmpty do
      if raw.charAt(index) != '%' then
        output.append(raw.charAt(index))
        index += 1
      else if index + 2 >= raw.length then
        error = Some(EyeLinkOracleError.InvalidEscape(source, line, field, index, raw))
      else
        raw.substring(index, index + 3).toUpperCase match
          case "%09" => output.append('\t'); index += 3
          case "%0A" => output.append('\n'); index += 3
          case "%0D" => output.append('\r'); index += 3
          case "%25" => output.append('%'); index += 3
          case _     =>
            error = Some(EyeLinkOracleError.InvalidEscape(source, line, field, index, raw))
    error.toLeft(output.toString)

  private def encode(value: String): String =
    val output = new java.lang.StringBuilder(value.length)
    var index  = 0
    while index < value.length do
      value.charAt(index) match
        case '\t' => output.append("%09")
        case '\n' => output.append("%0A")
        case '\r' => output.append("%0D")
        case '%'  => output.append("%25")
        case char => output.append(char)
      index += 1
    output.toString

end EyeLinkOracleManifest

enum EyeLinkOracleError derives CanEqual:
  case InvalidPreamble(source: String, actual: String)
  case InvalidHeader(source: String, line: Int, expected: String, actual: String)
  case WrongFieldCount(source: String, line: Int, expected: Int, actual: Int)
  case InvalidEscape(source: String, line: Int, field: String, index: Int, value: String)
  case InvalidValue(source: String, line: Int, field: String, value: String, expected: String)
  case InvalidDigest(source: String, line: Int, field: String, detail: String)
  case InvalidDescriptor(oracleId: String, field: String, value: String, expected: String)
  case InvalidFact(
      recordOrdinal: Int,
      fieldOrdinal: Int,
      field: String,
      value: String,
      expected: String
  )
  case EmptyManifest(oracleId: String)
  case NonContiguousRecords(oracleId: String, expected: Vector[Int], actual: Vector[Int])
  case NonContiguousFields(
      oracleId: String,
      recordOrdinal: Int,
      expected: Vector[Int],
      actual: Vector[Int]
  )
  case InconsistentRecordMetadata(
      oracleId: String,
      recordOrdinal: Int,
      field: String,
      values: Vector[String]
  )
  case DuplicateFieldPath(oracleId: String, recordOrdinal: Int, paths: Vector[String])
  case OrderingConflict(
      oracleId: String,
      expected: String,
      actual: Vector[Option[Long]]
  )
  case MissingOrderingDisclosure(oracleId: String, recordOrdinals: Vector[Int])

  def message: String = this match
    case InvalidPreamble(source, actual) =>
      s"EyeLink oracle source='$source' has invalid preamble='$actual'."
    case InvalidHeader(source, line, expected, actual) =>
      s"EyeLink oracle source='$source' line=$line has header='$actual'; expected='$expected'."
    case WrongFieldCount(source, line, expected, actual) =>
      s"EyeLink oracle source='$source' line=$line has fields=$actual; expected=$expected."
    case InvalidEscape(source, line, field, index, value) =>
      s"EyeLink oracle source='$source' line=$line field='$field' has invalid escape at index=$index in value='$value'."
    case InvalidValue(source, line, field, value, expected) =>
      s"EyeLink oracle source='$source' line=$line field='$field' value='$value' is invalid; expected=$expected."
    case InvalidDigest(source, line, field, detail) =>
      s"EyeLink oracle source='$source' line=$line field='$field' has invalid digest: $detail"
    case InvalidDescriptor(oracleId, field, value, expected) =>
      s"EyeLink oracle='$oracleId' descriptor field='$field' value='$value' is invalid; expected=$expected."
    case InvalidFact(recordOrdinal, fieldOrdinal, field, value, expected) =>
      s"EyeLink oracle record=$recordOrdinal field-ordinal=$fieldOrdinal field='$field' value='$value' is invalid; expected=$expected."
    case EmptyManifest(oracleId) =>
      s"EyeLink oracle='$oracleId' contains no facts."
    case NonContiguousRecords(oracleId, expected, actual) =>
      s"EyeLink oracle='$oracleId' has record ordinals=${actual.mkString(",")}; expected=${expected.mkString(",")}."
    case NonContiguousFields(oracleId, recordOrdinal, expected, actual) =>
      s"EyeLink oracle='$oracleId' record=$recordOrdinal has field ordinals=${actual.mkString(",")}; expected=${expected.mkString(",")}."
    case InconsistentRecordMetadata(oracleId, recordOrdinal, field, values) =>
      s"EyeLink oracle='$oracleId' record=$recordOrdinal has inconsistent $field values=${values.mkString(",")}."
    case DuplicateFieldPath(oracleId, recordOrdinal, paths) =>
      s"EyeLink oracle='$oracleId' record=$recordOrdinal repeats field paths=${paths.mkString(",")}."
    case OrderingConflict(oracleId, expected, actual) =>
      s"EyeLink oracle='$oracleId' has source-order=${actual.map(_.fold("absent")(_.toString)).mkString(",")}; expected=$expected."
    case MissingOrderingDisclosure(oracleId, recordOrdinals) =>
      s"EyeLink oracle='$oracleId' declares ordering unavailable but records=${recordOrdinals.mkString(",")} lack an omitted oracle.source-order fact."

end EyeLinkOracleError

// Test-scope evidence apparatus (CR9): its codes stay in IoDiagnosticCatalog,
// which only ever appends, and its Diagnose instance lives with the enum.
object EyeLinkOracleError:
  given Diagnose[EyeLinkOracleError, Nothing] =
    Diagnose.derived[EyeLinkOracleError, Nothing](IoDiagnosticCatalog.eyeLinkOracle, lines)(
      _.message
    )

  private def line(source: String, number: Long): Vector[Locus[Nothing]] =
    Vector(Locus.Line(source, number))

  private def lines(error: EyeLinkOracleError): Vector[Locus[Nothing]] =
    error match
      case InvalidHeader(source, number, _, _)    => line(source, number)
      case WrongFieldCount(source, number, _, _)  => line(source, number)
      case InvalidEscape(source, number, _, _, _) => line(source, number)
      case InvalidValue(source, number, _, _, _)  => line(source, number)
      case InvalidDigest(source, number, _, _)    => line(source, number)
      case InvalidPreamble(_, _) | InvalidDescriptor(_, _, _, _) | InvalidFact(_, _, _, _, _) |
          EmptyManifest(_) | NonContiguousRecords(_, _, _) | NonContiguousFields(_, _, _, _) |
          InconsistentRecordMetadata(_, _, _, _) | DuplicateFieldPath(_, _, _) |
          OrderingConflict(_, _, _) | MissingOrderingDisclosure(_, _) =>
        Vector.empty
