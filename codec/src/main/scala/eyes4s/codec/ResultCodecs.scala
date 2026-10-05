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

package eyes4s.codec

import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** Built-in schemas for completed fixation-study results and the shipped score types. */
object StudyResultCodecs:
  val schema: DefinitionId = DefinitionId.studyResult

  /** The ordinary cosine route, matching `StudyCodecs.cosine`. */
  def cosine[U <: Unit2D: UnitLabel]
      : StudyResultCodec[StudyKey, U, Unit, Similarity, SignedDifference] =
    registered[U](ComparisonMethods.cosine)

  /** The ordinary route of a registered map method, matching `StudyCodecs.similarity`. */
  def registered[U <: Unit2D: UnitLabel](
      method: ComparisonMethod
  ): StudyResultCodec[StudyKey, U, Unit, Similarity, SignedDifference] =
    StudyCodecs.similarity[U](method).results(similarity(), signedDifference())

  /** The trial-keyed route of a registered map method, matching `StudyCodecs.trialSimilarity`. */
  def trialRegistered[U <: Unit2D: UnitLabel](
      method: ComparisonMethod
  ): StudyResultCodec[TrialKey, U, Unit, Similarity, SignedDifference] =
    StudyCodecs.trialSimilarity[U](method).results(similarity(), signedDifference())

  /** A finite similarity; a non-finite value is refused as the score type refuses it. */
  def similarity(schema: DefinitionId = DefinitionId.similarity): VersionedCodec[Similarity] =
    VersionedCodec.checked[Similarity](schema)(value =>
      Either.cond(
        value.value.isFinite,
        Json.fromDoubleOrNull(value.value),
        CodecError.Field("similarity", Json.Null, "similarity must be finite")
      )
    )(json =>
      json.asNumber
        .map(_.toDouble)
        .toRight(CodecError.Field("similarity", json, "expected a finite number"))
        .flatMap(v =>
          Similarity.of(v).left.map(e => CodecError.Field("similarity", json, e.message))
        )
    )

  def measureDistance(
      schema: DefinitionId = DefinitionId.measureDistance
  ): VersionedCodec[MeasureDistance] =
    VersionedCodec.checked[MeasureDistance](schema)(value =>
      Either.cond(
        value.value.isFinite,
        Json.fromDoubleOrNull(value.value),
        CodecError.Field("distance", Json.Null, "distance must be finite")
      )
    )(json =>
      json.asNumber
        .map(_.toDouble)
        .toRight(CodecError.Field("distance", json, "expected a finite number"))
        .flatMap(v =>
          MeasureDistance.of(v).left.map(e => CodecError.Field("distance", json, e.message))
        )
    )

  /** A finite scalar score. */
  def scalar(schema: DefinitionId = DefinitionId.scalar): VersionedCodec[Double] =
    VersionedCodec.checked[Double](schema)(value =>
      Either.cond(
        value.isFinite,
        Json.fromDoubleOrNull(value),
        CodecError.Field("scalar", Json.Null, "scalar must be finite")
      )
    )(json =>
      json.asNumber
        .map(_.toDouble)
        .filter(_.isFinite)
        .toRight(CodecError.Field("scalar", json, "expected a finite number"))
    )

  /** A finite signed difference between two finite operands. */
  def signedDifference(
      schema: DefinitionId = DefinitionId.signedDifference
  ): VersionedCodec[SignedDifference] =
    VersionedCodec.checked[SignedDifference](schema)(value =>
      Either.cond(
        value.value.isFinite,
        Json.fromDoubleOrNull(value.value),
        CodecError.Field("difference", Json.Null, "difference must be finite")
      )
    )(json =>
      json.asNumber
        .map(_.toDouble)
        .toRight(CodecError.Field("difference", json, "expected a finite number"))
        .flatMap(v =>
          SignedDifference
            .between(v, 0.0)
            .left
            .map(e => CodecError.Field("difference", json, e.message))
        )
    )

/** Versioned codec for one completed [[StudyResult]] of one plan family.
  *
  * The archive keeps the result's identity (plan description, input digest,
  * layout and method identities, the score and difference schemas), every
  * scale's estimation outcomes as densities on the plan grid or typed
  * failures, both directed pair analyses with every pair row, pairing
  * diagnostics, evaluation specification and provenance, the by-focal
  * reductions with their per-key denominators and reports, and the keyed
  * contrast rows. Decoding rebuilds the result through the checked
  * reconstruction APIs, which regroup the reductions from the pair rows,
  * re-derive every provenance from the rows and the plan description, and
  * refuse an archive whose counts, keys, denominators, specification or
  * provenance disagree, naming the operands that disagree. Only a completed result can be encoded: cancelled
  * or failed execution never produces a `StudyResult`.
  */
final class StudyResultCodec[K, U <: Unit2D, P, S, D](
    val schema: DefinitionId,
    val layout: StudyLayout[K],
    val keys: VersionedCodec[K],
    val method: StudyMethod[P, U, S, D],
    val scores: VersionedCodec[S],
    val differences: VersionedCodec[D],
    val parameters: Option[VersionedCodec[P]] = None
)(using unit: UnitLabel[U]):
  private given Ordering[K] = layout.ordering

  /** Encodes and digests the archive from one description (`describe`): its
    * digest is the digest of the encoded archive, computed one estimation,
    * pair row, reduction row or contrast row at a time.
    */
  val codec: VersionedCodec[StudyResult[K, U, S, D]] =
    VersionedCodec.described(schema)(describe)(read)

  private type Failure  = StudyFailure[K]
  private type Source   = DirectedPairwiseAnalysis[K, K, Failure, S]
  private type Estimate = (K, Either[Failure, Mass[U]])

  private def described(
      description: Vector[(String, Vector[Provenance.Param])],
      field: String,
      expected: DefinitionId
  ): Either[CodecError, Unit] =
    description.collectFirst { case (`field`, params) => params } match
      case Some(Vector(Provenance.Param.Text(name), Provenance.Param.Num(version)))
          if version.isWhole && version >= 1 =>
        DefinitionId
          .of(name, version.toInt)
          .left
          .map(CodecError.Definition.apply)
          .flatMap(found =>
            Either.cond(found == expected, (), CodecError.Schema(expected, found))
          )
      case other =>
        Left(
          CodecError.Field(
            field,
            ResultWire.values(other.getOrElse(Vector.empty)),
            s"description must declare $field as a name and version"
          )
        )

  private def describe(result: StudyResult[K, U, S, D]): Either[CodecError, CanonicalDoc] = for
    _     <- described(result.description, "layout", layout.id)
    _     <- described(result.description, "method", method.id)
    table <- result.scales.zipWithIndex.foldLeft[Either[CodecError, DocumentIdentities]](
      Right(DocumentIdentities.empty)
    ) { case (acc, (scale, index)) =>
      scale.estimation.foldLeft(acc) {
        case (table, (_, Right(mass))) =>
          table.flatMap(_.addGrid(mass.grid).left.map(Wire.at(s"scales[$index]")))
        case (table, _) => table
      }
    }
  yield
    import CanonicalDoc.Leaf
    CanonicalDoc.obj(
      "layout"           -> Leaf(Wire.id(layout.id)),
      "keySchema"        -> Leaf(Wire.id(keys.schema)),
      "method"           -> Leaf(Wire.id(method.id)),
      "scoreSchema"      -> Leaf(Wire.id(scores.schema)),
      "differenceSchema" -> Leaf(Wire.id(differences.schema)),
      "unit"             -> Leaf(Json.fromString(unit.symbol)),
      "input"            -> Leaf(Json.fromString(result.input.digest)),
      "description"      -> Leaf(Json.arr(result.description.map { case (field, values) =>
        Json.obj("field" -> Json.fromString(field), "values" -> ResultWire.values(values))
      }*)),
      "identities" -> Leaf(table.json),
      "scales"     -> CanonicalDoc.items(result.scales) { (scale, index) =>
        describeScale(scale).left
          .map(Wire.at(s"scales[$index]"))
          .map(_.mapError(Wire.at(s"scales[$index]")))
      }
    )

  private def read(json: Json): Either[CodecError, StudyResult[K, U, S, D]] =
    readIn(json, Vector.empty)

  /** Decode a result executed under a provenance context, as every temporal
    * cell is; see `StudyResult.reconstruct`.
    */
  private[codec] def decodeIn(
      document: Json,
      context: Vector[(String, Provenance.Param)]
  ): Either[CodecError, StudyResult[K, U, S, D]] = for
    found   <- Wire.definition(document, "schema")
    _       <- Either.cond(found == schema, (), CodecError.Schema(schema, found))
    payload <- Wire.field[Json](document, "value")
    result  <- readIn(payload, context)
  yield result

  private def readIn(
      json: Json,
      context: Vector[(String, Provenance.Param)]
  ): Either[CodecError, StudyResult[K, U, S, D]] = for
    _      <- Wire.requireId(json, "layout", layout.id)
    _      <- Wire.requireId(json, "keySchema", keys.schema)
    _      <- Wire.requireId(json, "method", method.id)
    _      <- Wire.requireId(json, "scoreSchema", scores.schema)
    _      <- Wire.requireId(json, "differenceSchema", differences.schema)
    symbol <- Wire.field[String](json, "unit")
    _      <- Either.cond(
      symbol == unit.symbol,
      (),
      CodecError.Field("unit", json, s"expected ${unit.symbol}, got $symbol")
    )
    digest <- Wire.field[String](json, "input")
    input  <- ArtifactRef.parse[StudyInput[K, U]](digest).left.map(CodecError.Definition.apply)
    fields <- Wire.field[Vector[Json]](json, "description")
    description <- fields.zipWithIndex.traverse { case (entry, index) =>
      (for
        field  <- Wire.field[String](entry, "field")
        values <- ResultWire.readValues(entry, "values")
      yield field -> values).left.map(Wire.at(s"description[$index]"))
    }
    _       <- described(description, "layout", layout.id)
    _       <- described(description, "method", method.id)
    table   <- Wire.field[Json](json, "identities").flatMap(DocumentIdentities.read)
    entries <- Wire.field[Vector[Json]](json, "scales")
    scales  <- entries.zipWithIndex.traverse { case (entry, index) =>
      readScale(table, entry).left.map(Wire.at(s"scales[$index]"))
    }
    result <- StudyResult
      .reconstruct(input, layout, description, scales, context)
      .left
      .map(e => CodecError.Result(e))
  yield result

  private def describeScale(
      scale: StudyScaleResult[K, U, S, D]
  ): Either[CodecError, CanonicalDoc] =
    import CanonicalDoc.Leaf
    val estimation = CanonicalDoc.items(scale.estimation) { case ((key, outcome), index) =>
      (for
        k <- keys.encode(key)
        o <- outcome match
          case Right(mass) =>
            Right(
              ResultWire.tagged(
                "mass",
                "grid"       -> Json.fromString(mass.grid.id.name),
                "values"     -> Json.arr(mass.values.toVector.map(Json.fromDoubleOrNull)*),
                "provenance" -> ResultWire.provenance(mass.provenance)
              )
            )
          case Left(failure) =>
            ResultWire
              .studyFailure(keys)(failure)
              .map(f => ResultWire.tagged("failure", "failure" -> f))
      yield Leaf(Json.obj("key" -> k, "outcome" -> o))).left
        .map(Wire.at(s"estimation[$index]"))
    }
    // The small members are written when the scale is described; the
    // estimation and every row array are made one item at a time.
    for
      excluded <- scale.excludedPhases.traverse(keys.encode).left.map(Wire.at("excludedPhases"))
      matched  <- describeAnalysis(scale.analyses.matchedSource, scale.analyses.matched).left
        .map(Wire.at("analyses.matched"))
      control <- describeAnalysis(scale.analyses.controlSource, scale.analyses.control).left
        .map(Wire.at("analyses.control"))
      contrast <- scale.contrast match
        case Right(value) => Right(describeContrast(value).mapError(Wire.at("contrast")))
        case Left(error)  =>
          ResultWire
            .contrastError[K, U](keys)(error)
            .map(e => Leaf(ResultWire.tagged("error", "error" -> e)))
            .left
            .map(Wire.at("contrast"))
    yield CanonicalDoc.obj(
      "estimate"       -> Leaf(StudyWire.estimate(scale.estimate)),
      "estimation"     -> estimation,
      "excludedPhases" -> Leaf(Json.arr(excluded*)),
      "analyses"       -> CanonicalDoc.obj(
        "matched" -> matched.mapError(Wire.at("analyses.matched")),
        "control" -> control.mapError(Wire.at("analyses.control"))
      ),
      "contrast" -> contrast
    )

  private def readScale(
      table: DocumentIdentities,
      json: Json
  ): Either[CodecError, StudyScaleResult[K, U, S, D]] = for
    estimate   <- Wire.field[Json](json, "estimate").flatMap(StudyWire.readEstimate[U])
    rows       <- Wire.field[Vector[Json]](json, "estimation")
    estimation <- rows.zipWithIndex.traverse { case (row, index) =>
      readEstimate(table, row).left.map(Wire.at(s"estimation[$index]"))
    }
    excluded  <- ResultWire.keyVector(keys, json, "excludedPhases")
    analysesJ <- Wire.field[Json](json, "analyses")
    matched   <- Wire
      .field[Json](analysesJ, "matched")
      .flatMap(readAnalysis)
      .left
      .map(Wire.at("analyses.matched"))
    control <- Wire
      .field[Json](analysesJ, "control")
      .flatMap(readAnalysis)
      .left
      .map(Wire.at("analyses.control"))
    analyses <- StudyAnalyses
      .of(matched._1, matched._2, control._1, control._2)
      .left
      .map(e => CodecError.Result(e))
    contrast <- Wire
      .field[Json](json, "contrast")
      .flatMap(readContrast(analyses, _))
      .left
      .map(Wire.at("contrast"))
    scale <- StudyScaleResult
      .reconstruct(estimate, estimation, excluded, analyses, contrast)
      .left
      .map(e => CodecError.Result(e))
  yield scale

  private def readEstimate(
      table: DocumentIdentities,
      json: Json
  ): Either[CodecError, Estimate] =
    for
      key     <- ResultWire.keyed(keys, json, "key")
      outcome <- Wire.field[Json](json, "outcome")
      kind    <- ResultWire.kind(outcome)
      value   <- kind match
        case "mass" =>
          for
            grid <- Wire
              .field[String](outcome, "grid")
              .flatMap(name => table.grid[U](GridId(name)))
            values <- Wire.field[Vector[Double]](outcome, "values")
            _      <- values.zipWithIndex.collectFirst { case (v, i) if !v.isFinite => i } match
              case Some(i) =>
                Left(CodecError.Field(s"values[$i]", outcome, "expected a finite number"))
              case None => Right(())
            provenance <- Wire
              .field[Json](outcome, "provenance")
              .flatMap(ResultWire.readProvenance)
            mass <- Surface
              .mass(grid, IArray.from(values), provenance)
              .left
              .map(e => CodecError.Field("values", outcome, e.message))
          yield Right(mass)
        case "failure" =>
          Wire
            .field[Json](outcome, "failure")
            .flatMap(ResultWire.readStudyFailure(keys))
            .map(Left(_))
        case other => Left(ResultWire.unknown(outcome, "estimation outcome", other))
    yield key -> value

  private def describeContrast(contrast: Contrast[K, S, D]): CanonicalDoc =
    CanonicalDoc.obj(
      "kind" -> CanonicalDoc.Leaf(Json.fromString("contrast")),
      "rows" -> CanonicalDoc.items(contrast.rows) { (row, index) =>
        (for
          key        <- keys.encode(row.key)
          difference <- row.difference match
            case Right(value) =>
              ResultWire
                .payload(differences, value)
                .map(d => ResultWire.tagged("value", "value" -> d))
            case Left(error) =>
              ResultWire
                .contrastRowError(keys)(error)
                .map(e => ResultWire.tagged("error", "error" -> e))
        yield CanonicalDoc.Leaf(
          Json.obj(
            "key"        -> key,
            "matched"    -> Json.fromBoolean(row.matched.isDefined),
            "control"    -> Json.fromBoolean(row.control.isDefined),
            "difference" -> difference
          )
        )).left.map(Wire.at(s"rows[$index]"))
      }
    )

  private def readContrast(
      analyses: StudyAnalyses[K, S],
      json: Json
  ): Either[CodecError, Either[ContrastError[K], Contrast[K, S, D]]] =
    ResultWire.kind(json).flatMap {
      case "error" =>
        Wire
          .field[Json](json, "error")
          .flatMap(ResultWire.readContrastError[K, U](keys))
          .map(Left(_))
      case "contrast" =>
        val matchedRows = analyses.matched.entries.map(row => row.key -> row).toMap
        val controlRows = analyses.control.entries.map(row => row.key -> row).toMap
        for
          entries <- Wire.field[Vector[Json]](json, "rows")
          rows    <- entries.zipWithIndex.traverse { case (entry, index) =>
            readContrastRow(entry, matchedRows, controlRows).left.map(Wire.at(s"rows[$index]"))
          }
          contrast <- Contrast
            .reconstruct(analyses.matched, analyses.control, rows, method.difference.components)
            .left
            .map(e => CodecError.Reconstruction(e))
        yield Right(contrast)
      case other => Left(ResultWire.unknown(json, "contrast outcome", other))
    }

  private def readContrastRow(
      json: Json,
      matchedRows: Map[K, ReductionRow[K, S]],
      controlRows: Map[K, ReductionRow[K, S]]
  ): Either[CodecError, ContrastRow[K, S, D]] = for
    key        <- ResultWire.keyed(keys, json, "key")
    hasMatched <- Wire.field[Boolean](json, "matched")
    hasControl <- Wire.field[Boolean](json, "control")
    matched    <-
      if !hasMatched then Right(None)
      else
        matchedRows
          .get(key)
          .toRight(
            CodecError.Reconstruction(
              ReconstructionError.ContrastOperand(key, ContrastOperand.Matched)
            )
          )
          .map(Some(_))
    control <-
      if !hasControl then Right(None)
      else
        controlRows
          .get(key)
          .toRight(
            CodecError.Reconstruction(
              ReconstructionError.ContrastOperand(key, ContrastOperand.Control)
            )
          )
          .map(Some(_))
    outcome    <- Wire.field[Json](json, "difference")
    kind       <- ResultWire.kind(outcome)
    difference <- kind match
      case "value" =>
        Wire
          .field[Json](outcome, "value")
          .flatMap(ResultWire.unwrap(differences, _))
          .map(Right(_))
      case "error" =>
        Wire
          .field[Json](outcome, "error")
          .flatMap(ResultWire.readContrastRowError(keys))
          .map(Left(_))
      case other => Left(ResultWire.unknown(outcome, "contrast difference", other))
    row <- ContrastRow
      .reconstruct(key, matched, control, difference)
      .left
      .map(e => CodecError.Reconstruction(e))
  yield row

  private def describeAnalysis(
      source: Source,
      analysis: Analysis[K, S]
  ): Either[CodecError, CanonicalDoc] = for
    written <- describeSource(source).left.map(Wire.at("source"))
    entries = CanonicalDoc.items(analysis.entries) { (row, index) =>
      (for
        key    <- keys.encode(row.key)
        result <- row.result match
          case Right(score) =>
            ResultWire.payload(scores, score).map(s => ResultWire.tagged("score", "score" -> s))
          case Left(error) =>
            ResultWire
              .reductionError(keys)(error)
              .map(e => ResultWire.tagged("error", "error" -> e))
      yield CanonicalDoc.Leaf(
        Json.obj(
          "key"          -> key,
          "result"       -> result,
          "successful"   -> Json.fromInt(row.successful),
          "failed"       -> Json.fromInt(row.failed),
          "contributing" -> Json.fromInt(row.contributing)
        )
      )).left.map(Wire.at(s"entries[$index]"))
    }
    diagnostics <- ResultWire
      .reductionReport(keys)(analysis.diagnostics)
      .left
      .map(Wire.at("diagnostics"))
  yield CanonicalDoc.obj(
    "source"      -> written.mapError(Wire.at("source")),
    "entries"     -> entries,
    "diagnostics" -> CanonicalDoc.Leaf(diagnostics),
    "provenance"  -> CanonicalDoc.Leaf(ResultWire.provenance(analysis.provenance))
  )

  private def readAnalysis(json: Json): Either[CodecError, (Source, Analysis[K, S])] = for
    source  <- Wire.field[Json](json, "source").flatMap(readSource).left.map(Wire.at("source"))
    entries <- Wire.field[Vector[Json]](json, "entries")
    rows    <- entries.zipWithIndex.traverse { case (entry, index) =>
      (for
        key     <- ResultWire.keyed(keys, entry, "key")
        outcome <- Wire.field[Json](entry, "result")
        kind    <- ResultWire.kind(outcome)
        result  <- kind match
          case "score" =>
            Wire
              .field[Json](outcome, "score")
              .flatMap(ResultWire.unwrap(scores, _))
              .map(Right(_))
          case "error" =>
            Wire
              .field[Json](outcome, "error")
              .flatMap(ResultWire.readReductionError(keys))
              .map(Left(_))
          case other => Left(ResultWire.unknown(outcome, "reduction outcome", other))
        successful   <- Wire.field[Int](entry, "successful")
        failed       <- Wire.field[Int](entry, "failed")
        contributing <- Wire.field[Int](entry, "contributing")
        row          <- ReductionRow
          .reconstruct(key, result, successful, failed, contributing)
          .left
          .map(e => CodecError.Reconstruction(e))
      yield row).left.map(Wire.at(s"entries[$index]"))
    }
    diagnostics <- Wire
      .field[Json](json, "diagnostics")
      .flatMap(ResultWire.readReductionReport(keys))
      .left
      .map(Wire.at("diagnostics"))
    provenance <- Wire.field[Json](json, "provenance").flatMap(ResultWire.readProvenance)
    analysis   <- Analysis
      .reconstructByLeft(rows, diagnostics, provenance, source)
      .left
      .map(e => CodecError.Reconstruction(e))
  yield (source, analysis)

  private def describeSource(source: Source): Either[CodecError, CanonicalDoc] =
    val rows = CanonicalDoc.items(source.rows) { (row, index) =>
      (for
        left   <- keys.encode(row.left)
        right  <- keys.encode(row.right)
        result <- row.result match
          case Right(score) =>
            ResultWire.payload(scores, score).map(s => ResultWire.tagged("score", "score" -> s))
          case Left(failure) =>
            ResultWire
              .studyFailure(keys)(failure)
              .map(f => ResultWire.tagged("failure", "failure" -> f))
      yield CanonicalDoc.Leaf(
        Json.obj("left" -> left, "right" -> right, "result" -> result)
      )).left
        .map(Wire.at(s"rows[$index]"))
    }
    for
      diagnostics <- ResultWire
        .pairingReport(keys)(source.diagnostics)
        .left
        .map(Wire.at("diagnostics"))
      evaluation <- ResultWire
        .evaluationInfo[U](source.evaluation)
        .left
        .map(Wire.at("evaluation"))
    yield CanonicalDoc.obj(
      "rows"        -> rows,
      "diagnostics" -> CanonicalDoc.Leaf(diagnostics),
      "provenance"  -> CanonicalDoc.Leaf(ResultWire.provenance(source.provenance)),
      "evaluation"  -> CanonicalDoc.Leaf(evaluation)
    )

  private def readSource(json: Json): Either[CodecError, Source] = for
    entries <- Wire.field[Vector[Json]](json, "rows")
    rows    <- entries.zipWithIndex.traverse { case (entry, index) =>
      (for
        left    <- ResultWire.keyed(keys, entry, "left")
        right   <- ResultWire.keyed(keys, entry, "right")
        outcome <- Wire.field[Json](entry, "result")
        kind    <- ResultWire.kind(outcome)
        result  <- kind match
          case "score" =>
            Wire
              .field[Json](outcome, "score")
              .flatMap(ResultWire.unwrap(scores, _))
              .map(Right(_))
          case "failure" =>
            Wire
              .field[Json](outcome, "failure")
              .flatMap(ResultWire.readStudyFailure(keys))
              .map(Left(_))
          case other => Left(ResultWire.unknown(outcome, "pair outcome", other))
      yield PairScore[K, K, Failure, S](left, right, result)).left.map(Wire.at(s"rows[$index]"))
    }
    diagnostics <- Wire
      .field[Json](json, "diagnostics")
      .flatMap(ResultWire.readPairingReport(keys))
      .left
      .map(Wire.at("diagnostics"))
    provenance <- Wire.field[Json](json, "provenance").flatMap(ResultWire.readProvenance)
    evaluation <- Wire
      .field[Json](json, "evaluation")
      .flatMap(ResultWire.readEvaluationInfo[U])
      .left
      .map(Wire.at("evaluation"))
    _ <- evaluation.specification match
      case Some(spec) if spec.components != method.difference.components =>
        Left(CodecError.ScoreComponents(method.difference.components, spec.components))
      case _ => Right(())
    source <- DirectedPairwiseAnalysis
      .reconstruct(rows, diagnostics, provenance, evaluation)
      .left
      .map(e => CodecError.Reconstruction(e))
  yield source

  def registration: StudyResultRegistration[K, U] =
    new StudyResultRegistration[K, U]:
      val id                                                         = method.id
      def decode(json: Json): Either[CodecError, LoadedResult[K, U]] =
        codec.decode(json).map(value => loaded(value, None))
      def decodeWithPayloads(
          json: Json,
          payloads: PayloadRef => Option[VerifiedPayload]
      ): Either[CodecError, LoadedResult[K, U]] =
        val archives = new DensityArchiveCodec(StudyResultCodec.this)
        archives.codec.decode(json).flatMap { archive =>
          archive
            .materialize(payloads)
            .left
            .map(error => CodecError.Field("density", json, error.message))
            .map(value => loaded(value, Some(archive)))
        }
      private def loaded(
          value: StudyResult[K, U, S, D],
          archive: Option[StudyResultArchive[K, U, P, S, D]]
      ): LoadedResult[K, U] =
        new LoadedResult[K, U]:
          type Score      = S
          type Difference = D
          def result              = value
          override def stampClaim = archive.flatMap(_.stampClaim)
          def encode              = archive.filter(_.stampClaim.nonEmpty) match
            case None        => codec.encode(value)
            case Some(saved) =>
              new DensityArchiveCodec(StudyResultCodec.this).codec.encode(saved)
          override def scoreSchema(planParameters: Json) =
            parameters match
              case None       => super.scoreSchema(planParameters)
              case Some(read) =>
                read
                  .decode(planParameters)
                  .flatMap(p =>
                    ScoreSchema
                      .method(method, p)
                      .left
                      .map(e => CodecError.Field("parameters", planParameters, e.message))
                  )

/** A decoded result whose score and difference types stay abstract but typed. */
trait LoadedResult[K, U <: Unit2D]:
  /** The pair score type the result's method produces. */
  type Score

  /** The type of a difference between two scores of that method. */
  type Difference
  def result: StudyResult[K, U, Score, Difference]
  def encode: Either[CodecError, Json]

  /** None is a legacy result; a saved claim still requires actual plan/input verification. */
  def stampClaim: Option[RunStamp[?, StudyInput[K, U]]] = None

  /** The components of the result's own method, under the method
    * parameters of the plan document `planParameters` (a plan's
    * `LoadedStudy.parametersDocument`), read with the result codec's own
    * parameter codec, so the components are typed by the result's scores.
    * A result decoded without a parameter codec cannot describe them.
    */
  def scoreSchema(planParameters: Json): Either[CodecError, ScoreSchema[Score, Difference]] =
    Left(
      CodecError.Field(
        "parameters",
        planParameters,
        "the result codec was built without its plan's parameter codec"
      )
    )

/** One study result codec as a registry sees it, keyed by method identity, with decoders
  * for inline archives and for archives whose densities are verified payloads. Sealed:
  * only the result codec's `registration` builds one.
  */
sealed trait StudyResultRegistration[K, U <: Unit2D]:
  def id: DefinitionId
  def decode(json: Json): Either[CodecError, LoadedResult[K, U]]
  def decodeWithPayloads(
      json: Json,
      payloads: PayloadRef => Option[VerifiedPayload]
  ): Either[CodecError, LoadedResult[K, U]]

/** Result codecs registered by method identity; lookup reads the payload's
  * `method` and refuses missing or duplicate registrations.
  */
final class StudyResultRegistry[K, U <: Unit2D] private (
    val entries: Vector[StudyResultRegistration[K, U]]
):
  def register(
      entry: StudyResultRegistration[K, U]
  ): Either[CodecError, StudyResultRegistry[K, U]] =
    if entries.exists(_.id == entry.id) then Left(CodecError.DuplicateResultCodec(entry.id))
    else Right(new StudyResultRegistry(entries :+ entry))

  def decode(json: Json): Either[CodecError, LoadedResult[K, U]] = for
    payload    <- Wire.field[Json](json, "value")
    method     <- Wire.definition(payload, "method")
    registered <- entries.find(_.id == method).toRight(CodecError.MissingResultCodec(method))
    result     <- registered.decode(json)
  yield result

  /** Materialize packed result archives using only the supplied verified chunks. */
  def decodeWithPayloads(
      json: Json,
      payloads: PayloadRef => Option[VerifiedPayload]
  ): Either[CodecError, LoadedResult[K, U]] = for
    payload    <- Wire.field[Json](json, "value")
    method     <- Wire.definition(payload, "method")
    registered <- entries.find(_.id == method).toRight(CodecError.MissingResultCodec(method))
    result     <- registered.decodeWithPayloads(json, payloads)
  yield result

object StudyResultRegistry:
  def empty[K, U <: Unit2D]: StudyResultRegistry[K, U] = new StudyResultRegistry(Vector.empty)
