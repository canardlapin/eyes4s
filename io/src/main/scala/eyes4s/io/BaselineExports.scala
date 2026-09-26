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

import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** Adapters for the finite baseline matrix. These are export views, not new result archives.
  * Each builds a table of the one result-table layer ([[eyes4s.results.ResultTable]]); reports
  * have their own families in [[eyes4s.results.ReportTables]].
  */
object ResultExports:
  import ResultCell.{Text as T, Integer as I, Number as N, Flag as B, Missing as M}
  private def text(name: String, meaning: String = "identifier", nullable: Boolean = false) =
    ResultColumn(
      name,
      if name.endsWith("_json") then ResultColumnType.JsonUtf8 else ResultColumnType.Utf8,
      nullable,
      "text",
      meaning
    )
  private def integer(name: String, unit: String = "count", nullable: Boolean = false) =
    ResultColumn(name, ResultColumnType.Int64, nullable, unit, name)
  private def number(name: String, unit: String, nullable: Boolean = true) =
    ResultColumn(name, ResultColumnType.Float64, nullable, unit, name)
  private def flag(name: String) =
    ResultColumn(name, ResultColumnType.Boolean, false, "boolean", name)
  private val status = ResultColumn(
    "status",
    ResultColumnType.Utf8,
    false,
    "label",
    "scientific outcome",
    Vector("success", "failure", "excluded")
  )
  private val outcome = Vector(status, text("error_json", "typed failure with operands", true))
  private val counts  =
    Vector(integer("selected"), integer("successful"), integer("contributing"))
  private def encoded[A](v: Either[CodecError, A]) = v.left.map(ResultExportError.Codec.apply)
  private def key[K](k: K, c: VersionedCodec[K]) = encoded(c.encode(k)).map(j => T(j.noSpaces))
  private def keySchema[K](keys: VersionedCodec[K]) = Json.obj(
    "name"    -> Json.fromString(keys.schema.name),
    "version" -> Json.fromInt(keys.schema.version)
  )
  private def scalar(v: Either[Json, Double]): Vector[ResultCell] = v match
    case Right(n) => Vector(N(n), T("success"), M)
    case Left(e)  => Vector(M, T("failure"), T(e.noSpaces))
  private def component[S, D](
      value: Either[Json, S],
      columns: ScoreColumns[S, D]
  ): Either[ResultExportError, Vector[Either[Json, Double]]] = value match
    case Left(e)  => Right(Vector.fill(columns.names.size)(Left(e)))
    case Right(s) =>
      columns
        .scores(s, "export score")
        .left
        .map(ResultExportError.Score.apply)
        .map(_.map(Right.apply))
  private def fields[K, U <: Unit2D: UnitLabel, E, S](
      a: PairwiseAnalysis[K, K, E, S],
      keys: VersionedCodec[K]
  ): Either[ResultExportError, Json] = for
    _ <- Either.cond(
      a.evaluation.specification.nonEmpty,
      (),
      ResultExportError.Context(a.evaluation.name, "export requires a versioned EvaluationSpec")
    )
    info   <- encoded(ExportMetadata.evaluation[U](a.evaluation))
    report <- encoded(ExportMetadata.pairing(keys, a.diagnostics))
  yield Json.obj(
    "key_schema" -> keySchema(keys),
    "evaluation" -> info,
    "pairing"    -> report,
    "provenance" -> ExportMetadata.provenance(a.provenance)
  )
  private def matchComponents[S, D](
      info: EvaluationInfo,
      columns: ScoreColumns[S, D]
  ): Either[ResultExportError, Unit] =
    Either.cond(
      info.specification.exists(_.components == columns.names),
      (),
      ResultExportError.Context(
        info.name,
        s"component columns ${columns.names} do not match declared specification"
      )
    )

  def pairs[K, U <: Unit2D: UnitLabel, S, D](
      a: PairwiseAnalysis[K, K, CompareError, S],
      keys: VersionedCodec[K],
      columns: ScoreColumns[S, D],
      scoreUnit: String
  ): Either[ResultExportError, ResultTable] = for
    _       <- matchComponents(a.evaluation, columns)
    context <- fields[K, U, CompareError, S](a, keys)
    rows    <- a.rows.zipWithIndex.traverse { (row, i) =>
      for
        left   <- key(row.left, keys); right <- key(row.right, keys)
        scores <- component(row.result.left.map(ExportMetadata.compareError), columns)
      yield scores.zip(columns.names).map { (v, c) =>
        Vector(I(i.toLong), left, right, T(c)) ++ scalar(v)
      }
    }
    table <- ResultTable.of(
      ResultFamily.PairScores,
      Vector(
        integer("edge_index"),
        text("left_key_json"),
        text("right_key_json"),
        text("component"),
        number("value", scoreUnit)
      ) ++ outcome,
      rows.flatten,
      context
    )
  yield table

  def reductions[K, U <: Unit2D: UnitLabel, S, D](
      a: Analysis[K, S],
      source: PairwiseAnalysis[K, K, CompareError, S],
      keys: VersionedCodec[K],
      columns: ScoreColumns[S, D],
      scoreUnit: String
  ): Either[ResultExportError, ResultTable] = for
    _ <- matchComponents(a.evaluation, columns)
    _ <- Either.cond(
      a.source == source,
      (),
      ResultExportError.Context("reduction source", "does not match retained source analysis")
    )
    sourceContext <- fields[K, U, CompareError, S](source, keys)
    info          <- encoded(ExportMetadata.evaluation[U](a.evaluation))
    report        <- encoded(ExportMetadata.reduction(keys, a.diagnostics))
    rows          <- a.entries.traverse { row =>
      for
        k   <- key(row.key, keys)
        raw <- row.result.swap
          .traverse(e => encoded(ExportMetadata.reductionError(keys, e)))
          .map(_.swap)
        values <- component(raw, columns)
      yield values
        .zip(columns.names)
        .map((v, c) =>
          Vector(k, T(c)) ++ scalar(v) ++ Vector(
            I(row.selected.toLong),
            I(row.successful.toLong),
            I(row.contributing.toLong)
          )
        )
    }
    table <- ResultTable.of(
      ResultFamily.Reductions,
      Vector(
        text("key_json"),
        text("component"),
        number("value", scoreUnit)
      ) ++ outcome ++ counts,
      rows.flatten,
      Json.obj(
        "key_schema" -> keySchema(keys),
        "source"     -> sourceContext,
        "evaluation" -> info,
        "reduction"  -> report,
        "provenance" -> ExportMetadata.provenance(a.provenance)
      )
    )
  yield table

  /** All three roles retain their own outcome and denominators; difference has no invented count. */
  def contrasts[K, U <: Unit2D: UnitLabel, S, D](
      a: Contrast[K, S, D],
      matchedSource: PairwiseAnalysis[K, K, CompareError, S],
      controlSource: PairwiseAnalysis[K, K, CompareError, S],
      keys: VersionedCodec[K],
      columns: ScoreColumns[S, D],
      scoreUnit: String
  ): Either[ResultExportError, ResultTable] = for
    matched     <- reductions[K, U, S, D](a.matched, matchedSource, keys, columns, scoreUnit)
    control     <- reductions[K, U, S, D](a.control, controlSource, keys, columns, scoreUnit)
    differences <- a.rows.traverse { row =>
      for
        k   <- key(row.key, keys)
        raw <- row.difference.swap
          .traverse(e => encoded(ExportMetadata.contrastError(keys, e)))
          .map(_.swap)
        values <- raw match
          case Left(e)  => Right(Vector.fill(columns.names.size)(Left(e)))
          case Right(d) =>
            columns
              .differences(d)
              .left
              .map(ResultExportError.Score.apply)
              .map(_.map(Right.apply))
      yield values
        .zip(columns.names)
        .map((v, c) => Vector(T("difference"), k, T(c)) ++ scalar(v) ++ Vector(M, M, M))
    }
    table <- ResultTable.of(
      ResultFamily.Contrasts,
      Vector(text("role", "matched, control or matched-minus-control")) ++ matched.columns.map(
        c =>
          if Set("selected", "successful", "contributing")(c.name) then c.copy(nullable = true)
          else c
      ),
      matched.rows.map(T("matched") +: _) ++ control.rows.map(
        T("control") +: _
      ) ++ differences.flatten,
      Json.obj(
        "matched" -> matched.context.circe,
        "control" -> control.context.circe,
        "sign"    -> Json.fromString("matched minus control")
      )
    )
  yield table

  /** Repetition plans carry their complete saved description alongside the two edge tables. */
  def repetition[K, U <: Unit2D: UnitLabel](
      plan: RepetitionPlan[K, U],
      codec: VersionedCodec[RepetitionPlan[K, U]],
      keys: VersionedCodec[K]
  ): Either[ResultExportError, Vector[ResultTable]] = for
    saved <- encoded(codec.encode(plan))
    result = plan.run
    matched <- pairs[K, U, Similarity, SignedDifference](
      result.matched,
      keys,
      ScoreColumns.similarity,
      "unitless"
    )
    control <- pairs[K, U, Similarity, SignedDifference](
      result.controls,
      keys,
      ScoreColumns.similarity,
      "unitless"
    )
    tables <- Vector("matched" -> matched, "control" -> control).traverse { (role, t) =>
      ResultTable.of(
        t.family,
        t.columns,
        t.rows,
        t.context.circe.deepMerge(
          Json.obj(
            "role"      -> Json.fromString(role),
            "plan"      -> saved,
            "plan_hash" -> Json.fromString(plan.planHash.render)
          )
        )
      )
    }
  yield tables

  def points[K, U <: Unit2D: UnitLabel](
      archive: PointSamplingArchive[K, U],
      codec: PointSamplingCodec[K, U]
  ): Either[ResultExportError, Vector[ResultTable]] = for
    saved <- encoded(codec.archive.encode(archive))
    result  = archive.result
    context = Json.obj(
      "archive"         -> saved,
      "spatial_unit"    -> Json.fromString(summon[UnitLabel[U]].symbol),
      "value_unit"      -> Json.fromString("normalized template amplitude"),
      "difference_sign" -> Json.fromString("observed minus control")
    )
    keyed <- result.rows.traverse { row =>
      for
        k        <- key(row.key, codec.keys)
        template <- row.template.traverse(k => encoded(codec.keys.encode(k)))
      yield (row, k, template)
    }
    sourceRows = keyed.map { (row, k, template) =>
      Vector(I(row.index.toLong), k) ++ (template match
        case Right(t) => Vector(T(t.noSpaces), T("success"), M)
        case Left(e)  => Vector(M, T("failure"), T(codec.error(e).noSpaces))) ++ Vector(
        I(row.eligibleControls.toLong),
        I(row.controls.size.toLong)
      )
    }
    sources <- ResultTable.of(
      ResultFamily.PointSources,
      Vector(
        integer("source_index"),
        text("key_json"),
        text("template_key_json", nullable = true)
      ) ++ outcome ++ Vector(integer("eligible_controls"), integer("selected_controls")),
      sourceRows,
      context
    )
    controlRows <- keyed.traverse { (row, k, _) =>
      row.controls.zipWithIndex
        .traverse { (c, ci) =>
          for
            occurrence <- key(c.occurrence, codec.keys); template <- key(c.template, codec.keys)
          yield c.values.zipWithIndex.map((v, i) =>
            Vector(
              I(row.index.toLong),
              k,
              I(ci.toLong),
              occurrence,
              template,
              I(i.toLong),
              I(archive.plan.spec.queries(i).toMicros)
            ) ++ scalar(v.left.map(codec.error))
          )
        }
        .map(_.flatten)
    }
    controls <- ResultTable.of(
      ResultFamily.PointControls,
      Vector(
        integer("source_index"),
        text("key_json"),
        integer("control_index"),
        text("occurrence_key_json"),
        text("template_key_json"),
        integer("query_index"),
        integer("time_us", "microseconds"),
        number("value", "template amplitude")
      ) ++ outcome,
      controlRows.flatten,
      context
    )
    pointRows = keyed.flatMap { (row, k, _) =>
      row.points.flatMap { p =>
        val prefix = Vector(
          I(row.index.toLong),
          k,
          I(p.index.toLong),
          I(p.time.toMicros),
          p.bin.fold[ResultCell](M)(i => I(i.toLong)),
          p.point.fold[ResultCell](M)(v => N(v.x)),
          p.point.fold[ResultCell](M)(v => N(v.y))
        )
        val observed =
          prefix ++ Vector(T("observed")) ++ scalar(p.value.left.map(codec.error)) ++ Vector(
            I(1),
            I(if p.value.isRight then 1 else 0),
            I(if p.value.isRight then 1 else 0)
          )
        Vector(observed) ++ p.control.toVector.map(c =>
          prefix ++ Vector(T("control")) ++ scalar(c.result.left.map(codec.error)) ++ Vector(
            I(c.requested.toLong),
            I(c.successful.toLong),
            I(c.contributing.toLong)
          )
        )
      }
    }
    samples <- ResultTable.of(
      ResultFamily.PointSamples,
      Vector(
        integer("source_index"),
        text("key_json"),
        integer("query_index"),
        integer("time_us", "microseconds"),
        integer("bin_index", nullable = true),
        number("x", summon[UnitLabel[U]].symbol),
        number("y", summon[UnitLabel[U]].symbol),
        text("role"),
        number("value", "template amplitude")
      ) ++ outcome ++ counts,
      pointRows,
      context
    )
    binRows = keyed.flatMap { (row, k, _) =>
      row.bins.flatMap { b =>
        val prefix = Vector(
          I(row.index.toLong),
          k,
          I(b.index.toLong),
          I(b.from.toMicros),
          I(b.until.toMicros),
          B(b.includesFinalEndpoint),
          T(b.queries.mkString("[", ",", "]"))
        )
        def mean(role: String, m: PointMean) = prefix ++ Vector(T(role)) ++ scalar(
          m.result.left.map(codec.error)
        ) ++ Vector(I(m.requested.toLong), I(m.successful.toLong), I(m.contributing.toLong))
        Vector(mean("observed", b.observed)) ++ b.control.toVector.map(
          mean("control", _)
        ) ++ b.difference.toVector.map(d =>
          prefix ++ Vector(T("difference")) ++ scalar(d.left.map(codec.error)) ++ Vector(
            M,
            M,
            M
          )
        )
      }
    }
    bins <- ResultTable.of(
      ResultFamily.PointBins,
      Vector(
        integer("source_index"),
        text("key_json"),
        integer("bin_index"),
        integer("from_us", "microseconds"),
        integer("until_us", "microseconds"),
        flag("includes_final_endpoint"),
        text("query_indices_json"),
        text("role"),
        number("value", "template amplitude")
      ) ++ outcome ++ counts.map(_.copy(nullable = true)),
      binRows,
      context
    )
  yield Vector(sources, controls, samples, bins)

  def fixedTemplate[K: KeyDigest](
      split: TemplateSplit[K],
      schema: DefinitionId,
      keys: VersionedCodec[K]
  ): Either[ResultExportError, Vector[ResultTable]] = for
    saved  <- encoded(NativeTemplateRecipeCodec.of(schema, keys).encode(split))
    fitted <- FittedTemplate
      .fitNoIntercept(split.training)
      .left
      .map(e => ResultExportError.Context("template fit", e.message))
    evaluation <- fitted
      .evaluate(split.heldOut)
      .left
      .map(e => ResultExportError.Context("template evaluation", e.message))
    context = Json.obj(
      "recipe"        -> saved,
      "method"        -> Json.fromString(fitted.methodId),
      "backend"       -> Json.fromString(fitted.backend),
      "training_hash" -> Json.fromString(evaluation.trainingHash.render),
      "held_out_hash" -> Json.fromString(evaluation.heldOutHash.render),
      "key_schema"    -> keySchema(keys),
      "response_unit" -> Json.fromString(split.basis.responseUnit),
      "residual_sign" -> Json.fromString("observed minus predicted")
    )
    coefficients <- ResultTable.of(
      ResultFamily.TemplateCoefficients,
      Vector(
        text("feature"),
        number("coefficient", split.basis.responseUnit + " / feature unit", false)
      ),
      split.basis.columns.zip(fitted.coefficients).map((name, v) => Vector(T(name), N(v))),
      context
    )
    rows <- evaluation.rows.traverse { r =>
      key(r.key, keys).map { k =>
        val result = r.result.left.map(e =>
          Json.obj(
            "kind"    -> Json.fromString(e.productPrefix),
            "message" -> Json.fromString(e.message)
          )
        )
        Vector(k, T(r.fold), N(r.observed)) ++ scalar(result.map(_._1)) ++ Vector(
          result.fold(_ => M, p => N(p._2))
        )
      }
    }
    predictions <- ResultTable.of(
      ResultFamily.TemplatePredictions,
      Vector(
        text("key_json"),
        text("fold"),
        number("observed", split.basis.responseUnit, false),
        number("predicted", split.basis.responseUnit)
      ) ++ outcome ++ Vector(number("residual", split.basis.responseUnit)),
      rows,
      context
    )
  yield Vector(coefficients, predictions)

  def learnedTemplate[K: KeyDigest, U <: Unit2D: UnitLabel](
      split: MapTemplateSplit[K, U],
      schema: DefinitionId,
      keys: VersionedCodec[K]
  ): Either[ResultExportError, Vector[ResultTable]] = for
    saved  <- encoded(LearnedTemplateRecipeCodec.of[K, U](schema, keys).encode(split))
    fitted <- LearnedTemplate
      .fit(split.training)
      .left
      .map(e => ResultExportError.Context("learned template fit", e.message))
    evaluation <- fitted
      .evaluate(split.heldOut)
      .left
      .map(e => ResultExportError.Context("learned template evaluation", e.message))
    unit    = split.training.responseUnit
    context = Json.obj(
      "recipe"         -> saved,
      "method"         -> Json.fromString(LearnedTemplate.method),
      "training_hash"  -> Json.fromString(evaluation.trainingHash.render),
      "held_out_hash"  -> Json.fromString(evaluation.heldOutHash.render),
      "key_schema"     -> keySchema(keys),
      "response_unit"  -> Json.fromString(unit),
      "residual_sign"  -> Json.fromString("observed minus predicted"),
      "training_count" -> Json.fromString(split.training.rows.size.toString),
      "held_out_count" -> Json.fromString(split.heldOut.rows.size.toString),
      "excluded_count" -> Json.fromString(split.excluded.size.toString)
    )
    coefficients <- ResultTable.of(
      ResultFamily.TemplateCoefficients,
      Vector(text("feature"), number("coefficient", unit + " / cosine similarity", false)),
      Vector(Vector(T("training-mean cosine"), N(fitted.slope))),
      context
    )
    rows <- evaluation.rows.traverse { r =>
      key(r.key, keys).map { k =>
        val result = r.result.left.map(e =>
          Json.obj(
            "kind"    -> Json.fromString(e.productPrefix),
            "message" -> Json.fromString(e.message)
          )
        )
        Vector(k, T(r.splitGroup), T(r.matchGroup), N(r.observed)) ++ scalar(
          result.map(_._1)
        ) ++ Vector(result.fold(_ => M, p => N(p._2)))
      }
    }
    predictions <- ResultTable.of(
      ResultFamily.TemplatePredictions,
      Vector(
        text("key_json"),
        text("split_group"),
        text("match_group"),
        number("observed", unit, false),
        number("predicted", unit)
      ) ++ outcome ++ Vector(number("residual", unit)),
      rows,
      context
    )
    excludedRows <- split.excluded.traverse(e =>
      key(e.row.key, keys).map(k =>
        Vector(k, T(e.row.splitGroup), T(e.row.matchGroup), T("excluded"), T(e.reason.toString))
      )
    )
    excluded <- ResultTable.of(
      ResultFamily.TemplateExclusions,
      Vector(
        text("key_json"),
        text("split_group"),
        text("match_group"),
        status,
        text("reason")
      ),
      excludedRows,
      context
    )
  yield Vector(coefficients, predictions, excluded)

  def ols[U <: Unit2D: UnitLabel](
      fit: SurfaceOlsFit[U]
  ): Either[ResultExportError, Vector[ResultTable]] =
    val grid    = fit.fitted.grid
    val context = Json.obj(
      "method"        -> Json.fromString("surface-ols-householder"),
      "provenance"    -> ExportMetadata.provenance(fit.fitted.provenance),
      "frame"         -> ExportMetadata.frame(grid.frame),
      "grid_id"       -> Json.fromString(grid.id.name),
      "nx"            -> Json.fromInt(grid.nx),
      "ny"            -> Json.fromInt(grid.ny),
      "residual_sign" -> Json.fromString("observed minus fitted"),
      "r_squared"     -> Json.fromString(if fit.intercept.isDefined then "centered"
      else "uncentered"),
      "scaled_diagonal_ratio" -> Json.fromString(
        "ratio of largest to smallest scaled R diagonal; not a condition number"
      )
    )
    for
      coefficients <- ResultTable.of(
        ResultFamily.OlsCoefficients,
        Vector(
          text("predictor"),
          text("role"),
          number("value", "see value_unit", false),
          text("value_unit")
        ),
        fit.coefficients.map((k, v) =>
          Vector(T(k.value), T("predictor"), N(v), T("response mass / predictor mass"))
        ) ++ fit.intercept.toVector.map(v =>
          Vector(T("intercept"), T("intercept"), N(v), T("response mass/cell"))
        ),
        context
      )
      diagnostics <- ResultTable.of(
        ResultFamily.OlsDiagnostics,
        Vector(
          integer("rank"),
          integer("cells"),
          number("residual_sum_squares", "squared response mass", false),
          number("r_squared", "unitless"),
          text("r_squared_status"),
          number("scaled_diagonal_ratio", "unitless", false)
        ),
        Vector(
          Vector(
            I(fit.diagnostics.rank.toLong),
            I(fit.fitted.size.toLong),
            N(fit.diagnostics.residualSumSquares),
            fit.diagnostics.rSquared.fold[ResultCell](M)(N.apply),
            T(if fit.diagnostics.rSquared.isDefined then "defined"
            else "zero total response variation"),
            N(fit.diagnostics.scaledDiagonalRatio)
          )
        ),
        context
      )
      cells <- ResultTable.of(
        ResultFamily.OlsCells,
        Vector(
          integer("cell_index"),
          integer("column_index"),
          integer("row_index"),
          number("fitted", "response mass/cell", false),
          number("residual", "response mass/cell", false)
        ),
        Vector.tabulate(fit.fitted.size)(i =>
          Vector(
            I(i.toLong),
            I((i % grid.nx).toLong),
            I((i / grid.nx).toLong),
            N(fit.fitted.values(i)),
            N(fit.residual.values(i))
          )
        ),
        context
      )
    yield Vector(coefficients, diagnostics, cells)

  /** Existing public CSV builders remain authoritative for study and duration-window rows. */
  def study[K, U <: Unit2D: UnitLabel, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      result: StudyResult[K, U, S, D],
      codec: StudyCodec[K, U, P, S, D],
      columns: ScoreColumns[S, D]
  ): Either[ResultExportError, ResultTable] =
    for
      saved <- encoded(codec.codec.encode(plan))
      doc   <- ContrastCsv
        .document(plan, result, codec, columns)
        .left
        .map(ResultExportError.Score.apply)
      table <- legacy(
        ResultFamily.StudyContrasts,
        doc,
        ContrastCsv.schemaVersion,
        Json.obj("plan" -> saved, "key_schema" -> keySchema(codec.keys))
      )
    yield table

  def temporal[K, U <: Unit2D: UnitLabel, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      result: TemporalStudyResult[K, U, P, S, D],
      codec: TemporalStudyCodec[K, U, P, S, D],
      columns: ScoreColumns[S, D]
  ): Either[ResultExportError, Vector[ResultTable]] = for
    saved <- encoded(codec.codec.encode(plan))
    docs  <- TemporalContrastCsv
      .document(plan, result, codec, columns)
      .left
      .map(ResultExportError.Score.apply)
    context = Json.obj("plan" -> saved, "key_schema" -> keySchema(codec.study.keys))
    contrasts <- legacy(
      ResultFamily.TemporalContrasts,
      docs.contrasts,
      TemporalContrastCsv.schemaVersion,
      context
    )
    coverage <- legacy(
      ResultFamily.TemporalCoverage,
      docs.coverage,
      TemporalContrastCsv.schemaVersion,
      context
    )
  yield Vector(contrasts, coverage)

  private def legacy(
      family: ResultFamily,
      doc: TidyCsvDocument,
      sourceSchema: String,
      context: Json
  ): Either[ResultExportError, ResultTable] =
    val times    = Set("from_us", "until_us", "observed_us", "missing_us", "retained_us")
    val integers = times ++ Set(
      "excluded_fixation_count",
      "method_version",
      "matched_selected",
      "matched_successful",
      "matched_failed",
      "matched_contributing",
      "control_selected",
      "control_successful",
      "control_failed",
      "control_contributing"
    )
    val numbers = Set("matched", "control", "difference", "sigma", "sigma_x", "sigma_y")
    val columns = doc.header.map { name =>
      if integers(name) then
        integer(name, if times(name) then "microseconds" else "count", true)
      else if numbers(name) then
        number(
          name,
          if name.startsWith("sigma") then "spatial_unit column" else "score_scale column"
        )
      else
        text(
          name,
          "existing CSV field; original schema and plan retained",
          nullable = name.endsWith("_json")
        )
    }
    val rows = doc.rows.zipWithIndex.traverse { (row, i) =>
      row.zip(columns).traverse { (v, c) =>
        if v.isEmpty && c.nullable then Right(M: ResultCell)
        else if c.kind == ResultColumnType.Utf8 || c.kind == ResultColumnType.JsonUtf8 then
          Right(T(v): ResultCell)
        else if c.kind == ResultColumnType.Int64 then
          v.toLongOption
            .map(I.apply)
            .toRight(ResultExportError.Cell(i, c.name, T(v), "expected exact Int64"))
        else
          v.toDoubleOption
            .filter(_.isFinite)
            .map(N.apply)
            .toRight(ResultExportError.Cell(i, c.name, T(v), "expected finite Float64"))
      }
    }
    rows.flatMap(
      ResultTable.of(
        family,
        columns,
        _,
        context.deepMerge(
          Json.obj(
            "source_schema"     -> Json.fromString(sourceSchema),
            "evidence_location" -> Json.fromString(
              "plan_json, key_json, provenance and failure columns in each row"
            )
          )
        )
      )
    )
