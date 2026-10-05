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

import eyes4s.kernel.Unit2D
import eyes4s.plan.*
import eyes4s.results.*

/** The public way to obtain a bound [[eyes4s.results.ReportSource]]: the
  * binding is computed from the very plan, input, result and ledger the
  * source reads, never supplied by the caller.
  */
object ReportSources:
  /** A source over `result`, which `plan` computed on `input`, bound to the
    * canonical digests of all three and, when `ledger` is given, of the
    * ledger. The ledger must be consistent with the input
    * (`AdmissionLedger.checkAgainst`); the covariates declared by
    * `covariates` are read from its trials table, which it must then carry,
    * each study key joined to its trial through the layout. Without a ledger the
    * source has no covariates and a report that reads any is refused
    * (`ReportError.UnboundCovariates`).
    */
  def study[K, U <: Unit2D, P, S, D](
      plans: StudyCodec[K, U, P, S, D],
      inputs: StudyInputCodec[K, U],
      results: StudyResultCodec[K, U, P, S, D]
  )(
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      result: StudyResult[K, U, S, D],
      ledger: Option[AdmissionLedger[K]],
      covariates: CovariateSchema
  ): Either[CodecError, ReportSource[K]] =
    val table = covariateTable(plan, input, ledger, covariates)
    for
      found   <- table
      binding <- ReportCodecs.binding(
        (plans.codec, plan),
        (inputs.input, input),
        (results.codec, result),
        ledger.map(l => (inputs.ledger, l))
      )
      source <- ReportSource
        .study(plan, input, result, found, binding)
        .left
        .map(CodecError.Report.apply)
    yield source

  /** A source over query tables a host already holds, one per scale in
    * scale order, with the covariate table their covariate values come
    * from. The binding is the canonical digest of exactly what the source
    * reads, never supplied: `plan` of each scale's index and score
    * components, `input` of each query's identity (key, participant, item,
    * phase, occurrence), its unparsed covariate cells and its window values,
    * `result` of each query's stored outcomes, and `covariates` of the
    * covariate table's schema and of its rows for the queried keys (in query
    * order; rows for other keys are not read and not bound). A change to any
    * value the source reads changes the binding.
    *
    * The tables must be indexed in order (`ReportError.TableOrder`); their
    * covariates must be the table's schema (`ReportError.TableCovariates`)
    * and, query by query, the table's values and unparsed cells (a host that
    * holds tables reads its covariates from one place). Without a covariate
    * table the queries must carry no covariate.
    */
  def tables[K: Ordering](
      keys: io.circe.Encoder[K]
  )(
      queryTables: Vector[QueryTable[K]],
      covariates: Option[CovariateTable[K]]
  ): Either[CodecError, ReportSource[K]] =
    def refuse[A](e: ReportError[K]): Either[CodecError, A] = Left(CodecError.Report(e))
    def names(s: CovariateSchema)                           = s.names.map(_.value)
    val declared  = covariates.fold(CovariateSchema.empty)(_.schema)
    val misplaced = queryTables.zipWithIndex.collectFirst {
      case (t, i) if t.scale != i => ReportError.TableOrder(i, t.scale)
    }
    val schema = queryTables.collectFirst {
      case t if t.covariates != declared =>
        if covariates.isEmpty then ReportError.UnboundCovariates(names(t.covariates))
        else ReportError.TableCovariates(names(t.covariates), names(declared))
    }
    val disagreeing = covariates.flatMap(table =>
      queryTables.iterator
        .flatMap(_.queries)
        .flatMap { q =>
          val unparsed = table.row(q.key).fold(Vector.empty)(_.unparsed)
          declared.names
            .collectFirst {
              case name if q.covariate(name) != table.value(q.key, name) =>
                s"its covariate ${name.value} is ${q.covariate(name)} in the query table " +
                  s"and ${table.value(q.key, name)} in the covariate table"
            }
            .orElse(
              Option.when(q.unparsed != unparsed)(
                s"its unparsed covariate cells are ${q.unparsed} in the query table and " +
                  s"$unparsed in the covariate table"
              )
            )
            .map(ReportError.InvalidQuery(q.key, _))
        }
        .nextOption()
    )
    misplaced.orElse(schema).orElse(disagreeing) match
      case Some(error) => refuse(error)
      case None        =>
        for binding <- TableBinding.of(keys, queryTables, covariates)
        yield ReportSource.fromQueries(queryTables, binding)

  /** The covariates of `input`'s keys from `ledger`'s trials table, when
    * `covariates` declares any; none is needed otherwise.
    */
  private def covariateTable[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      ledger: Option[AdmissionLedger[K]],
      covariates: CovariateSchema
  ): Either[CodecError, Option[CovariateTable[K]]] =
    val keys = input.trials.rows.map(_.key)
    ledger match
      case None =>
        Either.cond(
          covariates.covariates.isEmpty,
          None,
          CodecError.Report(ReportError.UnboundCovariates(covariates.names.map(_.value)))
        )
      case Some(l) =>
        l.checkAgainst(input).left.map(CodecError.Admission.apply).flatMap { _ =>
          (l.inventory, covariates.covariates.isEmpty) match
            case (_, true)     => Right(None)
            case (None, false) =>
              Left(
                CodecError.Report(
                  ReportError.UnboundCovariates(covariates.names.map(_.value))
                )
              )
            case (Some(inventory), false) =>
              CovariateTable
                .forKeys(covariates, inventory, plan.layout, keys)
                .left
                .map(CodecError.Covariates.apply)
                .map(Some(_))
        }

  /** Re-evaluate a stored report over the decoded documents its binding was
    * verified against, and refuse it unless it is exactly what the shipped
    * reduction computes: the first differing cell is named, or else the first
    * differing part. Only window tallies and a linear reduction are
    * recomputed; no pair is scored.
    *
    * No type is assumed shared between the plan and the result: the plan
    * supplies the layout, window tallies and covariates, and the result is
    * reduced through its own method's score schema.
    */
  private[codec] def reevaluate[K, U <: Unit2D](
      plan: LoadedStudy[K, U],
      input: StudyInput[K, U],
      result: LoadedResult[K, U],
      ledger: Option[AdmissionLedger[K]],
      stored: Report[K]
  ): Option[RelationMismatch] =
    // The result is read with its own method's components: the plan's
    // parameters, re-read by the result codec's parameter codec, give the
    // result method's score schema, typed by the result's scores. The plan's
    // and the result's methods must name the same components.
    val planComponents = ScoreSchema.study(plan.plan).map(_.ids)
    val resultSchema   = plan.parametersDocument.flatMap(result.scoreSchema)
    val components     = (planComponents, resultSchema) match
      case (Right(planned), Right(schema)) if planned != schema.ids =>
        Some(RelationMismatch.ReportComponents(planned, schema.ids))
      case _ => None
    lazy val recomputed = for
      schema     <- resultSchema
      covariates <- CovariateSchema
        .of(stored.spec.covariates)
        .left
        .map(e => CodecError.Covariates(e))
      table <- covariateTable(plan.plan, input, ledger, covariates)
      source = ReportSource.of(
        result.result,
        plan.plan.layout,
        plan.plan.windowTallies(input),
        table,
        schema,
        stored.binding
      )
      report <- Report.evaluate(stored.spec, source).left.map(CodecError.Report.apply)
    yield report
    components.orElse(recomputed match
      case Left(error) =>
        Some(
          RelationMismatch.ReportRecomputed("evaluation", "the stored report", error.message)
        )
      case Right(fresh) if fresh == stored => None
      case Right(fresh)                    => Some(difference(stored, fresh)))

  /** The first cell (by position) that differs, else the first other part. */
  private def difference[K](stored: Report[K], fresh: Report[K]): RelationMismatch =
    val size  = math.max(stored.cells.size, fresh.cells.size)
    val cells = (0 until size).iterator
      .map(i => (stored.cells.lift(i), fresh.cells.lift(i)))
      .collectFirst {
        case (a, b) if a != b =>
          val at = a.orElse(b).get
          RelationMismatch.ReportCell(
            at.group.render,
            at.role.toString,
            at.component,
            a.fold("no cell")(_.toString),
            b.fold("no cell")(_.toString)
          )
      }
    cells.getOrElse {
      def part[A](name: String, a: A, b: A)(using CanEqual[A, A]) =
        Option.when(a != b)(RelationMismatch.ReportRecomputed(name, a.toString, b.toString))
      part("groups", stored.groups, fresh.groups)
        .orElse(part("contrasts", stored.contrasts, fresh.contrasts))
        .orElse(part("accounting", stored.accounting, fresh.accounting))
        .orElse(part("findings", stored.findings, fresh.findings))
        .getOrElse(
          RelationMismatch.ReportRecomputed("report", stored.toString, fresh.toString)
        )
    }

/** The canonical documents a [[ReportSources.tables]] binding digests. Each
  * names its schema and lists values in table and query order, so equal
  * tables give equal digests on the JVM and Scala.js.
  */
private object TableBinding:
  import io.circe.Json

  private def value[A](v: Value[A])(json: A => Json): Json = v match
    case Value.Present(a) => Json.obj("present" -> json(a))
    case Value.Missing(r) => Json.obj("missing" -> ReportCodecs.absence(r))

  private def unparsed(cells: Vector[(CovariateName, String)]): Json =
    Json.arr(
      cells.map((n, raw) =>
        Json.obj("name" -> Json.fromString(n.value), "raw" -> Json.fromString(raw))
      )*
    )

  private def covariate(v: CovariateValue): Json = v match
    case CovariateValue.Number(d) => Json.obj("number" -> Json.fromDoubleOrNull(d))
    case CovariateValue.Level(l)  => Json.obj("level" -> Json.fromString(l))
    case CovariateValue.Flag(b)   => Json.obj("flag" -> Json.fromBoolean(b))

  private def outcome(o: RoleOutcome): Json = o match
    case RoleOutcome.Scored(v) => Json.obj("scored" -> Json.arr(v.map(Json.fromDoubleOrNull)*))
    case RoleOutcome.Failed(code, message) =>
      Json.obj(
        "failed" -> Json.obj(
          "code"    -> Json.fromString(code.render),
          "message" -> Json.fromString(message)
        )
      )
    case RoleOutcome.NotStored => Json.obj("notStored" -> Json.True)

  private def doc(kind: String, body: Json): Json =
    Json.obj("schema" -> Json.fromString(s"eyes4s.report-tables.$kind/1"), "body" -> body)

  def of[K](
      keys: io.circe.Encoder[K],
      tables: Vector[QueryTable[K]],
      covariates: Option[CovariateTable[K]]
  ): Either[CodecError, ReportBinding] =
    def each(f: Query[K] => Json) =
      Json.arr(tables.map(t => Json.arr(t.queries.map(f)*))*)
    // The covariate rows the source reads: the queried keys, in query order.
    val read = tables.flatMap(_.queries.map(_.key)).distinct
    val plan = doc(
      "plan",
      Json.arr(
        tables.map(t =>
          Json.obj(
            "scale"      -> Json.fromInt(t.scale),
            "components" -> Json.arr(t.components.map(Json.fromString)*)
          )
        )*
      )
    )
    val input = doc(
      "input",
      each(q =>
        Json.obj(
          "key"         -> keys(q.key),
          "participant" -> Json.fromString(q.participant),
          "item"        -> Json.fromString(q.item),
          "phase"       -> Json.fromString(q.phase),
          "occurrence"  -> Json.fromInt(q.occurrence),
          "unparsed"    -> unparsed(q.unparsed),
          "window"      -> Json.arr(
            q.window.map((m, v) =>
              Json.obj(
                "measure" -> Json.fromString(m.toString),
                "value"   -> value(v)(Json.fromDoubleOrNull)
              )
            )*
          )
        )
      )
    )
    val result = doc(
      "result",
      each(q =>
        Json.obj(
          "key"        -> keys(q.key),
          "matched"    -> outcome(q.matched),
          "control"    -> outcome(q.control),
          "difference" -> outcome(q.difference)
        )
      )
    )
    val covariateDoc = covariates.map(t =>
      doc(
        "covariates",
        Json.obj(
          "schema" -> Json.arr(
            t.schema.covariates.map(c =>
              Json.obj(
                "name" -> Json.fromString(c.name.value),
                "type" -> Json.fromString(c.kind.render)
              )
            )*
          ),
          "rows" -> Json.arr(
            read.map(k =>
              Json.obj(
                "key"    -> keys(k),
                "values" -> Json.arr(
                  t.schema.names.map(n =>
                    Json.obj(
                      "name"  -> Json.fromString(n.value),
                      "value" -> value(t.value(k, n))(covariate)
                    )
                  )*
                ),
                "unparsed" -> unparsed(t.row(k).fold(Vector.empty)(_.unparsed))
              )
            )*
          )
        )
      )
    )
    def digest(json: Json, field: String) =
      CanonicalDigest.document[Json](json).flatMap(ReportCodecs.digest(_, field))
    for
      p <- digest(plan, "plan")
      i <- digest(input, "input")
      r <- digest(result, "result")
      c <- covariateDoc.fold[Either[CodecError, Option[BindingDigest]]](Right(None))(d =>
        digest(d, "covariates").map(Some(_))
      )
    yield ReportBinding(p, i, r, c)
