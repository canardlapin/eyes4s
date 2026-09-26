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

package eyes4s.results

/** A report as result tables: the one table layer every transport renders
  * (CSV and Arrow in `eyes4s-io`), and the rows figures and methods text
  * cite. A missing value is a null cell with its absence named beside it,
  * never a zero.
  */
object ReportTables:
  import ResultCell.{Text as T, Integer as I, Number as N, Flag as B, Missing as M}

  /** The label of a role in every report table. */
  def role(r: Role): String = r match
    case Role.Matched    => "matched"
    case Role.Control    => "control"
    case Role.Difference => "difference"

  /** The label of an absence in every report table. */
  def absence(a: Absence): String = a match
    case Absence.NotRecorded        => "not-recorded"
    case Absence.Unparsed           => "unparsed"
    case Absence.Failed(_, _)       => "failed"
    case Absence.Undefined(_)       => "undefined"
    case Absence.EmptyGroup         => "empty-group"
    case Absence.Unpaired           => "unpaired"
    case Absence.BelowMinimum(_, _) => "below-minimum"

  private val absences =
    Vector(
      "not-recorded",
      "unparsed",
      "failed",
      "undefined",
      "empty-group",
      "unpaired",
      "below-minimum"
    )

  private def text(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Utf8, false, "text", meaning)
  private def json(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.JsonUtf8, false, "text", meaning)
  private def count(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Int64, false, "count", meaning)
  private def number(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Float64, true, "score component", meaning)
  private def reason(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Utf8, true, "label", meaning, absences)
  private val roleColumn =
    ResultColumn(
      "role",
      ResultColumnType.Utf8,
      false,
      "label",
      "matched, control, or matched minus control",
      Role.values.toVector.map(role)
    )
  private def value(v: Value[Double]): Vector[ResultCell] = v match
    case Value.Present(x) => Vector(N(x), M)
    case Value.Missing(a) => Vector(M, T(absence(a)))
  private def valued(name: String, meaning: String) =
    Vector(number(name, meaning), reason(name + "_absence", s"why $name is missing"))
  private def spreadColumns(spec: ReportSpec) =
    Vector(count("n", "values the estimate averages")) ++
      valued("sd", "sample standard deviation of those values (n - 1)") ++
      (if spec.spread == Spread.StandardDeviationAndError then
         valued("sem", "standard error of the mean, sd / sqrt(n)")
       else Vector.empty)
  private def spreadCells(spec: ReportSpec, d: CellSpread) =
    Vector(I(d.n.toLong)) ++ value(d.sd) ++
      (if spec.spread == Spread.StandardDeviationAndError then
         value(d.sem.getOrElse(Value.Missing(Absence.NotRecorded)))
       else Vector.empty)
  private def groupJson(g: GroupKey): String =
    TableJson
      .Arr(
        g.levels.map((t, l) =>
          TableJson.obj("term" -> TableJson.text(t), "level" -> TableJson.text(l))
        )
      )
      .canonical
  private def head(r: Report[?]) =
    Vector(T(r.spec.id.value), I(r.spec.scale.toLong))
  private val headColumns = Vector(
    text("report_id", "report specification id"),
    count("scale", "estimation scale index")
  )

  /** The context every report table carries: the report, its binding, how
    * it reduced and filtered, and each role's accounting.
    */
  def context(r: Report[?]): TableJson =
    val spec = r.spec
    TableJson.obj(
      "report" -> TableJson.text(spec.id.value),
      "scale"  -> TableJson.integer(spec.scale.toLong),
      "reduce" -> (spec.reduce match
        case ReducePolicy.ParticipantMeans(m) =>
          TableJson.obj(
            "kind"           -> TableJson.text("participant-means"),
            "minimumQueries" -> TableJson.integer(m.value.toLong),
            "weight"         -> TableJson.text("each participant equally")
          )
        case ReducePolicy.PooledQueries =>
          TableJson.obj(
            "kind"   -> TableJson.text("pooled-queries"),
            "weight" -> TableJson.text("each query equally")
          )),
      "spread"   -> TableJson.text(spec.spread.toString),
      "interval" -> TableJson.text("none: n and spread only"),
      "filter"   -> spec.filter.fold[TableJson](TableJson.Null)(p => TableJson.text(p.render)),
      "groupBy"  -> TableJson.Arr(spec.groupBy.map(g => TableJson.text(g.on.render))),
      "contrast" -> spec.contrast.fold[TableJson](TableJson.Null)(c =>
        TableJson.obj(
          "term"       -> TableJson.text(c.term.render),
          "minuend"    -> TableJson.text(c.minuend),
          "subtrahend" -> TableJson.text(c.subtrahend),
          "sign"       -> TableJson.text("minuend minus subtrahend, within participant")
        )
      ),
      "difference" -> TableJson.text("matched minus control"),
      "binding"    -> TableJson.obj(
        "plan"       -> TableJson.text(r.binding.plan.hex),
        "input"      -> TableJson.text(r.binding.input.hex),
        "result"     -> TableJson.text(r.binding.result.hex),
        "covariates" -> r.binding.covariates.fold[TableJson](TableJson.Null)(d =>
          TableJson.text(d.hex)
        )
      ),
      "accounting" -> TableJson.Arr(r.accounting.map { a =>
        TableJson.obj(
          "role"                  -> TableJson.text(role(a.role)),
          "eligible"              -> TableJson.integer(a.eligible.toLong),
          "kept"                  -> TableJson.integer(a.kept.toLong),
          "filteredOut"           -> TableJson.integer(a.filteredOut.toLong),
          "unknownPredicate"      -> TableJson.integer(a.unknownPredicate.toLong),
          "failed"                -> TableJson.integer(a.failed.toLong),
          "missingGroupAttribute" -> TableJson.integer(a.missingGroupAttribute.toLong),
          "belowMinimum"          -> TableJson.integer(a.belowMinimum.toLong)
        )
      })
    )

  /** One row per cell: group, role, component, estimate, counts and spread. */
  def cells(r: Report[?]): Either[ResultTableError, ResultTable] =
    ResultTable.of(
      ResultFamily.ReportCells,
      headColumns ++ Vector(
        json("group_json", "one level of each grouping term, in grouping order"),
        roleColumn,
        text("component", "score component")
      ) ++ valued("estimate", "the group estimate") ++ Vector(
        count("participants", "participants whose value contributes"),
        count("queries", "queries of the group with a stored value"),
        count("failed", "queries of the group that passed the filter with no stored value")
      ) ++ spreadColumns(r.spec),
      r.cells.map(c =>
        head(r) ++ Vector(T(groupJson(c.group)), T(role(c.role)), T(c.component)) ++
          value(c.estimate) ++
          Vector(I(c.participants.toLong), I(c.queries.toLong), I(c.failed.toLong)) ++
          spreadCells(r.spec, c.dispersion)
      ),
      context(r)
    )

  /** One row per participant of each cell. */
  def participants(r: Report[?]): Either[ResultTableError, ResultTable] =
    ResultTable.of(
      ResultFamily.ReportParticipants,
      headColumns ++ Vector(
        json("group_json", "one level of each grouping term, in grouping order"),
        roleColumn,
        text("component", "score component"),
        text("participant", "participant, as the study layout names it"),
        count("queries", "the participant's queries in the group")
      ) ++ valued("value", "mean of the participant's queries in the group") ++ Vector(
        ResultColumn(
          "contributes",
          ResultColumnType.Boolean,
          false,
          "boolean",
          "whether the value enters the estimate"
        )
      ),
      r.cells.flatMap(c =>
        c.perParticipant.map(p =>
          head(r) ++ Vector(
            T(groupJson(c.group)),
            T(role(c.role)),
            T(c.component),
            T(p.participant),
            I(p.queries.toLong)
          ) ++ value(p.value) ++ Vector(B(p.value.isPresent))
        )
      ),
      context(r)
    )

  /** One row per within-participant level contrast. */
  def contrasts(r: Report[?]): Either[ResultTableError, ResultTable] =
    ResultTable.of(
      ResultFamily.ReportContrasts,
      headColumns ++ Vector(
        json("stratum_json", "one level of each other grouping term"),
        text("term", "the contrasted grouping term"),
        text("minuend", "level subtracted from"),
        text("subtrahend", "level subtracted"),
        roleColumn,
        text("component", "score component")
      ) ++ valued("estimate", "mean within-participant difference, minuend minus subtrahend") ++
        spreadColumns(r.spec) ++ Vector(
          count("paired", "participants with a value at both levels"),
          count("unpaired", "participants with a value at one level only"),
          json(
            "unpaired_json",
            "each unpaired participant, the level it has and the one it lacks"
          )
        ),
      r.contrasts.map(s =>
        head(r) ++ Vector(
          T(groupJson(s.stratum)),
          T(s.term),
          T(s.minuend),
          T(s.subtrahend),
          T(role(s.role)),
          T(s.component)
        ) ++ value(s.estimate) ++ spreadCells(r.spec, s.dispersion) ++ Vector(
          I(s.paired.size.toLong),
          I(s.unpaired.size.toLong),
          T(
            TableJson
              .Arr(
                s.unpaired.map(u =>
                  TableJson.obj(
                    "participant" -> TableJson.text(u.participant),
                    "present"     -> TableJson.text(u.present),
                    "missing"     -> TableJson.text(u.missing)
                  )
                )
              )
              .canonical
          )
        )
      ),
      context(r)
    )

  /** The three families of one report, in family order. */
  def all(r: Report[?]): Either[ResultTableError, Vector[ResultTable]] =
    for
      a <- cells(r)
      b <- participants(r)
      c <- contrasts(r)
    yield Vector(a, b, c)
