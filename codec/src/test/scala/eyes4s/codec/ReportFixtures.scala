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

import eyes4s.plan.*
import eyes4s.results.*

/** The pinned report documents' values: a covariate schema of every type, a
  * specification with a three-valued filter, two grouping dimensions (one
  * binned), a minimum, a level contrast and the standard error, and the
  * report it gives over a small query table with a failed query, an unknown
  * filter, an ungrouped query, a participant below the minimum and an
  * unpaired participant. `covariate-schema-v1.json`, `report-spec-v1.json`
  * and `report-v1.json` are these values as the codecs write them.
  */
object ReportFixtures:
  private def get[E, A](e: Either[E, A]): A =
    e.fold(error => throw new AssertionError(s"$error"), identity)

  val keys: VersionedCodec[StudyKey] = StudyCodecs.key(DefinitionId.studyKey)

  val memory: CovariateName     = get(CovariateName.of("memory"))
  val confidence: CovariateName = get(CovariateName.of("confidence"))
  val levels: Levels            = get(Levels.of(Vector("Remembered", "Forgotten")))
  val rating: NumericUnit       = get(NumericUnit.of("rating"))
  val memoryTerm: LevelTerm     = LevelTerm.Categorical(memory, levels)

  val covariateSchema: CovariateSchema = get(
    CovariateSchema.of(
      Vector(
        Covariate(memory, CovariateType.Categorical(levels)),
        Covariate(confidence, CovariateType.Numeric(rating)),
        Covariate(
          get(CovariateName.of("certainty")),
          CovariateType.Ordinal(get(Levels.of(Vector("guess", "sure"))))
        ),
        Covariate(get(CovariateName.of("seen")), CovariateType.Binary)
      )
    )
  )

  private val tableSchema = get(
    CovariateSchema.of(covariateSchema.covariates.take(2))
  )

  val spec: ReportSpec = get(
    ReportSpec.of(
      get(ReportId.of("memory by confidence")),
      0,
      get(ReportSelection.of(Vector(Role.Difference, Role.Matched), Vector("value"))),
      Some(
        Predicate.Cmp(
          NumericTerm.Window(WindowMeasure.OutsideWindowShare),
          Comparison.LessOrEqual,
          0.25
        ) && !Predicate.IsMissing(memoryTerm)
      ),
      Vector(
        Grouping.ByBins(
          NumericTerm.Covariate(confidence, rating),
          get(
            Bins.of(
              Vector(
                get(Bin.of("low", 1, 3, UpperEdge.Excluded)),
                get(Bin.of("high", 3, 4, UpperEdge.Included))
              )
            )
          )
        ),
        Grouping.ByLevel(memoryTerm)
      ),
      ReducePolicy.ParticipantMeans(get(MinimumQueries.of(2))),
      Some(LevelContrast(memoryTerm, "Remembered", "Forgotten")),
      Spread.StandardDeviationAndError
    )
  )

  val binding: ReportBinding = ReportBinding(
    get(BindingDigest.parse("plan", "1" * 64)),
    get(BindingDigest.parse("input", "2" * 64)),
    get(BindingDigest.parse("result", "3" * 64)),
    Some(get(BindingDigest.parse("covariates", "4" * 64)))
  )

  private def query(
      p: String,
      item: String,
      remembered: Option[Boolean],
      rated: Option[Double],
      share: Option[Double],
      difference: Option[Double]
  ): Query[StudyKey] = get(
    Query.of(
      StudyKey(p, item, "recall"),
      p,
      item,
      "recall",
      1,
      Vector(
        memory -> remembered.fold(Value.Missing(Absence.NotRecorded))(r =>
          Value.Present(CovariateValue.Level(if r then "Remembered" else "Forgotten"))
        ),
        confidence -> rated.fold(Value.Missing(Absence.Unparsed))(r =>
          Value.Present(CovariateValue.Number(r))
        )
      ),
      rated.fold(Vector(confidence -> "high"))(_ => Vector.empty),
      Vector(
        WindowMeasure.OutsideWindowShare -> share.fold(
          Value.Missing(Absence.Undefined(UndefinedReason.ZeroDuration))
        )(Value.Present(_))
      ),
      difference.fold(RoleOutcome.NotStored)(v => RoleOutcome.Scored(Vector(v + 0.5))),
      RoleOutcome.NotStored,
      difference.fold(
        RoleOutcome.Failed(DiagnosticCode("contrast-row", "arithmetic"), "overflowed")
      )(v => RoleOutcome.Scored(Vector(v)))
    )
  )

  val table: QueryTable[StudyKey] = get(
    QueryTable.of(
      0,
      Vector("value"),
      tableSchema,
      Vector(
        query("p1", "a", Some(true), Some(4), Some(0.0), Some(0.25)),
        query("p1", "b", Some(true), Some(3), Some(0.125), Some(0.5)),
        query("p1", "c", Some(false), Some(4), Some(0.0), Some(-0.125)),
        query("p1", "d", Some(false), Some(4), Some(0.25), Some(-0.375)),
        query("p2", "a", Some(true), Some(3.5), Some(0.0), Some(0.75)),
        query("p2", "b", Some(true), Some(4), Some(0.0), None),
        query("p2", "c", Some(false), Some(1), None, Some(0.125)),
        query("p2", "d", Some(false), None, Some(0.0), Some(0.0)),
        query("p3", "a", Some(true), Some(3), Some(0.0), Some(1.0)),
        query("p3", "b", Some(true), Some(3), Some(0.5), Some(0.5)),
        query("p3", "e", Some(true), Some(3.5), Some(0.0), Some(0.25))
      )
    )
  )

  val report: Report[StudyKey] = get(Report.reduce(spec, table, binding))

  val covariateSchemaFile = "covariate-schema-v1.json"
  val reportSpecFile      = "report-spec-v1.json"
  val reportFile          = "report-v1.json"
