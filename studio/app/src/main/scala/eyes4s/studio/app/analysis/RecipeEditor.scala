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

package eyes4s.studio.app.analysis

import cats.syntax.all.*
import eyes4s.studio.core.backend.{AnalysisRevision, Phase}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*

/** Text remains editable until all fields parse into the document's domain values. */
final case class RecipeInput(
    focal: String,
    reference: String,
    columns: String,
    rows: String,
    scales: String,
    pixelsPerDegree: String,
    xMin: String,
    yMin: String,
    xMax: String,
    yMax: String,
    weighting: WeightChoice,
    unmatched: UnmatchedChoice
) derives CanEqual

enum RecipeInputError derives CanEqual:
  case Invalid(field: String, value: String, reason: String)
  def message: String = this match
    case Invalid(field, value, reason) => s"$field '$value': $reason"

object RecipeEditor:
  def input(r: Recipe): RecipeInput =
    RecipeInput(
      r.phases.focal.label,
      r.phases.reference.label,
      r.grid.columns.toString,
      r.grid.rows.toString,
      r.scales.values.map(_.degrees.toString).mkString(", "),
      r.angularScale.fold("")(_.value.toString),
      r.window.fold("")(_.xMin.toString),
      r.window.fold("")(_.yMin.toString),
      r.window.fold("")(_.xMax.toString),
      r.window.fold("")(_.yMax.toString),
      r.weighting,
      r.unmatched
    )

  def parse(base: Recipe, f: RecipeInput): Either[RecipeInputError, Recipe] =
    def number(name: String, raw: String): Either[RecipeInputError, Double] =
      raw.trim.toDoubleOption
        .filter(_.isFinite)
        .toRight(RecipeInputError.Invalid(name, raw, "enter a finite number"))
    def integer(name: String, raw: String): Either[RecipeInputError, Int] =
      raw.trim.toIntOption
        .filter(_ > 0)
        .toRight(RecipeInputError.Invalid(name, raw, "enter a positive integer"))
    def domain[A](name: String, raw: String)(v: Either[DocumentError, A]) =
      v.leftMap(e => RecipeInputError.Invalid(name, raw, e.message))
    def phase(name: String, raw: String): Either[RecipeInputError, Phase] =
      Either.cond(
        raw.trim.nonEmpty,
        Phase(raw.trim),
        RecipeInputError.Invalid(name, raw, "enter a phase label")
      )
    val coordinates = Vector(f.xMin, f.yMin, f.xMax, f.yMax)
    for
      focal     <- phase("Query phase", f.focal)
      reference <- phase("Reference phase", f.reference)
      _         <- Either.cond(
        focal != reference,
        (),
        RecipeInputError.Invalid("Phases", f.focal, "query and reference must differ")
      )
      cols   <- integer("Grid columns", f.columns)
      rows   <- integer("Grid rows", f.rows)
      grid   <- domain("Grid", s"${f.columns} × ${f.rows}")(GridSize.of(cols, rows))
      scales <- f.scales
        .split(",", -1)
        .toVector
        .traverse(raw =>
          number("Gaussian σ (degrees)", raw)
            .flatMap(v => domain("Gaussian σ", raw)(Sigma.of(v)))
        )
        .flatMap(v => domain("Gaussian scales", f.scales)(ScaleSet.of(v)))
      ppd <-
        if f.pixelsPerDegree.trim.isEmpty then Right(None)
        else
          number("Pixels per degree", f.pixelsPerDegree)
            .flatMap(v =>
              domain("Pixels per degree", f.pixelsPerDegree)(DeclaredPixelsPerDegree.of(v))
            )
            .map(Some(_))
      window <-
        if coordinates.forall(_.trim.isEmpty) then Right(None)
        else
          for
            x0 <- number("Window x minimum", f.xMin)
            y0 <- number("Window y minimum", f.yMin)
            x1 <- number("Window x maximum", f.xMax)
            y1 <- number("Window y maximum", f.yMax)
            w  <- domain("Half-open window", coordinates.mkString(", "))(
              AnalysisWindow.of(x0, y0, x1, y1)
            )
          yield Some(w)
    yield base.copy(
      phases = PhasePair(focal, reference),
      grid = grid,
      scales = scales,
      angularScale = ppd,
      window = window,
      offWindow = window.map(_ => base.offWindow.getOrElse(OffWindowChoice.Exclude)),
      weighting = f.weighting,
      unmatched = f.unmatched
    )

  /** A single undoable edit; stale-before checks stay in the command reducer. */
  def command(
      document: StudioDocument,
      input: RecipeInput,
      revision: Option[AnalysisRevision] = None
  ): Either[RecipeInputError, Option[Command]] =
    PresetPicker
      .edited(document, revision)
      .toRight(
        RecipeInputError.Invalid("Analysis", "", "choose a preset to start an analysis")
      )
      .flatMap { (context, base) =>
        Either
          .cond(
            document.draft.forall(_.id == context.id),
            (),
            RecipeInputError.Invalid(
              "Analysis",
              context.id.toString,
              "finish or discard the other draft first"
            )
          )
          .flatMap(_ => parse(base, input))
          .map { next =>
            val changes = RecipeChange.between(base, next)
            Option.when(changes.nonEmpty)(
              if document.draft.isEmpty then Command.StartDraft(context.id, None, changes)
              else Command.ChangeRecipes(changes)
            )
          }
      }
