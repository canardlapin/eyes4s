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

package eyes4s.plan

import cats.syntax.all.*
import eyes4s.core.{FixationBoundary, Viewing}
import eyes4s.detect.InterpolationGap
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** The typed values of a temporal study's own form fields. */
final case class TemporalRecipe(
    boundary: FixationBoundary,
    windows: Vector[StudyWindow],
    repetitions: Vector[RepetitionContrast]
):
  /** The temporal plan these values describe over `base`, or the plan's own
    * whole-recipe refusal.
    */
  def plan[K, U <: Unit2D: UnitLabel, P, S, D](
      base: StudyPlan[K, U, P, S, D],
      input: ArtifactRef[TemporalStudyInput[K, U]]
  ): Either[TemporalStudyError, TemporalStudyPlan[K, U, P, S, D]] =
    TemporalStudyPlan.of(base, input, windows, repetitions, boundary)

/** The form of a temporal study's own fields; its base study has a
  * [[StudyForm]]. Each field parses on its own.
  */
final class TemporalForm:
  import RawParts.*
  import RecipeViews as V
  private type E = RecipeParameterError
  private def err(e: E): String = e.message

  val boundary: StructuredField[E, FixationBoundary] =
    StructuredField.of[E, FixationBoundary](RecipeParameters.boundary.info.view)(
      raw => choose("temporal.boundary", raw, FixationBoundary.values.toVector),
      choice(_),
      err
    )

  val windows: StructuredField[E, Vector[StudyWindow]] =
    StructuredField.of[E, Vector[StudyWindow]](
      V.view(
        "windows",
        "Named half-open windows relative to each trial's measured anchor",
        FieldKind.Repeated(
          V.window("window", RecipeParameters.studyWindow.info.meaning),
          1,
          None
        )
      )
    )(
      {
        case RawValue.Items(items) =>
          items.traverse { w =>
            for
              name  <- tokenPart("windows", w, "name")
              start <- long("windows", w, "start")
              end   <- long("windows", w, "end")
              span  <- Window
                .of(Span.micros(start), Span.micros(end))
                .left
                .map(RecipeParameterError.Time.apply)
              sw <- StudyWindow.of(name, span).left.map(RecipeParameterError.Temporal.apply)
            yield sw
          }
        case _ => Left(missing("windows", "items"))
      },
      ws =>
        RawValue.Items(
          ws.map(w =>
            group(
              "name"  -> RawValue.Text(w.name),
              "start" -> number(w.window.from.toMicros),
              "end"   -> number(w.window.until.toMicros)
            )
          )
        ),
      err
    )

  val repetitions: StructuredField[E, Vector[RepetitionContrast]] =
    StructuredField.of[E, Vector[RepetitionContrast]](
      V.view(
        "repetitions",
        "Named focal/reference phase comparisons within participant",
        FieldKind.Repeated(
          V.phases(
            "repetition",
            RecipeParameters.repetition.info.meaning,
            Vector(V.text("name", "The repetition's name"))
          ),
          1,
          None
        )
      )
    )(
      {
        case RawValue.Items(items) =>
          items.traverse { r =>
            for
              name <- tokenPart("repetitions", r, "name")
              f    <- tokenPart("repetitions", r, "focal")
              ref  <- tokenPart("repetitions", r, "reference")
              c    <- RepetitionContrast
                .withinParticipant(name, f, ref)
                .left
                .map(RecipeParameterError.Temporal.apply)
            yield c
          }
        case _ => Left(missing("repetitions", "items"))
      },
      rs =>
        RawValue.Items(
          rs.map(r =>
            group(
              "name"      -> RawValue.Text(r.name),
              "focal"     -> RawValue.Choice(r.focalPhase),
              "reference" -> RawValue.Choice(r.referencePhase)
            )
          )
        ),
      err
    )

  val fields: Vector[FormField[E, ?]] = Vector(boundary, windows, repetitions)
  def views: Vector[FieldView]        = fields.map(_.view)

  def validate(field: FieldId, raw: RawValue): Either[FieldError[E], Unit] =
    fields
      .find(_.view.id == field)
      .toRight(FieldError.UnknownField(field))
      .flatMap(_.parse(raw).map(_ => ()))

  def parse(values: FormValues): Either[FormParse.Refusals, TemporalRecipe] =
    val b = boundary.parse(values.get(boundary.view.id))
    val w = windows.parse(values.get(windows.view.id))
    val r = repetitions.parse(values.get(repetitions.view.id))
    FormParse.errors(b, w, r).toLeft(()).flatMap { _ =>
      (for b1 <- b; w1 <- w; r1 <- r yield TemporalRecipe(b1, w1, r1)).left.map(
        cats.data.NonEmptyVector.one
      )
    }

  def values[K, U <: Unit2D, P, S, D](plan: TemporalStudyPlan[K, U, P, S, D]): FormValues =
    FormValues.of(
      boundary.view.id    -> boundary.raw(plan.boundary),
      windows.view.id     -> windows.raw(plan.windows),
      repetitions.view.id -> repetitions.raw(plan.repetitions)
    )

/** The typed values of a recording plan's own form fields. */
final case class RecordingRecipe(
    viewing: Option[Viewing],
    syncModel: SyncFitMode,
    residualLimit: Option[SyncResidualLimit],
    interpolationGap: InterpolationGap,
    marks: Vector[SyncMark],
    areas: Vector[RecordingArea]
):
  /** The recording plan these values describe in `template`'s context (its
    * input, source, display frame, clocks, angular frame and detector), or
    * the plan's own whole-recipe refusal.
    */
  def plan[P](template: RecordingPlan[P]): Either[RecordingPlanError, RecordingPlan[P]] =
    RecordingPlan.of(
      template.input,
      template.source,
      template.display,
      template.trackerClock,
      template.analysisClock,
      template.angularFrameId,
      viewing,
      syncModel,
      marks,
      residualLimit,
      interpolationGap,
      areas,
      template.method,
      template.parameters
    )

/** The form of a recording plan's own fields. The input, display frame,
  * clocks and angular frame come from outside the form; the detector's
  * parameters have their own descriptor fields ([[ParameterSet.validate]]).
  */
final class RecordingForm:
  import RawParts.*
  import RecipeViews as V
  private type E = RecipeParameterError
  private def err(e: E): String = e.message

  val viewing: StructuredField[E, Option[Viewing]] =
    StructuredField.of[E, Option[Viewing]](
      V.view(
        "viewing",
        RecipeParameters.perspective.info.meaning,
        FieldKind.Optional(V.perspective, "No physical viewing geometry is declared")
      )
    )(
      raw =>
        optional(raw).traverse { p =>
          for
            d <- real("viewing", p, "distance")
            w <- real("viewing", p, "width")
            h <- real("viewing", p, "height")
            q <- geometry(Perspective.millimetres(d, w, h))
          yield Viewing(q)
        },
      _.fold(RawValue.Absent)(v =>
        group(
          "distance" -> number(v.perspective.distance.toMm),
          "width"    -> number(v.perspective.surfaceWidth.toMm),
          "height"   -> number(v.perspective.surfaceHeight.toMm)
        )
      ),
      err
    )

  val syncModel: StructuredField[E, SyncFitMode] =
    StructuredField.of[E, SyncFitMode](V.syncModel)(
      raw => choose("syncModel", raw, SyncFitMode.values.toVector),
      choice(_),
      err
    )

  val residualLimit: StructuredField[E, Option[SyncResidualLimit]] =
    StructuredField.of[E, Option[SyncResidualLimit]](
      V.view(
        "residualLimitMicros",
        "Optional nonnegative synchronization residual rejection threshold",
        FieldKind.Optional(
          RecipeParameters.forms.residualLimit.view,
          "Every synchronization residual is accepted"
        )
      )
    )(
      raw =>
        optional(raw).traverse(n =>
          longOf("residualLimitMicros", n).flatMap(RecipeParameters.forms.residualLimit.domain)
        ),
      _.fold(RawValue.Absent)(l => number(l.span.toMicros)),
      err
    )

  val interpolationGap: NumericField[E, Long, InterpolationGap] =
    RecipeParameters.forms.interpolationGap

  val marks: StructuredField[E, Vector[SyncMark]] =
    StructuredField.of[E, Vector[SyncMark]](
      V.view(
        "marks",
        "Named common marks with exact source/target microseconds",
        FieldKind.Repeated(
          V.syncMark("mark", "Named common mark with exact source/target microseconds"),
          0,
          None
        )
      )
    )(
      {
        case RawValue.Items(items) =>
          items.traverse { m =>
            for
              name <- tokenPart("marks", m, "name")
              s    <- long("marks", m, "source")
              t    <- long("marks", m, "target")
              mark <- SyncMark
                .of(name, Instant.micros(s), Instant.micros(t))
                .left
                .map(RecipeParameterError.Synchronization.apply)
            yield mark
          }
        case _ => Left(missing("marks", "items"))
      },
      ms =>
        RawValue.Items(
          ms.map(m =>
            group(
              "name"   -> RawValue.Text(m.id),
              "source" -> number(m.onSource.toMicros),
              "target" -> number(m.onTarget.toMicros)
            )
          )
        ),
      err
    )

  val areas: StructuredField[E, Vector[RecordingArea]] =
    StructuredField.of[E, Vector[RecordingArea]](
      V.view(
        "areas",
        "AOIs with their display labels and pixel bounds",
        FieldKind.Repeated(
          V.area("area", "AOI ID, display label and ordered pixel bounds"),
          1,
          None
        )
      )
    )(
      {
        case RawValue.Items(items) =>
          items.traverse { a =>
            for
              id    <- tokenPart("areas", a, "areaId")
              label <- tokenPart("areas", a, "label")
              x0    <- real("areas", a, "xMin")
              y0    <- real("areas", a, "yMin")
              x1    <- real("areas", a, "xMax")
              y1    <- real("areas", a, "yMax")
              b     <- geometry(Bounds.of[Px](x0, y0, x1, y1))
              area  <- RecordingArea
                .of(id, label, b)
                .left
                .map(RecipeParameterError.Recording.apply)
            yield area
          }
        case _ => Left(missing("areas", "items"))
      },
      as =>
        RawValue.Items(
          as.map(a =>
            group(
              "areaId" -> RawValue.Text(a.id),
              "label"  -> RawValue.Text(a.label),
              "xMin"   -> number(a.bounds.xMin),
              "yMin"   -> number(a.bounds.yMin),
              "xMax"   -> number(a.bounds.xMax),
              "yMax"   -> number(a.bounds.yMax)
            )
          )
        ),
      err
    )

  val fields: Vector[FormField[E, ?]] =
    Vector(viewing, syncModel, residualLimit, interpolationGap, marks, areas)
  def views: Vector[FieldView] = fields.map(_.view)

  def validate(field: FieldId, raw: RawValue): Either[FieldError[E], Unit] =
    fields
      .find(_.view.id == field)
      .toRight(FieldError.UnknownField(field))
      .flatMap(_.parse(raw).map(_ => ()))

  def parse(values: FormValues): Either[FormParse.Refusals, RecordingRecipe] =
    val v = viewing.parse(values.get(viewing.view.id))
    val s = syncModel.parse(values.get(syncModel.view.id))
    val r = residualLimit.parse(values.get(residualLimit.view.id))
    val g = interpolationGap.parse(values.get(interpolationGap.view.id))
    val m = marks.parse(values.get(marks.view.id))
    val a = areas.parse(values.get(areas.view.id))
    FormParse.errors(v, s, r, g, m, a).toLeft(()).flatMap { _ =>
      (for
        v1 <- v
        s1 <- s
        r1 <- r
        g1 <- g
        m1 <- m
        a1 <- a
      yield RecordingRecipe(v1, s1, r1, g1, m1, a1)).left.map(cats.data.NonEmptyVector.one)
    }

  def values[P](plan: RecordingPlan[P]): FormValues =
    FormValues.of(
      viewing.view.id          -> viewing.raw(plan.viewing),
      syncModel.view.id        -> syncModel.raw(plan.synchronizationModel),
      residualLimit.view.id    -> residualLimit.raw(plan.residualLimit),
      interpolationGap.view.id -> interpolationGap.raw(plan.interpolationGap),
      marks.view.id            -> marks.raw(plan.marks),
      areas.view.id            -> areas.raw(plan.areas)
    )
