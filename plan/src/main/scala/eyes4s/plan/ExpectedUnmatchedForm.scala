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

/** The optional expected-unmatched selection, including its declared assignments. */
object ExpectedUnmatchedForm:
  import RawParts.*
  private val V          = RecipeViews
  private val assignment = V.view(
    "assignment",
    "Declared trial attribute",
    FieldKind.Group(
      Vector(
        V.text("participant", "Participant identity"),
        V.text("phase", "Phase identity"),
        V.text("trial", "Trial identity"),
        V.atLeastOne("occurrence", "Trial occurrence", Counted.Occurrences),
        V.view(
          "value",
          "Declared text value, absent for a blank cell",
          FieldKind.Optional(V.text("text", "Attribute value"), "Blank declaration")
        )
      ),
      GroupRule.Independent
    )
  )
  val view: FieldView = V.view(
    "expectedUnmatched",
    "Trials explicitly unmatched by design",
    FieldKind.Optional(
      V.view(
        "selection",
        "Text attribute selection",
        FieldKind.Group(
          Vector(
            V.text("column", "Declared trial attribute column"),
            V.view(
              "values",
              "Values unmatched by design",
              FieldKind.Repeated(V.text("value", "Selected value"), 1, None)
            ),
            V.view(
              "assignments",
              "Pre-admission trial assignments",
              FieldKind.Repeated(assignment, 0, None)
            )
          ),
          GroupRule.Independent
        )
      ),
      "No declared expected-unmatched selection"
    )
  )
  private def items(raw: RawValue, name: String) =
    part("expectedUnmatched", raw, name).flatMap {
      case RawValue.Items(xs) => Right(xs)
      case _                  => Left(missing("expectedUnmatched", name))
    }
  val field: StructuredField[RecipeParameterError, Option[ExpectedUnmatched]] =
    StructuredField.of(view)(
      raw =>
        optional(raw).traverse { value =>
          for
            column         <- tokenPart("expectedUnmatched", value, "column")
            rawValues      <- items(value, "values")
            values         <- rawValues.traverse(token("expectedUnmatched", _))
            rawAssignments <- items(value, "assignments")
            assignments    <- rawAssignments.traverse { a =>
              for
                p          <- tokenPart("expectedUnmatched", a, "participant")
                phase      <- tokenPart("expectedUnmatched", a, "phase")
                trial      <- tokenPart("expectedUnmatched", a, "trial")
                n          <- int("expectedUnmatched", a, "occurrence")
                occurrence <- TrialOccurrence.of(n).left.map(RecipeParameterError.Plan.apply)
                identity   <- TrialIdentity
                  .of(p, phase, trial, occurrence)
                  .left
                  .map(RecipeParameterError.Plan.apply)
                v <- part("expectedUnmatched", a, "value").flatMap(v =>
                  optional(v).traverse(token("expectedUnmatched", _))
                )
              yield identity -> v.getOrElse("")
            }
            selected <- ExpectedUnmatched
              .of(column, values, assignments)
              .left
              .map(RecipeParameterError.Plan.apply)
          yield selected
        },
      value =>
        value.fold[RawValue](RawValue.Absent)(s =>
          group(
            "column"      -> RawValue.Text(s.column),
            "values"      -> RawValue.Items(s.values.map(RawValue.Text(_))),
            "assignments" -> RawValue.Items(s.assignments.map { (t, v) =>
              group(
                "participant" -> RawValue.Text(t.participant),
                "phase"       -> RawValue.Text(t.phase),
                "trial"       -> RawValue.Text(t.trial),
                "occurrence"  -> number(t.occurrence.value),
                "value"       -> (if v.isEmpty then RawValue.Absent else RawValue.Text(v))
              )
            })
          )
        ),
      _.message
    )
