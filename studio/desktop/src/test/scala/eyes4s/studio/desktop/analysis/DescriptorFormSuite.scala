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

package eyes4s.studio.desktop.analysis

import eyes4s.kernel.{Frame, Unit2D}
import eyes4s.plan.*
import eyes4s.design.Trials
import eyes4s.studio.core.backend.{RecipeEditRequest, RecipeFormResult}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.real.RealRecipeForm
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.StudioFxSuite
import javafx.scene.layout.VBox

class DescriptorFormSuite extends StudioFxSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val document                          = get(StoryMoments.t2)
  private val context                           = document.draftContext.get
  private def served: RecipeFormResult          = get(
    RealRecipeForm.serve(
      RecipeEditRequest(context.id, context.dataset, context.recipe),
      get(Frame.screen("screen", 1920, 1080)),
      StudyInput[TrialKey, Unit2D.Px](Trials(Vector.empty)),
      None
    )
  )
  private def id(value: String) = get(FieldId.of(value))

  fxStage.test(
    "a new served descriptor appears and preserves incomplete input without a field table"
  ) { fx =>
    val (view, values) = get(served.decoded)
    val synthetic      = get(
      FieldView.of(
        id("syntheticThreshold"),
        7,
        "Synthetic threshold",
        FieldKind.Numeric(Quantity.Dimensionless, NumberShape.Real, NumericBounds.positive),
        None
      )
    )
    val extended = get(FormView.of(view.definition, view.fields :+ synthetic))
    var reported = Option.empty[FormValues]
    val form     = runOnFx(DescriptorForm(value => reported = Some(value)))
    val root     = runOnFx {
      form.show(extended, values.updated(synthetic.id, RawValue.Number("2.5")))
      val root = VBox(form.node)
      root.getStylesheets.setAll(get(StudioStyles.stylesheets(Theme.Light))*)
      root
    }
    fx.show(root)
    fx.awaitLayout()
    runOnFx {
      assertEquals(form.textFields("syntheticThreshold").getText, "2.5")
      form.textFields("syntheticThreshold").setText("-")
      assertEquals(reported.map(_.get(synthetic.id)), Some(RawValue.Number("-")))
      assertEquals(form.values.get(id("grid")), values.get(id("grid")))
      assertEquals(form.textFields("grid.columns").getAccessibleText, "Grid columns")
    }
  }

  fxStage.test("repeated fields derive focus order from their drawn rows after an addition") {
    fx =>
      val item     = get(FieldView.of(id("value"), 1, "Selected value", FieldKind.Text, None))
      val repeated = get(
        FieldView.of(
          id("values"),
          1,
          "Selected values",
          FieldKind.Repeated(item, 1, None),
          None
        )
      )
      val view = get(FormView.of(DefinitionId.study, Vector(repeated)))
      val form = runOnFx(DescriptorForm(_ => ()))
      val root = runOnFx {
        form.show(
          view,
          FormValues.empty.updated(repeated.id, RawValue.Items(Vector(RawValue.Text("lure"))))
        )
        VBox(form.node)
      }
      fx.show(root)
      runOnFx {
        assertEquals(
          form.focusStops.map(_.name),
          Vector("Selected value", "Add Selected values")
        )
        form.node
          .lookupAll(".button")
          .toArray
          .toVector
          .collectFirst {
            case b: javafx.scene.control.Button if b.getId == "recipe-values.add" => b
          }
          .get
          .asInstanceOf[javafx.scene.control.Button]
          .fire()
        assertEquals(
          form.focusStops.map(_.name),
          Vector(
            "Selected value",
            "Remove Selected values",
            "Selected value",
            "Remove Selected values",
            "Add Selected values"
          )
        )
      }
  }
