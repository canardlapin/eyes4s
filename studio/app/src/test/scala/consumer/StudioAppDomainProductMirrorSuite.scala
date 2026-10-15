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

package consumer

import scala.compiletime.testing.typeCheckErrors

class StudioAppDomainProductMirrorSuite extends munit.FunSuite:
  test("eyes4s.studio.app.ProjectName rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.ProjectName; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.ProjectName]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.ClockTime rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.ClockTime; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.ClockTime]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.AppModel rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.AppModel; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.AppModel]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.maps.ColourLimits.Sequential rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.maps.ColourLimits.Sequential; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.maps.ColourLimits.Sequential]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.maps.ColourLimits.Symmetric rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.maps.ColourLimits.Symmetric; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.maps.ColourLimits.Symmetric]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.maps.MapStyle rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.maps.MapStyle; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.maps.MapStyle]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.maps.MapOpacity rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.maps.MapOpacity; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.maps.MapOpacity]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.maps.MapRaster rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.maps.MapRaster; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.maps.MapRaster]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.maps.RasterBudget rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.maps.RasterBudget; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.maps.RasterBudget]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.maps.RasterCache rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.maps.RasterCache; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.maps.RasterCache]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.maps.MapGrid rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.maps.MapGrid; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.maps.MapGrid]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.plot.ViewSelection rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.plot.ViewSelection; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.plot.ViewSelection]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.plot.TableTwinState rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.plot.TableTwinState; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.plot.TableTwinState]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.plot.Timeline rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.plot.Timeline; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.plot.Timeline]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.plot.HalfOpenSpan rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.plot.HalfOpenSpan; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.plot.HalfOpenSpan]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.plot.ColumnId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.plot.ColumnId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.plot.ColumnId]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.plot.PlotSource rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.plot.PlotSource; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.plot.PlotSource]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.driver.StudioDriver rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.driver.StudioDriver; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.driver.StudioDriver]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.layout.PaneId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.layout.PaneId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.layout.PaneId]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.layout.LayoutId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.layout.LayoutId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.layout.LayoutId]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.project.ProjectOperationId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.project.ProjectOperationId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.project.ProjectOperationId]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.project.ProjectLifecycle rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.project.ProjectLifecycle; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.project.ProjectLifecycle]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.figures.HighestControl rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.figures.HighestControl; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.figures.HighestControl]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.figures.FigureBinding rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.figures.FigureBinding; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.figures.FigureBinding]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.figures.FigureMethods rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.figures.FigureMethods; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.figures.FigureMethods]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.figures.Zoom rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.figures.Zoom; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.figures.Zoom]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.figures.FigureComposer rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.figures.FigureComposer; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.figures.FigureComposer]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.icons.SvgPath rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.icons.SvgPath; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.icons.SvgPath]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.keys.CommandId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.keys.CommandId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.keys.CommandId]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.geometry.DensitySteps rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.geometry.DensitySteps; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.geometry.DensitySteps]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.nav.Navigation rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.nav.Navigation; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.nav.Navigation]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.jobs.JobBoard rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.jobs.JobBoard; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.jobs.JobBoard]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.runs.RunStoragePanel rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.runs.RunStoragePanel; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.runs.RunStoragePanel]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.tokens.Colour rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.tokens.Colour; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.tokens.Colour]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.importing.ImportWizard rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.studio.app.importing.ImportWizard; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.app.importing.ImportWizard]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.studio.app.AppModel keeps internal copy private") {
    val errors = typeCheckErrors("def forge(value: eyes4s.studio.app.AppModel) = value.copy()")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.maps.RasterCache keeps internal copy private") {
    assertEquals(
      typeCheckErrors(
        "def fields(value: eyes4s.studio.app.maps.RasterCache) = (value.entries, value.order)"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "def forge(value: eyes4s.studio.app.maps.RasterCache) = value.copy(entries = value.entries, order = value.order)"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.plot.ViewSelection keeps internal copy private") {
    assertEquals(
      typeCheckErrors(
        "def fields(value: eyes4s.studio.app.plot.ViewSelection) = value.sequence"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "def forge(value: eyes4s.studio.app.plot.ViewSelection) = value.copy(sequence = value.sequence)"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.plot.TableTwinState keeps internal copy private") {
    val errors =
      typeCheckErrors("def forge(value: eyes4s.studio.app.plot.TableTwinState) = value.copy()")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.driver.StudioDriver keeps internal copy private") {
    val errors =
      typeCheckErrors("def forge(value: eyes4s.studio.app.driver.StudioDriver) = value.copy()")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.project.ProjectLifecycle keeps internal copy private") {
    assertEquals(
      typeCheckErrors(
        "def field(value: eyes4s.studio.app.project.ProjectLifecycle) = value.operation"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "def forge(value: eyes4s.studio.app.project.ProjectLifecycle) = value.copy(operation = value.operation)"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.figures.FigureBinding keeps internal copy private") {
    val errors = typeCheckErrors(
      "def forge(value: eyes4s.studio.app.figures.FigureBinding) = value.copy()"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.figures.FigureMethods keeps internal copy private") {
    val errors = typeCheckErrors(
      "def forge(value: eyes4s.studio.app.figures.FigureMethods) = value.copy()"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.figures.FigureComposer keeps internal copy private") {
    val errors = typeCheckErrors(
      "def forge(value: eyes4s.studio.app.figures.FigureComposer) = value.copy()"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.jobs.JobBoard keeps internal copy private") {
    val errors =
      typeCheckErrors("def forge(value: eyes4s.studio.app.jobs.JobBoard) = value.copy()")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.runs.RunStoragePanel keeps internal copy private") {
    val errors =
      typeCheckErrors("def forge(value: eyes4s.studio.app.runs.RunStoragePanel) = value.copy()")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("eyes4s.studio.app.importing.ImportWizard keeps internal copy private") {
    val errors = typeCheckErrors(
      "def forge(value: eyes4s.studio.app.importing.ImportWizard) = value.copy()"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("copy")))
  }
