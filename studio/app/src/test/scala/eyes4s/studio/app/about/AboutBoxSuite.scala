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

package eyes4s.studio.app.about

import eyes4s.studio.app.{AppEffect, AppModel, Intent, PlatformDialog, StoryModels}
import eyes4s.studio.app.keys.CommandRegistry
import eyes4s.studio.core.backend.ProtocolVersion
import eyes4s.studio.core.engine.StudioBuild

/** The About box headlessly (ticket S1.14): it reads the build's version,
  * commit and protocol from the generated StudioBuild, the running Java and
  * JavaFX from the host, and lists every component of the generated notices
  * with its version and licence; Help › About opens it.
  */
class AboutBoxSuite extends munit.FunSuite:

  private val tsv =
    "dependency\torg.openjfx\tjavafx-base\t24.0.1\tGPL-2.0-only WITH Classpath-exception-2.0\tOracle\n" +
      "dependency\tio.github.canardlapin\tscaladock-core_3\t0.0.0-628c46f\tApache-2.0\tcanardlapin\n" +
      "bundled\tfont\tIBM Plex Sans\t1.1.0\tOFL-1.1\tIBM Corp.\n"

  private def facts(components: Either[String, Vector[AboutComponent]]) =
    AboutFacts("25.0.1", "Eclipse Adoptium", Some("24.0.1+3"), components)

  test("components.tsv parses into components, kind by kind") {
    val cs = AboutBox.parse(tsv).fold(fail(_), identity)
    assertEquals(
      cs.map(_.kind),
      Vector(ComponentKind.Library, ComponentKind.Library, ComponentKind.Bundled)
    )
    assertEquals(
      cs.head,
      AboutComponent(
        ComponentKind.Library,
        "org.openjfx",
        "javafx-base",
        "24.0.1",
        "GPL-2.0-only WITH Classpath-exception-2.0",
        "Oracle"
      )
    )
  }

  test("a malformed or empty list is refused, naming the line") {
    assertEquals(AboutBox.parse("dependency\ta\tb\n"), Left("line 1 has 3 cells, not 6"))
    assertEquals(
      AboutBox.parse("module\ta\tb\tc\td\te\n"),
      Left("line 1 has the kind 'module'")
    )
    assertEquals(AboutBox.parse("\n"), Left("it lists no component"))
  }

  test("the lines: version, commit, protocol, Java and JavaFX, from the build and the host") {
    val vm = AboutBox.vm(facts(AboutBox.parse(tsv)))
    assertEquals(vm.title, "About Eyes Studio")
    assertEquals(
      vm.lines.map(l => l.label -> l.value),
      Vector(
        "Version" -> s"eyes4s ${StudioBuild.eyes4sBaseVersion}",
        "Commit"  -> StudioBuild.commit.fold("not built from a Git checkout")(_.take(12)),
        "Backend protocol" -> ProtocolVersion.Current.render,
        "Java"             -> "25.0.1 (Eclipse Adoptium)",
        "JavaFX"           -> s"24.0.1+3 (built against ${StudioBuild.javaFxVersion})"
      )
    )
    // A Git checkout's build names its commit.
    StudioBuild.commit.foreach(c => assert(c.matches("[0-9a-f]{40}"), c))
    val headless = AboutBox.vm(facts(AboutBox.parse(tsv)).copy(javaFxVersion = None))
    assertEquals(
      headless.lines.last.value,
      s"not running (built against ${StudioBuild.javaFxVersion})"
    )
  }

  test("every component is listed with its version and licence, after Eyes Studio itself") {
    val vm = AboutBox.vm(facts(AboutBox.parse(tsv)))
    assertEquals(vm.componentsTitle, "Bundled components (3)")
    assertEquals(
      vm.components,
      Vector(
        ComponentRow(
          "Eyes Studio and eyes4s (Apache-2.0)",
          StudioBuild.eyes4sBaseVersion,
          "Apache-2.0"
        ),
        ComponentRow("io.github.canardlapin:scaladock-core_3", "0.0.0-628c46f", "Apache-2.0"),
        ComponentRow(
          "org.openjfx:javafx-base",
          "24.0.1",
          "GPL-2.0-only WITH Classpath-exception-2.0"
        ),
        ComponentRow("IBM Plex Sans (font)", "1.1.0", "OFL-1.1")
      )
    )
    assertEquals(vm.problem, None)
  }

  test("an unreadable list says why, and the box still shows the build") {
    val vm = AboutBox.vm(facts(Left("components.tsv is not in the application")))
    assertEquals(
      vm.problem,
      Some("The component list could not be read: components.tsv is not in the application")
    )
    assertEquals(vm.components.size, 1)
    assertEquals(vm.lines.size, 5)
  }

  test("Help › About Eyes Studio opens the About box") {
    val m = StoryModels.t2Compare
    assertEquals(CommandRegistry.about.intent(m), Some(Intent.ShowAbout))
    val (after, effects) = AppModel.update(m, Intent.ShowAbout)
    assertEquals(effects, Vector(AppEffect.OpenDialog(PlatformDialog.About)))
    assertEquals(after.document, m.document)
  }
