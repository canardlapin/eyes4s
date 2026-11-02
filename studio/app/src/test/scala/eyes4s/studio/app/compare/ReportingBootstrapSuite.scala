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

package eyes4s.studio.app.compare

import eyes4s.studio.app.*
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.document.{Perspective, ReportingSpec}

class ReportingBootstrapSuite extends munit.FunSuite:
  private def fresh = AppModel.newProject.fold(e => fail(e.message), identity)

  test("Results creates and journals its first report without requiring a seeded fixture") {
    val before = fresh
    assertEquals(before.document.reporting, Vector.empty)
    val (after, effects) =
      AppModel.update(before, Intent.SwitchPerspective(Perspective.Results))
    assertEquals(after.document.reporting, Vector(ReportingSpec.overall))
    assertEquals(after.location.trail, Vector(Place.Summary(ReportingSpec.overall.id)))
    assert(after.save.edited)
    assert(
      effects.contains(
        AppEffect.Journal(JournalEntry.Apply(Command.PutReporting(ReportingSpec.overall)))
      )
    )
    assert(!effects.exists { case AppEffect.Execution(_) => true; case _ => false })
    assertEquals(after.document.runs, before.document.runs)
    assertEquals(after.document.analyses, before.document.analyses)
  }

  test("Report first and repeated Results navigation reuse one saved report") {
    val (report, _)  = AppModel.update(fresh, Intent.SwitchPerspective(Perspective.Report))
    val (results, _) = AppModel.update(report, Intent.SwitchPerspective(Perspective.Results))
    val (again, effects) =
      AppModel.update(results, Intent.SwitchPerspective(Perspective.Results))
    assertEquals(again.document.reporting, Vector(ReportingSpec.overall))
    assertEquals(again.location.trail, Vector(Place.Summary(ReportingSpec.overall.id)))
    assert(!effects.exists {
      case AppEffect.Journal(JournalEntry.Apply(Command.PutReporting(_))) => true
      case _                                                              => false
    })
    val reopened = AppModel.open(again.document, None)
    assertEquals(
      reopened.navigation.at(Perspective.Results).trail,
      Vector(Place.Summary(ReportingSpec.overall.id))
    )
  }
