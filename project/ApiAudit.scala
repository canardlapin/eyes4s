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

import sbt._
import sbt.Keys._

/** Opt-in per-suite JVMs give JaCoCo evidence an unambiguous executed suite owner. */
object ApiAudit {
  val settings: Seq[Def.Setting[_]] = Seq(
    Test / testGrouping := {
      val ordinary = (Test / testGrouping).value
      sys.props.get("eyes4s.audit.agent") match {
        case None        => ordinary
        case Some(agent) =>
          val directory =
            (LocalRootProject / baseDirectory).value / "target" / "api-audit" / "execution" / thisProject.value.id
          IO.createDirectory(directory)
          val options = (Test / forkOptions).value
          (Test / definedTests).value.map { test =>
            val output       = directory / (test.name + ".exec")
            val instrumented = options.withRunJVMOptions(
              options.runJVMOptions ++ Vector(
                "-Xmx2g",
                s"-javaagent:$agent=destfile=${output.getAbsolutePath},append=false,includes=eyes4s.*"
              )
            )
            new Tests.Group(test.name, Seq(test), Tests.SubProcess(instrumented))
          }
      }
    }
  )
}
