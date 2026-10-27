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

package eyes4s.studio.desktop.packaging

import cats.effect.IO
import eyes4s.studio.core.bundle.ProjectStore
import eyes4s.studio.desktop.platform.TempDirs
import java.nio.file.Files

class PackageCheckSuite extends munit.CatsEffectSuite:
  test(
    "packaged resources copy and reopen through the production store; existing output is refused"
  ) {
    TempDirs.resource("package-check").use { root =>
      val output = root.resolve("check")
      for
        receipt <- PackageCheck.check(output)
        before  <- IO.blocking(
          Files
            .readString(output.resolve("memory-study.eyes").resolve(ProjectStore.ManifestName))
        )
        retry <- PackageCheck.check(output).attempt
        after <- IO.blocking(
          Files
            .readString(output.resolve("memory-study.eyes").resolve(ProjectStore.ManifestName))
        )
      yield
        assertEquals(receipt.hcursor.get[String]("status"), Right("passed"))
        assertEquals(receipt.hcursor.get[Int]("inputs_verified"), Right(259))
        assert(retry.isLeft)
        assertEquals(after, before)
    }
  }
