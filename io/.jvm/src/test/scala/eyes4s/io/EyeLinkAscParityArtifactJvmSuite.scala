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

package eyes4s.io

import scala.io.Source
import scala.util.Using

class EyeLinkAscParityArtifactJvmSuite extends munit.FunSuite:
  private val resource = "/META-INF/eyes4s/eyelink/parity-manifests.tsv"

  test("the packaged parity artifact is the deterministic checked-in baseline") {
    val stream = Option(getClass.getResourceAsStream(resource)).getOrElse(
      fail(s"missing packaged EyeLink parity artifact resource=$resource")
    )
    val actual = Using.resource(Source.fromInputStream(stream, "UTF-8"))(_.mkString)

    assertEquals(actual, EyeLinkAscParityExpected.renderTsv)
    val rows = actual.linesIterator.toVector
    assertEquals(rows.length, EyeLinkAscParityExpected.rows.length + 1)
    assertEquals(
      EyeLinkAscParityExpected.rows.map(_.fixture).distinct.length,
      EyeLinkAscParityExpected.rows.length
    )
  }

end EyeLinkAscParityArtifactJvmSuite
