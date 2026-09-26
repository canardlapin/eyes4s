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

package eyes4s.laws

import scala.compiletime.testing.typeCheckErrors

/** Runs the published record, fixation and line-layout laws. */
class RecordIdentityLawsSuite extends munit.DisciplineSuite:
  checkAll("RecordIdentity", RecordIdentityLaws.conversion)
  checkAll("FixationIdentity", RecordIdentityLaws.fixations)
  checkAll("RecordLines", RecordIdentityLaws.lines(RecordIdentityLaws.layouts))

  test("outside eyes4s.plan no identity is built or copied without its smart constructor") {
    assert(
      typeCheckErrors("""import eyes4s.plan.*
         val r = DataRecord(7214)""").nonEmpty,
      """import eyes4s.plan.*
         val r = DataRecord(7214)"""
    )
    assert(
      typeCheckErrors("""import eyes4s.plan.*
         val r = new DataRecord(7214)""").nonEmpty,
      """import eyes4s.plan.*
         val r = new DataRecord(7214)"""
    )
    assert(
      typeCheckErrors("""import eyes4s.plan.*
         val c = CsvRecord.of(2).toOption.get.copy(value = 1)""").nonEmpty,
      """import eyes4s.plan.*
         val c = CsvRecord.of(2).toOption.get.copy(value = 1)"""
    )
    assert(
      typeCheckErrors("""import eyes4s.plan.*
         val l = SourceLine(0L)""").nonEmpty,
      """import eyes4s.plan.*
         val l = SourceLine(0L)"""
    )
    assert(
      typeCheckErrors("""import eyes4s.plan.*
         val p = ScanpathPosition(-1)""").nonEmpty,
      """import eyes4s.plan.*
         val p = ScanpathPosition(-1)"""
    )
    assert(
      typeCheckErrors("""import eyes4s.plan.*
         val n = FixationNumber(0)""").nonEmpty,
      """import eyes4s.plan.*
         val n = FixationNumber(0)"""
    )
  }
