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

class DiagnosticSourceRefSuite extends munit.FunSuite:
  private case class LegacyRef(digest: String)
  private case class LegacySource(
      label: String,
      records: LegacyRef,
      interpretation: SourceInterpretation
  )

  test("validated artifact references keep their former read-only diagnostic structure") {
    val source   = SourceRef.of("fixture", Vector("x", "y"), Vector(Vector("1", "2")))
    val expected = summon[DiagnosticOperand[LegacySource, Nothing]].apply(
      LegacySource(source.label, LegacyRef(source.records.digest), source.interpretation)
    )
    val actual = summon[DiagnosticOperand[SourceRef, Nothing]].apply(source)
    assertEquals(actual, expected)
  }
