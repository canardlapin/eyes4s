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

package eyes4s.codec

import eyes4s.kernel.Unit2D.Px

/** The pretty-printed resource files re-encode byte for byte on the JVM.
  * Scala.js renders integral doubles without a fraction, so the portable
  * suite checks JSON value identity instead.
  */
class StudyInputV1JvmSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A  = e.fold(error => fail(s"$error"), identity)
  private def resource(name: String): String =
    val stream = Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .getOrElse(fail(s"missing resource $name"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()
  private val codec = StudyInputCodecs.study[Px]

  test("study-input-v1.json re-encodes byte-identically and matches the portable string") {
    val text  = resource("study-input-v1.json")
    val input = get(codec.input.parse(text))
    assertEquals(get(codec.input.encode(input)).spaces2, text)
    assertEquals(
      get(io.circe.parser.parse(text)),
      get(io.circe.parser.parse(StudyInputFixtures.inputVersionOne))
    )
  }

  test("admission-ledger-v1.json re-encodes byte-identically and matches the portable string") {
    val text   = resource("admission-ledger-v1.json")
    val ledger = get(codec.ledger.parse(text))
    assertEquals(get(codec.ledger.encode(ledger)).spaces2, text)
    assertEquals(
      get(io.circe.parser.parse(text)),
      get(io.circe.parser.parse(StudyInputFixtures.ledgerVersionOne))
    )
  }
