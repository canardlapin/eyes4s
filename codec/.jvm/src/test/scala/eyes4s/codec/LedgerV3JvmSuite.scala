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

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** The pinned admission-ledger-v3.json is exactly the pretty-printed encoding
  * of [[LedgerV3Fixtures]], re-encodes byte for byte, and carries the same
  * JSON value as the portable compact mirror.
  *
  * Set `EYES4S_WRITE_V3_FIXTURES=1` to rewrite the resource from the fixture
  * value; the run then fails so the change is reviewed.
  */
class LedgerV3JvmSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A  = e.fold(error => fail(s"$error"), identity)
  private val file                           = "admission-ledger-v3.json"
  private def resource(name: String): String =
    val stream = Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .getOrElse(fail(s"missing resource $name"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()

  private val codec = StudyInputCodecs.trial[Px].ledger
  private def json  = get(codec.encode(LedgerV3Fixtures.ledger))

  test("the pinned v3 resource is the encoding of the fixture value") {
    if sys.env
        .get("EYES4S_WRITE_V3_FIXTURES")
        .orElse(sys.props.get("EYES4S_WRITE_V3_FIXTURES"))
        .contains("1")
    then
      Files.write(
        Paths.get("codec/src/test/resources/eyes4s").resolve(file),
        json.spaces2.getBytes(StandardCharsets.UTF_8)
      )
      fail(s"rewrote $file; review it and run again")
    assertEquals(resource(file), json.spaces2)
  }

  test("admission-ledger-v3.json decodes to the fixture and re-encodes byte-identically") {
    val ledger = get(codec.parse(resource(file)))
    assertEquals(ledger, LedgerV3Fixtures.ledger)
    assertEquals(get(codec.encode(ledger)).spaces2, resource(file))
  }

  test("the portable mirror carries the same JSON value") {
    assertEquals(get(io.circe.parser.parse(LedgerV3Mirrors.ledgerVersionThree)), json)
  }
