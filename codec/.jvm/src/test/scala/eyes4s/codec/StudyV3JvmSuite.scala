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

/** The pinned study-v3.json is exactly the pretty-printed encoding of
  * [[StudyV3Fixtures]], re-encodes byte for byte, carries the same JSON value
  * as the portable compact mirror, and is written under version 3 because
  * version 2 cannot express its initial-fixation policy.
  *
  * Set `EYES4S_WRITE_STUDY_V3_FIXTURE=1` to rewrite the resource from the fixture
  * value; the run then fails so the change is reviewed.
  */
class StudyV3JvmSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A  = e.fold(error => fail(s"$error"), identity)
  private val root                           = Paths.get("codec/src/test/resources/eyes4s")
  private def resource(name: String): String =
    val stream = Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .getOrElse(fail(s"missing resource $name"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()

  private val plans = StudyCodecs.cosine[Px]
  private val file  = "study-v3.json"

  test("the pinned v3 resource is the encoding of the fixture value, under version 3") {
    val json = get(plans.codec.encode(StudyV3Fixtures.plan))
    if sys.env
        .get("EYES4S_WRITE_STUDY_V3_FIXTURE")
        .orElse(sys.props.get("EYES4S_WRITE_STUDY_V3_FIXTURE"))
        .contains("1")
    then
      Files.write(root.resolve(file), json.spaces2.getBytes(StandardCharsets.UTF_8))
      Files.write(
        Paths.get("target/study-v3.compact.json"),
        json.noSpaces.getBytes(StandardCharsets.UTF_8)
      )
      fail("rewrote the v3 fixture; review it and run again")
    assertEquals(resource(file), json.spaces2)
    assertEquals(plans.ladder.earliest(StudyV3Fixtures.plan), plans.schemaV3)
  }

  test("study-v3.json re-encodes byte-identically") {
    val plan = get(plans.codec.parse(resource(file)))
    assertEquals(get(plans.codec.encode(plan)).spaces2, resource(file))
  }

  test("the portable mirror carries the same JSON value") {
    assertEquals(
      get(io.circe.parser.parse(StudyV3Mirrors.studyVersionThree)),
      get(plans.codec.encode(StudyV3Fixtures.plan))
    )
  }
