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
import io.circe.Json

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** The pinned study-v2.json and admission-ledger-v2.json are exactly the
  * pretty-printed encodings of [[StudyV2Fixtures]], re-encode byte for byte,
  * and carry the same JSON values as the portable compact mirrors.
  *
  * Set `EYES4S_WRITE_V2_FIXTURES=1` to rewrite the two resources from the
  * fixture values; the run then fails so the change is reviewed.
  */
class StudyV2JvmSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A  = e.fold(error => fail(s"$error"), identity)
  private val root                           = Paths.get("codec/src/test/resources/eyes4s")
  private def resource(name: String): String =
    val stream = Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .getOrElse(fail(s"missing resource $name"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()

  private val plans   = StudyCodecs.cosine[Px]
  private val ledgers = StudyInputCodecs.study[Px]

  private val documents: Vector[(String, Json, String)] = Vector(
    (
      "study-v2.json",
      get(plans.codec.encode(StudyV2Fixtures.plan)),
      StudyV2Mirrors.studyVersionTwo
    ),
    (
      "study-trial-v2.json",
      get(StudyCodecs.trialCosine[Px].codec.encode(StudyV2Fixtures.trialPlan)),
      StudyV2Mirrors.trialStudyVersionTwo
    ),
    (
      "admission-ledger-v2.json",
      get(ledgers.ledger.encode(StudyV2Fixtures.ledger)),
      StudyV2Mirrors.ledgerVersionTwo
    )
  )

  test("the pinned v2 resources are the encodings of the fixture values") {
    if sys.env
        .get("EYES4S_WRITE_V2_FIXTURES")
        .orElse(sys.props.get("EYES4S_WRITE_V2_FIXTURES"))
        .contains("1")
    then
      documents.foreach { (file, json, _) =>
        Files.write(root.resolve(file), json.spaces2.getBytes(StandardCharsets.UTF_8))
      }
      fail("rewrote the v2 fixtures; review them and run again")
    documents.foreach { (file, json, _) => assertEquals(resource(file), json.spaces2, file) }
  }

  test("study-v2.json and admission-ledger-v2.json re-encode byte-identically") {
    val plan = get(plans.codec.parse(resource("study-v2.json")))
    assertEquals(get(plans.codec.encode(plan)).spaces2, resource("study-v2.json"))
    val trials = StudyCodecs.trialCosine[Px].codec
    val trial  = get(trials.parse(resource("study-trial-v2.json")))
    assertEquals(get(trials.encode(trial)).spaces2, resource("study-trial-v2.json"))
    val ledger = get(ledgers.ledger.parse(resource("admission-ledger-v2.json")))
    assertEquals(
      get(ledgers.ledger.encode(ledger)).spaces2,
      resource("admission-ledger-v2.json")
    )
  }

  test("the portable mirrors carry the same JSON values") {
    documents.foreach { (file, json, mirror) =>
      assertEquals(get(io.circe.parser.parse(mirror)), json, file)
    }
  }
