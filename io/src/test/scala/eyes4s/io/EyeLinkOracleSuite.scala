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

class EyeLinkOracleSuite extends munit.FunSuite:

  private def digest(value: String): Sha256 =
    Sha256.fromHex("test-digest", value * 64).fold(error => fail(error.message), identity)

  private def descriptor(
      ordering: EyeLinkOracleOrdering = EyeLinkOracleOrdering.Unavailable,
      invocation: String = "read_asc(samples=TRUE,events=TRUE,parse_all=FALSE)"
  ): EyeLinkOracleDescriptor =
    EyeLinkOracleDescriptor
      .create(
        "synthetic-events-eyelinker",
        "synthetic-events-messages",
        EyeLinkOracleKind.Eyelinker,
        EyeLinkOracleInput.Asc,
        ordering,
        "CRAN eyelinker",
        "0.2.2",
        digest("1"),
        digest("2"),
        digest("3"),
        invocation,
        None
      )
      .fold(error => fail(error.message), identity)

  private def fact(
      record: Int,
      field: Int,
      path: String,
      value: String = "",
      presence: EyeLinkOraclePresence = EyeLinkOraclePresence.Value,
      detail: String = "",
      sourceOrder: Option[Long] = None,
      kind: String = "sample"
  ): EyeLinkOracleFact =
    EyeLinkOracleFact
      .create(
        record,
        field,
        kind,
        Some(1),
        sourceOrder,
        path,
        presence,
        value,
        detail
      )
      .fold(error => fail(error.message), identity)

  private def unavailableRecord(eye: String): Vector[EyeLinkOracleFact] =
    Vector(
      fact(
        1,
        1,
        "oracle.source-order",
        presence = EyeLinkOraclePresence.Omitted,
        detail = "independent reader does not retain cross-table source order"
      ),
      fact(1, 2, "sample.eye", eye),
      fact(1, 3, "sample.pupil", "", EyeLinkOraclePresence.Missing, "native missing value")
    )

  private def manifest(
      descriptor: EyeLinkOracleDescriptor,
      facts: Vector[EyeLinkOracleFact]
  ): EyeLinkOracleManifest =
    EyeLinkOracleManifest
      .create(descriptor, facts)
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

  test("canonical oracle preserves explicit values, missing fields, omissions, and escapes") {
    val original = manifest(descriptor(), unavailableRecord("L\tmeasured%value"))
    val rendered = original.renderTsv
    val reparsed = EyeLinkOracleManifest
      .parseTsv("embedded-oracle.tsv", rendered)
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

    assertEquals(reparsed.renderTsv, rendered)
    assert(rendered.contains("L%09measured%25value"))
    assertEquals(reparsed.records.length, 1)
    assertEquals(reparsed.records.head(1).value, "L\tmeasured%value")
    assertEquals(reparsed.records.head(2).presence, EyeLinkOraclePresence.Missing)
  }

  test("descriptor identity includes exact invocation and eye-channel values") {
    val baseline       = manifest(descriptor(), unavailableRecord("L"))
    val changedOptions = manifest(
      descriptor(invocation = "read_asc(samples=FALSE,events=TRUE,parse_all=FALSE)"),
      unavailableRecord("L")
    )
    val swappedEye = manifest(descriptor(), unavailableRecord("R"))

    assertNotEquals(baseline.scientificDigest, changedOptions.scientificDigest)
    assertNotEquals(baseline.scientificDigest, swappedEye.scientificDigest)
  }

  test("unavailable ordering requires an explicit omission in every record") {
    val values = unavailableRecord("L")
      .filterNot(_.fieldPath == "oracle.source-order")
      .zipWithIndex
      .map { case (value, index) =>
        fact(
          value.recordOrdinal,
          index + 1,
          value.fieldPath,
          value.value,
          value.presence,
          value.detail
        )
      }
    val errors = EyeLinkOracleManifest
      .create(descriptor(), values)
      .fold(_.toVector, _ => fail("expected missing ordering disclosure to fail"))

    assert(errors.exists(_.isInstanceOf[EyeLinkOracleError.MissingOrderingDisclosure]))
  }

  test("complete ordering requires unique strictly increasing source order") {
    val values = Vector(
      fact(1, 1, "sample.time", "100", sourceOrder = Some(2L)),
      fact(2, 1, "sample.time", "101", sourceOrder = Some(1L))
    )
    val errors = EyeLinkOracleManifest
      .create(descriptor(EyeLinkOracleOrdering.Complete), values)
      .fold(_.toVector, _ => fail("expected decreasing source order to fail"))

    assert(errors.exists(_.isInstanceOf[EyeLinkOracleError.OrderingConflict]))
  }

  test("records and fields must be contiguous with unique field paths") {
    val values = Vector(
      fact(
        2,
        2,
        "oracle.source-order",
        presence = EyeLinkOraclePresence.Omitted,
        detail = "not exposed"
      ),
      fact(2, 4, "sample.eye", "L"),
      fact(2, 5, "sample.eye", "R")
    )
    val errors = EyeLinkOracleManifest
      .create(descriptor(), values)
      .fold(_.toVector, _ => fail("expected structural errors"))

    assert(errors.exists(_.isInstanceOf[EyeLinkOracleError.NonContiguousRecords]))
    assert(errors.exists(_.isInstanceOf[EyeLinkOracleError.NonContiguousFields]))
    assert(errors.exists(_.isInstanceOf[EyeLinkOracleError.DuplicateFieldPath]))
  }

  test("missing and omitted states cannot silently carry or lose a reason") {
    val withValue = EyeLinkOracleFact.create(
      1,
      1,
      "message",
      None,
      Some(1L),
      "message.effective-time",
      EyeLinkOraclePresence.Omitted,
      "100",
      "reader does not expose offsets"
    )
    val withoutReason = EyeLinkOracleFact.create(
      1,
      1,
      "message",
      None,
      Some(1L),
      "message.effective-time",
      EyeLinkOraclePresence.Omitted,
      "",
      ""
    )

    assert(withValue.isLeft)
    assert(withoutReason.isLeft)
  }

  test("EDF Access API identity cannot be attached to ASC input") {
    val created = EyeLinkOracleDescriptor.create(
      "edf-api-one",
      "fixture-one",
      EyeLinkOracleKind.EdfAccessApi,
      EyeLinkOracleInput.Asc,
      EyeLinkOracleOrdering.Complete,
      "SR EDF Access API",
      "4.2",
      digest("1"),
      digest("2"),
      digest("3"),
      "edf_open_file(load_events=1,load_samples=1)",
      None
    )

    assert(created.isLeft)
  }

end EyeLinkOracleSuite
