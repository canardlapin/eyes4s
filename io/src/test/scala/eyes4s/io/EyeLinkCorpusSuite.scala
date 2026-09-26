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

class EyeLinkCorpusSuite extends munit.FunSuite:

  private val ZeroDigest  = "00" * 32
  private val OneDigest   = "11" * 32
  private val TwoDigest   = "22" * 32
  private val ThreeDigest = "33" * 32

  private val syntheticBase: Map[String, String] = Map(
    "fixture_id"            -> "synthetic-one",
    "kind"                  -> "synthetic",
    "availability"          -> "included",
    "license"               -> "project-generated-apache-2.0",
    "privacy"               -> "no-human-data",
    "permission_ref"        -> "docs/corpus.md#license",
    "local_path"            -> "synthetic-one.asc",
    "hardware"              -> "synthetic",
    "sampling_rates_hz"     -> "1000",
    "eye_layout"            -> "left",
    "tracking_mode"         -> "head-fixed",
    "coordinate_modes"      -> "gaze",
    "asc_digest"            -> ZeroDigest,
    "expected_capabilities" ->
      "content-samples,coordinates-gaze,eye-left,rate-1000,source-asc,tracking-head-fixed"
  )

  private val plannedPrivateBase: Map[String, String] = Map(
    "fixture_id"            -> "planned-private",
    "kind"                  -> "private-real-pair",
    "availability"          -> "planned",
    "license"               -> "permission-pending",
    "privacy"               -> "review-pending",
    "permission_ref"        -> "acquisition-plan:private",
    "hardware"              -> "eyelink-1000-family",
    "sampling_rates_hz"     -> "1000",
    "eye_layout"            -> "left",
    "tracking_mode"         -> "head-fixed",
    "coordinate_modes"      -> "gaze",
    "expected_capabilities" ->
      "converter-receipt,coordinates-gaze,eye-left,hardware-1000-family,rate-1000,source-asc,tracking-head-fixed"
  )

  private def row(fields: Map[String, String]): String =
    EyeLinkCorpusManifest.columns.map(field => fields.getOrElse(field, "")).mkString("\t")

  private def document(rows: String*): String =
    Vector(
      s"# eyes4s-eyelink-corpus\t${EyeLinkCorpusManifest.schemaVersion}\t2026.08.15.test\t${EyeLinkSupport.contractVersion}",
      EyeLinkCorpusManifest.columns.mkString("\t")
    ).++(rows).mkString("\n") + "\n"

  private def parsed(rows: String*): EyeLinkCorpusManifest =
    EyeLinkCorpusManifest
      .parseTsv("embedded-corpus.tsv", document(rows*))
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

  private def errors(rows: String*): Vector[EyeLinkCorpusError] =
    EyeLinkCorpusManifest
      .parseTsv("embedded-corpus.tsv", document(rows*))
      .fold(_.toVector, _ => fail("expected corpus manifest to be rejected"))

  test("synthetic fixture parses and canonical rendering round-trips") {
    val manifest = parsed(
      row(
        syntheticBase.updated(
          "permission_ref",
          "permission%2Cscope%25literal"
        )
      )
    )
    val fixture = manifest.fixtures.head

    assertEquals(fixture.id, "synthetic-one")
    assert(fixture.toString.contains("synthetic-one"))
    assertEquals(fixture.kind, EyeLinkCorpusKind.Synthetic)
    assertEquals(fixture.permissionReference, "permission,scope%literal")
    assertEquals(fixture.samplingRatesHz, Vector(1000))
    assertEquals(fixture.coordinateModes, Vector(EyeLinkCorpusCoordinates.Gaze))
    assert(fixture.isPubliclyLoadable)
    assert(!fixture.containsAcquiredRealData)
    assertEquals(fixture.ascDigest.map(_.hex), Some(ZeroDigest))

    val reparsed = EyeLinkCorpusManifest
      .parseTsv("rendered.tsv", manifest.renderTsv)
      .fold(values => fail(values.toVector.map(_.message).mkString("\n")), identity)
    assertEquals(reparsed.renderTsv, manifest.renderTsv)
  }

  test("complete private digest-only pair retains exact converter evidence") {
    val privateFields = plannedPrivateBase ++ Map(
      "fixture_id"         -> "private-pair",
      "availability"       -> "digest-only",
      "license"            -> "private-no-redistribution",
      "privacy"            -> "private-restricted",
      "permission_ref"     -> "irb:private-retention-only",
      "converter_name"     -> "edf2asc",
      "converter_digest"   -> OneDigest,
      "converter_version"  -> "4.2.1",
      "converter_options"  -> "-s,--label%2Cvalue",
      "converter_platform" -> "macos-arm64",
      "converter_recovery" -> "normal",
      "edf_digest"         -> TwoDigest,
      "asc_digest"         -> ThreeDigest
    )
    val fixture = parsed(row(privateFields)).fixtures.head
    val receipt = fixture.conversionReceipt.getOrElse(fail("missing conversion receipt"))

    assertEquals(fixture.localPath, None)
    assert(fixture.containsAcquiredRealData)
    assert(!fixture.isPubliclyLoadable)
    assertEquals(receipt.edfDigest.map(_.hex), Some(TwoDigest))
    assertEquals(receipt.ascDigest.hex, ThreeDigest)
    assertEquals(receipt.converter.executableDigest.hex, OneDigest)
    assertEquals(receipt.arguments.values, Vector("-s", "--label,value"))
    assertEquals(receipt.authority, ConversionAuthority.UserDeclared)
  }

  test("coverage never promotes synthetic or planned fixtures to real evidence") {
    val manifest = parsed(row(syntheticBase), row(plannedPrivateBase))
    manifest.coverage.dimensionPairs.foreach(p => assert(p.toString.contains(p.first)))
    val byId = manifest.coverage.capabilities.map(value => value.capability.id -> value).toMap

    assertEquals(
      byId("source-asc").status,
      EyeLinkCorpusCoverageStatus.SyntheticOnly
    )
    assertEquals(
      byId("converter-receipt").status,
      EyeLinkCorpusCoverageStatus.PlannedOnly
    )
    assertEquals(
      byId("hardware-1000-family").status,
      EyeLinkCorpusCoverageStatus.PlannedOnly
    )
    assert(
      manifest.coverage.missingRequiredCapabilities.exists(_.id == "eye-binocular")
    )
    assert(
      manifest.coverage.unvalidatedRequiredCapabilities.exists(_.id == "source-asc")
    )
  }

  test("planned acquisition cannot masquerade as observed evidence") {
    val invalid = plannedPrivateBase ++ Map(
      "local_path" -> "private.asc",
      "asc_digest" -> ZeroDigest
    )
    val found = errors(row(invalid))

    assert(found.exists {
      case EyeLinkCorpusError.InvalidFixture(_, _, "planned-private", detail) =>
        detail.contains("planned acquisition cannot carry")
      case _ => false
    })
  }

  test("private bytes cannot receive a public corpus path") {
    val invalid = plannedPrivateBase ++ Map(
      "availability" -> "included",
      "license"      -> "private-no-redistribution",
      "privacy"      -> "private-restricted",
      "local_path"   -> "private.asc",
      "asc_digest"   -> ZeroDigest
    )
    assert(errors(row(invalid)).exists {
      case EyeLinkCorpusError.InvalidFixture(_, _, _, detail) =>
        detail.contains("private real data cannot be included") ||
        detail.contains("included real fixture requires")
      case _ => false
    })
  }

  test("synthetic fixture cannot claim converter or hardware evidence") {
    val invalid = syntheticBase.updated(
      "expected_capabilities",
      syntheticBase("expected_capabilities") +
        ",converter-receipt,hardware-1000-family"
    )
    assert(errors(row(invalid)).exists {
      case EyeLinkCorpusError.InvalidFixture(_, _, _, detail) =>
        detail.contains("synthetic fixtures cannot claim")
      case _ => false
    })
  }

  test("unknown capabilities and partial converter receipts are named") {
    val unknown = syntheticBase.updated(
      "expected_capabilities",
      syntheticBase("expected_capabilities") + ",not-a-capability"
    )
    val partial = plannedPrivateBase.updated("converter_name", "edf2asc")

    assert(errors(row(unknown)).exists {
      case EyeLinkCorpusError.InvalidFixture(_, _, _, detail) =>
        detail.contains("unknown support capabilities=not-a-capability")
      case _ => false
    })
    assert(
      errors(row(partial)).exists(_.isInstanceOf[EyeLinkCorpusError.PartialConverterEvidence])
    )
  }

  test("unsafe paths, duplicate ids, and duplicate paths are rejected") {
    val unsafe = syntheticBase.updated("local_path", "../private.asc")
    assert(errors(row(unsafe)).exists(_.isInstanceOf[EyeLinkCorpusError.UnsafeLocalPath]))

    val second        = syntheticBase.updated("fixture_id", "synthetic-two")
    val duplicateId   = errors(row(syntheticBase), row(syntheticBase))
    val duplicatePath = errors(row(syntheticBase), row(second))
    assert(duplicateId.exists(_.isInstanceOf[EyeLinkCorpusError.DuplicateFixtureId]))
    assert(duplicatePath.exists(_.isInstanceOf[EyeLinkCorpusError.DuplicateLocalPath]))
  }

  test("malformed manifest errors retain source and operands") {
    val wrongFields = errors("too\tfew")
    assert(wrongFields.forall(_.message.contains("source='embedded-corpus.tsv'")))
    assert(wrongFields.exists(_.message.contains("fields=2")))

    val badDigest    = syntheticBase.updated("asc_digest", "xyz")
    val digestErrors = errors(row(badDigest))
    assert(digestErrors.forall(_.message.contains("source='embedded-corpus.tsv'")))
    assert(digestErrors.exists(_.message.contains("field='asc_digest'")))
  }

end EyeLinkCorpusSuite
