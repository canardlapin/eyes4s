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

import scala.compiletime.testing.typeCheckErrors

class Edf2AscProvenanceSuite extends munit.FunSuite:

  private val edfDigest  = Sha256.ofUtf8("edf-source")
  private val ascDigest  = Sha256.ofUtf8("asc-output")
  private val executable = Sha256.ofUtf8("edf2asc-binary")
  private val converter = Edf2AscConverter.of("edf2asc", executable, Some("4.2.1")).toOption.get
  private val arguments = Edf2AscArguments.of(Vector("-s", "-e", "-vel", "-res")).toOption.get

  test("SHA-256 hexadecimal parsing round-trips and names malformed operands") {
    assertEquals(Sha256.fromHex("ASC", ascDigest.hex), Right(ascDigest))
    assertEquals(Sha256.fromHex("ASC", ascDigest.hex.toUpperCase), Right(ascDigest))
    assertEquals(
      Sha256.fromHex("EDF", "abc"),
      Left(Sha256Error.WrongLength("EDF", 3))
    )
    val invalid = ascDigest.hex.updated(17, 'x')
    assertEquals(
      Sha256.fromHex("converter", invalid),
      Left(Sha256Error.InvalidCharacter("converter", 17, 'x'))
    )
  }

  test("converter and argument constructors reject ambiguous evidence") {
    assert(converter.toString.contains("edf2asc"))
    assert(converter.toString.contains(executable.hex))
    assert(Edf2AscConverter.of(" ", executable, None).isLeft)
    assert(Edf2AscConverter.of("edf2asc", executable, Some(" ")).isLeft)
    assert(Edf2AscArguments.of(Vector("-s", " ")).isLeft)
    assert(Edf2AscArguments.of(Vector("-s\n-e")).isLeft)
    assertEquals(Edf2AscArguments.of(Vector.empty).map(_.values), Right(Vector.empty))
  }

  test("an observed receipt cannot certify a failed converter process") {
    val failed = Edf2AscReceipt.observed(
      Some(edfDigest),
      ascDigest,
      converter,
      arguments,
      "macOS-arm64",
      exitCode = 2,
      Edf2AscRecovery.Normal
    )

    assertEquals(
      failed,
      Left(Edf2AscProvenanceError.UnsuccessfulConversion(2, ascDigest))
    )
  }

  test("manual conversion remains declared while satisfying complete release evidence") {
    val receipt = Edf2AscReceipt
      .declared(
        Some(edfDigest),
        ascDigest,
        converter,
        arguments,
        "Linux-x86_64",
        Edf2AscRecovery.Normal
      )
      .toOption
      .get
    val report = EyeLinkAscOrigin
      .Converted(receipt)
      .assess(ConversionEvidencePolicy.Release)
      .toOption
      .get

    assertEquals(receipt.authority, ConversionAuthority.UserDeclared)
    assertEquals(report.origin.ascDigest, ascDigest)
    assertEquals(
      report.warnings,
      Vector(ConversionEvidenceWarning.UserDeclaredConversion(ascDigest))
    )
  }

  test("release evidence requires both a receipt and the EDF digest") {
    val withoutEdf = Edf2AscReceipt
      .declared(
        None,
        ascDigest,
        converter,
        arguments,
        "Windows-x86_64",
        Edf2AscRecovery.Normal
      )
      .toOption
      .get

    assertEquals(
      EyeLinkAscOrigin.Converted(withoutEdf).assess(ConversionEvidencePolicy.Release),
      Left(Edf2AscProvenanceError.MissingEdfDigest(ascDigest))
    )
    assertEquals(
      EyeLinkAscOrigin.Unidentified(ascDigest).assess(ConversionEvidencePolicy.Release),
      Left(Edf2AscProvenanceError.MissingConversionReceipt(ascDigest))
    )
  }

  test("exploratory evidence retains every provenance weakening as a warning") {
    val withoutEdf = Edf2AscReceipt
      .declared(
        None,
        ascDigest,
        converter,
        arguments,
        "Windows-x86_64",
        Edf2AscRecovery.Failsafe
      )
      .toOption
      .get
    val report = EyeLinkAscOrigin
      .Converted(withoutEdf)
      .assess(ConversionEvidencePolicy.Exploratory)
      .toOption
      .get

    assertEquals(
      report.warnings,
      Vector(
        ConversionEvidenceWarning.UserDeclaredConversion(ascDigest),
        ConversionEvidenceWarning.MissingEdfDigest(ascDigest),
        ConversionEvidenceWarning.FailsafeRecovery(ascDigest)
      )
    )
    assertEquals(
      EyeLinkAscOrigin
        .Unidentified(ascDigest)
        .assess(ConversionEvidencePolicy.Exploratory)
        .toOption
        .get
        .warnings,
      Vector(ConversionEvidenceWarning.UnidentifiedConversion(ascDigest))
    )
  }

  test("canonical receipt identity changes with scientific evidence") {
    val observed = Edf2AscReceipt
      .observed(
        Some(edfDigest),
        ascDigest,
        converter,
        arguments,
        "macOS-arm64",
        exitCode = 0,
        Edf2AscRecovery.Normal
      )
      .toOption
      .get
    val differentOptions = Edf2AscReceipt
      .observed(
        Some(edfDigest),
        ascDigest,
        converter,
        Edf2AscArguments.of(Vector("-s", "-e")).toOption.get,
        "macOS-arm64",
        exitCode = 0,
        Edf2AscRecovery.Normal
      )
      .toOption
      .get
    val declared = Edf2AscReceipt
      .declared(
        Some(edfDigest),
        ascDigest,
        converter,
        arguments,
        "macOS-arm64",
        Edf2AscRecovery.Normal
      )
      .toOption
      .get

    assertNotEquals(observed.canonicalDigest, differentOptions.canonicalDigest)
    assertNotEquals(observed.canonicalDigest, declared.canonicalDigest)
  }

  test("law-bearing provenance constructors are private") {
    val errors = typeCheckErrors("""
      import eyes4s.io.*
      val digest = Sha256.ofUtf8("x")
      val converter = new Edf2AscConverter("", digest, None)
      val arguments = new Edf2AscArguments(Vector(""))
      val receipt = new Edf2AscReceipt(
        None,
        digest,
        converter,
        arguments,
        "",
        Edf2AscRecovery.Normal,
        ConversionAuthority.ObservedProcess
      )
    """)

    assert(errors.nonEmpty)
  }

end Edf2AscProvenanceSuite
