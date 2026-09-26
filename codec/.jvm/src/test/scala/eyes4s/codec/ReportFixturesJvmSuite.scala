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

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** The pinned report documents are exactly the writers' output, byte for
  * byte, match the portable mirrors as JSON values, and carry independently
  * known digests. Set EYES4S_WRITE_REPORT_FIXTURES=1 to rewrite them; the
  * run then fails so the change is reviewed.
  */
class ReportFixturesJvmSuite extends munit.FunSuite:
  import ReportFixtures.*

  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private def written: Vector[(String, String)] = Vector(
    covariateSchemaFile -> get(ReportCodecs.covariates.encode(covariateSchema)).spaces2,
    reportSpecFile      -> get(ReportCodecs.reportSpec.encode(spec)).spaces2,
    reportFile          -> get(ReportCodecs.report(keys).encode(report)).spaces2
  )

  /** Digests computed outside eyes4s with `shasum -a 256`. */
  private val shasum = Map(
    covariateSchemaFile -> "d84665c51968550f2378374b68c0c95d72faac2560345ef54d3e0587017e124d",
    reportSpecFile      -> "257d54952779b37e701a0ad6d59392893c0787315d6620b371ad8d41982c5220",
    reportFile          -> "e42425e054fe825c22c21d975b4f9baa7e56e67b4d5d0787a88f8bf0b0f61e07"
  )

  private val directory: Path =
    Iterator
      .iterate(Paths.get(sys.props("user.dir")).toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve("codec/src/test/resources/eyes4s"))
      .find(Files.isDirectory(_))
      .getOrElse(fail("codec/src/test/resources/eyes4s not found"))

  test("the pinned report documents are exactly the writers' output") {
    val rewrite = sys.env
      .get("EYES4S_WRITE_REPORT_FIXTURES")
      .orElse(sys.props.get("EYES4S_WRITE_REPORT_FIXTURES"))
      .contains("1")
    val stale = written.filter((file, text) =>
      !Files.exists(directory.resolve(file)) ||
        Files.readString(directory.resolve(file), StandardCharsets.UTF_8) != text
    )
    if rewrite && stale.nonEmpty then
      stale.foreach((file, text) =>
        Files.writeString(directory.resolve(file), text, StandardCharsets.UTF_8)
      )
      fail(s"Rewrote ${stale.map(_._1)}; review them and rerun.")
    else assertEquals(stale.map(_._1), Vector.empty)
  }

  test("the pinned report documents have independently known digests") {
    written.foreach { (file, _) =>
      val bytes = get(GenerateManifestV1.resource(file))
      assertEquals(ByteDigest.sha256(bytes).hex, shasum(file), file)
    }
  }

  test("the pinned report documents match the portable mirrors") {
    Vector(
      covariateSchemaFile -> ReportFixtureMirrors.covariateSchemaVersionOne,
      reportSpecFile      -> ReportFixtureMirrors.reportSpecVersionOne,
      reportFile          -> ReportFixtureMirrors.reportVersionOne
    ).foreach { (file, mirror) =>
      val text =
        String(IArray.genericWrapArray(get(GenerateManifestV1.resource(file))).toArray, "UTF-8")
      assertEquals(get(io.circe.parser.parse(text)), get(io.circe.parser.parse(mirror)), file)
    }
  }
