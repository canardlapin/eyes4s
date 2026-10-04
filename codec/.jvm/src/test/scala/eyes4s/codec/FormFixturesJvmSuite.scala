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
import java.nio.file.{Files, Paths}

/** The pinned form-view-v1.json and form-values-v1.json are exactly the
  * pretty-printed encodings of [[FormFixtures]], re-encode byte for byte, and
  * carry the same JSON values as the portable compact mirrors.
  *
  * Set `EYES4S_WRITE_FORM_FIXTURES=1` to rewrite the resources from the
  * fixture values; the run then fails so the change is reviewed.
  */
class FormFixturesJvmSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A  = e.fold(error => fail(s"$error"), identity)
  private def resource(name: String): String =
    val stream = Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .getOrElse(fail(s"missing resource $name"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()

  private def pinned[A](
      file: String,
      codec: VersionedCodec[A],
      value: A,
      mirror: String
  ): Unit =
    val json = get(codec.encode(value))
    test(s"$file is the encoding of its fixture value") {
      if sys.env
          .get("EYES4S_WRITE_FORM_FIXTURES")
          .orElse(sys.props.get("EYES4S_WRITE_FORM_FIXTURES"))
          .contains("1")
      then
        val resources = Paths.get("codec/src/test/resources/eyes4s")
        val directory = Iterator
          .iterate(Paths.get(sys.props("user.dir")).toAbsolutePath.normalize)(_.getParent)
          .takeWhile(_ != null)
          .map(_.resolve(resources))
          .find(Files.isDirectory(_))
          .getOrElse(fail(s"$resources not found from ${sys.props("user.dir")}"))
        Files.write(
          directory.resolve(file),
          json.spaces2.getBytes(StandardCharsets.UTF_8)
        )
        fail(s"rewrote $file; review it and run again")
      assertEquals(resource(file), json.spaces2)
    }
    test(s"$file decodes to its fixture and re-encodes byte-identically") {
      val read = get(codec.parse(resource(file)))
      assertEquals(read, value)
      assertEquals(get(codec.encode(read)).spaces2, resource(file))
    }
    test(s"the portable mirror of $file carries the same JSON value") {
      assertEquals(get(io.circe.parser.parse(mirror)), json)
    }

  pinned("form-view-v1.json", FormCodecs.view, FormFixtures.view, FormMirrors.view)
  pinned("form-values-v1.json", FormCodecs.values, FormFixtures.values, FormMirrors.values)
