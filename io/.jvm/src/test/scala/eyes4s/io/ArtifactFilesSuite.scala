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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.codec.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

import java.nio.file.{Files, Path}

/** The JVM file source over a directory the test lays out: end-to-end
  * resolution; missing and changed files named by entry; and names that leave
  * the root, symbolic links out of it, non-regular files and oversize files
  * refused without being read.
  */
class ArtifactFilesSuite extends munit.FunSuite:
  import SavedGraphFixture.*

  private def get[E, A](e: Either[E, A]): A           = e.fold(x => fail(s"$x"), identity)
  private def array(bytes: IArray[Byte]): Array[Byte] = Array.tabulate(bytes.length)(bytes(_))
  private def name(value: String): ArtifactName       = get(ArtifactName.of(value))

  /** A fresh directory for one test, deleted afterwards; links are deleted,
    * never followed.
    */
  private def withDirectories[A](count: Int)(body: Vector[Path] => A): A =
    val roots = Vector.fill(count)(Files.createTempDirectory("eyes4s-manifest"))
    try body(roots)
    finally
      roots.foreach { root =>
        val walk = Files.walk(root)
        try
          walk
            .sorted(java.util.Comparator.reverseOrder[Path]())
            .forEach(p => Files.deleteIfExists(p): Unit)
        finally walk.close()
      }

  /** Write the manifest and every artifact whose name stays inside `root`. */
  private def write(value: SavedManifest, root: Path): Unit =
    Files.write(root.resolve("manifest.json"), array(value.bytes))
    value.artifacts.foreach { a =>
      val path = root.resolve(a.name.value).normalize
      if path.startsWith(root) then Files.write(path, array(a.bytes)): Unit
    }

  private def resolve(value: SavedManifest, root: Path) =
    ArtifactFiles
      .resolve[IO, StudyKey, Px](value.address, root, "manifest.json", decoders)
      .unsafeRunSync()
      .left
      .map(_.toVector)

  private def base(root: Path): String = root.toRealPath().toString

  test("a saved study written to a directory resolves through the file source") {
    withDirectories(1) { roots =>
      val value = saved()
      write(value, roots(0))
      val resolved = get(resolve(value, roots(0)))
      assertEquals(resolved.inputs.map(_._2.reference.digest), Vector("cebe7474ab5c2aec"))
      assertEquals(
        resolved.results.map(_._2.encode),
        Vector(StudyResultCodecs.cosine[Px].codec.encode(result))
      )
      assertEquals(resolved.ledgers.map(_._2.outcome), Vector(AdmissionOutcome.Complete))
    }
  }

  test("missing and changed files are named by entry; a missing manifest by its address") {
    withDirectories(2) { roots =>
      val value = saved()
      write(value, roots(0))
      Files.delete(roots(0).resolve("plan.json"))
      val changed = Files.readAllBytes(roots(0).resolve("result.json"))
      changed(10) = (changed(10) ^ 1).toByte
      Files.write(roots(0).resolve("result.json"), changed)
      val errors = get(resolve(value, roots(0)).swap)
      assertEquals(errors.head, ResolveError.Missing(name("plan.json")))
      assert(errors(1) match
        case ResolveError.Digest(n, _, _) => n == name("result.json")
        case _                            => false)
      assertEquals(
        resolve(value, roots(1)),
        Left(Vector(ResolveError.MissingManifest(value.address)))
      )
    }
  }

  test("relative and absolute names that leave the root are refused unread") {
    withDirectories(2) { roots =>
      // The absolute name denotes a real file holding the declared bytes; it
      // would verify if it were read, so only the refusal keeps it out.
      val absolute = roots(1).toRealPath().resolve("ledger.json").toString
      Vector("../ledger.json", absolute).foreach { ledgerName =>
        val value = saved(ledgerName)
        write(value, roots(0))
        Files.write(roots(1).resolve("ledger.json"), array(value.artifacts(2).bytes))
        assertEquals(
          resolve(value, roots(0)),
          Left(
            Vector(
              ResolveError.Refused(
                name(ledgerName),
                SourceFailure.OutsideRoot(ledgerName, base(roots(0)))
              )
            )
          )
        )
      }
    }
  }

  test("a symbolic link out of the root is refused, and its target's digest never leaks") {
    withDirectories(2) { roots =>
      val value = saved()
      write(value, roots(0))
      val outside = roots(1).resolve("ledger.json")
      Files.move(roots(0).resolve("ledger.json"), outside)
      Files.createSymbolicLink(roots(0).resolve("ledger.json"), outside)
      assertEquals(
        resolve(value, roots(0)),
        Left(
          Vector(
            ResolveError.Refused(
              name("ledger.json"),
              SourceFailure.OutsideRoot("ledger.json", base(roots(0)))
            )
          )
        )
      )
    }
  }

  test("a link to a directory is refused as not a regular file") {
    withDirectories(1) { roots =>
      val value = saved()
      write(value, roots(0))
      Files.delete(roots(0).resolve("ledger.json"))
      val directory = Files.createDirectory(roots(0).resolve("records"))
      Files.createSymbolicLink(roots(0).resolve("ledger.json"), directory)
      assertEquals(
        resolve(value, roots(0)),
        Left(
          Vector(
            ResolveError.Refused(
              name("ledger.json"),
              SourceFailure.NotRegularFile("ledger.json")
            )
          )
        )
      )
    }
  }

  test(
    "an entry larger than its declared length, or an oversized manifest, is refused unread"
  ) {
    withDirectories(1) { roots =>
      val value = saved()
      write(value, roots(0))
      val declared = value.manifest.entries(2).length
      Files.write(
        roots(0).resolve("ledger.json"),
        array(value.artifacts(2).bytes) ++ Array.fill[Byte](10)(32)
      )
      assertEquals(
        resolve(value, roots(0)),
        Left(
          Vector(
            ResolveError.Refused(
              name("ledger.json"),
              SourceFailure.Oversize("ledger.json", declared + 10, declared)
            )
          )
        )
      )
      val small = ArtifactFiles.source[IO](
        roots(0),
        {
          case ByteRequest.Manifest(_)  => "manifest.json"
          case ByteRequest.Entry(entry) => entry.name.value
        },
        manifestLimit = 100
      )
      assertEquals(
        ArtifactLoading
          .resolve(value.address, small, decoders)
          .unsafeRunSync()
          .left
          .map(_.toVector),
        Left(
          Vector(
            ResolveError.RefusedManifest(
              value.address,
              SourceFailure.Oversize("manifest.json", value.bytes.length.toLong, 100)
            )
          )
        )
      )
    }
  }
