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

package eyes4s.audittool

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import io.circe.Json
import scala.quoted.*
import scala.tasty.inspector.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.util.{SourceFile as CompilerSourceFile}

class InventorySuite extends munit.FunSuite:
  private val root = Path.of(".").toAbsolutePath.normalize
  private val classes = Path.of(classOf[fixtures.DeclaredValue].getProtectionDomain.getCodeSource.getLocation.toURI)
  private val classpath = System.getProperty("java.class.path").split(java.io.File.pathSeparator).toList
  private val files =
    val stream = Files.walk(classes.resolve("eyes4s/audittool/fixtures"))
    try stream.iterator.asScala.filter(_.toString.endsWith(".tasty")).map(_.toString).toList.sorted
    finally stream.close()

  private def entries(files: List[String]): Vector[Json] =
    Inventory.inspect(root, "jvm", files, classpath).hcursor.get[Vector[Json]]("entries").toOption.get

  test("canonical source lines use UTF-16 offsets and Scala line terminators") {
    val directory = Files.createTempDirectory("eyes4s-origin-")
    val file = directory.resolve("Unicode.scala")
    try
      Files.writeString(file, "a😀\r\nb\rc\nd\fe\u001af")
      val origin = new Inventory.DeclarationOrigin(directory, "Unicode.scala")
      assertEquals(origin.path, "Unicode.scala")
      assertEquals(List(0, 4, 5, 7, 9, 11, 13).map(origin.lineAt), List(1, 1, 2, 3, 4, 5, 6))
      intercept[IllegalArgumentException](origin.lineAt(-1))
      intercept[IllegalArgumentException](origin.lineAt(15))
    finally
      Files.deleteIfExists(file)
      Files.deleteIfExists(directory)
  }

  test("member spans are attributed to the owning declaration despite a referring tree source") {
    // Model the compiler boundary defect directly: a member's offsets are from
    // Declarations.scala, but lazy loading has attached References.scala to it.
    // The owning TASTy's source must win regardless of the member's source.
    val inspector = new Inspector:
      def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
        import quotes.reflect.*
        val reference = tastys.find(_.path.endsWith("References.tasty")).get
        val referringSource = reference.ast.pos.sourceFile.asInstanceOf[CompilerSourceFile]
        val checked = scala.collection.mutable.Set.empty[String]
        for tasty <- tastys if !tasty.path.endsWith("References.tasty") do
          val origin = new Inventory.DeclarationOrigin(root, tasty.ast.pos.sourceFile.path)
          val traverser = new TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit =
              tree match
                case definition: ClassDef if Set("DeclaredError", "InvalidName", "DeclaredValue").contains(definition.name) =>
                  val loaded = tree.asInstanceOf[tpd.Tree].cloneIn(referringSource).asInstanceOf[Tree]
                  assertEquals(loaded.pos.sourceFile.path, reference.ast.pos.sourceFile.path)
                  val position = Inventory.declarationPosition(loaded, origin)
                  assertEquals(position.source, "src/test/scala/eyes4s/audittool/fixtures/Declarations.scala")
                  val expectedLine = Map("DeclaredError" -> 19, "InvalidName" -> 20, "DeclaredValue" -> 23)(definition.name)
                  assertEquals(position.line, expectedLine)
                  checked += definition.name
                case _ => ()
              super.traverseTree(tree)(owner)
          traverser.traverseTree(tasty.ast)(Symbol.spliceOwner)
        assertEquals(checked.toSet, Set("DeclaredError", "InvalidName", "DeclaredValue"))
    assert(TastyInspector.inspectAllTastyFiles(files, Nil, classpath)(inspector))
  }

  test("declaration origins and spans come from declarations despite reference-first TASTy loading") {
    val declarationFirst = files.sortBy(path => if path.endsWith("References.tasty") then 1 else 0)
    val referenceFirst = declarationFirst.reverse
    val expectedSource = "src/test/scala/eyes4s/audittool/fixtures/Declarations.scala"
    val expected = Map(
      "class eyes4s.audittool.fixtures.DeclaredError" -> 19,
      "class eyes4s.audittool.fixtures.DeclaredError$.InvalidName" -> 20,
      "value eyes4s.audittool.fixtures.DeclaredError$.Missing" -> 21,
      "class eyes4s.audittool.fixtures.DeclaredValue" -> 23
    )
    val forward = entries(declarationFirst)
    val backward = entries(referenceFirst)
    for rows <- List(forward, backward) do
      val indexed = rows.map(row =>
        val cursor = row.hcursor
        (cursor.get[String]("kind").toOption.get + " " + cursor.get[String]("id").toOption.get) -> row
      ).toMap
      expected.foreach { (key, line) =>
        val cursor = indexed.getOrElse(key, fail(s"missing $key; keys: ${indexed.keys.mkString(", ")}")).hcursor
        assertEquals(cursor.get[String]("source").toOption.get, expectedSource, key)
        assertEquals(cursor.get[Int]("line").toOption.get, line, key)
      }
    assertEquals(backward, forward)
  }
