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

package eyes4s.studio.app.icons

/** SVG path data parsing (ticket S1.3), and the catalogue's pure invariants. */
class SvgPathSuite extends munit.FunSuite:

  private def commands(d: String): List[(Char, List[Double])] =
    SvgPath.parse(d).fold(e => fail(e.message), _.commands.map(c => (c.command, c.arguments)))

  test("parses the boards' compact forms") {
    assertEquals(
      commands("M10 3 5 8l5 5"),
      List(('M', List(10.0, 3, 5, 8)), ('l', List(5.0, 5)))
    )
    assertEquals(commands("m6 3 5 5-5 5"), List(('m', List(6.0, 3, 5, 5, -5, 5))))
    assertEquals(commands("M4 .5 7.5 4"), List(('M', List(4.0, 0.5, 7.5, 4))))
    assertEquals(
      commands("M.5.5h1Z"),
      List(('M', List(0.5, 0.5)), ('h', List(1.0)), ('Z', Nil))
    )
    assertEquals(
      commands("M5.5 7V5a2.5 2.5 0 0 1 5 0v2"),
      List(
        ('M', List(5.5, 7)),
        ('V', List(5.0)),
        ('a', List(2.5, 2.5, 0, 0, 1, 5, 0)),
        ('v', List(2.0))
      )
    )
    // Arc flags may be written without separators.
    assertEquals(
      commands("M0 0a1 1 0 1110 0"),
      List(('M', List(0.0, 0)), ('a', List(1.0, 1, 0, 1, 1, 10, 0)))
    )
    assertEquals(commands("M1e1,2E-1"), List(('M', List(10.0, 0.2))))
  }

  test("rejects what is not path data, naming the path and the offset") {
    assertEquals(SvgPath.parse("  "), Left(SvgPathError.Empty("  ")))
    assertEquals(SvgPath.parse("L1 1"), Left(SvgPathError.MissingMoveTo("L1 1", 'L')))
    assertEquals(SvgPath.parse("1 1"), Left(SvgPathError.MissingMoveTo("1 1", '1')))
    assertEquals(SvgPath.parse("M1 1X2"), Left(SvgPathError.UnknownCommand("M1 1X2", 4, 'X')))
    assertEquals(
      SvgPath.parse("M1 1 1"),
      Left(SvgPathError.ArgumentCount("M1 1 1", 0, 'M', 3, 2))
    )
    assertEquals(
      SvgPath.parse("M1 1Z3"),
      Left(SvgPathError.ArgumentCount("M1 1Z3", 4, 'Z', 1, 0))
    )
    assertEquals(
      SvgPath.parse("M1 1h"),
      Left(SvgPathError.ArgumentCount("M1 1h", 4, 'h', 0, 1))
    )
    assertEquals(SvgPath.parse("M1 #"), Left(SvgPathError.BadNumber("M1 #", 3, "#")))
    assertEquals(
      SvgPath.parse("M0 0a1 1 0 2 1 3 3"),
      Left(SvgPathError.BadFlag("M0 0a1 1 0 2 1 3 3", 11, "2"))
    )
    SvgPath
      .parse("M1 1X2")
      .left
      .foreach(e => assert(e.message.contains("'X' at offset 4"), e.message))
  }

  test("every icon's layers parse") {
    val failures = for
      icon  <- Icon.values.toList
      layer <- icon.layers
      error <- SvgPath.parse(layer.pathData).left.toOption
    yield s"${icon.id}: ${error.message}"
    assert(failures.isEmpty, failures.mkString("\n", "\n", ""))
  }

  test("icon ids are unique kebab-case resource names") {
    val ids = Icon.values.toList.map(_.id)
    assertEquals(ids.distinct, ids)
    assert(ids.forall(_.matches("[a-z]+(-[a-z]+)*")), ids)
    Icon.values.foreach(i => assertEquals(Icon.byId(i.id), Some(i)))
    assertEquals(Icon.byId("no-such-icon"), None)
  }

  test("accessible text is never blank") {
    assertEquals(AccessibleText.parse(" \t"), Left(AccessibleTextError.Blank(" \t")))
    assertEquals(AccessibleText.parse(" Back (⌘[) ").map(_.text), Right("Back (⌘[)"))
    import scala.compiletime.testing.typeCheckErrors
    assert(typeCheckErrors("val a: AccessibleText = \"Back\"").nonEmpty)
  }
