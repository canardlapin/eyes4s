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

package eyes4s.studio.desktop.journey

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

/** One row of FIXTURE.md's participant table (σ 2°), as written. */
final case class FixtureRow(
    participant: String,
    requested: Int,
    contributing: Int,
    failed: Int,
    noMatch: Int,
    notAdmitted: Int,
    m: BigDecimal,
    b: BigDecimal,
    d: BigDecimal,
    remembered: (BigDecimal, Int),
    forgotten: (BigDecimal, Int)
) derives CanEqual

/** docs/studio/fixture/FIXTURE.md as the journey reads it (ticket S10.1):
  * every number the journey asserts is either a phrase of the file, found
  * verbatim, or a row of its participant table. Nothing here is computed. JVM
  * test scope only.
  */
object FixtureDoc:

  /** The repository root, as the build records it for the desktop tests. */
  lazy val root: Path =
    val in = getClass.getClassLoader.getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
    try Paths.get(String(in.readAllBytes, UTF_8).trim)
    finally in.close()

  lazy val text: String =
    String(Files.readAllBytes(root.resolve("docs/studio/fixture/FIXTURE.md")), UTF_8)

  /** docs/studio/PARITY_CHECKLIST.md: the boards' items the fixture implies. */
  lazy val checklist: String =
    String(Files.readAllBytes(root.resolve("docs/studio/PARITY_CHECKLIST.md")), UTF_8)

  /** Whether the parity checklist lists `phrase`, verbatim. */
  def lists(phrase: String): Boolean = checklist.contains(phrase)

  /** Whether FIXTURE.md states `phrase`, verbatim. */
  def states(phrase: String): Boolean = text.contains(phrase)

  private def number(cell: String): BigDecimal = BigDecimal(cell.trim.stripPrefix("+"))

  // "+0.26 (11)": a mean and its n.
  private def meanN(cell: String): (BigDecimal, Int) =
    cell.trim.split("[ ()]+").toList match
      case List(m, n) => (number(m), n.toInt)
      case other      => throw IllegalStateException(s"FIXTURE.md cell '$cell': $other")

  /** The participant table, in order. */
  lazy val participants: Vector[FixtureRow] =
    text.linesIterator
      .filter(_.startsWith("| P"))
      .filterNot(_.startsWith("| P |"))
      .map { line =>
        line.split('|').map(_.trim).filter(_.nonEmpty).toList match
          case List(p, req, con, fail, nm, na, m, b, d, rem, forg) =>
            FixtureRow(
              p,
              req.toInt,
              con.toInt,
              fail.toInt,
              nm.toInt,
              na.toInt,
              number(m),
              number(b),
              number(d),
              meanN(rem),
              meanN(forg)
            )
          case other => throw IllegalStateException(s"FIXTURE.md row '$line': $other")
      }
      .toVector

  /** A bracketed list after `label` on one line: "M [0.41, 0.58, 0.73, 0.86]". */
  def list(label: String): Vector[BigDecimal] =
    val at = text.indexOf(label)
    if at < 0 then throw IllegalStateException(s"FIXTURE.md has no '$label'")
    val open  = text.indexOf('[', at)
    val close = text.indexOf(']', open)
    text.substring(open + 1, close).split(',').map(number).toVector

  /** The list called `name` on FIXTURE.md's one line holding `line`:
    * `named("M/B/D by scale:", "B")` reads "B [0.22, 0.29, 0.35, 0.63]".
    */
  def named(line: String, name: String): Vector[BigDecimal] =
    val lines = text.linesIterator.filter(_.contains(line)).toVector
    if lines.size != 1 then
      throw IllegalStateException(s"FIXTURE.md has ${lines.size} lines with '$line'")
    val list = s"(?:^|[\\s:])${java.util.regex.Pattern.quote(name)} \\[([^\\]]*)\\]".r
    list.findFirstMatchIn(lines.head) match
      case Some(m) => m.group(1).split(',').map(number).toVector
      case None    => throw IllegalStateException(s"FIXTURE.md: no '$name [..]' after '$line'")

  /** `value` as FIXTURE.md writes it: to `places` decimals, half up. */
  def rounded(value: Double, places: Int = 2): BigDecimal =
    BigDecimal(value).setScale(places, BigDecimal.RoundingMode.HALF_UP)
