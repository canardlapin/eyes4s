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

package eyes4s.studio.desktop.typography

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The font families the studio's CSS name, located by file and line.
  *
  * A family is the value of a `-fx-font-family` declaration, or the quoted
  * names in a `-fx-font` shorthand; a comma-separated list yields each name.
  * Comments are skipped.
  */
object StudioFontFamilies:

  /** One family name at a file and line. */
  final case class Named(file: Path, line: Int, family: String)

  private val comment     = "(?s)/\\*.*?\\*/".r
  private val declaration = "(?<![\\w-])-fx-font-family\\s*:\\s*([^;}\\n]+)".r
  private val shorthand   = "(?<![\\w-])-fx-font\\s*:\\s*([^;}\\n]+)".r
  private val quoted      = "\"([^\"]+)\"|'([^']+)'".r

  private def names(value: String): List[String] =
    value.split(",").toList.map(unquote).filter(_.nonEmpty)

  private def unquote(name: String): String =
    val t = name.trim
    if t.length >= 2 && (t.head == '"' || t.head == '\'') && t.last == t.head then
      t.substring(1, t.length - 1).trim
    else t

  /** Every family named in one CSS text. */
  def scan(file: Path, text: String): List[Named] =
    val readable = comment.replaceAllIn(
      text,
      m => java.util.regex.Matcher.quoteReplacement(m.matched.replaceAll("[^\\n]", " "))
    )
    def lineOf(offset: Int) = readable.take(offset).count(_ == '\n') + 1
    val declared            = declaration.findAllMatchIn(readable).toList.flatMap { m =>
      names(m.group(1)).map(n => (m.start, Named(file, lineOf(m.start), n)))
    }
    val short = shorthand.findAllMatchIn(readable).toList.flatMap { m =>
      quoted.findAllMatchIn(m.group(1)).toList.map { q =>
        (m.start, Named(file, lineOf(m.start), Option(q.group(1)).getOrElse(q.group(2))))
      }
    }
    (declared ++ short).sortBy(_._1).map(_._2)

  /** Every family named in the main CSS under `studioRoot`. */
  def named(studioRoot: Path): List[Named] =
    val stream = Files.walk(studioRoot)
    try
      stream.iterator.asScala
        .filter(Files.isRegularFile(_))
        .filter { p =>
          val s = p.toString.replace('\\', '/')
          s.contains("/src/main/") && !s.contains("/target/") && s.endsWith(".css")
        }
        .toList
        .sortBy(_.toString)
        .flatMap(p => scan(p, String(Files.readAllBytes(p), UTF_8)))
    finally stream.close()
