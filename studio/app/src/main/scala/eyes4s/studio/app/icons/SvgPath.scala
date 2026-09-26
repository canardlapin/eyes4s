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

/** One command of SVG path data, with its arguments. `command` is the letter
  * as written: upper case is absolute, lower case relative.
  */
final case class PathCommand(command: Char, arguments: List[Double])

/** Why a string is not SVG path data. Each case names the path and the
  * offset of the offending character.
  */
enum SvgPathError derives CanEqual:
  case Empty(path: String)
  case MissingMoveTo(path: String, found: Char)
  case UnknownCommand(path: String, offset: Int, found: Char)
  case BadNumber(path: String, offset: Int, found: String)
  case BadFlag(path: String, offset: Int, found: String)
  case ArgumentCount(path: String, offset: Int, command: Char, count: Int, arity: Int)

  def message: String = this match
    case Empty(p)                => s"path data '$p' has no commands"
    case MissingMoveTo(p, c)     => s"path data '$p' starts with '$c', not a move-to"
    case UnknownCommand(p, i, c) => s"path data '$p' has unknown command '$c' at offset $i"
    case BadNumber(p, i, s) => s"path data '$p' has '$s' at offset $i where a number belongs"
    case BadFlag(p, i, s) => s"path data '$p' has arc flag '$s' at offset $i; it must be 0 or 1"
    case ArgumentCount(p, i, c, n, a) =>
      s"path data '$p': command '$c' at offset $i has $n argument(s), not a multiple of $a"

/** Parsed SVG path data (SVG 1.1 section 8.3): the grammar JavaFX `SVGPath`
  * reads. A value exists only for data that parses.
  */
final case class SvgPath private (text: String, commands: List[PathCommand])

object SvgPath:

  private val arity: Map[Char, Int] =
    Map(
      'M' -> 2,
      'L' -> 2,
      'H' -> 1,
      'V' -> 1,
      'C' -> 6,
      'S' -> 4,
      'Q' -> 4,
      'T' -> 2,
      'A' -> 7,
      'Z' -> 0
    )

  private val number = "[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?".r

  private def separator(c: Char): Boolean = c == ',' || c.isWhitespace

  /** Parses `text` as SVG path data. */
  def parse(text: String): Either[SvgPathError, SvgPath] =
    val n = text.length

    def skip(from: Int): Int =
      var i = from
      while i < n && separator(text(i)) do i += 1
      i

    // The arguments of one command, from `from` to the next command letter.
    def arguments(command: Char, from: Int): Either[SvgPathError, (List[Double], Int)] =
      val upper = command.toUpper
      val out   = List.newBuilder[Double]
      var i     = skip(from)
      var count = 0
      var error = Option.empty[SvgPathError]
      while error.isEmpty && i < n && !text(i).isLetter do
        val isFlag = upper == 'A' && (count % 7 == 3 || count % 7 == 4)
        if isFlag then
          val c = text(i)
          if c == '0' || c == '1' then
            out += (if c == '1' then 1.0 else 0.0)
            count += 1
            i = skip(i + 1)
          else error = Some(SvgPathError.BadFlag(text, i, c.toString))
        else
          number.findPrefixOf(text.substring(i)) match
            case Some(s) =>
              out += s.toDouble
              count += 1
              i = skip(i + s.length)
            case None =>
              val end = text.indexWhere(c => separator(c) || c.isLetter, i + 1) match
                case -1 => n
                case e  => e
              error = Some(SvgPathError.BadNumber(text, i, text.substring(i, end)))
      error
        .toLeft {
          (out.result(), i)
        }
        .flatMap { case (args, next) =>
          val a  = arity(upper)
          val ok = if a == 0 then args.isEmpty else args.nonEmpty && args.size % a == 0
          if ok then Right((args, next))
          else Left(SvgPathError.ArgumentCount(text, from - 1, command, args.size, a))
        }

    def loop(i: Int, acc: List[PathCommand]): Either[SvgPathError, List[PathCommand]] =
      val at = skip(i)
      if at >= n then Right(acc.reverse)
      else
        val c = text(at)
        if !arity.contains(c.toUpper) then Left(SvgPathError.UnknownCommand(text, at, c))
        else if acc.isEmpty && c.toUpper != 'M' then Left(SvgPathError.MissingMoveTo(text, c))
        else
          arguments(c, at + 1).flatMap { case (args, next) =>
            loop(next, PathCommand(c, args) :: acc)
          }

    val start = skip(0)
    if start >= n then Left(SvgPathError.Empty(text))
    else if !text(start).isLetter then Left(SvgPathError.MissingMoveTo(text, text(start)))
    else loop(start, Nil).map(commands => SvgPath(text, commands))
