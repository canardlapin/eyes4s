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

package eyes4s.results

/** A JSON value carried by a result table: its metadata context and the text
  * of its JSON columns. The results layer depends on no JSON library, so a
  * Scala.js client can read tables without one; `eyes4s-io` converts to and
  * from circe.
  *
  * A number keeps the text it was written with ([[TableJson.Number.text]]),
  * so converting a document in and out prints it unchanged. Its canonical
  * spelling, which a table's identity digest is taken over, is the exact
  * decimal with trailing zeros stripped (see [[TableJson.canonical]]).
  * Object members keep their order; the canonical spelling sorts them.
  */
sealed trait TableJson derives CanEqual:
  /** The compact spelling, members in their own order. */
  def compact: String = TableJson.print(this, sorted = false)

  /** The canonical spelling: members sorted by name, numbers as their exact
    * decimal with trailing zeros stripped.
    */
  def canonical: String = TableJson.print(this, sorted = true)

  /** The member called `name`, when this is an object that has one. */
  def field(name: String): Option[TableJson] = this match
    case TableJson.Obj(fields) => fields.reverseIterator.collectFirst { case (`name`, v) => v }
    case _                     => None

object TableJson:
  case object Null                                          extends TableJson
  final case class Bool(value: Boolean)                     extends TableJson
  final case class Text(value: String)                      extends TableJson
  final case class Arr(items: Vector[TableJson])            extends TableJson
  final case class Obj(fields: Vector[(String, TableJson)]) extends TableJson

  /** A JSON number, kept as the text it was written with. The text is
    * checked against the JSON number grammar, so it always parses.
    */
  final case class Number private (text: String) extends TableJson

  object Number:
    private val grammar = "-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?".r

    /** The number spelled `text`, when it is a JSON number. */
    def of(text: String): Option[Number] = Option.when(grammar.matches(text))(new Number(text))

    def apply(value: Long): Number = new Number(value.toString)

  def obj(fields: (String, TableJson)*): TableJson = Obj(fields.toVector)
  def arr(items: TableJson*): TableJson            = Arr(items.toVector)
  def text(value: String): TableJson               = Text(value)
  def integer(value: Long): TableJson              = Number(value)

  /** A strict RFC 8259 reading of `input`: one value, optionally surrounded
    * by whitespace. The failure is the offset and what was expected there.
    */
  def parse(input: String): Either[(Int, String), TableJson] = new Parser(input).document

  /** The exact decimal of a number, trailing zeros stripped and without an
    * exponent. A number whose exponent the decimal type cannot hold keeps its
    * written text.
    */
  private[results] def canonicalNumber(text: String): String =
    try new java.math.BigDecimal(text).stripTrailingZeros.toPlainString
    catch case _: NumberFormatException => text

  /** JSON string quoting: the six short escapes and `\\u` with lowercase
    * hexadecimal digits for every other ISO control character; all other
    * characters, non-ASCII included, are written as they are.
    */
  private[results] def quote(value: String): String =
    val out = new java.lang.StringBuilder(value.length + 2)
    out.append('"')
    var i = 0
    while i < value.length do
      val c = value.charAt(i)
      c match
        case '"'                                    => out.append("\\\"")
        case '\\'                                   => out.append("\\\\")
        case '\b'                                   => out.append("\\b")
        case '\f'                                   => out.append("\\f")
        case '\n'                                   => out.append("\\n")
        case '\r'                                   => out.append("\\r")
        case '\t'                                   => out.append("\\t")
        case other if Character.isISOControl(other) =>
          val digits = "0123456789abcdef"
          out.append("\\u")
          out.append(digits.charAt((other >> 12) & 15))
          out.append(digits.charAt((other >> 8) & 15))
          out.append(digits.charAt((other >> 4) & 15))
          out.append(digits.charAt(other & 15))
        case other => out.append(other)
      i += 1
    out.append('"')
    out.toString

  private def print(value: TableJson, sorted: Boolean): String = value match
    case Null        => "null"
    case Bool(b)     => b.toString
    case Number(t)   => if sorted then canonicalNumber(t) else t
    case Text(s)     => quote(s)
    case Arr(items)  => items.map(print(_, sorted)).mkString("[", ",", "]")
    case Obj(fields) =>
      val members =
        if sorted then
          // A repeated name keeps its last value, as a JSON object reader does.
          fields.reverse.distinctBy(_._1).sortBy(_._1)
        else fields
      members.map((k, v) => quote(k) + ":" + print(v, sorted)).mkString("{", ",", "}")

  private final class Parser(input: String):
    private var at = 0

    def document: Either[(Int, String), TableJson] =
      try
        space()
        val v = value()
        space()
        if at != input.length then fail("the end of the document") else Right(v)
      catch case Refused(offset, expected) => Left(offset -> expected)

    private final case class Refused(offset: Int, expected: String) extends Exception

    private def fail(expected: String): Nothing = throw Refused(at, expected)

    private def peek: Int = if at < input.length then input.charAt(at).toInt else -1

    private def space(): Unit =
      while at < input.length && " \t\r\n".indexOf(input.charAt(at).toInt) >= 0 do at += 1

    private def expect(c: Char): Unit =
      if peek == c.toInt then at += 1 else fail(s"'$c'")

    private def literal(word: String, v: TableJson): TableJson =
      if input.startsWith(word, at) then
        at += word.length
        v
      else fail(word)

    private def value(): TableJson = peek match
      case '{'                                     => obj()
      case '['                                     => arr()
      case '"'                                     => Text(string())
      case 't'                                     => literal("true", Bool(true))
      case 'f'                                     => literal("false", Bool(false))
      case 'n'                                     => literal("null", Null)
      case c if c == '-' || (c >= '0' && c <= '9') => number()
      case _                                       => fail("a JSON value")

    private def obj(): TableJson =
      expect('{')
      space()
      val fields = Vector.newBuilder[(String, TableJson)]
      if peek == '}' then at += 1
      else
        var more = true
        while more do
          space()
          if peek != '"' then fail("a member name")
          val name = string()
          space()
          expect(':')
          space()
          fields += name -> value()
          space()
          peek match
            case ',' => at += 1
            case '}' =>
              at += 1
              more = false
            case _ => fail("',' or '}'")
      Obj(fields.result())

    private def arr(): TableJson =
      expect('[')
      space()
      val items = Vector.newBuilder[TableJson]
      if peek == ']' then at += 1
      else
        var more = true
        while more do
          space()
          items += value()
          space()
          peek match
            case ',' => at += 1
            case ']' =>
              at += 1
              more = false
            case _ => fail("',' or ']'")
      Arr(items.result())

    private def string(): String =
      expect('"')
      val out = new java.lang.StringBuilder
      var end = false
      while !end do
        val c = peek
        if c < 0 then fail("a closing '\"'")
        else if c == '"' then
          at += 1
          end = true
        else if c == '\\' then
          at += 1
          peek match
            case '"'  => out.append('"'); at += 1
            case '\\' => out.append('\\'); at += 1
            case '/'  => out.append('/'); at += 1
            case 'b'  => out.append('\b'); at += 1
            case 'f'  => out.append('\f'); at += 1
            case 'n'  => out.append('\n'); at += 1
            case 'r'  => out.append('\r'); at += 1
            case 't'  => out.append('\t'); at += 1
            case 'u'  =>
              at += 1
              if at + 4 > input.length then fail("four hexadecimal digits")
              val digits = input.substring(at, at + 4)
              if !digits.forall(d => Character.digit(d, 16) >= 0) then
                fail("four hexadecimal digits")
              out.append(Integer.parseInt(digits, 16).toChar)
              at += 4
            case _ => fail("an escape")
        else if c < 0x20 then fail("an escaped control character")
        else
          out.append(c.toChar)
          at += 1
      out.toString

    private def number(): TableJson =
      val start = at
      if peek == '-' then at += 1
      if peek == '0' then at += 1
      else if peek >= '1' && peek <= '9' then while peek >= '0' && peek <= '9' do at += 1
      else fail("a digit")
      if peek == '.' then
        at += 1
        if !(peek >= '0' && peek <= '9') then fail("a digit")
        while peek >= '0' && peek <= '9' do at += 1
      if peek == 'e' || peek == 'E' then
        at += 1
        if peek == '+' || peek == '-' then at += 1
        if !(peek >= '0' && peek <= '9') then fail("a digit")
        while peek >= '0' && peek <= '9' do at += 1
      Number.of(input.substring(start, at)).getOrElse(fail("a JSON number"))
