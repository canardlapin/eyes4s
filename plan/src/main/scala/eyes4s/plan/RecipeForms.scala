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

package eyes4s.plan

import cats.data.NonEmptyVector
import eyes4s.kernel.*

/** A form's raw values by field id. An `Absent` value is not stored, so a
  * field the map lacks and an absent field are the same form.
  */
final case class FormValues private (values: Map[FieldId, RawValue]) derives CanEqual:
  def get(id: FieldId): RawValue                      = values.getOrElse(id, RawValue.Absent)
  def updated(id: FieldId, raw: RawValue): FormValues =
    if raw == RawValue.Absent then new FormValues(values - id)
    else new FormValues(values.updated(id, raw))

object FormValues:
  val empty: FormValues = new FormValues(Map.empty)

  def of(values: (FieldId, RawValue)*): FormValues = from(values.toMap)

  def from(values: Map[FieldId, RawValue]): FormValues =
    new FormValues(values.filterNot(_._2 == RawValue.Absent))

/** A typed field over any [[FieldView]]: a raw value first passes the view's
  * own checks (shape, bounds and rules, [[FieldView.check]]), then `read`
  * builds the typed value through the domain constructors. `write` gives the
  * raw value of a typed one.
  */
final class StructuredField[E, A] private (
    val view: FieldView,
    read: RawValue => Either[E, A],
    write: A => RawValue,
    val errorMessage: E => String
) extends FormField[E, A]:
  def parse(raw: RawValue): Either[FieldError[E], A] =
    view
      .check(raw)
      .flatMap(_ =>
        read(raw).left.map(e => FieldError.Refused(view.id, raw, e, errorMessage(e)))
      )

  def raw(value: A): RawValue = write(value)

object StructuredField:
  def of[E, A](view: FieldView)(
      read: RawValue => Either[E, A],
      write: A => RawValue,
      errorMessage: E => String
  ): StructuredField[E, A] = new StructuredField(view, read, write, errorMessage)

/** Parses every field of a form on its own and accumulates the refusals. */
private[plan] object FormParse:
  /** Every refused field of a form, at least one. */
  type Refusals = NonEmptyVector[FieldError[RecipeParameterError]]

  def errors(results: Either[FieldError[RecipeParameterError], ?]*): Option[Refusals] =
    NonEmptyVector.fromVector(results.toVector.flatMap(_.left.toOption))

/** Reading typed parts from a raw value that already passed its view's checks.
  * A part the checks guarantee but that is missing is `MissingPart`, never a
  * throw.
  */
private[plan] object RawParts:
  /** A typed part, or why it could not be read. */
  type R[A] = Either[RecipeParameterError, A]

  def missing(field: String, part: String): RecipeParameterError =
    RecipeParameterError.MissingPart(field, part)

  def fields(raw: RawValue): Vector[(FieldId, RawValue)] = raw match
    case RawValue.Group(fs)      => fs
    case RawValue.Variant(_, fs) => fs
    case _                       => Vector.empty

  def part(field: String, raw: RawValue, id: String): R[RawValue] =
    fields(raw).find(_._1.value == id).map(_._2).toRight(missing(field, id))

  def optional(raw: RawValue): Option[RawValue] = raw match
    case RawValue.Absent => None
    case other           => Some(other)

  private def numeral[N](field: String, id: String, raw: RawValue)(using n: Numeral[N]): R[N] =
    raw match
      case RawValue.Number(t) => n.read(t).toRight(missing(field, id))
      case _                  => Left(missing(field, id))

  def real(field: String, raw: RawValue, id: String): R[Double] =
    part(field, raw, id).flatMap(numeral[Double](field, id, _))
  def int(field: String, raw: RawValue, id: String): R[Int] =
    part(field, raw, id).flatMap(numeral[Int](field, id, _))
  def long(field: String, raw: RawValue, id: String): R[Long] =
    part(field, raw, id).flatMap(numeral[Long](field, id, _))

  def realOf(field: String, raw: RawValue): R[Double] = numeral[Double](field, field, raw)
  def intOf(field: String, raw: RawValue): R[Int]     = numeral[Int](field, field, raw)
  def longOf(field: String, raw: RawValue): R[Long]   = numeral[Long](field, field, raw)

  def token(field: String, raw: RawValue): R[String] = raw match
    case RawValue.Choice(t)     => Right(t)
    case RawValue.Text(t)       => Right(t)
    case RawValue.Variant(t, _) => Right(t)
    case _                      => Left(missing(field, field))

  def tokenPart(field: String, raw: RawValue, id: String): R[String] =
    part(field, raw, id).flatMap(token(field, _))

  def choose[A](field: String, raw: RawValue, values: Vector[A])(using l: Labelled[A]): R[A] =
    token(field, raw).flatMap(t =>
      values.find(l.token(_) == t).toRight(unknown(field, t, values.map(l.token)))
    )

  def unknown(field: String, token: String, options: Vector[String]): RecipeParameterError =
    RecipeParameterError.UnknownToken(field, token, options)

  def number(value: Double): RawValue = RawValue.Number(Numeral.real.write(value))
  def number(value: Int): RawValue    = RawValue.Number(Numeral.int32.write(value))
  def number(value: Long): RawValue   = RawValue.Number(Numeral.int64.write(value))

  def group(parts: (String, RawValue)*): RawValue =
    RawValue.Group(parts.toVector.map((k, v) => FieldId.literal(k) -> v))

  def variant(token: String, parts: (String, RawValue)*): RawValue =
    RawValue.Variant(token, parts.toVector.map((k, v) => FieldId.literal(k) -> v))

  def choice[A](value: A)(using l: Labelled[A]): RawValue = RawValue.Choice(l.token(value))

  def geometry[A](e: Either[GeometryError, A]): R[A] =
    e.left.map(RecipeParameterError.Geometry.apply)
