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

import eyes4s.plan.*

/** The name of a declared covariate: a trials-table attribute column. */
final case class CovariateName private (value: String) derives CanEqual:
  override def toString: String = value

object CovariateName:
  def of(value: String): Either[CovariateError[Nothing], CovariateName] =
    Either.cond(value.trim.nonEmpty, new CovariateName(value), CovariateError.BlankName(value))
  given Ordering[CovariateName] = Ordering.by(_.value)

/** The declared levels of an ordinal or categorical covariate: non-empty,
  * distinct and non-blank. For an ordinal covariate the order is the
  * declared order, lowest first; for a categorical one it is only the order
  * groups are listed in.
  */
final case class Levels private (values: Vector[String]) derives CanEqual:
  /** The position of `level` in the declared order. */
  def rank(level: String): Option[Int] = Option(values.indexOf(level)).filter(_ >= 0)
  def contains(level: String): Boolean = values.contains(level)

object Levels:
  def of(values: Vector[String]): Either[CovariateError[Nothing], Levels] =
    val repeated = values.diff(values.distinct).distinct
    if values.isEmpty || values.exists(_.trim.isEmpty) || repeated.nonEmpty then
      Left(CovariateError.InvalidLevels(values, repeated))
    else Right(new Levels(values))

/** The unit a numeric covariate is measured in, e.g. `rating` or `ms`. */
final case class NumericUnit private (symbol: String) derives CanEqual

object NumericUnit:
  def of(symbol: String): Either[CovariateError[Nothing], NumericUnit] =
    Either.cond(symbol.trim.nonEmpty, new NumericUnit(symbol), CovariateError.BlankUnit(symbol))

/** The declared type of a trial covariate. It fixes how a trials-table cell
  * is read and which filters and groupings apply: a numeric covariate is
  * compared and binned, an ordinal one is compared by rank, a categorical
  * one is matched by level, a binary one is true or false.
  */
enum CovariateType derives CanEqual:
  /** A finite real in `unit`; groups only through declared bins. */
  case Numeric(unit: NumericUnit)

  /** One of `levels`, ordered lowest first. */
  case Ordinal(levels: Levels)

  /** One of `levels`, unordered. */
  case Categorical(levels: Levels)

  /** True or false: written `1`/`0` or `true`/`false`. */
  case Binary

  def render: String = this match
    case Numeric(unit)       => s"numeric(${unit.symbol})"
    case Ordinal(levels)     => levels.values.mkString("ordinal(", " < ", ")")
    case Categorical(levels) => levels.values.mkString("categorical(", ", ", ")")
    case Binary              => "binary"

  /** Whether a trials-table column of `kind` can hold this covariate. */
  def admits(kind: AttributeKind): Boolean = (this, kind) match
    case (Numeric(_), AttributeKind.Number | AttributeKind.Integer)                => true
    case (Ordinal(_) | Categorical(_), AttributeKind.Text | AttributeKind.Integer) => true
    case (Binary, AttributeKind.Integer | AttributeKind.Text)                      => true
    case _                                                                         => false

  /** Read one typed attribute cell: a blank is not recorded, a value of
    * another type or outside the declared levels is unparsed.
    */
  def read(value: AttributeValue): Value[CovariateValue] =
    import Value.*
    val unparsed = Missing(Absence.Unparsed)
    (this, value) match
      case (_, AttributeValue.Blank)               => Missing(Absence.NotRecorded)
      case (Numeric(_), AttributeValue.Number(d))  => Present(CovariateValue.Number(d))
      case (Numeric(_), AttributeValue.Integer(l)) =>
        Present(CovariateValue.Number(l.toDouble))
      case (Ordinal(ls), AttributeValue.Text(s)) if ls.contains(s) =>
        Present(CovariateValue.Level(s))
      case (Categorical(ls), AttributeValue.Text(s)) if ls.contains(s) =>
        Present(CovariateValue.Level(s))
      case (Ordinal(ls), AttributeValue.Integer(l)) if ls.contains(l.toString) =>
        Present(CovariateValue.Level(l.toString))
      case (Categorical(ls), AttributeValue.Integer(l)) if ls.contains(l.toString) =>
        Present(CovariateValue.Level(l.toString))
      case (Binary, AttributeValue.Integer(0L)) | (Binary, AttributeValue.Text("0")) |
          (Binary, AttributeValue.Text("false")) =>
        Present(CovariateValue.Flag(false))
      case (Binary, AttributeValue.Integer(1L)) | (Binary, AttributeValue.Text("1")) |
          (Binary, AttributeValue.Text("true")) =>
        Present(CovariateValue.Flag(true))
      case _ => unparsed

object CovariateType:
  /** The text of an attribute cell, as a finding reports it. */
  def raw(value: AttributeValue): String = value match
    case AttributeValue.Text(s)    => s
    case AttributeValue.Integer(l) => l.toString
    case AttributeValue.Number(d)  => DecimalText.render(d)
    case AttributeValue.Blank      => ""

/** One read covariate value. */
enum CovariateValue derives CanEqual:
  case Number(value: Double)
  case Level(value: String)
  case Flag(value: Boolean)

/** One declared covariate. */
final case class Covariate(name: CovariateName, kind: CovariateType) derives CanEqual

/** The declared covariates of a report's trials: distinct names, each with
  * its type. The schema is what `eyes4s.covariate-schema@1` persists.
  */
final case class CovariateSchema private (covariates: Vector[Covariate]) derives CanEqual:
  def names: Vector[CovariateName]                    = covariates.map(_.name)
  def get(name: CovariateName): Option[CovariateType] =
    covariates.collectFirst { case Covariate(`name`, kind) => kind }

object CovariateSchema:
  val empty: CovariateSchema = new CovariateSchema(Vector.empty)

  def of(covariates: Vector[Covariate]): Either[CovariateError[Nothing], CovariateSchema] =
    val names    = covariates.map(_.name.value)
    val repeated = names.diff(names.distinct).distinct
    Either.cond(
      repeated.isEmpty,
      new CovariateSchema(covariates),
      CovariateError.DuplicateCovariate(repeated)
    )

/** The covariates of one trial: each declared covariate's value, and the
  * raw text of each cell that did not parse.
  */
final case class CovariateRow[+K] private[results] (
    key: K,
    values: Vector[(CovariateName, Value[CovariateValue])],
    unparsed: Vector[(CovariateName, String)]
) derives CanEqual:
  def value(name: CovariateName): Value[CovariateValue] =
    values.collectFirst { case (`name`, v) => v }.getOrElse(Value.Missing(Absence.NotRecorded))

/** Typed trial covariates joined to trials by key, never by row position.
  *
  * A key with no row, and a declared covariate a row does not carry, are
  * not recorded. Built from the admission ledger's trial inventory (the
  * UI-H trials importer) with [[CovariateTable.fromLedger]], or from keyed
  * attributes with [[CovariateTable.of]].
  */
final class CovariateTable[K] private (
    val schema: CovariateSchema,
    val rows: Vector[CovariateRow[K]]
):
  private lazy val index: Map[K, CovariateRow[K]] = rows.map(r => r.key -> r).toMap

  def row(key: K): Option[CovariateRow[K]] = index.get(key)

  def value(key: K, name: CovariateName): Value[CovariateValue] =
    index.get(key).fold(Value.Missing(Absence.NotRecorded))(_.value(name))

  override def equals(other: Any): Boolean = other match
    case that: CovariateTable[?] => schema == that.schema && rows == that.rows
    case _                       => false
  override def hashCode: Int = (schema, rows).hashCode

object CovariateTable:
  /** Read each key's attributes under `schema`. Keys must be distinct; a
    * declared covariate the attributes do not name is not recorded.
    */
  def of[K](
      schema: CovariateSchema,
      entries: Vector[(K, Attributes)]
  ): Either[CovariateError[K], CovariateTable[K]] =
    val keys = entries.map(_._1)
    keys.diff(keys.distinct).headOption match
      case Some(key) => Left(CovariateError.DuplicateKey(key))
      case None => Right(new CovariateTable(schema, entries.map((k, a) => read(schema, k, a))))

  /** The covariates of every inventory trial whose item is resolved, keyed
    * by its trial key. Every declared covariate must be an attribute column
    * of the inventory, of a kind that can hold it.
    */
  def fromLedger(
      schema: CovariateSchema,
      ledger: InventoryLedger
  ): Either[CovariateError[TrialKey], CovariateTable[TrialKey]] =
    declared(schema, ledger).flatMap { _ =>
      val entries = ledger.trials.flatMap(t =>
        t.item.flatMap(item => t.identity.withItem(item).toOption).map(_ -> t.attributes)
      )
      of(schema, entries)
    }

  /** The covariates of `keys`, each joined to the inventory trial of the
    * identity the study layout projects from it (participant, phase, trial
    * label and occurrence); a key whose trial the inventory does not declare
    * has no row. The layout must project a trial label.
    */
  def forKeys[K](
      schema: CovariateSchema,
      ledger: InventoryLedger,
      layout: StudyLayout[K],
      keys: Vector[K]
  ): Either[CovariateError[K], CovariateTable[K]] =
    TrialIdentity.projection(layout) match
      case None           => Left(CovariateError.NoTrialProjection(layout.id))
      case Some(identity) =>
        declared(schema, ledger).flatMap { _ =>
          of(
            schema,
            keys.distinct.flatMap(k => ledger.trial(identity(k)).map(t => k -> t.attributes))
          )
        }

  /** Every declared covariate is an attribute column of a kind that can hold it. */
  private def declared(
      schema: CovariateSchema,
      ledger: InventoryLedger
  ): Either[CovariateError[Nothing], Unit] =
    val columns = ledger.attributeColumns
    schema.covariates.iterator
      .map { c =>
        columns.find(_.name == c.name.value) match
          case None =>
            Some(CovariateError.UnknownAttribute(c.name.value, columns.map(_.name)))
          case Some(column) if !c.kind.admits(column.kind) =>
            Some(
              CovariateError.IncompatibleKind(c.name.value, c.kind.render, column.kind.toString)
            )
          case _ => None
      }
      .collectFirst { case Some(e) => e }
      .toLeft(())

  private def read[K](
      schema: CovariateSchema,
      key: K,
      attributes: Attributes
  ): CovariateRow[K] =
    val cells = schema.covariates.map { c =>
      val cell = attributes.get(c.name.value)
      (c.name, cell.fold(Value.Missing(Absence.NotRecorded))(c.kind.read), cell)
    }
    CovariateRow(
      key,
      cells.map((n, v, _) => n -> v),
      cells.collect { case (n, Value.Missing(Absence.Unparsed), Some(raw)) =>
        n -> CovariateType.raw(raw)
      }
    )

/** Why a covariate declaration or table was refused; every case names its
  * operands.
  */
enum CovariateError[+K] derives CanEqual:
  case BlankName(value: String)
  case BlankUnit(symbol: String)
  case InvalidLevels(levels: Vector[String], repeated: Vector[String])
  case DuplicateCovariate(names: Vector[String])
  case DuplicateKey(key: K)
  case UnknownAttribute(covariate: String, declared: Vector[String])
  case IncompatibleKind(covariate: String, declared: String, attribute: String)
  case NoTrialProjection(layout: DefinitionId)

  def message: String = this match
    case BlankName(value)  => s"A covariate name must not be blank, got '$value'."
    case BlankUnit(symbol) =>
      s"A numeric covariate needs a non-blank unit, got '$symbol'."
    case InvalidLevels(levels, repeated) =>
      s"Covariate levels $levels must be non-empty, non-blank and distinct; repeated: $repeated."
    case DuplicateCovariate(names) => s"Covariates $names are declared more than once."
    case DuplicateKey(key)         => s"Trial $key has more than one covariate row."
    case UnknownAttribute(covariate, declared) =>
      s"Covariate '$covariate' is not an attribute column of the trials table; declared: $declared."
    case IncompatibleKind(covariate, declared, attribute) =>
      s"Covariate '$covariate' is declared $declared, but its trials-table column holds $attribute."
    case NoTrialProjection(layout) =>
      s"Layout ${layout.name}@${layout.version} names no trial label, so covariates cannot be " +
        "joined to its keys."
