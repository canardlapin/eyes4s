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

package eyes4s.studio.core.document

import cats.syntax.all.*
import eyes4s.plan.AttributeColumn
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}

/** A trial inventory's column mapping (ticket S5.4; the "Trial metadata"
  * tab): the columns eyes4s `TrialColumns` reads the trial identity from
  * (participant, phase, trial and, optionally, occurrence), the optional item
  * and response, and every other column declared as an attribute (UI-H reads
  * only declared columns). eyes4s joins fixation records to the inventory's
  * trials by this identity; the mapping is what declares the inventory, so a
  * revision without one has no inventory to count absent trials against.
  *
  * Every offered role once, every required role present, no column in two
  * roles, no attribute column also a role's.
  */
final case class InventoryMapping private (
    bindings: Vector[ColumnBinding],
    attributes: DeclaredAttributes,
    displays: Option[DisplayColumns]
) derives CanEqual:
  def column(role: ColumnRole): Option[ColumnName] = bindings.find(_.role == role).map(_.column)

  /** The eyes4s attribute declarations of the inventory's attribute columns. */
  def coreAttributes: Vector[AttributeColumn] = attributes.core

object InventoryMapping:

  /** The trial identity eyes4s `TrialColumns` requires. */
  val required: Vector[ColumnRole] =
    Vector(ColumnRole.Participant, ColumnRole.Phase, ColumnRole.Trial)

  /** The roles a trial inventory offers; any other is read from fixations. */
  val offered: Vector[ColumnRole] =
    required ++ Vector(ColumnRole.Occurrence, ColumnRole.Item, ColumnRole.Response)

  def of(
      bindings: Vector[ColumnBinding],
      attributes: DeclaredAttributes
  ): Either[DocumentError, InventoryMapping] =
    for
      _ <- bindings
        .find(b => !offered.contains(b.role))
        .map(b => DocumentError.InventoryRoleNotRead(b.column.value, b.role))
        .toLeft(())
      _ <- ColumnRole.values.toVector.traverse_ { role =>
        val columns = bindings.filter(_.role == role).map(_.column.value)
        Either.cond(columns.size <= 1, (), DocumentError.DuplicateColumnRole(role, columns))
      }
      _ <- bindings.groupBy(_.column).toVector.sortBy(_._1.value).traverse_ { (column, bs) =>
        Either.cond(bs.size <= 1, (), DocumentError.SharedColumn(column.value, bs.map(_.role)))
      }
      missing = required.filterNot(r => bindings.exists(_.role == r))
      _ <- Either.cond(missing.isEmpty, (), DocumentError.MissingInventoryRoles(missing))
      _ <- attributes.bindings
        .collectFirst(
          Function.unlift(a =>
            bindings
              .find(_.column == a.column)
              .map(b => DocumentError.InventoryAttributeIsMapped(a.column.value, b.role))
          )
        )
        .toLeft(())
    yield new InventoryMapping(bindings.sortBy(_.role.ordinal), attributes, None)

  /** `mapping` reading each trial's display from `displays` (S5.7), or no
    * display (`None`): its columns are no role's (a column declared as an
    * attribute may also be read as a display), and the kind and the file
    * are two columns.
    */
  def withDisplays(
      mapping: InventoryMapping,
      displays: Option[DisplayColumns]
  ): Either[DocumentError, InventoryMapping] =
    displays
      .traverse_ { d =>
        for
          _ <- d.file
            .filter(_ == d.kind)
            .map(c => DocumentError.DisplayColumnsShared(c.value))
            .toLeft(())
          _ <- d.columns.traverse_ { c =>
            mapping.bindings
              .find(_.column == c)
              .map(b => DocumentError.DisplayColumnMapped(c.value, b.role.label))
              .toLeft(())
          }
        yield ()
      }
      .as(mapping.copy(displays = displays))

  /** `displays` is written only when the mapping reads them, so a mapping
    * without them is written as before S5.7, byte for byte.
    */
  given Encoder.AsObject[InventoryMapping] = Encoder.AsObject.instance { m =>
    val o =
      io.circe.JsonObject("bindings" -> m.bindings.asJson, "attributes" -> m.attributes.asJson)
    m.displays.fold(o)(d => o.add("displays", d.asJson))
  }

  given Decoder[InventoryMapping] = Decoder.instance { c =>
    for
      bindings   <- c.get[Vector[ColumnBinding]]("bindings")
      attributes <- c.get[DeclaredAttributes]("attributes")
      displays   <- c.get[Option[DisplayColumns]]("displays")
      mapping    <- of(bindings, attributes)
        .flatMap(withDisplays(_, displays))
        .left
        .map(e => io.circe.DecodingFailure(e.message, c.history))
    yield mapping
  }

/** The trial inventory's display columns (S5.7): the column naming each
  * trial's display kind, and the one naming its asset file, if any.
  */
final case class DisplayColumns(kind: ColumnName, file: Option[ColumnName])
    derives CanEqual,
      io.circe.Codec.AsObject:
  def columns: Vector[ColumnName] = kind +: file.toVector
