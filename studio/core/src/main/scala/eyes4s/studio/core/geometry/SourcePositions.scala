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

package eyes4s.studio.core.geometry

import eyes4s.codec.ByteDigest
import eyes4s.kernel.{Pt, Unit2D}
import eyes4s.studio.core.backend.{DatasetRevision, Phase, TrialKey}
import eyes4s.studio.core.document.{ColumnRole, DatasetRevisionSpec}
import eyes4s.studio.core.importing.{CsvSniffer, SniffedSource}

/** One fixation record's position exactly as its source holds it, in screen
  * pixels. It is never rewritten, whatever corrections are recorded.
  * `record` is the data record number (from 1, header excluded).
  */
final case class SourcePosition(record: Int, trial: TrialKey, x: Double, y: Double)
    derives CanEqual:
  def point: Pt[Unit2D.Px] = Pt(x, y)

/** A record whose position could not be read: returned as data, not dropped. */
final case class UnplacedRecord(record: Int, reason: String) derives CanEqual

/** The positions of a revision's fixation source, read under its mapping, in
  * record order, and the records whose position is not a finite number
  * (eyes4s rejects those at admission; here they are counted, never placed).
  */
final case class SourcePositions private (
    dataset: DatasetRevision,
    source: ByteDigest,
    positions: Vector[SourcePosition],
    unplaced: Vector[UnplacedRecord]
) derives CanEqual:
  def records: Int = positions.size + unplaced.size

  private lazy val byRecord: Map[Int, SourcePosition] = positions.map(p => p.record -> p).toMap

  def position(record: Int): Option[SourcePosition] = byRecord.get(record)

  /** Every trial with a readable record, in first-record order. */
  lazy val trials: Vector[TrialKey] = positions.map(_.trial).distinct

  lazy val byTrial: Map[TrialKey, Vector[SourcePosition]] = positions.groupBy(_.trial)

object SourcePositions:

  /** Read `bytes`, which must be `spec`'s fixation source, under its column
    * mapping. An unmapped phase reads as blank and an unmapped occurrence as
    * 1, as the mapping keys the trial.
    */
  def read(
      spec: DatasetRevisionSpec,
      bytes: IArray[Byte]
  ): Either[GeometryProblem, SourcePositions] =
    val id     = spec.id
    val source = spec.sources.fixations
    val digest = ByteDigest.sha256(bytes)
    val file   = source.fold("fixations")(_.path.value.split('/').last)

    def at(header: Vector[String], role: ColumnRole): Option[Either[GeometryProblem, Int]] =
      spec.mapping
        .column(role)
        .map(c =>
          Option(header.indexOf(c.value))
            .filter(_ >= 0)
            .toRight(GeometryProblem.MissingColumn(id, role, c))
        )
    def required(header: Vector[String], role: ColumnRole) =
      at(header, role).getOrElse(Left(GeometryProblem.Unmapped(id, role)))
    def optional(header: Vector[String], role: ColumnRole) =
      at(header, role).fold(Right(None))(_.map(Some(_)))

    for
      _ <- source.fold(Right(())) { s =>
        Either.cond(
          s.bytes == digest,
          (),
          GeometryProblem.NotTheSource(id, s.path.value, s.bytes.hex, digest.hex)
        )
      }
      text <- SniffedSource.decodeUtf8(file, bytes).left.map(GeometryProblem.Unreadable(id, _))
      head <- CsvSniffer.chooseDelimiter(file, text).left.map(GeometryProblem.Unreadable(id, _))
      rows <- CsvSniffer
        .records(file, text, head._1)
        .left
        .map(GeometryProblem.Unreadable(id, _))
      header = rows.headOption.getOrElse(Vector.empty)
      participant <- required(header, ColumnRole.Participant)
      trial       <- required(header, ColumnRole.Trial)
      x           <- required(header, ColumnRole.X)
      y           <- required(header, ColumnRole.Y)
      phase       <- optional(header, ColumnRole.Phase)
      occurrence  <- optional(header, ColumnRole.Occurrence)
    yield
      val placed   = Vector.newBuilder[SourcePosition]
      val unplaced = Vector.newBuilder[UnplacedRecord]
      rows.iterator.drop(1).zipWithIndex.foreach { (fields, i) =>
        def field(name: String, index: Int): Either[String, String] =
          fields.lift(index).toRight(s"it has ${fields.size} fields and no $name")
        def number(index: Int): Either[String, Double] =
          field(header(index), index).flatMap(t =>
            t.trim.toDoubleOption
              .filter(_.isFinite)
              .toRight(s"${header(index)} '$t' is not a finite number")
          )
        val read = for
          p <- field(header(participant), participant)
          t <- field(header(trial), trial)
          f <- phase.fold(Right(""))(ix => field(header(ix), ix))
          o <- occurrence.fold(Right(1))(ix =>
            field(header(ix), ix).flatMap(v =>
              v.trim.toIntOption.toRight(s"${header(ix)} '$v' is not a whole number")
            )
          )
          px <- number(x)
          py <- number(y)
        yield SourcePosition(i + 1, TrialKey(p, Phase(f), t, o), px, py)
        read match
          case Right(p)     => placed += p
          case Left(reason) => unplaced += UnplacedRecord(i + 1, reason)
      }
      new SourcePositions(id, digest, placed.result(), unplaced.result())
