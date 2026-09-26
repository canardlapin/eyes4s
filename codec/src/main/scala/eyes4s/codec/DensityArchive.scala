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

package eyes4s.codec

import cats.syntax.all.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** Density persistence policy. Packed always means one float64 LE chunk per
  * participant, across scales; recomputation is the default.
  */
enum DensityStorage derives CanEqual:
  case Inline, Packed, Recomputable

object DensityArchiveDefinitions:
  val studyResultV2: DefinitionId = DefinitionId.builtIn("eyes4s.study-result", 2)

/** Storage-independent identity of one map and its complete saved geometry.
  * CanonicalDigest supplies SHA-256; cells use row-major float64 LE bytes.
  */
final class DensityDigest[U <: Unit2D] private (val canonical: CanonicalDigest[DensityView[U]]):
  def sha256: ByteDigest                       = canonical.sha256
  def hex: String                              = sha256.hex
  def sameAs(other: DensityDigest[U]): Boolean = canonical.sameAs(other.canonical)
  override def equals(other: Any): Boolean     = other match
    case that: DensityDigest[?] => sha256 == that.sha256
    case _                      => false
  override def hashCode: Int    = canonical.hashCode
  override def toString: String = canonical.display

object DensityDigest:
  given [U <: Unit2D]: CanEqual[DensityDigest[U], DensityDigest[U]] = CanEqual.derived
  def parse[U <: Unit2D](hex: String): Either[ByteDigestError, DensityDigest[U]] =
    CanonicalDigest.parse[DensityView[U]](hex).map(new DensityDigest(_))

  def of[U <: Unit2D](
      mass: Mass[U],
      description: Vector[(String, Vector[Provenance.Param])]
  ): Either[CodecError, DensityDigest[U]] =
    val bytes = new Array[Byte](mass.grid.size * 8)
    mass.values.iterator.zipWithIndex.foreach { (value, index) =>
      Bytes.putLong(bytes, index * 8, java.lang.Double.doubleToLongBits(value))
    }
    val digits = "0123456789abcdef"
    val cells  = new StringBuilder(bytes.length * 2)
    bytes.foreach { value =>
      cells.append(digits((value & 0xff) >>> 4))
      cells.append(digits(value & 0x0f))
    }
    CanonicalDigest
      .document[DensityView[U]](
        Json.obj(
          "format"   -> Json.fromString("eyes4s.density-view/1"),
          "geometry" -> Json.obj(
            "frame" -> Json.obj(
              "id"    -> Json.fromString(mass.grid.frame.id.name),
              "xMin"  -> Json.fromDoubleOrNull(mass.grid.frame.bounds.xMin),
              "yMin"  -> Json.fromDoubleOrNull(mass.grid.frame.bounds.yMin),
              "xMax"  -> Json.fromDoubleOrNull(mass.grid.frame.bounds.xMax),
              "yMax"  -> Json.fromDoubleOrNull(mass.grid.frame.bounds.yMax),
              "yAxis" -> Json.fromString(mass.grid.frame.yAxis.toString)
            ),
            "grid"    -> Json.fromString(mass.grid.id.name),
            "nx"      -> Json.fromInt(mass.grid.nx),
            "ny"      -> Json.fromInt(mass.grid.ny),
            "order"   -> Json.fromString("row-major-x-fastest"),
            "context" -> Json.arr(
              description.map((k, v) =>
                Json.obj("field" -> Json.fromString(k), "values" -> ResultWire.values(v))
              )*
            )
          ),
          "provenance"     -> ResultWire.provenance(mass.provenance),
          "cellsFloat64LE" -> Json.fromString(cells.result())
        )
      )
      .map(new DensityDigest(_))

/** A located refusal to read one archived density. */
enum DensityError[K]:
  case UnknownScale(scale: Int, count: Int)
  case UnknownKey(scale: Int, key: K)
  case AmbiguousKey(scale: Int, key: K, count: Int)
  case Failed(scale: Int, key: K, failure: StudyFailure[K])
  case MissingPayload(scale: Int, key: K, reference: PayloadRef)
  case Payload(scale: Int, key: K, underlying: PayloadError)
  case PayloadReference(scale: Int, key: K, expected: PayloadRef, actual: PayloadRef)
  case RecomputeUnavailable(scale: Int, key: K)
  case SourceMismatch(scale: Int, key: K, expected: String, actual: String)
  case DigestMismatch(scale: Int, key: K, expected: ByteDigest, actual: ByteDigest)
  case Decode(scale: Int, key: K, underlying: CodecError)
  case Geometry(underlying: InspectionError[K])
  case Materialize(underlying: CodecError)
  case UnknownRow(scale: Int, row: Int, count: Int)
  case RowKeyMismatch(scale: Int, row: Int, expected: K, actual: K)
  def message: String = this match
    case UnknownScale(s, n)      => s"Density scale $s is outside $n archived scales."
    case UnknownKey(s, k)        => s"Density scale=$s has no trial key=$k."
    case AmbiguousKey(s, k, n)   => s"Density scale=$s key=$k has $n occurrences."
    case Failed(s, k, e)         => s"Density scale=$s key=$k failed: ${e.message}"
    case MissingPayload(s, k, r) => s"Density scale=$s key=$k needs payload ${r.sha256.hex}."
    case Payload(s, k, e)        => s"Density scale=$s key=$k: ${e.message}"
    case PayloadReference(s, k, e, a) =>
      s"Density scale=$s key=$k requested payload $e but received $a."
    case RecomputeUnavailable(s, k) =>
      s"Density scale=$s key=$k needs its saved plan and input."
    case SourceMismatch(s, k, e, a) =>
      s"Density scale=$s key=$k expected source $e, received $a."
    case DigestMismatch(s, k, e, a) =>
      s"Density scale=$s key=$k expected SHA-256 ${e.hex}, recomputed ${a.hex}."
    case Decode(s, k, e)            => s"Density scale=$s key=$k: ${e.message}"
    case Geometry(e)                => e.message
    case Materialize(e)             => e.message
    case UnknownRow(s, r, n)        => s"Density scale=$s row=$r is outside $n input rows."
    case RowKeyMismatch(s, r, e, a) => s"Density scale=$s row=$r expected key=$e, found key=$a."

/** An immutable archive document. Storage and key metadata are parsed at
  * opening; it is not a validated StudyResult until materialize succeeds.
  * No provider is called while opening or creating a reader.
  * Recomputation callbacks receive (scale, input row index, full key).
  */
final class StudyResultArchive[K, U <: Unit2D, P, S, D] private[codec] (
    private[codec] val owner: DensityArchiveCodec[K, U, P, S, D],
    private[codec] val payload: Json,
    private[codec] val rows: Vector[Vector[(K, Json)]],
    private[codec] val identities: DocumentIdentities,
    val description: Vector[(String, Vector[Provenance.Param])],
    private[codec] val original: Boolean
)(using UnitLabel[U]):
  def densityReader(
      payloads: PayloadRef => Option[VerifiedPayload] = _ => None,
      recompute: Option[(Int, Int, K) => Either[DensityError[K], Mass[U]]] = None
  ): DensityReader[K, U] =
    new DensityReader(rows, identities, description, owner.result.keys, payloads, recompute)

  /** Reconstruct all maps and then run the existing completed-result gates. */
  def materialize(
      payloads: PayloadRef => Option[VerifiedPayload] = _ => None,
      recompute: Option[(Int, Int, K) => Either[DensityError[K], Mass[U]]] = None
  ): Either[DensityError[K], StudyResult[K, U, S, D]] =
    val reader = densityReader(payloads, recompute)
    rows.zipWithIndex
      .traverse { (entries, scale) =>
        entries.zipWithIndex.traverse { case ((_, outcome), index) =>
          if outcome.hcursor.get[String]("kind").contains("failure") then Right(outcome)
          else reader.massAt(scale, index).map(DensityWire.inline)
        }
      }
      .flatMap { outcomes =>
        val restored = DensityWire.replaceOutcomes(payload, outcomes)
        owner.result.codec
          .decode(Json.obj("schema" -> Wire.id(owner.result.schema), "value" -> restored))
          .left
          .map(DensityError.Materialize(_))
      }

/** Verified, lazy access to an individual map. Providers are pure injected
  * functions; file access and any verified-chunk cache belong to io/fs2.
  * Duplicate full keys are explicitly ambiguous.
  */
final class DensityReader[K, U <: Unit2D] private[codec] (
    rows: Vector[Vector[(K, Json)]],
    identities: DocumentIdentities,
    description: Vector[(String, Vector[Provenance.Param])],
    keys: VersionedCodec[K],
    payloads: PayloadRef => Option[VerifiedPayload],
    recompute: Option[(Int, Int, K) => Either[DensityError[K], Mass[U]]]
)(using UnitLabel[U]):
  def density(scale: Int, key: K): Either[DensityError[K], DensityView[U]] =
    for
      entries <- rows.lift(scale).toRight(DensityError.UnknownScale(scale, rows.size))
      matches = entries.zipWithIndex.collect { case ((k, _), i) if k == key => i }
      index <- matches match
        case Vector(i) => Right(i)
        case Vector()  => Left(DensityError.UnknownKey(scale, key))
        case many      => Left(DensityError.AmbiguousKey(scale, key, many.size))
      mass <- massAt(scale, index)
      view <- DensityView
        .of(mass, description, ResultRef.Estimation(scale, key))
        .left
        .map(DensityError.Geometry(_))
    yield view

  private[codec] def massAt(scale: Int, index: Int): Either[DensityError[K], Mass[U]] =
    val (key, outcome)                                                   = rows(scale)(index)
    def decoded[A](e: Either[CodecError, A]): Either[DensityError[K], A] =
      e.left.map(DensityError.Decode(scale, key, _))
    if outcome.hcursor.get[String]("kind").contains("failure") then
      return decoded(
        Wire.field[Json](outcome, "failure").flatMap(ResultWire.readStudyFailure(keys))
      )
        .flatMap(f => Left(DensityError.Failed(scale, key, f)))
    for
      grid <- decoded(
        Wire.field[String](outcome, "grid").flatMap(n => identities.grid[U](GridId(n)))
      )
      provenance <- decoded(
        Wire.field[Json](outcome, "provenance").flatMap(ResultWire.readProvenance)
      )
      _ <- DensityGeometry
        .of(grid, description, ResultRef.Estimation(scale, key))
        .left
        .map(DensityError.Geometry(_))
      storage = outcome.hcursor.downField("storage").focus
      kind <- decoded(storage.fold(Right("inline"))(Wire.field[String](_, "kind")))
      mass <- kind match
        case "inline" =>
          for
            values <- decoded(Wire.field[Vector[Double]](storage.getOrElse(outcome), "values"))
            mass   <- decoded(
              Surface
                .mass(grid, IArray.from(values), provenance)
                .left
                .map(e => CodecError.Field("values", outcome, e.message))
            )
          yield mass
        case "packed" =>
          for
            source <- decoded(
              storage.toRight(CodecError.Field("storage", outcome, "missing packed storage"))
            )
            reference <- decoded(Wire.field[Json](source, "chunk").flatMap(PayloadRef.read))
            row       <- decoded(Wire.field[Int](source, "row"))
            _         <- decoded(
              Either.cond(
                reference.layout.element == ElementKind.Float64 &&
                  reference.layout.order == ArrayOrder.RowMajor &&
                  reference.layout.shape.size == 3 &&
                  reference.layout.shape.drop(1) == Vector(grid.ny, grid.nx) &&
                  row >= 0 && row < reference.layout.shape.head,
                (),
                CodecError.Field(
                  "chunk",
                  source,
                  s"expected float64 [maps,${grid.ny},${grid.nx}] row=$row"
                )
              )
            )
            chunk <- payloads(reference).toRight(
              DensityError.MissingPayload(scale, key, reference)
            )
            _ <- Either.cond(
              chunk.ref == reference,
              (),
              DensityError.PayloadReference(scale, key, reference, chunk.ref)
            )
            // VerifiedPayload owns checked bytes. Only this map is decoded.
            cells = IArray.tabulate(grid.size)(i =>
              java.lang.Double.longBitsToDouble(
                Bytes.getLong(chunk.bytes, (row * grid.size + i) * 8)
              )
            )
            mass <- decoded(
              Surface
                .mass(grid, cells, provenance)
                .left
                .map(e => CodecError.Field("values", source, e.message))
            )
          yield mass
        case "recomputable" =>
          recompute
            .toRight(DensityError.RecomputeUnavailable(scale, key))
            .flatMap(_(scale, index, key))
        case other =>
          decoded(Left(CodecError.Field("storage.kind", outcome, s"unknown $other")))
      _ <- decoded(
        Agreement
          .grids(grid, mass.grid)
          .left
          .map(e => CodecError.Field("grid", outcome, e.message))
      )
      _ <- decoded(
        Either.cond(
          provenance == mass.provenance,
          (),
          CodecError.Field(
            "provenance",
            outcome,
            "recomputed provenance differs from the stored map"
          )
        )
      )
      _ <- storage match
        case None                        => Right(())
        case Some(_) if kind == "inline" => Right(())
        case Some(source)                =>
          for
            expected <- decoded(
              Wire
                .field[String](source, "digest")
                .flatMap(hex =>
                  ByteDigest
                    .parse(hex)
                    .left
                    .map(e => CodecError.Field("digest", source, e.message))
                )
            )
            actual <- decoded(DensityWire.digest(mass, description))
            _      <- Either.cond(
              actual == expected,
              (),
              DensityError.DigestMismatch(scale, key, expected, actual)
            )
          yield ()
    yield mass

/** Encoded document plus participant chunks. Recomputable/inline archives
  * produce no chunks. Chunk values are already verified, immutable payloads.
  */
final class DensityArchiveBundle[K, U <: Unit2D, P, S, D] private[codec] (
    val archive: StudyResultArchive[K, U, P, S, D],
    val chunks: Vector[(String, VerifiedPayload)]
)

/** Storage-aware study-result codec. Existing inline archives keep their v1
  * bytes; v2 is an archive description, whose materialization reuses the v1
  * reconstruction checks. Opening never estimates a map or reads a chunk.
  */
final class DensityArchiveCodec[K, U <: Unit2D, P, S, D](
    val result: StudyResultCodec[K, U, P, S, D]
)(using unit: UnitLabel[U]):
  val ladder: SchemaLadder[StudyResultArchive[K, U, P, S, D]] =
    SchemaLadder
      .of[StudyResultArchive[K, U, P, S, D]]("study result densities", result.schema)(archive =>
        Either.cond(
          archive.original,
          archive.payload,
          CodecError.Unsupported("storage", "version 1 requires inline densities")
        )
      )(read(_, true))
      .next(_.original, DensityWire.lift)(archive =>
        Right(if archive.original then DensityWire.lift(archive.payload) else archive.payload)
      )(read(_, false))
  val codec: VersionedCodec[StudyResultArchive[K, U, P, S, D]] = ladder.codec

  def encode(
      value: StudyResult[K, U, S, D],
      storage: DensityStorage = DensityStorage.Recomputable
  ): Either[CodecError, DensityArchiveBundle[K, U, P, S, D]] =
    for
      document <- result.codec.encode(value)
      payload  <- Wire.field[Json](document, "value")
      maps = value.scales.zipWithIndex.flatMap { (scale, s) =>
        scale.estimation.zipWithIndex.collect { case ((key, Right(mass)), i) =>
          (s, i, key, mass)
        }
      }
      chunks <-
        if storage != DensityStorage.Packed then Right(Vector.empty[(String, VerifiedPayload)])
        else
          maps.groupBy(m => result.layout.participant(m._3)).toVector.sortBy(_._1).traverse {
            (participant, members) =>
              val first = members.head._4.grid
              for
                _ <- members.traverse_(m =>
                  Agreement
                    .grids(first, m._4.grid)
                    .left
                    .map(e => CodecError.Field("chunk.grid", payload, e.message))
                )
                shape <- PayloadLayout
                  .of(
                    ElementKind.Float64,
                    Vector(members.size, first.ny, first.nx),
                    ArrayOrder.RowMajor
                  )
                  .left
                  .map(CodecError.Payload("density chunk", _))
                packed <- PackedArrays
                  .pack(shape, IArray.from(members.flatMap(_._4.values)))
                  .left
                  .map(CodecError.Payload("density chunk", _))
              yield participant -> packed
          }
      positions = maps
        .groupBy(m => result.layout.participant(m._3))
        .values
        .toVector
        .flatMap(
          _.zipWithIndex.map { (m, row) => (m._1, m._2) -> row }
        )
        .toMap
      outcomes <- value.scales.zipWithIndex.traverse { (scale, s) =>
        scale.estimation.zipWithIndex.traverse { case ((key, outcome), i) =>
          outcome match
            case Left(failure) =>
              ResultWire
                .studyFailure(result.keys)(failure)
                .map(f => ResultWire.tagged("failure", "failure" -> f))
            case Right(mass) =>
              if storage == DensityStorage.Inline then Right(DensityWire.inline(mass))
              else
                for
                  digest   <- DensityWire.digest(mass, value.description)
                  location <- storage match
                    case DensityStorage.Recomputable =>
                      Right(
                        Json.obj(
                          "kind"   -> Json.fromString("recomputable"),
                          "digest" -> Json.fromString(digest.hex)
                        )
                      )
                    case DensityStorage.Packed =>
                      chunks
                        .find(_._1 == result.layout.participant(key))
                        .toRight(
                          CodecError.Unsupported(
                            "chunk",
                            s"missing participant ${result.layout.participant(key)}"
                          )
                        )
                        .map { (_, chunk) =>
                          Json.obj(
                            "kind"   -> Json.fromString("packed"),
                            "digest" -> Json.fromString(digest.hex),
                            "chunk"  -> PayloadRef.json(chunk.ref),
                            "row"    -> Json.fromInt(positions((s, i)))
                          )
                        }
                    case DensityStorage.Inline =>
                      Left(CodecError.Unsupported("storage", "unexpected inline route"))
                yield DensityWire
                  .inline(mass)
                  .mapObject(_.remove("values").add("storage", location))
        }
      }
      saved = DensityWire.replaceOutcomes(payload, outcomes)
      archive <- read(saved, storage == DensityStorage.Inline)
    yield new DensityArchiveBundle(archive, chunks)

  /** A recomputer tied to this result codec's method, a saved plan and input.
    * Each call estimates one trial only, through the ordinary estimator seam.
    * The row index is retained even when multiple input rows share a full key.
    */
  def recomputer(
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      archive: StudyResultArchive[K, U, P, S, D]
  ): (Int, Int, K) => Either[DensityError[K], Mass[U]] = (scale, row, key) =>
    for
      _ <- Either.cond(
        plan.description == archive.description,
        (),
        DensityError.SourceMismatch(
          scale,
          key,
          archive.description.toString,
          plan.description.toString
        )
      )
      _ <- Either.cond(
        plan.input == input.reference,
        (),
        DensityError.SourceMismatch(scale, key, plan.input.digest, input.reference.digest)
      )
      estimate <- plan.estimates
        .lift(scale)
        .toRight(DensityError.UnknownScale(scale, plan.estimates.size))
      trial <- input.trials.rows
        .lift(row)
        .toRight(DensityError.UnknownRow(scale, row, input.trials.rows.size))
      _ <- Either.cond(
        trial.key == key,
        (),
        DensityError.RowKeyMismatch(scale, row, key, trial.key)
      )
      mass <- StudyDensity
        .estimate(plan, estimate, key, trial.value)
        .left
        .map(DensityError.Failed(scale, key, _))
    yield mass

  private def read(
      payload: Json,
      original: Boolean
  ): Either[CodecError, StudyResultArchive[K, U, P, S, D]] =
    for
      _      <- Wire.requireId(payload, "layout", result.layout.id)
      _      <- Wire.requireId(payload, "keySchema", result.keys.schema)
      _      <- Wire.requireId(payload, "method", result.method.id)
      _      <- Wire.requireId(payload, "scoreSchema", result.scores.schema)
      _      <- Wire.requireId(payload, "differenceSchema", result.differences.schema)
      symbol <- Wire.field[String](payload, "unit")
      _      <- Either.cond(
        symbol == unit.symbol,
        (),
        CodecError.Field("unit", payload, s"expected ${unit.symbol}")
      )
      identities  <- Wire.field[Json](payload, "identities").flatMap(DocumentIdentities.read)
      fields      <- Wire.field[Vector[Json]](payload, "description")
      description <- fields.traverse(j =>
        for
          name   <- Wire.field[String](j, "field")
          values <- ResultWire.readValues(j, "values")
        yield name -> values
      )
      scales <- Wire.field[Vector[Json]](payload, "scales")
      _      <- scales.zipWithIndex.traverse_ { (scale, index) =>
        for
          estimate <- Wire.field[Json](scale, "estimate").flatMap(StudyWire.readEstimate[U])
          expected = estimate.parameters.flatMap((name, value) =>
            Vector(Provenance.Param.Text(name), value)
          )
          declared = description.collect {
            case (name, values) if name == s"estimate.$index" => values
          }
          _ <- Either.cond(
            declared == Vector(expected),
            (),
            CodecError.Field(
              s"scales[$index].estimate",
              scale,
              "estimate differs from the saved plan description"
            )
          )
        yield ()
      }
      rows <- scales.traverse(scale =>
        Wire
          .field[Vector[Json]](scale, "estimation")
          .flatMap(_.traverse { row =>
            for
              key     <- Wire.field[Json](row, "key").flatMap(result.keys.decode)
              outcome <- Wire.field[Json](row, "outcome")
              kind    <- ResultWire.kind(outcome)
              _       <- kind match
                case "failure" =>
                  Wire
                    .field[Json](outcome, "failure")
                    .flatMap(ResultWire.readStudyFailure(result.keys))
                    .void
                case "mass" =>
                  for
                    grid <- Wire
                      .field[String](outcome, "grid")
                      .flatMap(n => identities.grid[U](GridId(n)))
                    _ <- Wire
                      .field[Json](outcome, "provenance")
                      .flatMap(ResultWire.readProvenance)
                    _ <-
                      if original then
                        Either.cond(
                          outcome.hcursor.downField("storage").focus.isEmpty &&
                            outcome.hcursor.downField("values").focus.exists(_.isArray),
                          (),
                          CodecError
                            .Field("values", outcome, "version 1 requires inline values")
                        )
                      else DensityWire.checkStorage(outcome, grid)
                  yield ()
                case other =>
                  Left(CodecError.Field("kind", outcome, s"unknown density outcome $other"))
            yield key -> outcome
          })
      )
    yield
      val inlineOnly = rows.flatten.forall { (_, outcome) =>
        outcome.hcursor.get[String]("kind").contains("failure") ||
        outcome.hcursor
          .downField("storage")
          .focus
          .forall(_.hcursor.get[String]("kind").contains("inline"))
      }
      val normalized = if !inlineOnly then rows
      else
        rows.map(_.map { (key, outcome) =>
          val unwrapped = outcome.hcursor
            .downField("storage")
            .focus
            .fold(outcome)(storage =>
              ResultWire.tagged(
                "mass",
                "grid"       -> outcome.hcursor.downField("grid").focus.getOrElse(Json.Null),
                "values"     -> storage.hcursor.downField("values").focus.getOrElse(Json.Null),
                "provenance" -> outcome.hcursor
                  .downField("provenance")
                  .focus
                  .getOrElse(Json.Null)
              )
            )
          key -> unwrapped
        })
      val saved = if inlineOnly then
        DensityWire.replaceOutcomes(payload, normalized.map(_.map(_._2)))
      else payload
      new StudyResultArchive(this, saved, normalized, identities, description, inlineOnly)

private[codec] object DensityWire:
  def inline[U <: Unit2D](mass: Mass[U]): Json = ResultWire.tagged(
    "mass",
    "grid"       -> Json.fromString(mass.grid.id.name),
    "values"     -> Json.arr(mass.values.toVector.map(Json.fromDoubleOrNull)*),
    "provenance" -> ResultWire.provenance(mass.provenance)
  )

  def digest[U <: Unit2D](
      mass: Mass[U],
      description: Vector[(String, Vector[Provenance.Param])]
  ): Either[CodecError, ByteDigest] =
    DensityDigest.of(mass, description).map(_.sha256)

  def replaceOutcomes(payload: Json, outcomes: Vector[Vector[Json]]): Json =
    payload.mapObject(
      _.add(
        "scales",
        Json.arr(
          payload.hcursor
            .get[Vector[Json]]("scales")
            .getOrElse(Vector.empty)
            .zip(outcomes)
            .map { (scale, values) =>
              scale.mapObject(
                _.add(
                  "estimation",
                  Json.arr(
                    scale.hcursor
                      .get[Vector[Json]]("estimation")
                      .getOrElse(Vector.empty)
                      .zip(values)
                      .map((row, outcome) => row.mapObject(_.add("outcome", outcome)))*
                  )
                )
              )
            }*
        )
      )
    )

  def lift(payload: Json): Json =
    payload.mapObject(
      _.add(
        "scales",
        Json.arr(
          payload.hcursor.get[Vector[Json]]("scales").getOrElse(Vector.empty).map { scale =>
            scale.mapObject(
              _.add(
                "estimation",
                Json.arr(
                  scale.hcursor.get[Vector[Json]]("estimation").getOrElse(Vector.empty).map {
                    row =>
                      row.mapObject(
                        _.add(
                          "outcome",
                          row.hcursor.downField("outcome").focus.fold(Json.Null) { outcome =>
                            if outcome.hcursor.get[String]("kind").contains("mass") then
                              outcome.mapObject(o =>
                                o.remove("values")
                                  .add(
                                    "storage",
                                    Json.obj(
                                      "kind"   -> Json.fromString("inline"),
                                      "values" -> o("values").getOrElse(Json.Null)
                                    )
                                  )
                              )
                            else outcome
                          }
                        )
                      )
                  }*
                )
              )
            )
          }*
        )
      )
    )

  def checkStorage[U <: Unit2D](outcome: Json, grid: Grid[U]): Either[CodecError, Unit] =
    for
      storage <- Wire.field[Json](outcome, "storage")
      kind    <- Wire.field[String](storage, "kind")
      _       <- kind match
        case "inline" =>
          Either.cond(
            storage.hcursor.downField("values").focus.exists(_.isArray),
            (),
            CodecError.Field("values", storage, "expected an array")
          )
        case "packed" | "recomputable" =>
          for
            digest <- Wire.field[String](storage, "digest")
            _      <- ByteDigest
              .parse(digest)
              .left
              .map(e => CodecError.Field("digest", storage, e.message))
            _ <-
              if kind != "packed" then Right(())
              else
                for
                  ref <- Wire.field[Json](storage, "chunk").flatMap(PayloadRef.read)
                  row <- Wire.field[Int](storage, "row")
                  _   <- Either.cond(
                    ref.layout.element == ElementKind.Float64 && ref.layout.order == ArrayOrder.RowMajor &&
                      ref.layout.shape.size == 3 && ref.layout.shape
                        .drop(1) == Vector(grid.ny, grid.nx) &&
                      row >= 0 && row < ref.layout.shape.head,
                    (),
                    CodecError
                      .Field("chunk", storage, s"invalid float64 row-major map row $row")
                  )
                yield ()
          yield ()
        case other => Left(CodecError.Field("storage.kind", storage, s"unknown $other"))
    yield ()
