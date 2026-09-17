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

package eyes4s.design

import eyes4s.kernel.*

/** Spatial domain relevant to a score, retained independently of its source payload. */
final class EvaluationGeometry private (
    val unit: Option[(String, String)],
    val frame: Option[(FrameId, FrameSpec)],
    val grid: Option[(GridId, GridSpec)]
) derives CanEqual:
  /** Structural: a reconstructed geometry equals the one it was written from. */
  override def equals(other: Any): Boolean = other match
    case that: EvaluationGeometry =>
      unit == that.unit && frame == that.frame && grid == that.grid
    case _ => false
  override def hashCode: Int = (unit, frame, grid).hashCode

object EvaluationGeometry:
  val Independent: EvaluationGeometry = new EvaluationGeometry(None, None, None)
  def inFrame[U <: Unit2D](frame: Frame[U])(using unit: UnitLabel[U]): EvaluationGeometry =
    new EvaluationGeometry(Some(unit.symbol -> unit.name), Some(frame.id -> frame.spec), None)
  def onGrid[U <: Unit2D](grid: Grid[U])(using unit: UnitLabel[U]): EvaluationGeometry =
    new EvaluationGeometry(
      Some(unit.symbol   -> unit.name),
      Some(grid.frame.id -> grid.frame.spec),
      Some(grid.id       -> grid.spec)
    )

/** Whether score interpretation depends on ordering, elapsed time, or a shared clock. */
enum EvaluationTime derives CanEqual:
  case OrderFree
  case Ordered
  case RelativeMicroseconds
  case SharedClock(clock: ClockId)

/** Invalid method declarations are rejected before evaluation. */
enum EvaluationSpecError derives CanEqual:
  case EmptyField(field: String, value: String)
  case InvalidComponents(values: Vector[String])
  case InvalidParameters(values: Vector[(String, Provenance.Param)])

  def message: String = this match
    case EmptyField(field, value)  => s"Evaluation $field must be non-empty, got '$value'."
    case InvalidComponents(values) => s"Score components must be non-empty and unique: $values."
    case InvalidParameters(values) =>
      s"Parameters need unique non-empty names and finite numbers: $values."

/** Explicit method identity and conventions. Parameters are canonicalized by name.
  *
  * Authors declare every score-affecting parameter, including preprocessing.
  * Display names or rendered provenance are never used to infer this contract.
  */
final class EvaluationSpec private (
    val method: String,
    val revision: String,
    val parameters: Vector[(String, Provenance.Param)],
    val components: Vector[String],
    val geometry: EvaluationGeometry,
    val time: EvaluationTime
) derives CanEqual:
  /** Structural: a reconstructed specification equals the one it was written from. */
  override def equals(other: Any): Boolean = other match
    case that: EvaluationSpec =>
      method == that.method && revision == that.revision && parameters == that.parameters &&
      components == that.components && geometry == that.geometry && time == that.time
    case _ => false
  override def hashCode: Int =
    (method, revision, parameters, components, geometry, time).hashCode

  private[design] def steps: Vector[Provenance.Step] =
    import Provenance.Param.*
    val frame = geometry.frame.toVector.flatMap { case (id, spec) =>
      Vector(
        "frame" -> Text(id.name),
        "xMin"  -> Num(spec.xMin),
        "xMax"  -> Num(spec.xMax),
        "yMin"  -> Num(spec.yMin),
        "yMax"  -> Num(spec.yMax),
        "yAxis" -> Text(spec.yAxis.toString)
      )
    }
    val grid = geometry.grid.toVector.flatMap { case (id, spec) =>
      Vector(
        "grid" -> Text(id.name),
        "nx"   -> Num(spec.nx.toDouble),
        "ny"   -> Num(spec.ny.toDouble)
      )
    }
    val temporal = time match
      case EvaluationTime.SharedClock(clock) =>
        Vector("time" -> Text("SharedClock"), "clock" -> Text(clock.name))
      case other => Vector("time" -> Text(other.toString))
    Vector(
      Provenance.Step(
        "evaluationMethod",
        Vector("method" -> Text(method), "revision" -> Text(revision))
      ),
      Provenance.Step("evaluationParameters", parameters),
      Provenance.Step(
        "evaluationComponents",
        components.zipWithIndex.map { case (name, index) => index.toString -> Text(name) }
      ),
      Provenance.Step(
        "evaluationDomain",
        geometry.unit.toVector.flatMap { case (symbol, name) =>
          Vector("unit" -> Text(symbol), "unitName" -> Text(name))
        } ++ frame ++ grid ++ temporal
      )
    )

object EvaluationSpec:
  def of(
      method: String,
      revision: String,
      parameters: Vector[(String, Provenance.Param)],
      components: Vector[String],
      geometry: EvaluationGeometry,
      time: EvaluationTime
  ): Either[EvaluationSpecError, EvaluationSpec] =
    if method.trim.isEmpty then Left(EvaluationSpecError.EmptyField("method", method))
    else if revision.trim.isEmpty then
      Left(EvaluationSpecError.EmptyField("revision", revision))
    else if components.isEmpty || components.exists(
        _.trim.isEmpty
      ) || components.distinct.size != components.size
    then Left(EvaluationSpecError.InvalidComponents(components))
    else if parameters.map(_._1).distinct.size != parameters.size || parameters.exists {
        case (name, value) =>
          name.trim.isEmpty || (value match
            case Provenance.Param.Num(number) => !number.isFinite
            case _                            => false)
      }
    then Left(EvaluationSpecError.InvalidParameters(parameters))
    else
      Right(
        new EvaluationSpec(
          method,
          revision,
          parameters.sortBy(_._1),
          components,
          geometry,
          time
        )
      )
