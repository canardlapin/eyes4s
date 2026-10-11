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

package eyes4s.studio.core.scanpath

import cats.syntax.all.*
import eyes4s.compare.{BaselineScanpath, CompareOperand, ScanpathComparison}
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import io.circe.{Json, parser}

enum ScanpathStudyError derives CanEqual:
  case InvalidInput(operand: String, reason: String)
  case UnknownPhase(requested: String, available: Vector[String])
  def message: String = this match
    case InvalidInput(operand, reason)      => s"$operand: $reason"
    case UnknownPhase(requested, available) =>
      s"Query phase '$requested' is not among $available"

/** An explicit pair schedule over physical-screen scanpaths. Density bandwidths
  * and grid sizes have no meaning here. Short paths remain failed pair values.
  */
final class ScanpathStudy private (
    val request: Json,
    val phases: Vector[String],
    private val paths: Map[String, Either[String, Scanpath[Px]]],
    private val sources: Vector[ScanpathStudy.Source],
    private val schedule: Vector[ScanpathStudy.Pair]
):
  import ScanpathStudy.*
  def run(phase: String): Either[ScanpathStudyError, Json] =
    Either
      .cond(phases.contains(phase), (), ScanpathStudyError.UnknownPhase(phase, phases))
      .map { _ =>
        val selected = sources.filter(_.phase == phase)
        val ids      = selected.map(_.id).toSet
        val pairs    = schedule.filter(p => ids(p.source)).map { p =>
          val result = for
            left  <- paths(p.source)
            right <- paths(p.reference)
            a     <- BaselineScanpath.of(left, CompareOperand.Left).leftMap(_.message)
            b     <- BaselineScanpath.of(right, CompareOperand.Right).leftMap(_.message)
            score <- ScanpathComparison.baseline(a, b).leftMap(_.message)
          yield Vector(
            score.shape,
            score.direction,
            score.length,
            score.position,
            score.duration
          )
          p -> result
        }
        val grouped = pairs.groupMap(_._1.source)(identity)
        val queries = selected.map { s =>
          val own      = grouped(s.id)
          val failures = own.collect { case (p, Left(error)) =>
            p.json.deepMerge(Json.obj("error" -> Json.fromString(error)))
          }
          val values = if failures.nonEmpty then Json.Null
          else
            val matched  = own.collect { case (p, Right(v)) if p.kind == "matched" => v }.head
            val controls = own.collect { case (p, Right(v)) if p.kind == "control" => v }
            Json.obj(components.indices.map { i =>
              val baseline = controls.map(_(i)).sum / controls.size
              components(i) -> Json.obj(
                "observed"   -> Json.fromDoubleOrNull(matched(i)),
                "baseline"   -> Json.fromDoubleOrNull(baseline),
                "difference" -> Json.fromDoubleOrNull(matched(i) - baseline)
              )
            }*)
          Json.obj(
            "id"          -> Json.fromString(s.id),
            "participant" -> Json.fromString(s.participant),
            "phase"       -> Json.fromString(phase),
            "template"    -> Json.fromString(s.template),
            "controls"    -> Json.arr(
              own.filter(_._1.kind == "control").map(p => Json.fromString(p._1.reference))*
            ),
            "unavailable_pairs" -> Json.arr(failures*),
            "values"            -> values
          )
        }
        Json.obj(
          "phase"         -> Json.fromString(phase),
          "grouping"      -> Json.False,
          "normalization" -> Json.fromString("physical screen diagonal"),
          "policy"        -> Json.fromString("RequireAll; at least three fixations per path"),
          "queries"       -> Json.arr(queries*),
          "pairs"         -> Json.arr(pairs.map { (p, result) =>
            p.json.deepMerge(
              result.fold(
                e => Json.obj("error" -> Json.fromString(e)),
                v =>
                  Json.obj(
                    "values" -> Json
                      .obj(components.zip(v).map((k, x) => k -> Json.fromDoubleOrNull(x))*)
                  )
              )
            )
          }*)
        )
      }

object ScanpathStudy:
  val components: Vector[String] =
    Vector("shape", "direction", "length", "position", "duration")
  private final case class Source(
      id: String,
      participant: String,
      phase: String,
      template: String
  )
  private final case class Pair(source: String, reference: String, kind: String):
    def json: Json = Json.obj(
      "source"    -> Json.fromString(source),
      "reference" -> Json.fromString(reference),
      "kind"      -> Json.fromString(kind)
    )

  def parse(text: String): Either[ScanpathStudyError, ScanpathStudy] =
    parser
      .parse(text)
      .leftMap(e => ScanpathStudyError.InvalidInput("JSON request", e.message))
      .flatMap(read)

  def read(request: Json): Either[ScanpathStudyError, ScanpathStudy] =
    decode(request).leftMap(ScanpathStudyError.InvalidInput("Scanpath request", _))

  private def decode(request: Json): Either[String, ScanpathStudy] =
    val c                                                = request.hcursor
    def field[A: io.circe.Decoder](j: Json, key: String) =
      j.hcursor.get[A](key).leftMap(e => s"Field '$key': ${e.message}")
    def check(ok: Boolean, message: => String) = Either.cond(ok, (), message)
    def name(j: Json, key: String)             = field[String](j, key).flatMap(v =>
      check(v.trim.nonEmpty, s"Blank $key in scanpath item").as(v)
    )
    for
      version <- field[Int](request, "schema_version")
      _       <- check(version == 1, s"Unsupported scanpath request version $version")
      width   <- field[Int](request, "screen_width_px")
      height  <- field[Int](request, "screen_height_px")
      frame   <- Frame.screen("studio-scanpath-screen", width, height).leftMap(_.message)
      windowFields = Vector("x_min", "y_min", "x_max", "y_max")
      windowValues <- windowFields.traverse(k => c.get[Option[Double]](k).leftMap(_.toString))
      _            <- check(
        windowValues.forall(_.isEmpty) || windowValues.forall(_.nonEmpty),
        "A declared crop requires all four bounds"
      )
      window <-
        if windowValues.forall(_.isEmpty) then Right(None)
        else
          Bounds
            .of[Px](
              windowValues(0).get,
              windowValues(1).get,
              windowValues(2).get,
              windowValues(3).get
            )
            .leftMap(_.message)
            .map(Some(_))
      rate      <- field[Double](request, "sample_rate_hz")
      _         <- check(rate.isFinite && rate > 0, s"Invalid acquisition rate $rate")
      templates <- field[Vector[Json]](request, "templates")
      queries   <- field[Vector[Json]](request, "sources")
      _         <- check(
        templates.nonEmpty && queries.nonEmpty,
        "Both templates and queries are required"
      )
      owners <- (templates ++ queries).traverse(j =>
        (name(j, "id"), name(j, "participant")).tupled
      )
      _ <- check(owners.map(_._1).distinct.size == owners.size, "Duplicate scanpath identities")
      source <- queries.traverse(j =>
        (name(j, "id"), name(j, "participant"), name(j, "phase"), name(j, "template")).mapN(
          Source.apply
        )
      )
      tids <- templates.traverse(name(_, "id")).map(_.toSet)
      owner = owners.toMap
      _ <- source.traverse_(s =>
        check(
          tids(s.template) && owner.get(s.template).contains(s.participant),
          s"Query ${s.id} has no same-participant template ${s.template}"
        )
      )
      pool = source.groupMap(_.participant)(_.template).view.mapValues(_.distinct.sorted).toMap
      _ <- source.traverse_(s =>
        check(pool(s.participant).size >= 2, s"Query ${s.id} has no distinct control")
      )
      expected = source.flatMap(s =>
        Pair(s.id, s.template, "matched") +: pool(s.participant)
          .filterNot(_ == s.template)
          .map(Pair(s.id, _, "control"))
      )
      declared <- c.get[Option[Vector[Json]]]("pairs").leftMap(_.message).flatMap {
        case None       => Right(expected)
        case Some(rows) =>
          rows.traverse(j =>
            (name(j, "source"), name(j, "reference"), name(j, "kind")).mapN(Pair.apply)
          )
      }
      _ <- check(
        declared.size == expected.size && declared.toSet == expected.toSet,
        "Pair schedule differs from the exhaustive participant-specific matched/control design"
      )
      paths <- (templates ++ queries).traverse { j =>
        for
          id <- name(j, "id")
          f  <- field[Json](j, "fixations")
          xs <- field[Vector[Double]](f, "x")
          ys <- field[Vector[Double]](f, "y")
          ds <- field[Vector[Double]](f, "duration_ms")
          ts <- field[Vector[Double]](f, "onset_ms")
          _  <- check(
            Vector(xs.size, ys.size, ds.size, ts.size).distinct.size == 1,
            s"Fixation column lengths differ for $id"
          )
          _ <- check(
            (xs ++ ys ++ ds ++ ts).forall(_.isFinite) && ds.forall(_ > 0),
            s"Nonfinite fixation or nonpositive duration for $id"
          )
          _ <- check(
            xs.zip(ys).forall((x, y) => frame.bounds.contains(Pt[Px](x, y))),
            s"Fixation outside physical screen for $id"
          )
          _ <- check(
            window.forall(w => xs.zip(ys).forall((x, y) => w.contains(Pt[Px](x, y)))),
            s"Unaccounted fixation outside declared crop for $id"
          )
          _ <- check(
            (ds ++ ts).forall(v => math.abs(v) < Long.MaxValue.toDouble / 2000),
            s"Time out of microsecond range for $id"
          )
        yield
          val clock = ClockId("studio-scanpath-trial-relative")
          val path  = xs.indices.toVector
            .traverse { i =>
              val start    = math.round(ts(i) * 1000)
              val duration = math.round(ds(i) * 1000)
              val count    = math.max(
                1,
                math.min(Int.MaxValue.toDouble, math.ceil(duration * rate / 1000000.0)).toInt
              )
              for
                interval <- Interval
                  .of(clock, Instant.micros(start), Instant.micros(start + duration))
                  .leftMap(_.message)
                fixation <- Event.Fixation
                  .of(interval, Pt[Px](xs(i), ys(i)), 0, DispersionMethod.RmsRadius, count)
                  .leftMap(_.message)
              yield fixation
            }
            .flatMap(fs => Scanpath.of(frame, clock, IArray.from(fs)).leftMap(_.message))
          id -> path
      }
    yield new ScanpathStudy(
      request,
      source.map(_.phase).distinct.sorted,
      paths.toMap,
      source,
      declared
    )
