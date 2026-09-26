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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.surface.*

enum PointBinEndpoint derives CanEqual:
  case HalfOpen, IncludeFinalEndpoint

enum PointSamplingError derives CanEqual:
  case Boundaries(values: Vector[Long])
  case Clock(error: TimeError)
  case Frames(error: GeometryError)
  case MissingTemplate(participant: String, stimulus: String)
  case AmbiguousTemplate(participant: String, stimulus: String, indices: Vector[Int])
  case DuplicateSource(indices: Vector[Int])
  case Density(templateIndex: Int, error: DensityLookupError)
  case Point(error: DensityPointFailure)
  case Insufficient(level: String, requested: Int, successful: Int, minimum: Int)
  case NonFinite(level: String, index: Int, value: Double)
  def message: String = this match
    case Boundaries(v) =>
      s"Point bins require at least two strictly increasing microsecond boundaries, got $v."
    case Clock(e)                         => e.message
    case Frames(e)                        => e.message
    case MissingTemplate(p, s)            => s"No template for participant=$p, stimulus=$s."
    case AmbiguousTemplate(p, s, indices) =>
      s"Template participant=$p, stimulus=$s is ambiguous at rows=$indices."
    case DuplicateSource(indices) => s"Source occurrence key is duplicated at rows=$indices."
    case Density(i, e)            => s"Template[$i]: ${e.message}"
    case Point(e)                 => e.message
    case Insufficient(l, n, k, m) =>
      s"Point-sampling $l has $k successful of $n requested values; minimum=$m."
    case NonFinite(l, i, v) => s"Point-sampling $l value[$i]=$v is nonfinite."

/** Exact absolute point bins; final endpoint admission is separate from short-bin policy. */
final class PointBins private (
    val clock: ClockId,
    val boundaries: Vector[Instant],
    val endpoint: PointBinEndpoint
):
  def size: Int                         = boundaries.size - 1
  def index(time: Instant): Option[Int] =
    val t = time.toMicros
    if t < boundaries.head.toMicros || t > boundaries.last.toMicros then None
    else if t == boundaries.last.toMicros then
      Option.when(endpoint == PointBinEndpoint.IncludeFinalEndpoint)(size - 1)
    else
      var low = 0; var high = boundaries.size
      while low < high do
        val middle = low + (high - low) / 2
        if boundaries(middle).toMicros <= t then low = middle + 1 else high = middle
      Some(low - 1)
object PointBins:
  def of(
      clock: ClockId,
      boundaries: Vector[Instant],
      endpoint: PointBinEndpoint
  ): Either[PointSamplingError, PointBins] =
    val values = boundaries.map(_.toMicros)
    if values.size < 2 || values.zip(values.drop(1)).exists((a, b) => a >= b) then
      Left(PointSamplingError.Boundaries(values))
    else Right(new PointBins(clock, boundaries, endpoint))

enum PointControlSelection derives CanEqual:
  case Disabled
  case Candidates(selection: Selection)

/** The supported aggregation is an arithmetic mean, with explicit failure policy at both levels. */
final class PointSamplingSpec private (
    val clock: ClockId,
    val queries: Vector[Instant],
    val bins: Option[PointBins],
    val normalization: DensityNormalization,
    val lookup: DensityLookupPolicy,
    val endpoint: TrajectoryEndpoint,
    val controls: PointControlSelection,
    val controlPolicy: FailurePolicy,
    val binPolicy: FailurePolicy
):
  def binIndices: Vector[Option[Int]] = queries.map(t => bins.flatMap(_.index(t)))
  def unbinned: Vector[Int]           = if bins.isEmpty then Vector.empty
  else binIndices.zipWithIndex.collect { case (None, i) => i }
object PointSamplingSpec:
  val method: String = "eyes4s.static-density-onset-sampling-arithmetic-mean/1"
  def of(
      clock: ClockId,
      queries: Vector[Instant],
      bins: Option[PointBins],
      normalization: DensityNormalization,
      lookup: DensityLookupPolicy,
      endpoint: TrajectoryEndpoint,
      controls: PointControlSelection,
      controlPolicy: FailurePolicy,
      binPolicy: FailurePolicy
  ): Either[PointSamplingError, PointSamplingSpec] =
    bins
      .fold[Either[PointSamplingError, Unit]](Right(()))(b =>
        Agreement.clocks(clock, b.clock).left.map(PointSamplingError.Clock.apply).map(_ => ())
      )
      .map(_ =>
        new PointSamplingSpec(
          clock,
          queries,
          bins,
          normalization,
          lookup,
          endpoint,
          controls,
          controlPolicy,
          binPolicy
        )
      )

final class PointMean private[plan] (
    val requested: Int,
    val successful: Int,
    val policy: FailurePolicy,
    val result: Either[PointSamplingError, Double]
):
  def contributing: Int = if result.isRight then successful else 0

object PointSamplingMean:
  /** Used first across selected controls at each time, then across time means in each bin. */
  def apply(
      level: String,
      values: Vector[Either[PointSamplingError, Double]],
      policy: FailurePolicy
  ): PointMean =
    val good    = values.flatMap(_.toOption).filter(_.isFinite)
    val minimum = policy match
      case FailurePolicy.RequireAll        => math.max(1, values.size)
      case FailurePolicy.SuccessfulOnly(n) => n.value
    val invalid = values.zipWithIndex.collectFirst {
      case (Right(v), i) if !v.isFinite => (v, i)
    }
    val result = invalid match
      case Some((v, i))                => Left(PointSamplingError.NonFinite(level, i, v))
      case None if good.size < minimum =>
        Left(PointSamplingError.Insufficient(level, values.size, good.size, minimum))
      case None =>
        val mean = good.map(_ / good.size).sum
        Either.cond(mean.isFinite, mean, PointSamplingError.NonFinite(level, good.size, mean))
    new PointMean(values.size, good.size, policy, result)

/** One control template: `template` is its key and `occurrence` the admitted source that
  * represents it, the matching occurrence with the smallest key digest. Each distinct template is
  * a single candidate.
  */
final case class PointControl[K](
    occurrence: K,
    template: K,
    values: Vector[Either[PointSamplingError, Double]]
)
final case class PointObservation[U <: Unit2D](
    index: Int,
    time: Instant,
    point: Option[Pt[U]],
    value: Either[PointSamplingError, Double],
    control: Option[PointMean],
    bin: Option[Int]
)
final case class PointBinResult(
    index: Int,
    from: Instant,
    until: Instant,
    includesFinalEndpoint: Boolean,
    queries: Vector[Int],
    observed: PointMean,
    control: Option[PointMean]
):
  def difference: Option[Either[PointSamplingError, Double]] = control.map { mean =>
    for
      a <- observed.result
      b <- mean.result
      difference = a - b
      result <- Either.cond(
        difference.isFinite,
        difference,
        PointSamplingError.NonFinite(s"bin[$index] difference", index, difference)
      )
    yield result
  }
final case class PointTrialResult[K, U <: Unit2D](
    index: Int,
    key: K,
    template: Either[PointSamplingError, K],
    eligibleControls: Int,
    controls: Vector[PointControl[K]],
    points: Vector[PointObservation[U]],
    bins: Vector[PointBinResult]
)
final class PointSamplingResult[K, U <: Unit2D] private[plan] (
    val rows: Vector[PointTrialResult[K, U]],
    val controlPairing: Option[PairingReport[K, K]],
    val unbinned: Vector[Int],
    val inputHash: ContentHash,
    val planHash: ContentHash,
    val provenance: Provenance
)

/** Static templates sampled along each focal path. Control candidates are the distinct templates
  * matched by at least one admitted source: each counts once however many sources match it, and is
  * represented by the matching source occurrence with the smallest key digest. All same-stimulus templates are excluded before
  * selection. Missing/ambiguous sources stay in result rows, and never become an arbitrary first
  * match.
  */
final class PointSamplingPlan[K, U <: Unit2D] private (
    val layout: StudyLayout[K],
    val source: StudyInput[K, U],
    val templates: Trials[K, Unit, Signed[U]],
    val spec: PointSamplingSpec,
    val inputHash: ContentHash,
    val planHash: ContentHash
):
  def run: PointSamplingResult[K, U] =
    given KeyDigest[K] = layout.digest
    val sources        = source.trials.rows
    val duplicates = sources.zipWithIndex.groupBy(_._1.key).view.mapValues(_.map(_._2)).toMap
    val resolved   = sources.map { row =>
      val p          = layout.participant(row.key); val s = layout.stimulus(row.key)
      val candidates = templates.rows.zipWithIndex.collect {
        case (t, i) if layout.participant(t.key) == p && layout.stimulus(t.key) == s => i
      }
      if duplicates(row.key).size > 1 then
        Left(PointSamplingError.DuplicateSource(duplicates(row.key)))
      else
        candidates match
          case Vector(i) => Right(i)
          case Vector()  => Left(PointSamplingError.MissingTemplate(p, s))
          case indices   => Left(PointSamplingError.AmbiguousTemplate(p, s, indices))
    }
    val admitted = Trials(
      sources.zip(resolved).collect { case (row, Right(index)) => Trial(row.key, (), index) }
    )
    // One candidate per distinct matched template, so a template matched by several sources is
    // neither counted nor drawn more than once. It is represented by the matching source
    // occurrence with the smallest key digest: independent of source order, and the occurrence
    // itself when only one source matches, so keyed selection is unchanged for such inputs.
    val candidates = Trials(
      admitted.rows
        .groupBy(_.value)
        .values
        .map(_.minBy(row => KeyDigest[K].digest(row.key).value))
        .toVector
        .sortBy(_.value)
    )
    val relation =
      Relation.sameOn(layout.participant).and(Relation.differentOn(layout.stimulus))
    val paired = spec.controls match
      case PointControlSelection.Disabled              => None
      case PointControlSelection.Candidates(selection) =>
        Some(pair(admitted, candidates, PairDesign.BetweenDirected(relation, selection)))
    val selected = paired.toVector.flatMap(_.pairs).groupBy(_._1.key)
    val prepared = templates.rows.zipWithIndex.map { (row, i) =>
      DensityLookup
        .prepare(row.value, spec.normalization)
        .left
        .map(PointSamplingError.Density(i, _))
    }
    val binIndices = spec.binIndices
    val results    = sources.zip(resolved).zipWithIndex.map { case ((row, target), rowIndex) =>
      val trajectory = FixationTrajectory
        .fromScanpath(row.value)
        .sample(spec.clock, spec.queries, spec.endpoint)
        .left
        .map(PointSamplingError.Clock.apply)
      def sample(
          index: Either[PointSamplingError, Int]
      ): Vector[Either[PointSamplingError, Double]] =
        val samples = for
          i      <- index
          field  <- prepared(i)
          path   <- trajectory
          points <- field.along(path, spec.lookup).left.map(PointSamplingError.Frames.apply)
        yield points.map(_.value.left.map(PointSamplingError.Point.apply))
        samples.fold(e => Vector.fill(spec.queries.size)(Left(e)), identity)
      val matched  = sample(target)
      val controls = selected.getOrElse(row.key, Vector.empty).map { (_, candidate) =>
        PointControl(
          candidate.key,
          templates.rows(candidate.value).key,
          sample(Right(candidate.value))
        )
      }
      val points = spec.queries.zipWithIndex.map { (time, i) =>
        val control = spec.controls match
          case PointControlSelection.Disabled      => None
          case PointControlSelection.Candidates(_) =>
            Some(
              PointSamplingMean(
                s"query[$i] controls",
                controls.map(_.values(i)),
                spec.controlPolicy
              )
            )
        val point = trajectory.toOption.flatMap(_.rows(i).location.toOption.map(_.point))
        PointObservation(i, time, point, matched(i), control, binIndices(i))
      }
      val bins = spec.bins.toVector.flatMap { bins =>
        (0 until bins.size).map { b =>
          val indices = binIndices.zipWithIndex.collect {
            case (Some(index), i) if index == b => i
          }
          val observed = PointSamplingMean(
            s"bin[$b] observed",
            indices.map(i => points(i).value),
            spec.binPolicy
          )
          val control = spec.controls match
            case PointControlSelection.Disabled      => None
            case PointControlSelection.Candidates(_) =>
              Some(
                PointSamplingMean(
                  s"bin[$b] control-time means",
                  indices.flatMap(i => points(i).control.map(_.result)),
                  spec.binPolicy
                )
              )
          PointBinResult(
            b,
            bins.boundaries(b),
            bins.boundaries(b + 1),
            b == bins.size - 1 && bins.endpoint == PointBinEndpoint.IncludeFinalEndpoint,
            indices,
            observed,
            control
          )
        }
      }
      val eligible = if paired.isEmpty || target.isLeft then 0
      else candidates.rows.count(t => relation.accepts(row.key, t.key))
      PointTrialResult(
        rowIndex,
        row.key,
        target.map(i => templates.rows(i).key),
        eligible,
        controls,
        points,
        bins
      )
    }
    val provenance = Provenance(
      inputHash,
      Vector(
        Provenance.Step(
          PointSamplingSpec.method,
          Vector(
            "plan"             -> Provenance.Param.Text(planHash.render),
            "sourceRows"       -> Provenance.Param.Num(results.size.toDouble),
            "queriesPerSource" -> Provenance.Param.Num(spec.queries.size.toDouble)
          )
        )
      )
    )
    new PointSamplingResult(
      results,
      paired.map(_.diagnostics),
      spec.unbinned,
      inputHash,
      planHash,
      provenance
    )

object PointSamplingPlan:
  /** Inputs are checked domain values; scientific matching/lookup failures remain per-source data. */
  def of[K, U <: Unit2D: UnitLabel](
      layout: StudyLayout[K],
      source: StudyInput[K, U],
      templates: Trials[K, Unit, Signed[U]],
      spec: PointSamplingSpec
  ): PointSamplingPlan[K, U] =
    val maps = templates.rows.map { row =>
      val g = row.value.grid; val f = g.frame
      ContentHash.combineAll(
        Vector(
          layout.digest.digest(row.key),
          ContentHash.of(row.value.values),
          row.value.provenance.digest,
          ContentHash.ofString(f.id.name),
          ContentHash.ofString(g.id.name),
          ContentHash.ofString(f.spec.yAxis.toString),
          ContentHash.of(
            IArray(
              f.spec.xMin,
              f.spec.xMax,
              f.spec.yMin,
              f.spec.yMax,
              g.nx.toDouble,
              g.ny.toDouble
            )
          )
        )
      )
    }
    val input = ContentHash.combineAll(
      Vector(
        source.hash,
        ContentHash.ofString(summon[UnitLabel[U]].symbol),
        ContentHash.combineAll(maps)
      )
    )
    def times(v: Vector[Instant]) =
      ContentHash.combineAll(v.map(t => ContentHash.ofString(t.toMicros.toString)))
    val bins = spec.bins.fold(ContentHash.ofString("no-bins"))(b =>
      ContentHash.combineAll(
        Vector(
          ContentHash.ofString(b.clock.name),
          ContentHash.ofString(b.endpoint.toString),
          times(b.boundaries)
        )
      )
    )
    val selection = spec.controls match
      case PointControlSelection.Disabled                                         => "disabled"
      case PointControlSelection.Candidates(Selection.All)                        => "all"
      case PointControlSelection.Candidates(Selection.BottomK(cap, seed, sample)) =>
        s"bottomK:${cap.value}:${seed.value}:${sample.value}"
    val parameters = Vector(
      PointSamplingSpec.method,
      layout.id.name,
      layout.id.version.toString,
      spec.clock.name,
      spec.normalization.toString,
      spec.lookup.toString,
      spec.endpoint.toString,
      selection,
      spec.controlPolicy.render,
      spec.binPolicy.render
    )
    val hash = ContentHash.combineAll(
      Vector(input, times(spec.queries), bins) ++ parameters.map(ContentHash.ofString)
    )
    new PointSamplingPlan(layout, source, templates, spec, input, hash)
