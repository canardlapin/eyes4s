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

import eyes4s.aoi.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.surface.EstimateError

/** Construction helpers shared by every projection. Operand names are taken
  * from the error case's own field names, so a projection supplies values in
  * declaration order and cannot rename a field.
  */
private[eyes4s] object DiagnosticSupport:
  /** A diagnostic awaiting its operand values, in the case's field order. */
  final class Pending[K](
      family: DiagnosticFamily,
      error: scala.reflect.Enum,
      message: String,
      subject: Vector[Locus[K]]
  ):
    /** Fails on a projection defect: every field needs exactly one value.
      * DiagnosticCatalogSuite projects a sample of every case, so a defect
      * cannot reach a release.
      */
    def apply(values: Operand[K]*): Diagnostic[K] =
      val names = error.productElementNames.toVector
      require(
        names.size == values.size,
        s"${family.name} projection of ${error.productPrefix} gives ${values.size} values " +
          s"for fields $names"
      )
      Diagnostic(
        family.code(error.ordinal),
        family.severity,
        subject,
        names.zip(values),
        Vector.empty,
        message
      )

  def diagnostic[K](
      family: DiagnosticFamily,
      error: scala.reflect.Enum,
      message: String,
      subject: Vector[Locus[K]] = Vector.empty
  ): Pending[K] = new Pending(family, error, message, subject)

  def int(value: Int): Operand[Nothing]               = Operand.Integer(BigInt(value))
  def long(value: Long): Operand[Nothing]             = Operand.Integer(BigInt(value))
  def ints(values: Vector[Int]): Operand[Nothing]     = Operand.Integers(values.map(BigInt(_)))
  def real(value: Double): Operand[Nothing]           = Operand.Real(ExactDouble(value))
  def name(value: String): Operand[Nothing]           = Operand.Name(value)
  def names(values: Vector[String]): Operand[Nothing] = Operand.Names(values)
  def text(value: String): Operand[Nothing]           = Operand.Text(value)
  def token(value: String): Operand[Nothing]          = Operand.Token(value)
  def definition(id: DefinitionId): Operand[Nothing]  = Operand.Definition(id)
  def artifact(digest: String): Operand[Nothing]      = Operand.Artifact(digest)
  def span(value: Span): Operand[Nothing]             = Operand.Micros(value.toMicros)
  def instant(value: Instant): Operand[Nothing]       = Operand.Micros(value.toMicros)
  def lineage(value: Provenance): Operand[Nothing]    = Operand.Lineage(value)
  def params(values: Vector[Provenance.Param]): Operand[Nothing] = Operand.Params(values)
  def parameters(values: Vector[(String, Provenance.Param)]): Operand[Nothing] =
    Operand.Parameters(values)
  def key[K](value: K): Operand[K]                         = Operand.Key(value)
  def keys[K](values: Vector[K]): Operand[K]               = Operand.Keys(values)
  def cause[K](value: Diagnostic[K]): Operand[K]           = Operand.Cause(value)
  def optional[K](value: Option[Operand[K]]): Operand[K]   = value.getOrElse(Operand.Absent)
  def fields[K](values: (String, Operand[K])*): Operand[K] = Operand.Fields(values.toVector)

  def fieldId(id: FieldId): Operand[Nothing]  = name(id.value)
  def endpoint(e: Endpoint): Operand[Nothing] =
    fields("closed" -> token(e.isClosed.toString), "value" -> real(e.value))
  def numericBounds(b: NumericBounds): Operand[Nothing] =
    fields(
      "lower" -> optional(b.lower.map(endpoint)),
      "upper" -> optional(b.upper.map(endpoint))
    )
  def rawValue(raw: RawValue): Operand[Nothing] =
    fields("case" -> token(raw.productPrefix), "value" -> text(FieldChecks.show(raw)))
  def frame(id: FrameId): Operand[Nothing]              = name(id.name)
  def clock(id: ClockId): Operand[Nothing]              = name(id.name)
  def grid(id: GridId): Operand[Nothing]                = name(id.name)
  def recordingRef(ref: RecordingRef): Operand[Nothing] = name(ref.value)
  def detector(ref: DetectorRef): Operand[Nothing]      =
    fields("name" -> name(ref.name), "version" -> name(ref.version))
  def interval(value: Interval): Operand[Nothing] = fields(
    "clock"  -> clock(value.clock),
    "onset"  -> instant(value.onset),
    "offset" -> instant(value.offset)
  )
  def intervals(values: Vector[Interval]): Operand[Nothing] =
    Operand.Items(values.map(interval))
  def range(value: SampleRange): Operand[Nothing] =
    fields("from" -> int(value.from), "until" -> int(value.until))
  def frameSpec(spec: FrameSpec): Operand[Nothing] = fields(
    "xMin"  -> real(spec.xMin),
    "yMin"  -> real(spec.yMin),
    "xMax"  -> real(spec.xMax),
    "yMax"  -> real(spec.yMax),
    "yAxis" -> token(spec.yAxis.toString)
  )
  def gridSpec(spec: GridSpec): Operand[Nothing] = fields(
    "frameId" -> frame(spec.frameId),
    "frame"   -> frameSpec(spec.frame),
    "nx"      -> int(spec.nx),
    "ny"      -> int(spec.ny)
  )

  def windowTally(value: WindowTally): Operand[Nothing] = fields(
    "outsideScreen"         -> int(value.outsideScreen),
    "outsideWindow"         -> int(value.outsideWindow),
    "total"                 -> int(value.total),
    "outsideScreenDuration" -> span(value.outsideScreenDuration),
    "outsideWindowDuration" -> span(value.outsideWindowDuration),
    "totalDuration"         -> span(value.totalDuration)
  )

  /** A path that keeps an outer context and adds the inner subject's loci. */
  def merge[K](prefix: Vector[Locus[K]], inner: Diagnostic[K]): Vector[Locus[K]] =
    prefix ++ inner.subject.filterNot(prefix.contains)

  def gapPolicy(value: GapPolicy): Operand[Nothing] = value match
    case GapPolicy.Bridge(maximum) =>
      fields("kind" -> token("Bridge"), "maxDuration" -> span(maximum.span))
    case other => token(other.toString)
  def measureScale(value: MeasureScale): Operand[Nothing] = value match
    case MeasureScale.Bounded(lo, hi) =>
      fields("kind" -> token("Bounded"), "lo" -> real(lo), "hi" -> real(hi))
    case other => token(other.toString)
  def evaluationScale(value: EvaluationScale): Operand[Nothing] = value match
    case EvaluationScale.Measure(scale) =>
      fields("kind" -> token("Measure"), "scale" -> measureScale(scale))
    case other => token(other.toString)
  def evaluationTime(value: EvaluationTime): Operand[Nothing] = value match
    case EvaluationTime.SharedClock(id) =>
      fields("time" -> token("SharedClock"), "clock" -> clock(id))
    case other => token(other.toString)
  def evaluationGeometry(value: EvaluationGeometry): Operand[Nothing] = fields(
    "unit"  -> optional(value.unit.map { case (symbol, unit) => names(Vector(symbol, unit)) }),
    "frame" -> optional(value.frame.map { case (id, spec) =>
      fields("id" -> frame(id), "spec" -> frameSpec(spec))
    }),
    "grid" -> optional(value.grid.map { case (id, spec) =>
      fields("id" -> grid(id), "spec" -> gridSpec(spec))
    })
  )
  def specification(spec: EvaluationSpec): Operand[Nothing] = fields(
    "method"     -> name(spec.method),
    "revision"   -> name(spec.revision),
    "parameters" -> parameters(spec.parameters),
    "components" -> names(spec.components),
    "geometry"   -> evaluationGeometry(spec.geometry),
    "time"       -> evaluationTime(spec.time)
  )
  def evaluation(info: EvaluationInfo): Operand[Nothing] = fields(
    "name"          -> name(info.name),
    "scale"         -> evaluationScale(info.scale),
    "specification" -> optional(info.specification.map(specification))
  )

/** Projections of the lower-level errors that plan, design and preflight wrap.
  * Each is total over its enum; subjects name frames, clocks, samples, events
  * and areas where the error identifies them.
  */
private[plan] object CauseDiagnostics:
  import DiagnosticSupport.*
  import DiagnosticCatalog as C

  def geometry(e: GeometryError): Diagnostic[Nothing] =
    import GeometryError.*
    val d = diagnostic[Nothing](C.geometry, e, e.message)
    e match
      case DegenerateBounds(a, b, c, x)         => d(real(a), real(b), real(c), real(x))
      case NonFiniteBounds(a, b, c, x)          => d(real(a), real(b), real(c), real(x))
      case BoundsExtentOverflow(a, b, c, x)     => d(real(a), real(b), real(c), real(x))
      case FrameMismatch(left, right)           => d(frame(left), frame(right))
      case FrameIdentityConflict(id, l, r)      => d(frame(id), frameSpec(l), frameSpec(r))
      case NonFiniteLength(value, unit)         => d(real(value), token(unit.toString))
      case NegativeLength(value, unit)          => d(real(value), token(unit.toString))
      case NonPositivePerspective(a, b, c)      => d(real(a), real(b), real(c))
      case NotAffine(matrix)                    => d(text(matrix))
      case NonFiniteSigma(value)                => d(real(value))
      case NonPositiveSigma(value)              => d(real(value))
      case DegenerateGrid(nx, ny)               => d(int(nx), int(ny))
      case GridCellCountOverflow(nx, ny, cells) => d(int(nx), int(ny), long(cells))
      case DegenerateEllipse(rx, ry)            => d(real(rx), real(ry))
      case DegeneratePolygon(vertices)          => d(int(vertices))
      case NonFiniteRegion(shape)               => d(name(shape))
      case NonFiniteVelocity(value)             => d(real(value))
      case NegativeVelocity(value)              => d(real(value))
      case NonFiniteDistance(value)             => d(real(value))
      case NegativeDistance(value)              => d(real(value))
      case SubframeOutsideParent(window, a, b, c, x, parent, spec) =>
        d(frame(window), real(a), real(b), real(c), real(x), frame(parent), frameSpec(spec))
      case SubframeIdentity(window)           => d(frame(window))
      case NonPositiveAngularScale(id, value) => d(frame(id), real(value))
      case NonFiniteTranslation(dx, dy)       => d(real(dx), real(dy))

  def surface(e: SurfaceError): Diagnostic[Nothing] =
    import SurfaceError.*
    val d = diagnostic[Nothing](C.surface, e, e.message)
    e match
      case LengthMismatch(expected, actual) => d(int(expected), int(actual))
      case NegativeWeight(index, value)     => d(int(index), real(value))
      case NegativeValue(index, value)      => d(int(index), real(value))
      case NonFiniteValue(index, value)     => d(int(index), real(value))
      case DegenerateTotal(total)           => d(real(total))
      case GridMismatch(left, right)        => d(grid(left), grid(right))
      case GridIdentityConflict(id, l, r)   => d(grid(id), gridSpec(l), gridSpec(r))
      case EmptyCollection(operation)       => d(name(operation))

  def time(e: TimeError): Diagnostic[Nothing] =
    import TimeError.*
    val d = diagnostic[Nothing](C.time, e, e.message)
    e match
      case ReversedInterval(id, onset, offset) =>
        d(clock(id), Operand.Micros(onset), Operand.Micros(offset))
      case ReversedWindow(from, until)        => d(Operand.Micros(from), Operand.Micros(until))
      case ClockMismatch(left, right)         => d(clock(left), clock(right))
      case WrongSourceClock(expected, actual) => d(clock(expected), clock(actual))
      case NonPositiveRate(value)             => d(real(value))
      case NonFiniteDrift(from, to, drift)    => d(clock(from), clock(to), real(drift))
      case NonPositiveClockScale(from, to, drift) => d(clock(from), clock(to), real(drift))

  def estimate(e: EstimateError): Diagnostic[Nothing] =
    import EstimateError.*
    val d = diagnostic[Nothing](C.estimate, e, e.message)
    e match
      case FrameMismatch(measure, target)             => d(frame(measure), frame(target))
      case NoMass                                     => d()
      case DegenerateBandwidth(sigma, cell)           => d(real(sigma), real(cell))
      case DegenerateAxisBandwidth(axis, sigma, cell) =>
        d(token(axis.toString), real(sigma), real(cell))
      case KernelSupportOverflow(axis, sigma, cell) =>
        d(token(axis.toString), real(sigma), real(cell))
      case Surface(underlying) => d(cause(surface(underlying)))

  def comparisonValue(e: ComparisonValueError): Diagnostic[Nothing] =
    import ComparisonValueError.*
    val d = diagnostic[Nothing](C.comparisonValue, e, e.message)
    e match
      case NonFiniteMeasureDistance(value)         => d(real(value))
      case NegativeMeasureDistance(value)          => d(real(value))
      case NonFiniteSimilarity(value)              => d(real(value))
      case InvalidUnitSimilarity(component, value) => d(name(component), real(value))

  def compare(e: CompareError): Diagnostic[Nothing] =
    import CompareError.*
    val d = diagnostic[Nothing](C.compare, e, e.message)
    e match
      case Grids(underlying)                     => d(cause(surface(underlying)))
      case Frames(underlying)                    => d(cause(geometry(underlying)))
      case Estimation(underlying)                => d(cause(estimate(underlying)))
      case ConstantInput(m, o)                   => d(name(m), token(o.toString))
      case EmptyInput(m, o, total)               => d(name(m), token(o.toString), real(total))
      case ZeroNorm(m, l, r)                     => d(name(m), real(l), real(r))
      case RelativeEntropySupport(m, cell, l, r) => d(name(m), int(cell), real(l), real(r))
      case CostMatrixLimitExceeded(m, cells, limit)  => d(name(m), int(cells), int(limit))
      case WorkLimitExceeded(m, cells, pairs, limit) =>
        d(name(m), int(cells), long(pairs), long(limit))
      case InvalidSubstitutionCost(m, l, r, value) =>
        d(name(m), int(l), int(r), real(value))
      case InvalidScore(m, underlying) => d(name(m), cause(comparisonValue(underlying)))
      case TooShort(what, got, needed) => d(name(what), int(got), int(needed))

  def comparisonWork(e: ComparisonWorkError): Diagnostic[Nothing] =
    import ComparisonWorkError.*
    val d = diagnostic[Nothing](C.comparisonWork, e, e.message)
    e match
      case InvalidQuantum(value)            => d(int(value))
      case InvalidBudget(maximum)           => d(long(maximum))
      case WorkBudget(measure, units, most) => d(name(measure), long(units), long(most))

  def pairSchedule(e: PairScheduleError): Diagnostic[Nothing] =
    import PairScheduleError.*
    val d = diagnostic[Nothing](C.pairSchedule, e, e.message)
    e match
      case InvalidBudget(rows, candidates, selected) =>
        d(int(rows), long(candidates), int(selected))
      case InvalidCounts(left, right)            => d(long(left), long(right))
      case InvalidQuantum(value)                 => d(int(value))
      case SourceBudget(left, right, maximum)    => d(long(left), long(right), int(maximum))
      case CandidateBudget(left, right, maximum) => d(long(left), long(right), long(maximum))
      case SelectedBudget(relation, attempted, maximum) =>
        d(name(relation), long(attempted), int(maximum))

  def evaluationSpecError(e: EvaluationSpecError): Diagnostic[Nothing] =
    import EvaluationSpecError.*
    val d = diagnostic[Nothing](C.evaluationSpec, e, e.message, Vector.empty)
    e match
      case EmptyField(field, value)  => d(name(field), text(value))
      case InvalidComponents(values) => d(names(values))
      case InvalidParameters(values) => d(parameters(values))

  def scoreMean(e: ScoreMeanError): Diagnostic[Nothing] =
    import ScoreMeanError.*
    val d = diagnostic[Nothing](C.scoreMean, e, e.message)
    e match
      case EmptyValues(operand)                    => d(name(operand))
      case NonFiniteValue(component, index, value) =>
        d(name(component), int(index), real(value))
      case NonFiniteMean(component, value)               => d(name(component), real(value))
      case InvalidComparisonValue(component, underlying) =>
        d(name(component), cause(comparisonValue(underlying)))

  def difference(e: DifferenceError): Diagnostic[Nothing] =
    import DifferenceError.*
    val d = diagnostic[Nothing](C.difference, e, e.message)
    e match
      case NonFiniteOperands(component, matched, control) =>
        d(name(component), real(matched), real(control))
      case NonFiniteDifference(component, matched, control) =>
        d(name(component), real(matched), real(control))

  def contrastCompatibility(e: ContrastCompatibilityError): Diagnostic[Nothing] =
    import ContrastCompatibilityError.*
    val d = diagnostic[Nothing](C.contrastCompatibility, e, e.message)
    e match
      case Orientation(m, c)                   => d(token(m.toString), token(c.toString))
      case Policy(m, c)                        => d(token(m.render), token(c.render))
      case Scale(m, c)                         => d(evaluationScale(m), evaluationScale(c))
      case MissingSpecification(operand, info) =>
        d(token(operand.toString), evaluation(info))
      case Method(m, c)                            => d(specification(m), specification(c))
      case Components(operand, declared, required) =>
        d(token(operand.toString), names(declared), names(required))
      case SpatialConvention(m, c) => d(evaluationGeometry(m), evaluationGeometry(c))
      case Frames(underlying)      => d(cause(geometry(underlying)))
      case Grids(underlying)       => d(cause(surface(underlying)))
      case Time(m, c)              => d(evaluationTime(m), evaluationTime(c))
      case Clocks(underlying)      => d(cause(time(underlying)))

  def windowOccupancy(e: WindowOccupancyError): Diagnostic[Nothing] =
    import WindowOccupancyError.*
    val d = diagnostic[Nothing](C.windowOccupancy, e, e.message)
    e match
      case Time(underlying)                       => d(cause(time(underlying)))
      case Measure(underlying)                    => d(cause(surface(underlying)))
      case InvalidWidth(value, micros)            => d(interval(value), Operand.Integer(micros))
      case EmptyCoverageInterval(id, values)      => d(clock(id), intervals(values))
      case OverlappingCoverage(id, values)        => d(clock(id), intervals(values))
      case ObservedTime(value, observed, missing) =>
        d(interval(value), Operand.Micros(observed), Operand.Micros(missing))
      case Ledger(position, index, original, retained, boundary) =>
        d(
          int(position),
          int(index),
          Operand.Integer(original),
          Operand.Micros(retained),
          token(boundary.toString)
        )
      case MeasureSupport(retained, positions) => d(int(retained), int(positions))

  def syncEvidence(e: SyncEvidenceError): Diagnostic[Nothing] =
    import SyncEvidenceError.*
    val d = diagnostic[Nothing](C.syncEvidence, e, e.message)
    e match
      case EmptyMarkId(value)                           => d(text(value))
      case NegativeResidualLimit(limit)                 => d(span(limit))
      case NegativeErrorMagnitude(value)                => d(span(value))
      case TooFewCommonMarks(s, t, available, required) =>
        d(clock(s), clock(t), int(available), int(required))
      case TooFewRetainedMarks(s, t, supplied, retained, required) =>
        d(clock(s), clock(t), int(supplied), int(retained), int(required))
      case DuplicateMarkId(s, t, id, first, second) =>
        d(clock(s), clock(t), name(id), int(first), int(second))
      case NonIncreasingSourceMarks(s, t, index, previous, current) =>
        d(clock(s), clock(t), int(index), instant(previous), instant(current))
      case NonIncreasingTargetMarks(s, t, index, previous, current) =>
        d(clock(s), clock(t), int(index), instant(previous), instant(current))
      case DegenerateSourceVariance(s, t, variance) => d(clock(s), clock(t), real(variance))
      case NonFiniteFit(s, t, scale, offset)        =>
        d(clock(s), clock(t), real(scale), real(offset))
      case InvalidFittedSync(s, t, underlying) => d(clock(s), clock(t), cause(time(underlying)))

  def core(e: CoreError): Diagnostic[Nothing] =
    import CoreError.*
    def wrap(inner: Diagnostic[Nothing]) =
      diagnostic(C.core, e, e.message, inner.subject)(cause(inner))
    e match
      case OfTime(underlying)             => wrap(time(underlying))
      case OfGeometry(underlying)         => wrap(geometry(underlying))
      case OfSurface(underlying)          => wrap(surface(underlying))
      case OfRecording(underlying)        => wrap(recordingData(underlying))
      case OfScanpath(underlying)         => wrap(scanpath(underlying))
      case OfEvent(underlying)            => wrap(event(underlying))
      case OfDetectionSupport(underlying) => wrap(detectionSupport(underlying))

  def recordingData(e: RecordingError): Diagnostic[Nothing] =
    import RecordingError.*
    def at(index: Int) = Vector(Locus.Sample(index))
    e match
      case NoSamples => diagnostic[Nothing](C.recording, e, e.message)()
      case NonMonotonic(index, previous, current) =>
        diagnostic(C.recording, e, e.message, at(index))(
          int(index),
          Operand.Micros(previous),
          Operand.Micros(current)
        )
      case UnpairedEyes(timestamps, left, right) =>
        diagnostic[Nothing](C.recording, e, e.message)(int(timestamps), int(left), int(right))
      case NonFinitePosition(channel, index, state, x, y) =>
        diagnostic(C.recording, e, e.message, at(index))(
          token(channel.toString),
          int(index),
          token(state.toString),
          real(x),
          real(y)
        )
      case TrackedOutsideFrame(channel, index, id, spec, x, y) =>
        diagnostic(C.recording, e, e.message, at(index))(
          token(channel.toString),
          int(index),
          frame(id),
          frameSpec(spec),
          real(x),
          real(y)
        )
      case OffScreenInsideFrame(channel, index, id, spec, x, y) =>
        diagnostic(C.recording, e, e.message, at(index))(
          token(channel.toString),
          int(index),
          frame(id),
          frameSpec(spec),
          real(x),
          real(y)
        )
      case InvalidPupil(channel, index, value) =>
        diagnostic(C.recording, e, e.message, at(index))(
          token(channel.toString),
          int(index),
          real(value)
        )
      case UndeclaredPupilUnit(channel, index, value) =>
        diagnostic(C.recording, e, e.message, at(index))(
          token(channel.toString),
          int(index),
          real(value)
        )
      case NegativeSamplingTolerance(tolerance) =>
        diagnostic[Nothing](C.recording, e, e.message)(span(tolerance))
      case FixedRateMismatch(index, previous, current, period, tolerance, deviation) =>
        diagnostic(C.recording, e, e.message, at(index))(
          int(index),
          Operand.Micros(previous),
          Operand.Micros(current),
          span(period),
          span(tolerance.toSpan),
          span(deviation)
        )

  def scanpath(e: ScanpathError): Diagnostic[Nothing] =
    import ScanpathError.*
    def at(index: Int) = Vector(Locus.Fixation(index))
    e match
      case NoFixations => diagnostic[Nothing](C.scanpath, e, e.message)()
      case OutOfOrder(index, previous, current) =>
        diagnostic(C.scanpath, e, e.message, at(index))(
          int(index),
          text(previous),
          text(current)
        )
      case WrongClock(index, expected, actual) =>
        diagnostic(C.scanpath, e, e.message, at(index))(
          int(index),
          name(expected),
          name(actual)
        )
      case InvalidTransitionSpan(index, underlying) =>
        diagnostic(C.scanpath, e, e.message, at(index))(int(index), cause(time(underlying)))
      case InvalidExtent(underlying) =>
        diagnostic[Nothing](C.scanpath, e, e.message)(cause(time(underlying)))
      case UnmappableFixation(index, from, to, x, y) =>
        diagnostic(C.scanpath, e, e.message, at(index))(
          int(index),
          frame(from),
          frame(to),
          real(x),
          real(y)
        )

  def event(e: EventError): Diagnostic[Nothing] =
    import EventError.*
    val d = diagnostic[Nothing](C.event, e, e.message)
    e match
      case EmptySpan(kind, value)                  => d(name(kind), interval(value))
      case NonFinitePoint(kind, role, value, x, y) =>
        d(name(kind), name(role), interval(value), real(x), real(y))
      case InvalidDispersion(value, method)          => d(real(value), token(method.toString))
      case NonPositiveSampleCount(count)             => d(int(count))
      case EmptyPursuit(value)                       => d(interval(value))
      case NonFinitePursuitPoint(value, index, x, y) =>
        d(interval(value), int(index), real(x), real(y))

  def detectionSupport(e: DetectionSupportError): Diagnostic[Nothing] =
    import DetectionSupportError.*
    def on(source: RecordingRef, loci: Locus[Nothing]*) =
      Locus.Recording(source) +: loci.toVector
    e match
      case InvalidSampleRange(from, until) =>
        diagnostic[Nothing](C.detectionSupport, e, e.message)(int(from), int(until))
      case EventSupportCountMismatch(source, events, ranges) =>
        diagnostic(C.detectionSupport, e, e.message, on(source))(
          recordingRef(source),
          int(events),
          int(ranges)
        )
      case EventClockMismatch(source, index, expected, actual) =>
        diagnostic(C.detectionSupport, e, e.message, on(source, Locus.Event(index)))(
          recordingRef(source),
          int(index),
          clock(expected),
          clock(actual)
        )
      case SampleRangeOutsideRecording(source, index, value, samples) =>
        diagnostic(C.detectionSupport, e, e.message, on(source))(
          recordingRef(source),
          int(index),
          range(value),
          int(samples)
        )
      case OverlappingSampleRanges(source, index, previous, current) =>
        diagnostic(C.detectionSupport, e, e.message, on(source))(
          recordingRef(source),
          int(index),
          range(previous),
          range(current)
        )
      case EventSpanOutsideRecording(source, index, value, extent) =>
        diagnostic(C.detectionSupport, e, e.message, on(source, Locus.Event(index)))(
          recordingRef(source),
          int(index),
          interval(value),
          interval(extent)
        )
      case EventSpanHasNoSamples(source, index, value) =>
        diagnostic(C.detectionSupport, e, e.message, on(source, Locus.Event(index)))(
          recordingRef(source),
          int(index),
          interval(value)
        )
      case EventSampleRangeMismatch(source, index, value, declared, derived) =>
        diagnostic(C.detectionSupport, e, e.message, on(source, Locus.Event(index)))(
          recordingRef(source),
          int(index),
          interval(value),
          range(declared),
          range(derived)
        )
      case InvalidDerivedSampleRange(source, index, value, from, until) =>
        diagnostic(C.detectionSupport, e, e.message, on(source, Locus.Event(index)))(
          recordingRef(source),
          int(index),
          interval(value),
          int(from),
          int(until)
        )
      case InvalidDerivedFixation(source, index, value, underlying) =>
        val inner = core(underlying)
        diagnostic(
          C.detectionSupport,
          e,
          e.message,
          merge(on(source, Locus.Event(index)), inner)
        )(recordingRef(source), int(index), range(value), cause(inner))
      case NoUsableSourceSamples(source, index, value) =>
        diagnostic(C.detectionSupport, e, e.message, on(source, Locus.Event(index)))(
          recordingRef(source),
          int(index),
          range(value)
        )
      case UnmappableSourceSample(source, index, sample, from, to, x, y) =>
        diagnostic(
          C.detectionSupport,
          e,
          e.message,
          on(source, Locus.Event(index), Locus.Sample(sample))
        )(
          recordingRef(source),
          int(index),
          int(sample),
          frame(from),
          frame(to),
          real(x),
          real(y)
        )
      case UnmappableEventPoint(source, index, role, point, from, to, x, y) =>
        diagnostic(C.detectionSupport, e, e.message, on(source, Locus.Event(index)))(
          recordingRef(source),
          int(index),
          name(role),
          int(point),
          frame(from),
          frame(to),
          real(x),
          real(y)
        )

  def detectorDefinition(e: DetectorDefinitionError): Diagnostic[Nothing] =
    e match
      case DetectorDefinitionError.Configuration(id, values) =>
        diagnostic[Nothing](C.detectorDefinition, e, e.message)(
          name(id.value),
          parameters(values)
        )

  def detectionFailure(e: DetectionFailure): Diagnostic[Nothing] =
    def wrap(inner: Diagnostic[Nothing]) =
      diagnostic(C.detectionFailure, e, e.message, inner.subject)(cause(inner))
    e match
      case DetectionFailure.EventSummary(underlying) => wrap(core(underlying))
      case DetectionFailure.Kinematics(underlying)   => wrap(kinematics(underlying))

  def kinematics(e: KinematicsError): Diagnostic[Nothing] = e match
    case KinematicsError.InvalidSampling(underlying) =>
      diagnostic[Nothing](C.kinematics, e, e.message)(cause(configuration(underlying)))

  def configuration(e: ConfigurationError): Diagnostic[Nothing] =
    import ConfigurationError.*
    val d = diagnostic[Nothing](C.configuration, e, e.message)
    e match
      case NonPositiveWindowHalfWidth(value)                    => d(int(value))
      case InsufficientRegularSamples(count)                    => d(int(count))
      case NonPositiveSamplingInterval(index, value)            => d(int(index), span(value))
      case IrregularSamplingInterval(index, expected, observed) =>
        d(int(index), span(expected), span(observed))
      case NegativeMissingPadding(value)          => d(span(value))
      case NegativeInterpolationGap(value)        => d(span(value))
      case NegativeMaximumMergeGap(value)         => d(span(value))
      case NonPositiveMinimumEventDuration(value) => d(span(value))
      case NonPositiveIvtThreshold(value)         => d(real(value))
      case InvalidEkThresholds(x, y)              => d(real(x), real(y))
      case InvalidEkMultiplier(value)             => d(real(value))
      case NonPositiveEkMinimumSamples(value)     => d(int(value))

  def detectionResult(e: DetectionResultError): Diagnostic[Nothing] =
    import DetectionResultError.*
    def on(source: RecordingRef, loci: Locus[Nothing]*) =
      Locus.Recording(source) +: loci.toVector
    e match
      case DetectorEmissionFailed(source, ref, index, underlying) =>
        val inner = detectionFailure(underlying)
        diagnostic(C.detectionResult, e, e.message, merge(on(source), inner))(
          recordingRef(source),
          detector(ref),
          int(index),
          cause(inner)
        )
      case EventOutsideRecording(source, ref, index, value, extent) =>
        diagnostic(C.detectionResult, e, e.message, on(source, Locus.Event(index)))(
          recordingRef(source),
          detector(ref),
          int(index),
          interval(value),
          interval(extent)
        )
      case SourceSupport(source, ref, underlying) =>
        val inner = detectionSupport(underlying)
        diagnostic(C.detectionResult, e, e.message, merge(on(source), inner))(
          recordingRef(source),
          detector(ref),
          cause(inner)
        )
      case GapPolicyViolation(source, ref, index, gap, duration, policy) =>
        diagnostic(C.detectionResult, e, e.message, on(source, Locus.Event(index)))(
          recordingRef(source),
          detector(ref),
          int(index),
          range(gap),
          span(duration),
          gapPolicy(policy)
        )
      case InvalidDerivedRange(source, ref, role, from, until, underlying) =>
        val inner = detectionSupport(underlying)
        diagnostic(
          C.detectionResult,
          e,
          e.message,
          merge(on(source, Locus.Samples(from, until)), inner)
        )(
          recordingRef(source),
          detector(ref),
          name(role),
          int(from),
          int(until),
          cause(inner)
        )

  def aoi(e: AoiError): Diagnostic[Nothing] =
    import AoiError.*
    def area(id: AoiId) = Vector(Locus.Area(id.value))
    e match
      case BlankId(value)        => diagnostic[Nothing](C.aoi, e, e.message)(text(value))
      case BlankLabel(id, value) =>
        diagnostic(C.aoi, e, e.message, area(id))(name(id.value), text(value))
      case BlankAttributeKey(id, key) =>
        diagnostic(C.aoi, e, e.message, area(id))(name(id.value), text(key))
      case NonPositiveResolution(nx, ny) =>
        diagnostic[Nothing](C.aoi, e, e.message)(int(nx), int(ny))
      case EmptySet                       => diagnostic[Nothing](C.aoi, e, e.message)()
      case DuplicateId(id, first, second) =>
        diagnostic(C.aoi, e, e.message, area(id))(name(id.value), int(first), int(second))
      case FrameConflict(underlying) =>
        diagnostic[Nothing](C.aoi, e, e.message)(cause(geometry(underlying)))
      case ResolutionGridFailure(id, nx, ny, underlying) =>
        diagnostic[Nothing](C.aoi, e, e.message)(
          frame(id),
          int(nx),
          int(ny),
          cause(geometry(underlying))
        )
      case ObservedOverlap(id, sample, x, y, areas) =>
        diagnostic(C.aoi, e, e.message, Vector(Locus.Sample(sample)))(
          frame(id),
          int(sample),
          real(x),
          real(y),
          names(areas.map(_.value))
        )

  def descriptor(e: DescriptorError): Diagnostic[Nothing] =
    import DescriptorError.*
    val d = diagnostic[Nothing](C.descriptor, e, e.message)
    e match
      case InvalidField(id, version, meaning)   => d(name(id), int(version), text(meaning))
      case InvalidAlternatives(id, values)      => d(name(id), names(values))
      case InvalidDefault(field, reason)        => d(name(field), text(reason))
      case DuplicateFields(ids)                 => d(names(ids))
      case InvalidComponent(id, meaning, value) =>
        d(name(id), text(meaning), measureScale(value))
      case ParameterMismatch(described, declared) =>
        d(parameters(described), parameters(declared))
      case ComponentMismatch(described, declared) => d(names(described), names(declared))
      case MissingMethod(id)                      => d(definition(id))
      case MethodIdentity(expected, found)        => d(definition(expected), definition(found))
      case ExecutionMismatch(declared, actual)    =>
        d(token(declared.toString), token(actual.toString))
      case UnexplainedFields(ids) => d(names(ids))
      case InvalidFieldId(value)  => d(text(value))
      case InvalidBounds(lo, hi)  => d(optional(lo.map(endpoint)), optional(hi.map(endpoint)))
      case BoundsForShape(f, shape, b) =>
        d(fieldId(f), token(shape.toString), numericBounds(b))
      case UnknownRulePart(f, part)       => d(fieldId(f), fieldId(part))
      case InvalidRepetition(f, min, max) => d(fieldId(f), int(min), optional(max.map(int)))
      case DefaultRefused(f, error)       =>
        val inner = formField[Nothing, Nothing](error)(identity)
        diagnostic[Nothing](C.descriptor, e, e.message, Locus.Field(f.value) +: inner.subject)(
          fieldId(f),
          cause(inner)
        )
      case UntranslatableLegacy(f, units, domain) => d(name(f), token(units), token(domain))

  /** A form field's refusal, located at the field; a domain refusal keeps its
    * own diagnostic as the cause.
    */
  def formField[E, K](e: FieldError[E])(underlying: E => Diagnostic[K]): Diagnostic[K] =
    import FieldError.*
    val here = Vector(Locus.Field(e.field.value))
    val d    = diagnostic[K](C.formField, e, e.message, here)
    e match
      case Missing(f)                  => d(fieldId(f))
      case Malformed(f, raw, expected) =>
        d(fieldId(f), rawValue(raw), token(expected.toString))
      case OutOfBounds(f, t, v, side, bound, q) =>
        d(
          fieldId(f),
          text(t),
          real(v),
          token(side.toString),
          endpoint(bound),
          token(q.toString)
        )
      case NotAChoice(f, t, options)    => d(fieldId(f), token(t), names(options))
      case UnknownPart(f, part)         => d(fieldId(f), fieldId(part))
      case Unordered(f, lo, hi, lv, hv) =>
        d(fieldId(f), fieldId(lo), fieldId(hi), real(lv), real(hv))
      case Duplicate(f, part, t)      => d(fieldId(f), fieldId(part), token(t))
      case ItemCount(f, n, min, max)  => d(fieldId(f), int(n), int(min), optional(max.map(int)))
      case Refused(f, raw, u, reason) =>
        val inner = underlying(u)
        diagnostic[K](
          C.formField,
          e,
          e.message,
          here ++ inner.subject.filterNot(here.contains)
        )(
          fieldId(f),
          rawValue(raw),
          cause(inner),
          text(reason)
        )
