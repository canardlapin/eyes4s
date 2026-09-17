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

import cats.data.NonEmptyVector
import eyes4s.compare.*
import eyes4s.kernel.*
import scala.annotation.implicitNotFound

/** Failures name the component and both operands, including arithmetic overflow. */
enum DifferenceError derives CanEqual:
  case NonFiniteOperands(component: String, matched: Double, control: Double)
  case NonFiniteDifference(component: String, matched: Double, control: Double)

  def message: String = this match
    case NonFiniteOperands(component, matched, control) =>
      s"$component needs finite operands: matched=$matched, control=$control."
    case NonFiniteDifference(component, matched, control) =>
      s"$component subtraction overflowed: matched=$matched, control=$control."

/** A finite signed difference, distinct from bounded similarities and distances. */
opaque type SignedDifference = Double

object SignedDifference:
  def between(
      matched: Double,
      control: Double,
      component: String = "value"
  ): Either[DifferenceError, SignedDifference] =
    if !matched.isFinite || !control.isFinite then
      Left(DifferenceError.NonFiniteOperands(component, matched, control))
    else
      val difference = matched - control
      if difference.isFinite then Right(difference)
      else Left(DifferenceError.NonFiniteDifference(component, matched, control))

  extension (difference: SignedDifference) def value: Double = difference

/** Five signed components; no implicit scalar aggregation or bounded-score conversion. */
final class MultiMatchDifference private[design] (
    val shape: SignedDifference,
    val direction: SignedDifference,
    val length: SignedDifference,
    val position: SignedDifference,
    val duration: SignedDifference
) derives CanEqual

/** Subtraction need not be closed over the input score type.
  * Published ContrastLaws checks component-wise signed subtraction.
  */
@implicitNotFound(
  "No signed contrast is defined from ${S} to ${D}. Supply a Contrastable instance with a separate signed output type."
)
trait Contrastable[S, D]:
  def components: Vector[String]
  def subtract(matched: S, control: S): Either[DifferenceError, D]

object Contrastable:
  def apply[S, D](using instance: Contrastable[S, D]): Contrastable[S, D] = instance

  given Contrastable[Double, SignedDifference] with
    val components = Vector("value")
    def subtract(matched: Double, control: Double): Either[DifferenceError, SignedDifference] =
      SignedDifference.between(matched, control)

  given Contrastable[Similarity, SignedDifference] with
    val components = Vector("value")
    def subtract(
        matched: Similarity,
        control: Similarity
    ): Either[DifferenceError, SignedDifference] =
      SignedDifference.between(matched.value, control.value)

  given Contrastable[MeasureDistance, SignedDifference] with
    val components = Vector("value")
    def subtract(
        matched: MeasureDistance,
        control: MeasureDistance
    ): Either[DifferenceError, SignedDifference] =
      SignedDifference.between(matched.value, control.value)

  given Contrastable[MultiMatchScore, MultiMatchDifference] with
    val components = Vector("shape", "direction", "length", "position", "duration")
    def subtract(
        matched: MultiMatchScore,
        control: MultiMatchScore
    ): Either[DifferenceError, MultiMatchDifference] =
      for
        shape     <- SignedDifference.between(matched.shape, control.shape, "shape")
        direction <- SignedDifference.between(matched.direction, control.direction, "direction")
        length    <- SignedDifference.between(matched.length, control.length, "length")
        position  <- SignedDifference.between(matched.position, control.position, "position")
        duration  <- SignedDifference.between(matched.duration, control.duration, "duration")
      yield new MultiMatchDifference(shape, direction, length, position, duration)

enum ContrastOperand derives CanEqual:
  case Matched, Control

/** Compatibility is checked from typed values before any row is subtracted. */
enum ContrastCompatibilityError derives CanEqual:
  case Orientation(matched: ReductionOrientation, control: ReductionOrientation)
  case Policy(matched: FailurePolicy, control: FailurePolicy)
  case Scale(matched: EvaluationScale, control: EvaluationScale)
  case MissingSpecification(operand: ContrastOperand, evaluation: EvaluationInfo)
  case Method(matched: EvaluationSpec, control: EvaluationSpec)
  case Components(operand: ContrastOperand, declared: Vector[String], required: Vector[String])
  case SpatialConvention(matched: EvaluationGeometry, control: EvaluationGeometry)
  case Frames(underlying: GeometryError)
  case Grids(underlying: SurfaceError)
  case Time(matched: EvaluationTime, control: EvaluationTime)
  case Clocks(underlying: TimeError)

  def message: String = this match
    case Orientation(m, c) => s"Contrast orientations differ: matched=$m, control=$c."
    case Policy(m, c)      =>
      s"Contrast failure policies differ: matched=${m.render}, control=${c.render}."
    case Scale(m, c) => s"Contrast scales differ: matched=${m.render}, control=${c.render}."
    case MissingSpecification(operand, info) =>
      s"$operand evaluator '${info.name}' has no contrast specification."
    case Method(m, c) =>
      s"Contrast methods differ: matched=${m.method}@${m.revision} ${m.parameters}, control=${c.method}@${c.revision} ${c.parameters}."
    case Components(operand, declared, required) =>
      s"$operand components $declared differ from contrast components $required."
    case SpatialConvention(m, c) =>
      s"Contrast spatial conventions differ: matched=${m.unit}/${m.frame}/${m.grid}, control=${c.unit}/${c.frame}/${c.grid}."
    case Frames(error) => error.message
    case Grids(error)  => error.message
    case Time(m, c)    => s"Contrast temporal conventions differ: matched=$m, control=$c."
    case Clocks(error) => error.message

enum ContrastError[K] derives CanEqual:
  case Incompatible(issues: NonEmptyVector[ContrastCompatibilityError])
  case EmptyDomain(matchedKeys: Int, controlKeys: Int)
  case IndistinguishableOrdering(first: K, second: K)

  def message: String = this match
    case Incompatible(issues) => issues.toVector.map(_.message).mkString(" ")
    case EmptyDomain(m, c)    => s"Contrast has no focal keys: matched=$m, control=$c."
    case IndistinguishableOrdering(a, b) =>
      s"Contrast key ordering equates distinct keys $a and $b."

enum ContrastRowError[K] derives CanEqual:
  case MissingOperands(key: K, missing: Vector[ContrastOperand])
  case ReductionFailures(
      key: K,
      matched: Option[ReductionError[K]],
      control: Option[ReductionError[K]]
  )
  case Arithmetic(key: K, underlying: DifferenceError)

  def message: String = this match
    case MissingOperands(key, missing) => s"Contrast key $key is missing $missing."
    case ReductionFailures(key, m, c)  =>
      s"Contrast key $key failed: matched=${m.map(_.message)}, control=${c.map(_.message)}."
    case Arithmetic(key, error) => s"Contrast key $key: ${error.message}"

/** Both operands survive even when subtraction cannot produce a result. */
final class ContrastRow[K, S, D] private[design] (
    val key: K,
    val matched: Option[ReductionRow[K, S]],
    val control: Option[ReductionRow[K, S]],
    val difference: Either[ContrastRowError[K], D]
) derives CanEqual

object ContrastRow:
  /** Checked reconstruction: the stored difference must have the shape the two
    * operands determine. Missing operands, reduction failures and arithmetic
    * outcomes each name the operands that produced them, and those must agree
    * with the rows this contrast row refers to.
    */
  def reconstruct[K, S, D](
      key: K,
      matched: Option[ReductionRow[K, S]],
      control: Option[ReductionRow[K, S]],
      difference: Either[ContrastRowError[K], D]
  ): Either[ReconstructionError[K], ContrastRow[K, S, D]] =
    val expectedMissing =
      Option.when(matched.isEmpty)(ContrastOperand.Matched).toVector ++
        Option.when(control.isEmpty)(ContrastOperand.Control).toVector
    val consistent = (matched, control, difference) match
      case (Some(m), Some(c), Right(_)) =>
        m.key == key && c.key == key && m.result.isRight && c.result.isRight
      case (Some(m), Some(c), Left(ContrastRowError.Arithmetic(k, _))) =>
        k == key && m.key == key && c.key == key && m.result.isRight && c.result.isRight
      case (Some(m), Some(c), Left(ContrastRowError.ReductionFailures(k, mf, cf))) =>
        k == key && m.key == key && c.key == key &&
        (m.result.isLeft || c.result.isLeft) &&
        m.result.left.toOption == mf && c.result.left.toOption == cf
      case (_, _, Left(ContrastRowError.MissingOperands(k, missing))) =>
        k == key && expectedMissing.nonEmpty && missing == expectedMissing &&
        matched.forall(_.key == key) && control.forall(_.key == key)
      case _ => false
    Either.cond(
      consistent,
      new ContrastRow(key, matched, control, difference),
      ReconstructionError.ContrastRowShape(
        key,
        matched.map(_.result.map(_ => ())),
        control.map(_.result.map(_ => ())),
        difference.map(_ => ())
      )
    )

/** A keyed contrast with both complete source analyses and their provenance. */
final class Contrast[K, S, D] private[design] (
    val matched: Analysis[K, S],
    val control: Analysis[K, S],
    val rows: Vector[ContrastRow[K, S, D]]
) derives CanEqual

object Contrast:
  /** Checked reconstruction: the two analyses must be contrast-compatible for
    * the declared components, the rows must cover exactly the sorted key union
    * in the caller's ordering, and each row's operands must be the analyses'
    * own rows for that key.
    */
  def reconstruct[K, S, D](
      matched: Analysis[K, S],
      control: Analysis[K, S],
      rows: Vector[ContrastRow[K, S, D]],
      components: Vector[String]
  )(using ordering: Ordering[K]): Either[ReconstructionError[K], Contrast[K, S, D]] =
    val issues = ContrastCompatibility.check(matched, control, components)
    NonEmptyVector.fromVector(issues) match
      case Some(errors) => Left(ReconstructionError.Incompatible(errors))
      case None         =>
        val keys = (matched.entries.map(_.key) ++ control.entries.map(_.key)).distinct.sorted
        val matchedRows = matched.entries.map(row => row.key -> row).toMap
        val controlRows = control.entries.map(row => row.key -> row).toMap
        if rows.map(_.key) != keys then
          Left(ReconstructionError.ContrastDomain(keys, rows.map(_.key)))
        else
          rows
            .collectFirst {
              case row if row.matched != matchedRows.get(row.key) =>
                ReconstructionError.ContrastOperand(row.key, ContrastOperand.Matched)
              case row if row.control != controlRows.get(row.key) =>
                ReconstructionError.ContrastOperand(row.key, ContrastOperand.Control)
            }
            .toLeft(new Contrast(matched, control, rows))

/** Matched minus control, over the union of focal keys in the caller's explicit
  * key ordering. A lawful Ordering consistent with key equality is required;
  * observed ties between distinct keys are rejected. No key rendering or digest
  * collision can silently determine row alignment.
  */
def contrast[K, S, D](matched: Analysis[K, S], control: Analysis[K, S])(using
    algebra: Contrastable[S, D],
    ordering: Ordering[K]
): Either[ContrastError[K], Contrast[K, S, D]] =
  contrastWork(matched, control).map(ContrastWork.complete)

/** The same contrast as [[contrast]], in resumable steps. Compatibility, the
  * sorted key domain and the ordering check are decided here before any row
  * is subtracted; each step then subtracts at most `quantum` keys.
  */
def contrastWork[K, S, D](matched: Analysis[K, S], control: Analysis[K, S])(using
    algebra: Contrastable[S, D],
    ordering: Ordering[K]
): Either[ContrastError[K], ContrastCursor[K, S, D]] =
  val issues = ContrastCompatibility.check(matched, control, algebra.components)
  NonEmptyVector.fromVector(issues) match
    case Some(errors) => Left(ContrastError.Incompatible(errors))
    case None         =>
      val keys      = (matched.entries.map(_.key) ++ control.entries.map(_.key)).distinct.sorted
      val collision = keys.zip(keys.drop(1)).find { case (a, b) => ordering.compare(a, b) == 0 }
      if keys.isEmpty then
        Left(ContrastError.EmptyDomain(matched.entries.size, control.entries.size))
      else
        collision match
          case Some((a, b)) => Left(ContrastError.IndistinguishableOrdering(a, b))
          case None         =>
            Right(
              new ContrastCursor(
                matched,
                control,
                keys,
                matched.entries.map(row => row.key -> row).toMap,
                control.entries.map(row => row.key -> row).toMap,
                0,
                Vector.empty
              )
            )

/** One step of a keyed contrast. */
enum ContrastPage[K, S, D]:
  case More(workUnits: Int, next: ContrastCursor[K, S, D])
  case Done(workUnits: Int, contrast: Contrast[K, S, D])

/** An immutable position inside a keyed contrast, one unit per key. */
final class ContrastCursor[K, S, D] private[design] (
    private val matched: Analysis[K, S],
    private val control: Analysis[K, S],
    private val keys: Vector[K],
    private val matchedRows: Map[K, ReductionRow[K, S]],
    private val controlRows: Map[K, ReductionRow[K, S]],
    private val position: Int,
    private val rows: Vector[ContrastRow[K, S, D]]
)(using algebra: Contrastable[S, D]):
  /** Keys whose row is already recorded. */
  def contrastedKeys: Int = position

  def advance(quantum: PairQuantum): ContrastPage[K, S, D] =
    val end  = math.min(keys.size, position + quantum.value)
    val next = rows ++ (position until end).map { index =>
      val key    = keys(index)
      val m      = matchedRows.get(key)
      val c      = controlRows.get(key)
      val result = (m, c) match
        case (Some(left), Some(right)) =>
          (left.result, right.result) match
            case (Right(a), Right(b)) =>
              algebra.subtract(a, b).left.map(ContrastRowError.Arithmetic(key, _))
            case _ =>
              Left(
                ContrastRowError.ReductionFailures(
                  key,
                  left.result.left.toOption,
                  right.result.left.toOption
                )
              )
        case _ =>
          Left(
            ContrastRowError.MissingOperands(
              key,
              Option.when(m.isEmpty)(ContrastOperand.Matched).toVector ++
                Option.when(c.isEmpty)(ContrastOperand.Control).toVector
            )
          )
      new ContrastRow(key, m, c, result)
    }
    if end == keys.size then
      ContrastPage.Done(end - position, new Contrast(matched, control, next))
    else
      ContrastPage.More(
        end - position,
        new ContrastCursor(matched, control, keys, matchedRows, controlRows, end, next)
      )

object ContrastWork:
  /** Drive a contrast to completion with the default quantum. */
  def complete[K, S, D](cursor: ContrastCursor[K, S, D]): Contrast[K, S, D] =
    @annotation.tailrec
    def loop(cursor: ContrastCursor[K, S, D]): Contrast[K, S, D] =
      cursor.advance(PairQuantum.default) match
        case ContrastPage.More(_, next)     => loop(next)
        case ContrastPage.Done(_, contrast) => contrast
    loop(cursor)

private object ContrastCompatibility:
  def check[K, S](
      matched: Analysis[K, S],
      control: Analysis[K, S],
      components: Vector[String]
  ): Vector[ContrastCompatibilityError] =
    import ContrastCompatibilityError.*
    val m     = matched.evaluation
    val c     = control.evaluation
    val basic =
      Option
        .when(matched.diagnostics.orientation != control.diagnostics.orientation)(
          Orientation(matched.diagnostics.orientation, control.diagnostics.orientation)
        )
        .toVector ++
        Option
          .when(matched.diagnostics.policy != control.diagnostics.policy)(
            Policy(matched.diagnostics.policy, control.diagnostics.policy)
          )
          .toVector ++
        Option.when(m.scale != c.scale)(Scale(m.scale, c.scale)).toVector
    val missing = Vector(ContrastOperand.Matched -> m, ContrastOperand.Control -> c).flatMap {
      case (operand, info) =>
        Option.when(info.specification.isEmpty)(MissingSpecification(operand, info))
    }
    val declared = Vector(ContrastOperand.Matched -> m, ContrastOperand.Control -> c).flatMap {
      case (operand, info) =>
        info.specification.toVector.flatMap { spec =>
          Option.when(spec.components != components)(
            Components(operand, spec.components, components)
          )
        }
    }
    val method = (m.specification, c.specification) match
      case (Some(a), Some(b)) =>
        Option
          .when(
            a.method != b.method || a.revision != b.revision || a.parameters != b.parameters
          )(Method(a, b))
          .toVector ++
          spatial(a.geometry, b.geometry) ++ temporal(a.time, b.time)
      case _ => Vector.empty
    basic ++ missing ++ declared ++ method

  private def spatial(
      a: EvaluationGeometry,
      b: EvaluationGeometry
  ): Vector[ContrastCompatibilityError] =
    import ContrastCompatibilityError.*
    val frames = (a.frame, b.frame) match
      case (Some((ai, as)), Some((bi, bs))) =>
        Agreement.frames(ai, as, bi, bs).left.toOption.map(Frames.apply).toVector
      case (None, None) => Vector.empty
      case _            => Vector(SpatialConvention(a, b))
    val grids = (a.grid, b.grid) match
      case (Some((ai, as)), Some((bi, bs))) =>
        Agreement.grids(ai, as, bi, bs).left.toOption.map(Grids.apply).toVector
      case (None, None) => Vector.empty
      case _            => Vector(SpatialConvention(a, b))
    Option.when(a.unit != b.unit)(SpatialConvention(a, b)).toVector ++ frames ++ grids

  private def temporal(
      a: EvaluationTime,
      b: EvaluationTime
  ): Vector[ContrastCompatibilityError] =
    (a, b) match
      case (EvaluationTime.SharedClock(left), EvaluationTime.SharedClock(right)) =>
        Agreement
          .clocks(left, right)
          .left
          .toOption
          .map(ContrastCompatibilityError.Clocks.apply)
          .toVector
      case _ => Option.when(a != b)(ContrastCompatibilityError.Time(a, b)).toVector
