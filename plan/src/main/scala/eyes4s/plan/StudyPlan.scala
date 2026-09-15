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

import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.surface.*

/** Stable identity of a registered behavior, separate from its parameter schema. */
final case class DefinitionId private (name: String, version: Int) derives CanEqual
object DefinitionId:
  val cosine: DefinitionId      = new DefinitionId("eyes4s.cosine", 1)
  val study: DefinitionId       = new DefinitionId("eyes4s.study", 1)
  val studyKey: DefinitionId    = new DefinitionId("eyes4s.study-key", 1)
  val studyLayout: DefinitionId = new DefinitionId("eyes4s.participant-stimulus-phase", 1)
  val unit: DefinitionId        = new DefinitionId("eyes4s.unit", 1)
  def of(name: String, version: Int): Either[PlanError, DefinitionId] =
    if name.trim.isEmpty || version < 1 then Left(PlanError.InvalidDefinition(name, version))
    else Right(new DefinitionId(name, version))

/** A typed reference to separately stored input; no file access occurs in plan. */
final case class ArtifactRef[A] private (digest: String) derives CanEqual
object ArtifactRef:
  def of[A](hash: ContentHash): ArtifactRef[A]                    = new ArtifactRef(hash.render)
  def parse[A](digest: String): Either[PlanError, ArtifactRef[A]] =
    if digest.length == 16 && digest.forall(c => "0123456789abcdef".contains(c)) then
      Right(new ArtifactRef(digest))
    else Left(PlanError.InvalidArtifact(digest))

enum PlanError derives CanEqual:
  case InvalidDefinition(name: String, version: Int)
  case InvalidArtifact(digest: String)
  case MissingArtifact(digest: String)
  case ArtifactMismatch(expected: String, actual: String)
  case InvalidPhases(focal: String, reference: String)
  case EmptyScales(count: Int)
  case DuplicateScales(names: Vector[String])
  case Specification(underlying: EvaluationSpecError)

  def message: String = this match
    case InvalidDefinition(n, v) =>
      s"Definition requires a non-empty name and positive version: name='$n', version=$v."
    case InvalidArtifact(d) =>
      s"Artifact digest '$d' must have 16 lowercase hexadecimal digits."
    case MissingArtifact(d)     => s"Study requires fixation artifact $d."
    case ArtifactMismatch(e, a) => s"Study requires artifact $e, supplied $a."
    case InvalidPhases(f, r)    =>
      s"Focal and reference phases must be non-empty and distinct: '$f', '$r'."
    case EmptyScales(n)         => s"Study requires at least one estimation scale, got $n."
    case DuplicateScales(names) => s"Study estimation scales must be distinct: $names."
    case Specification(e)       => e.message

/** A registered interpretation of user keys. Identity and matching stay in K. */
final class StudyLayout[K](
    val id: DefinitionId,
    val participant: Projection[K, String],
    val stimulus: Projection[K, String],
    val phase: Projection[K, String]
)(using val digest: KeyDigest[K], val ordering: Ordering[K])

/** Ordinary study key; custom product keys work through StudyLayout and a codec. */
final case class StudyKey(participant: String, stimulus: String, phase: String) derives CanEqual
object StudyKey:
  given KeyDigest[StudyKey] = KeyDigest.derived[StudyKey]
  given Ordering[StudyKey]  = Ordering.by(k => (k.participant, k.stimulus, k.phase))
  def layout(id: DefinitionId): StudyLayout[StudyKey] = new StudyLayout(
    id,
    Projection.named("participant")(_.participant),
    Projection.named("stimulus")(_.stimulus),
    Projection.named("phase")(_.phase)
  )

/** Exact payload identity includes keys, nominal geometry, clocks and fixation summaries.
  * ContentHash is the library's portable non-cryptographic digest, not a security checksum.
  */
final class StudyInput[K, U <: Unit2D] private (
    val trials: Trials[K, Unit, Scanpath[U]],
    val hash: ContentHash
):
  def reference: ArtifactRef[StudyInput[K, U]] = ArtifactRef.of(hash)

object StudyInput:
  def apply[K, U <: Unit2D](
      trials: Trials[K, Unit, Scanpath[U]]
  )(using keys: KeyDigest[K], unit: UnitLabel[U]): StudyInput[K, U] =
    val hashes = trials.rows.map { trial =>
      val path  = trial.value
      val frame = path.frame
      val fixes = path.fixations.toVector.map { fix =>
        ContentHash.combineAll(
          Vector(
            ContentHash.ofString(fix.span.onset.toMicros.toString),
            ContentHash.ofString(fix.span.offset.toMicros.toString),
            ContentHash.of(IArray(fix.centre.x, fix.centre.y, fix.sampleCount.toDouble))
          )
        )
      }
      ContentHash.combineAll(
        Vector(
          keys.digest(trial.key),
          ContentHash.ofString(unit.symbol),
          ContentHash.ofString(frame.id.name),
          ContentHash.ofString(path.clock.name),
          ContentHash.ofString(frame.yAxis.toString),
          ContentHash.of(
            IArray(frame.spec.xMin, frame.spec.yMin, frame.spec.xMax, frame.spec.yMax)
          )
        ) ++ fixes
      )
    }
    new StudyInput(trials, ContentHash.combineAll(hashes))

/** The estimation choice is a domain value, including the Gaussian edge convention. */
enum StudyEstimate[U <: Unit2D] derives CanEqual:
  case Binned()
  case Gaussian(sigma: Sigma[U], edges: EdgePolicy)

  def name: String = this match
    case Binned()               => "binned"
    case Gaussian(sigma, edges) =>
      s"gaussian:${Provenance.Param.Num(sigma.value).render}:$edges"

  def parameters: Vector[(String, Provenance.Param)] = this match
    case Binned()               => Vector("estimator" -> Provenance.Param.Text("binned"))
    case Gaussian(sigma, edges) =>
      Vector(
        "estimator" -> Provenance.Param.Text("gaussian"),
        "sigma"     -> Provenance.Param.Num(sigma.value),
        "edges"     -> Provenance.Param.Text(edges.toString)
      )

enum StudyFailure[K] derives CanEqual:
  case Frame(key: K, underlying: GeometryError)
  case Occupancy(key: K, underlying: SurfaceError)
  case Temporal(key: K, underlying: TemporalStudyError)
  case Estimation(key: K, underlying: EstimateError)
  case Comparison(left: K, right: K, underlying: CompareError)

  def message: String = this match
    case Frame(k, e)         => s"Trial $k: ${e.message}"
    case Temporal(k, e)      => s"Trial $k: ${e.message}"
    case Occupancy(k, e)     => s"Trial $k: ${e.message}"
    case Estimation(k, e)    => s"Trial $k: ${e.message}"
    case Comparison(l, r, e) => s"Trials $l and $r: ${e.message}"

/** A method definition accepts typed P, S and D; registration never erases them to Any.
  * A method author supplies the existing ScoreMean and Contrastable capabilities.
  */
final class StudyMethod[P, U <: Unit2D, S, D](
    val id: DefinitionId,
    val name: String,
    val parameters: P => Vector[(String, Provenance.Param)],
    val comparison: P => Compare[Mass[U], Mass[U], S]
)(using val mean: ScoreMean[S], val difference: Contrastable[S, D])

object StudyMethod:
  /** No hidden parameters; geometry, weighting and estimation live in StudyPlan. */
  def cosine[U <: Unit2D](
      id: DefinitionId
  ): StudyMethod[Unit, U, Similarity, SignedDifference] =
    new StudyMethod(id, "Cosine similarity", _ => Vector.empty, _ => Distribution.cosine[U])

/** A field-level structural difference suitable for a review panel. */
final case class PlanChange(
    field: String,
    before: Vector[Provenance.Param],
    after: Vector[Provenance.Param]
) derives CanEqual

/** One scale retains estimation failures, excluded phase keys, and the full contrast. */
final class StudyScaleResult[K, U <: Unit2D, S, D] private[plan] (
    val estimate: StudyEstimate[U],
    val estimation: Vector[(K, Either[StudyFailure[K], Mass[U]])],
    val excludedPhases: Vector[K],
    val contrast: Either[ContrastError[K], Contrast[K, S, D]]
)
final class StudyResult[K, U <: Unit2D, S, D] private[plan] (
    val input: ArtifactRef[StudyInput[K, U]],
    val description: Vector[(String, Vector[Provenance.Param])],
    val scales: Vector[StudyScaleResult[K, U, S, D]]
)

/** A saved study describes exhaustive matched and different-stimulus controls
  * within each participant. Scale results remain separate; no implicit pooling.
  */
final class StudyPlan[K, U <: Unit2D, P, S, D] private (
    val input: ArtifactRef[StudyInput[K, U]],
    val layout: StudyLayout[K],
    val grid: Grid[U],
    val focalPhase: String,
    val referencePhase: String,
    val weight: Weight,
    val estimates: Vector[StudyEstimate[U]],
    val policy: FailurePolicy,
    val method: StudyMethod[P, U, S, D],
    val parameters: P
)(using unit: UnitLabel[U]):
  def description: Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    Vector(
      "input"         -> Vector(Text(input.digest)),
      "layout"        -> Vector(Text(layout.id.name), Num(layout.id.version.toDouble)),
      "method"        -> Vector(Text(method.id.name), Num(method.id.version.toDouble)),
      "phases"        -> Vector(Text(focalPhase), Text(referencePhase)),
      "weight"        -> Vector(Text(weight.toString)),
      "failurePolicy" -> Vector(Text(policy.render)),
      "frame"         -> Vector(
        Text(grid.frame.id.name),
        Text(unit.symbol),
        Num(grid.frame.spec.xMin),
        Num(grid.frame.spec.yMin),
        Num(grid.frame.spec.xMax),
        Num(grid.frame.spec.yMax),
        Text(grid.frame.yAxis.toString)
      ),
      "grid" -> Vector(Text(grid.id.name), Num(grid.nx.toDouble), Num(grid.ny.toDouble))
    ) ++
      method.parameters(parameters).sortBy(_._1).map { case (k, v) =>
        s"method.$k" -> Vector(v)
      } ++
      estimates.zipWithIndex.map { case (s, i) =>
        s"estimate.$i" -> s.parameters.flatMap { case (k, v) => Vector(Text(k), v) }
      }

  override def equals(other: Any): Boolean = other match
    case that: StudyPlan[?, ?, ?, ?, ?] => description == that.description
    case _                              => false
  override def hashCode: Int = description.hashCode

  def diff(that: StudyPlan[K, U, P, S, D]): Vector[PlanChange] =
    val left  = description.toMap
    val right = that.description.toMap
    (left.keySet ++ right.keySet).toVector.sorted.collect {
      case key if left.get(key) != right.get(key) =>
        PlanChange(key, left.getOrElse(key, Vector.empty), right.getOrElse(key, Vector.empty))
    }

  def prerequisites(available: Option[StudyInput[K, U]]): Vector[PlanError] = available match
    case None => Vector(PlanError.MissingArtifact(input.digest))
    case Some(value) if value.reference != input =>
      Vector(PlanError.ArtifactMismatch(input.digest, value.reference.digest))
    case _ => Vector.empty

  def run(available: StudyInput[K, U]): Either[PlanError, StudyResult[K, U, S, D]] =
    runPrepared(
      available,
      (key, path) => path.occupancy(weight).left.map(StudyFailure.Occupancy(key, _)),
      Vector.empty
    )

  private[plan] def runPrepared(
      available: StudyInput[K, U],
      prepare: (K, Scanpath[U]) => Either[StudyFailure[K], PointMeasure[U]],
      context: Vector[(String, Provenance.Param)]
  ): Either[PlanError, StudyResult[K, U, S, D]] =
    prerequisites(Some(available)).headOption match
      case Some(error) => Left(error)
      case None        =>
        estimates
          .traverse(runScale(available, _, prepare, context))
          .map(new StudyResult(input, description, _))

  private def runScale(
      available: StudyInput[K, U],
      estimate: StudyEstimate[U],
      prepare: (K, Scanpath[U]) => Either[StudyFailure[K], PointMeasure[U]],
      context: Vector[(String, Provenance.Param)]
  ): Either[PlanError, StudyScaleResult[K, U, S, D]] =
    given KeyDigest[K]       = layout.digest
    given Ordering[K]        = layout.ordering
    given ScoreMean[S]       = method.mean
    given Contrastable[S, D] = method.difference
    val cmp                  = method.comparison(parameters)
    val params               = Vector("weight" -> Provenance.Param.Text(weight.toString)) ++
      estimate.parameters.map { case (k, v) => s"estimate.$k" -> v } ++
      method.parameters(parameters).map { case (k, v) => s"method.$k" -> v } ++ context
    EvaluationSpec
      .of(
        method.id.name,
        method.id.version.toString,
        params,
        method.difference.components,
        EvaluationGeometry.onGrid(grid),
        if context.isEmpty then EvaluationTime.OrderFree
        else EvaluationTime.RelativeMicroseconds
      )
      .left
      .map(PlanError.Specification.apply)
      .map { specification =>
        val prepared = available.trials.rows.map { trial =>
          val mass = for
            _ <- Agreement
              .frames(grid.frame, trial.value.frame)
              .left
              .map(StudyFailure.Frame(trial.key, _))
            occupancy <- prepare(trial.key, trial.value)
            mass      <- estimate match
              case StudyEstimate.Binned() =>
                for
                  cells     <- occupancy.binned(grid).left.map(StudyFailure.Frame(trial.key, _))
                  intensity <- Surface
                    .intensity(grid, cells, occupancy.provenance)
                    .left
                    .map(StudyFailure.Occupancy(trial.key, _))
                  result <- intensity.normalised.left.map(StudyFailure.Occupancy(trial.key, _))
                yield result
              case StudyEstimate.Gaussian(sigma, edges) =>
                Smoother
                  .gaussian(sigma, edges)
                  .density(occupancy, grid)
                  .left
                  .map(StudyFailure.Estimation(trial.key, _))
          yield mass
          Trial(trial.key, (), mass)
        }
        val trials    = Trials(prepared.map(t => Trial(t.key, (), t.key -> t.value)))
        val focal     = trials.filterKey(k => layout.phase(k) == focalPhase)
        val reference = trials.filterKey(k => layout.phase(k) == referencePhase)
        val matched   = Pairing
          .between[K, K]
          .sameOn(layout.participant, layout.participant)
          .sameOn(layout.stimulus, layout.stimulus)
          .all
        val controls = Pairing
          .between[K, K]
          .sameOn(layout.participant, layout.participant)
          .differentOn(layout.stimulus, layout.stimulus)
          .all
        def evaluate(design: PairDesign.BetweenDirected[K, K]): Analysis[K, S] =
          evaluatePairs(
            pair(focal, reference, design),
            available.hash,
            EvaluationInfo.comparison(cmp, specification)
          ) { case ((leftKey, a), (rightKey, b)) =>
            a.flatMap(left =>
              b.flatMap(right =>
                cmp
                  .compare(left, right)
                  .left
                  .map(e => StudyFailure.Comparison(leftKey, rightKey, e))
              )
            )
          }.meanByLeft(policy)
        new StudyScaleResult(
          estimate,
          prepared.map(t => t.key -> t.value),
          available.trials.rows
            .filter(t =>
              layout.phase(t.key) != focalPhase && layout.phase(t.key) != referencePhase
            )
            .map(_.key),
          contrast(evaluate(matched), evaluate(controls))
        )
      }

object StudyPlan:
  /** Ordinary within-participant matched/control cosine study, using the same interpreter. */
  def cosine[U <: Unit2D](
      input: ArtifactRef[StudyInput[StudyKey, U]],
      grid: Grid[U],
      focalPhase: String,
      referencePhase: String,
      weight: Weight,
      estimates: Vector[StudyEstimate[U]],
      policy: FailurePolicy
  )(using
      UnitLabel[U]
  ): Either[PlanError, StudyPlan[StudyKey, U, Unit, Similarity, SignedDifference]] =
    of(
      input,
      StudyKey.layout(DefinitionId.studyLayout),
      grid,
      focalPhase,
      referencePhase,
      weight,
      estimates,
      policy,
      StudyMethod.cosine[U](DefinitionId.cosine),
      ()
    )

  def of[K, U <: Unit2D, P, S, D](
      input: ArtifactRef[StudyInput[K, U]],
      layout: StudyLayout[K],
      grid: Grid[U],
      focalPhase: String,
      referencePhase: String,
      weight: Weight,
      estimates: Vector[StudyEstimate[U]],
      policy: FailurePolicy,
      method: StudyMethod[P, U, S, D],
      parameters: P
  )(using UnitLabel[U]): Either[PlanError, StudyPlan[K, U, P, S, D]] =
    if focalPhase.trim.isEmpty || referencePhase.trim.isEmpty || focalPhase == referencePhase
    then Left(PlanError.InvalidPhases(focalPhase, referencePhase))
    else if estimates.isEmpty then Left(PlanError.EmptyScales(0))
    else if estimates.distinct.size != estimates.size then
      Left(PlanError.DuplicateScales(estimates.map(_.name)))
    else
      estimates
        .traverse { estimate =>
          EvaluationSpec
            .of(
              method.id.name,
              method.id.version.toString,
              method.parameters(parameters).map { case (k, v) =>
                s"method.$k" -> v
              } ++
                estimate.parameters.map { case (k, v) => s"estimate.$k" -> v },
              method.difference.components,
              EvaluationGeometry.onGrid(grid),
              EvaluationTime.OrderFree
            )
            .left
            .map(PlanError.Specification.apply)
        }
        .map(_ =>
          new StudyPlan(
            input,
            layout,
            grid,
            focalPhase,
            referencePhase,
            weight,
            estimates,
            policy,
            method,
            parameters
          )
        )
