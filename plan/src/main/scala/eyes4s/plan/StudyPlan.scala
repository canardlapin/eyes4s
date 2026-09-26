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
  val cosine: DefinitionId          = new DefinitionId("eyes4s.cosine", 1)
  val study: DefinitionId           = new DefinitionId("eyes4s.study", 1)
  val studyKey: DefinitionId        = new DefinitionId("eyes4s.study-key", 1)
  val studyLayout: DefinitionId     = new DefinitionId("eyes4s.participant-stimulus-phase", 1)
  val unit: DefinitionId            = new DefinitionId("eyes4s.unit", 1)
  val studyInput: DefinitionId      = new DefinitionId("eyes4s.study-input", 1)
  val trials: DefinitionId          = new DefinitionId("eyes4s.trials", 1)
  val scanpath: DefinitionId        = new DefinitionId("eyes4s.scanpath", 1)
  val admissionLedger: DefinitionId = new DefinitionId("eyes4s.admission-ledger", 1)
  // UI-S3: recording and temporal input payload schemas.
  val recording: DefinitionId          = new DefinitionId("eyes4s.recording", 1)
  val binocularRecording: DefinitionId = new DefinitionId("eyes4s.binocular-recording", 1)
  val recordingInput: DefinitionId     = new DefinitionId("eyes4s.recording-input", 1)
  val temporalStudyInput: DefinitionId = new DefinitionId("eyes4s.temporal-study-input", 1)
  val timeline: DefinitionId           = new DefinitionId("eyes4s.timeline", 1)
  // UI-S4: result archive and score schemas.
  val studyResult: DefinitionId      = new DefinitionId("eyes4s.study-result", 1)
  val similarity: DefinitionId       = new DefinitionId("eyes4s.similarity", 1)
  val measureDistance: DefinitionId  = new DefinitionId("eyes4s.measure-distance", 1)
  val scalar: DefinitionId           = new DefinitionId("eyes4s.scalar", 1)
  val signedDifference: DefinitionId = new DefinitionId("eyes4s.signed-difference", 1)
  def of(name: String, version: Int): Either[PlanError, DefinitionId] =
    if name.trim.isEmpty || version < 1 then Left(PlanError.InvalidDefinition(name, version))
    else Right(new DefinitionId(name, version))

  /** A built-in identity declared outside this companion.
    *
    * New built-in identities are not appended here. Each is declared in the
    * file that introduces it, as a `val` of an object whose name ends in
    * `Definitions` (for example `object KdeDefinitions` beside the KDE
    * method), so parallel changes never edit this companion. The schema
    * registry in `eyes4s-laws` finds every such object on the classpath and
    * requires each identity to be valid under [[of]] and to have exactly one
    * registry entry, with its law and pinned fixture. The identities above
    * stay where they are.
    */
  private[eyes4s] def builtIn(name: String, version: Int): DefinitionId =
    new DefinitionId(name, version)
  // UI-S5: artifact manifests and typed payload references.
  val manifest: DefinitionId        = new DefinitionId("eyes4s.manifest", 1)
  val packedRecording: DefinitionId = new DefinitionId("eyes4s.packed-recording", 1)
  val packedArray: DefinitionId     = new DefinitionId("eyes4s.packed-array", 1)
  // recording and temporal archives
  val recordingResult: DefinitionId = new DefinitionId("eyes4s.recording-result", 1)
  val temporalResult: DefinitionId  = new DefinitionId("eyes4s.temporal-result", 1)

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
  case Schedule(underlying: PairScheduleError)
  case StudyWorkBudget(
      focalTrials: Int,
      referenceTrials: Int,
      scales: Int,
      maximumCandidateVisits: Long
  )
  case ChangedPreparedPlan(method: DefinitionId, layout: DefinitionId)
  case ComparisonWork(underlying: ComparisonWorkError)
  case UnsupportedExecution(method: DefinitionId, capability: ExecutionCapability)
  case MissingAngularScale(scale: Int)
  case Geometry(underlying: GeometryError)
  case InvalidWindowTally(
      outsideScreen: Int,
      outsideWindow: Int,
      total: Int,
      outsideScreenMicros: Long,
      outsideWindowMicros: Long,
      totalMicros: Long
  )

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
    case Schedule(e)            => e.message
    case StudyWorkBudget(focal, reference, scales, maximum) =>
      s"Study with focal=$focal, reference=$reference and scales=$scales exceeds matched/control candidate-visit budget $maximum."
    case ChangedPreparedPlan(method, layout) =>
      s"Prepared study parameters changed for method $method and layout $layout; prepare the revised plan again."
    case ComparisonWork(e)                        => e.message
    case UnsupportedExecution(method, capability) =>
      s"Method ${method.name}@${method.version} executes as $capability and cannot promise bounded comparison work."
    case MissingAngularScale(scale) =>
      s"Scale $scale is declared in degrees, but the plan declares no units-per-degree scale."
    case Geometry(e) => e.message
    case InvalidWindowTally(screen, window, total, screenMicros, windowMicros, totalMicros) =>
      s"A window tally must partition its trial: outsideScreen=$screen and outsideWindow=$window " +
        s"of total=$total, outsideScreenMicros=$screenMicros and outsideWindowMicros=$windowMicros " +
        s"of totalMicros=$totalMicros."

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
      // UI-S3: a source-supported scanpath is also identified by its
      // evidence, so a different source recording or segmentation is a
      // different input even when the summaries agree.
      val source = (path.source, path.sourceRecording, path.sampleSupport) match
        case (Some(ref), Some(recording), Some(support)) =>
          Vector(
            ContentHash.ofString("source:" + ref.value),
            recording.contentHash
          ) ++ support.map(range => ContentHash.ofString(s"${range.from},${range.until}"))
        case _ => Vector.empty
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
        ) ++ fixes ++ source
      )
    }
    new StudyInput(trials, ContentHash.combineAll(hashes))

/** The estimation choice is a domain value, including the Gaussian edge convention. */
enum StudyEstimate[U <: Unit2D] derives CanEqual:
  case Binned()
  case Gaussian(sigma: Sigma[U], edges: EdgePolicy)
  case Anisotropic(sigmaX: Sigma[U], sigmaY: Sigma[U], edges: EdgePolicy)

  def name: String = this match
    case Binned()               => "binned"
    case Gaussian(sigma, edges) =>
      s"gaussian:${Provenance.Param.Num(sigma.value).render}:$edges"
    case Anisotropic(x, y, edges) =>
      s"anisotropic:${Provenance.Param.Num(x.value).render}:${Provenance.Param.Num(y.value).render}:$edges"

  def parameters: Vector[(String, Provenance.Param)] = this match
    case Anisotropic(x, y, edges) =>
      Vector(
        "estimator" -> Provenance.Param.Text("anisotropic"),
        "sigmaX"    -> Provenance.Param.Num(x.value),
        "sigmaY"    -> Provenance.Param.Num(y.value),
        "edges"     -> Provenance.Param.Text(edges.toString)
      )
    case Binned()               => Vector("estimator" -> Provenance.Param.Text("binned"))
    case Gaussian(sigma, edges) =>
      Vector(
        "estimator" -> Provenance.Param.Text("gaussian"),
        "sigma"     -> Provenance.Param.Num(sigma.value),
        "edges"     -> Provenance.Param.Text(edges.toString)
      )

  /** The provenance steps a density estimated at this scale carries after its
    * trial's occupancy: the Gaussian smoothing step and the normalisation gate.
    */
  def provenanceSteps: Vector[Provenance.Step] =
    val normalise = Provenance.Step.text("normalise", "of", "surface")
    this match
      case Anisotropic(x, y, edges) =>
        Vector(
          Provenance.Step(
            "smooth",
            Vector(
              "kernel" -> Provenance.Param.Text("anisotropic"),
              "sigmaX" -> Provenance.Param.Num(x.value),
              "sigmaY" -> Provenance.Param.Num(y.value),
              "edges"  -> Provenance.Param.Text(edges.toString)
            )
          ),
          normalise
        )
      case Binned()               => Vector(normalise)
      case Gaussian(sigma, edges) =>
        Vector(
          Provenance.Step(
            "smooth",
            Vector(
              "kernel" -> Provenance.Param.Text("gaussian"),
              "sigma"  -> Provenance.Param.Num(sigma.value),
              "edges"  -> Provenance.Param.Text(edges.toString)
            )
          ),
          normalise
        )

enum StudyFailure[K] derives CanEqual:
  case Frame(key: K, underlying: GeometryError)
  case Occupancy(key: K, underlying: SurfaceError)
  case Temporal(key: K, underlying: TemporalStudyError)
  case Estimation(key: K, underlying: EstimateError)
  case Comparison(left: K, right: K, underlying: CompareError)

  /** The trial's fixations lie outside the plan's analysis window: all of
    * them, or any of them under [[OffWindowPolicy.FailTrial]].
    */
  case OffWindow(key: K, tally: WindowTally)

  def message: String = this match
    case Frame(k, e)         => s"Trial $k: ${e.message}"
    case Temporal(k, e)      => s"Trial $k: ${e.message}"
    case Occupancy(k, e)     => s"Trial $k: ${e.message}"
    case Estimation(k, e)    => s"Trial $k: ${e.message}"
    case Comparison(l, r, e) => s"Trials $l and $r: ${e.message}"
    case OffWindow(k, tally) =>
      s"Trial $k has ${tally.outside} of ${tally.total} fixations outside the analysis window" +
        (if tally.allOutside then "; its map would be empty."
         else "; the plan fails such trials.")

object StudyFailure:
  /** The trial keys a failure names, in operand order. */
  def keys[K](failure: StudyFailure[K]): Vector[K] = failure match
    case StudyFailure.Frame(k, _)         => Vector(k)
    case StudyFailure.Temporal(k, _)      => Vector(k)
    case StudyFailure.Occupancy(k, _)     => Vector(k)
    case StudyFailure.Estimation(k, _)    => Vector(k)
    case StudyFailure.Comparison(l, r, _) => Vector(l, r)
    case StudyFailure.OffWindow(k, _)     => Vector(k)

/** Typed evidence of how a method's comparison executes. A synchronous closure
  * runs whole per pair; only a [[BoundedCompare]] can be declared bounded, so
  * the reported capability is never inferred from a name or a flag.
  */
enum MethodExecution[-P, U <: Unit2D, +S]:
  case Synchronous(comparison: P => Compare[Mass[U], Mass[U], S])
  case Bounded(comparison: P => BoundedCompare[Mass[U], Mass[U], S])

  def compare: P => Compare[Mass[U], Mass[U], S] = this match
    case Synchronous(comparison) => comparison
    case Bounded(comparison)     => comparison

  def capability: ExecutionCapability = this match
    case Synchronous(_) => ExecutionCapability.SynchronousWholeOperation
    case Bounded(_)     => ExecutionCapability.BoundedComparison

/** A method definition accepts typed P, S and D; registration never erases them to Any.
  * A method author supplies the existing ScoreMean and Contrastable capabilities.
  */
final class StudyMethod[P, U <: Unit2D, S, D](
    val id: DefinitionId,
    val name: String,
    val parameters: P => Vector[(String, Provenance.Param)],
    val execution: MethodExecution[P, U, S],
    val descriptor: Option[MethodDescriptor[P, S, D]]
)(using val mean: ScoreMean[S], val difference: Contrastable[S, D]):
  /** The ordinary extension route: a whole synchronous comparison per pair. */
  def this(
      id: DefinitionId,
      name: String,
      parameters: P => Vector[(String, Provenance.Param)],
      comparison: P => Compare[Mass[U], Mass[U], S],
      descriptor: Option[MethodDescriptor[P, S, D]] = None
  )(using ScoreMean[S], Contrastable[S, D]) =
    this(id, name, parameters, MethodExecution.Synchronous(comparison), descriptor)

  val comparison: P => Compare[Mass[U], Mass[U], S] = execution.compare
  def capability: ExecutionCapability               = execution.capability

object StudyMethod:
  /** No hidden parameters; geometry, weighting and estimation live in StudyPlan. */
  def cosine[U <: Unit2D](
      id: DefinitionId
  ): StudyMethod[Unit, U, Similarity, SignedDifference] =
    new StudyMethod[Unit, U, Similarity, SignedDifference](
      id,
      "Cosine similarity",
      _ => Vector.empty,
      MethodExecution.Bounded(_ => Distribution.cosine[U]),
      Some(MethodDescriptor.cosine[U](id))
    )

/** A field-level structural difference suitable for a review panel. */
final case class PlanChange(
    field: String,
    before: Vector[Provenance.Param],
    after: Vector[Provenance.Param]
) derives CanEqual
object PlanChange:
  /** Field-level differences between two plan descriptions, in field order. */
  def between(
      before: Vector[(String, Vector[Provenance.Param])],
      after: Vector[(String, Vector[Provenance.Param])]
  ): Vector[PlanChange] =
    val left  = before.toMap
    val right = after.toMap
    (left.keySet ++ right.keySet).toVector.sorted.collect {
      case key if left.get(key) != right.get(key) =>
        PlanChange(key, left.getOrElse(key, Vector.empty), right.getOrElse(key, Vector.empty))
    }

/** Both directed pair analyses of one scale and their by-focal reductions,
  * with key and failure types intact. Each reduction was derived from the
  * source beside it; [[StudyAnalyses.of]] checks that identity.
  */
final class StudyAnalyses[K, S] private[plan] (
    val matchedSource: DirectedPairwiseAnalysis[K, K, StudyFailure[K], S],
    val matched: Analysis[K, S],
    val controlSource: DirectedPairwiseAnalysis[K, K, StudyFailure[K], S],
    val control: Analysis[K, S]
):
  def source(design: StudyDesign): DirectedPairwiseAnalysis[K, K, StudyFailure[K], S] =
    design match
      case StudyDesign.Matched => matchedSource
      case StudyDesign.Control => controlSource
  def reduced(design: StudyDesign): Analysis[K, S] = design match
    case StudyDesign.Matched => matched
    case StudyDesign.Control => control

object StudyAnalyses:
  /** Checked: each reduced analysis must have been reduced from the typed
    * source supplied beside it (the very same value), so an analysis over an
    * undirected or foreign source is refused by design.
    */
  def of[K, S](
      matchedSource: DirectedPairwiseAnalysis[K, K, StudyFailure[K], S],
      matched: Analysis[K, S],
      controlSource: DirectedPairwiseAnalysis[K, K, StudyFailure[K], S],
      control: Analysis[K, S]
  ): Either[StudyResultError[K], StudyAnalyses[K, S]] =
    if !(matched.source eq matchedSource) then
      Left(StudyResultError.SourceIdentity(StudyDesign.Matched))
    else if !(control.source eq controlSource) then
      Left(StudyResultError.SourceIdentity(StudyDesign.Control))
    else Right(new StudyAnalyses(matchedSource, matched, controlSource, control))

/** One scale retains estimation failures, excluded phase keys, both typed
  * pair analyses with their reductions, and the full contrast.
  */
final class StudyScaleResult[K, U <: Unit2D, S, D] private[plan] (
    val estimate: StudyEstimate[U],
    val estimation: Vector[(K, Either[StudyFailure[K], Mass[U]])],
    val excludedPhases: Vector[K],
    val analyses: StudyAnalyses[K, S],
    val contrast: Either[ContrastError[K], Contrast[K, S, D]]
)
final class StudyResult[K, U <: Unit2D, S, D] private[plan] (
    val input: ArtifactRef[StudyInput[K, U]],
    val description: Vector[(String, Vector[Provenance.Param])],
    val scales: Vector[StudyScaleResult[K, U, S, D]]
)

/** Refusals while rebuilding a completed result from stored parts. A scale's
  * own refusal is wrapped in [[Scale]] with the scale index; every other case
  * names the trial keys, pair keys, design or declared values that disagree.
  */
enum StudyResultError[K] derives CanEqual:
  case Description(field: String, found: Vector[Provenance.Param])
  case InputMismatch(reference: String, described: Vector[Provenance.Param])
  case LayoutMismatch(expected: DefinitionId, described: DefinitionId)
  case ScaleCount(declared: Int, found: Int)
  case ScaleEstimate(declared: Vector[Provenance.Param], found: Vector[Provenance.Param])
  case MassGrid(key: K, declared: Vector[Provenance.Param], found: Vector[Provenance.Param])
  case MassProvenance(key: K, expected: Vector[Provenance.Step], found: Vector[Provenance.Step])
  case FailureKey(key: K, failure: StudyFailure[K])
  case PairFailure(left: K, right: K, failure: StudyFailure[K])
  case OrphanKey(key: K)
  case OrphanPair(left: K, right: K)
  case SourceIdentity(design: StudyDesign)
  case ContrastAnalyses(design: StudyDesign)
  case ProvenanceInputs(design: StudyDesign, declared: String, expected: String)
  case MissingSpecification(design: StudyDesign, evaluation: EvaluationInfo)
  case SpecificationMethod(
      design: StudyDesign,
      expected: DefinitionId,
      method: String,
      revision: String
  )
  case SpecificationParameters(
      design: StudyDesign,
      expected: Vector[(String, Provenance.Param)],
      found: Vector[(String, Provenance.Param)]
  )
  case Policy(design: StudyDesign, declared: String, found: FailurePolicy)
  case Phase(key: K, expected: String, found: String)
  case Reconstruction(underlying: ReconstructionError[K])
  case Scale(index: Int, underlying: StudyResultError[K])
  case SpecificationTime(design: StudyDesign, expected: EvaluationTime, found: EvaluationTime)

  def message: String = this match
    case Description(field, found) =>
      s"Description field $field is missing or malformed: $found."
    case InputMismatch(reference, described) =>
      s"Result refers to input $reference but its description declares $described."
    case LayoutMismatch(expected, described) =>
      s"Result is reconstructed for layout ${expected.name}@${expected.version} but describes ${described.name}@${described.version}."
    case ScaleCount(declared, found) =>
      s"Description declares $declared estimation scales; the result stores $found. " +
        "A completed result stores every declared scale; partial work is not a result."
    case ScaleEstimate(declared, found) =>
      s"Scale is declared as $declared but stores estimate $found."
    case MassGrid(key, declared, found) =>
      s"Trial $key has a density on grid $found; the plan grid is $declared."
    case MassProvenance(key, expected, found) =>
      s"Trial $key has density provenance ${found.map(_.render)}; the scale's estimator derives ${expected.map(_.render)}."
    case FailureKey(key, failure) =>
      s"Trial $key stores a failure naming another trial: ${failure.message}"
    case PairFailure(left, right, failure) =>
      s"Pair ($left, $right) stores a failure naming other trials: ${failure.message}"
    case OrphanKey(key)          => s"Key $key is referenced but no trial was estimated for it."
    case OrphanPair(left, right) =>
      s"Pair ($left, $right) refers to a trial that was not estimated."
    case SourceIdentity(design) =>
      s"The $design reduction was not reduced from the $design pair analysis supplied with it."
    case ContrastAnalyses(design) =>
      s"The contrast's $design analysis is not the scale's $design reduction."
    case ProvenanceInputs(design, declared, expected) =>
      s"The $design evaluation provenance names inputs $declared; the study input is $expected."
    case MissingSpecification(design, evaluation) =>
      s"The $design evaluator '${evaluation.name}' carries no method specification."
    case SpecificationMethod(design, expected, method, revision) =>
      s"The $design specification names $method@$revision; the plan method is ${expected.name}@${expected.version}."
    case SpecificationParameters(design, expected, found) =>
      s"The $design specification declares parameters $found; the plan description gives $expected."
    case Policy(design, declared, found) =>
      s"The $design reduction used ${found.render}; the plan declares $declared."
    case Phase(key, expected, found) =>
      s"Trial $key is in phase '$found' where the plan places phase '$expected'."
    case Reconstruction(underlying)                 => underlying.message
    case Scale(index, underlying)                   => s"Scale $index: ${underlying.message}"
    case SpecificationTime(design, expected, found) =>
      s"The $design specification orders time as $found; the plan's execution context gives $expected."

object StudyScaleResult:
  /** Checked reconstruction of one scale: every density carries the provenance
    * the scale's estimator derives, every stored failure names the row it sits
    * in, the contrast (when present) is over the scale's own reductions, and
    * every key the analyses refer to was estimated at this scale.
    */
  def reconstruct[K, U <: Unit2D, S, D](
      estimate: StudyEstimate[U],
      estimation: Vector[(K, Either[StudyFailure[K], Mass[U]])],
      excludedPhases: Vector[K],
      analyses: StudyAnalyses[K, S],
      contrast: Either[ContrastError[K], Contrast[K, S, D]]
  ): Either[StudyResultError[K], StudyScaleResult[K, U, S, D]] =
    val estimated                         = estimation.map(_._1).toSet
    def known(key: K): Boolean            = estimated.contains(key)
    val expectedSteps                     = estimate.provenanceSteps
    val rows: Option[StudyResultError[K]] = estimation.collectFirst {
      case (key, Left(failure)) if StudyFailure.keys(failure) != Vector(key) =>
        StudyResultError.FailureKey(key, failure)
      case (key, Right(mass)) if mass.provenance.steps != expectedSteps =>
        StudyResultError.MassProvenance(key, expectedSteps, mass.provenance.steps)
    }
    val contrastIdentity: Option[StudyResultError[K]] = contrast match
      case Right(c) if !(c.matched eq analyses.matched) =>
        Some(StudyResultError.ContrastAnalyses(StudyDesign.Matched))
      case Right(c) if !(c.control eq analyses.control) =>
        Some(StudyResultError.ContrastAnalyses(StudyDesign.Control))
      case _ => None
    val references: Vector[StudyResultError[K]] = StudyDesign.values.toVector.flatMap {
      design =>
        val source = analyses.source(design)
        source.rows.flatMap { row =>
          if !known(row.left) || !known(row.right) then
            Some(StudyResultError.OrphanPair(row.left, row.right))
          else
            row.result match
              case Left(failure)
                  if !StudyFailure.keys(failure).forall(k => k == row.left || k == row.right) =>
                Some(StudyResultError.PairFailure(row.left, row.right, failure))
              case _ => None
        } ++
          (source.diagnostics.unmatchedLeft ++ source.diagnostics.unmatchedRight ++
            analyses.reduced(design).entries.map(_.key)).collect {
            case key if !known(key) => StudyResultError.OrphanKey(key)
          }
    } ++ contrast.toOption.toVector.flatMap(_.rows.collect {
      case row if !known(row.key) => StudyResultError.OrphanKey(row.key)
    })
    rows
      .orElse(contrastIdentity)
      .orElse(excludedPhases.collectFirst {
        case key if !known(key) => StudyResultError.OrphanKey(key)
      })
      .orElse(references.headOption)
      .toLeft(new StudyScaleResult(estimate, estimation, excludedPhases, analyses, contrast))

object StudyResult:
  /** Checked reconstruction of a completed result from its identity and scales.
    * The description must declare this layout and exactly the stored scales,
    * with the same estimator parameters in the same order; every density must
    * lie on the declared plan grid; both pair analyses of every scale must
    * name the study input as their provenance inputs, carry the plan method's
    * specification with the described weight, estimator and method parameters,
    * reduce under the described failure policy, and pair a focal-phase trial
    * with a reference-phase trial under the supplied layout; every excluded
    * key must lie outside both phases.
    */
  def reconstruct[K, U <: Unit2D, S, D](
      input: ArtifactRef[StudyInput[K, U]],
      layout: StudyLayout[K],
      description: Vector[(String, Vector[Provenance.Param])],
      scales: Vector[StudyScaleResult[K, U, S, D]]
  ): Either[StudyResultError[K], StudyResult[K, U, S, D]] =
    reconstruct(input, layout, description, scales, Vector.empty)

  /** Checked reconstruction of a result executed under a provenance
    * `context`, as every cell of a temporal study is (see
    * `TemporalStudyPlan.provenanceContext`): the checks above, with the
    * context's parameters expected in every evaluation specification beside
    * the described ones, and the specification's time order the one the
    * context implies (`OrderFree` without a context, `RelativeMicroseconds`
    * with one).
    */
  def reconstruct[K, U <: Unit2D, S, D](
      input: ArtifactRef[StudyInput[K, U]],
      layout: StudyLayout[K],
      description: Vector[(String, Vector[Provenance.Param])],
      scales: Vector[StudyScaleResult[K, U, S, D]],
      context: Vector[(String, Provenance.Param)]
  ): Either[StudyResultError[K], StudyResult[K, U, S, D]] =
    import Provenance.Param.*
    val fields = description.toMap
    def field(name: String): Either[StudyResultError[K], Vector[Provenance.Param]] =
      fields.get(name).toRight(StudyResultError.Description(name, Vector.empty))
    def text(name: String): Either[StudyResultError[K], String] = field(name).flatMap {
      case Vector(Text(value)) => Right(value)
      case other               => Left(StudyResultError.Description(name, other))
    }
    def definition(name: String): Either[StudyResultError[K], DefinitionId] =
      field(name).flatMap {
        case found @ Vector(Text(id), Num(version)) if version.isWhole =>
          DefinitionId
            .of(id, version.toInt)
            .left
            .map(_ => StudyResultError.Description(name, found))
        case other => Left(StudyResultError.Description(name, other))
      }
    val declared = description.collect {
      case (name, params) if name.startsWith("estimate.") =>
        name.drop("estimate.".length).toIntOption -> params
    }
    val methodParameters = description.collect {
      case (name, Vector(value)) if name.startsWith("method.") =>
        name -> value
    }
    for
      described <- field("input")
      _         <- Either.cond(
        described == Vector(Text(input.digest)),
        (),
        StudyResultError.InputMismatch(input.digest, described)
      )
      describedLayout <- definition("layout")
      _               <- Either.cond(
        describedLayout == layout.id,
        (),
        StudyResultError.LayoutMismatch(layout.id, describedLayout)
      )
      method <- definition("method")
      grid   <- field("grid")
      weight <- text("weight")
      policy <- text("failurePolicy")
      phases <- field("phases").flatMap {
        case Vector(Text(focal), Text(reference)) => Right(focal -> reference)
        case other => Left(StudyResultError.Description("phases", other))
      }
      _ <- Either.cond(
        declared.size == scales.size && declared.map(_._1) == scales.indices.map(Some(_)),
        (),
        StudyResultError.ScaleCount(declared.size, scales.size)
      )
      _ <- scales.zipWithIndex.traverse { case (scale, index) =>
        checkScale(
          scale,
          input.digest,
          layout,
          declared(index)._2,
          grid,
          weight,
          methodParameters,
          method,
          policy,
          phases,
          context
        ).left
          .map(StudyResultError.Scale(index, _))
      }
    yield new StudyResult(input, description, scales)

  private def checkScale[K, U <: Unit2D, S, D](
      scale: StudyScaleResult[K, U, S, D],
      inputDigest: String,
      layout: StudyLayout[K],
      declaredEstimate: Vector[Provenance.Param],
      grid: Vector[Provenance.Param],
      weight: String,
      methodParameters: Vector[(String, Provenance.Param)],
      method: DefinitionId,
      policy: String,
      phases: (String, String),
      context: Vector[(String, Provenance.Param)]
  ): Either[StudyResultError[K], Unit] =
    import Provenance.Param.*
    val estimate = scale.estimate.parameters.flatMap { case (k, v) => Vector(Text(k), v) }
    val expectedParameters =
      (Vector("weight" -> Text(weight)) ++
        scale.estimate.parameters.map { case (k, v) => s"estimate.$k" -> v } ++
        methodParameters ++ context).sortBy(_._1)
    val expectedTime =
      if context.isEmpty then EvaluationTime.OrderFree else EvaluationTime.RelativeMicroseconds
    val (focal, reference)                                               = phases
    def analysis(design: StudyDesign): Either[StudyResultError[K], Unit] =
      val source  = scale.analyses.source(design)
      val reduced = scale.analyses.reduced(design)
      for
        _ <- Either.cond(
          source.provenance.inputs.render == inputDigest,
          (),
          StudyResultError.ProvenanceInputs(
            design,
            source.provenance.inputs.render,
            inputDigest
          )
        )
        spec <- source.evaluation.specification.toRight(
          StudyResultError.MissingSpecification(design, source.evaluation)
        )
        _ <- Either.cond(
          spec.method == method.name && spec.revision == method.version.toString,
          (),
          StudyResultError.SpecificationMethod(design, method, spec.method, spec.revision)
        )
        _ <- Either.cond(
          spec.parameters == expectedParameters,
          (),
          StudyResultError.SpecificationParameters(design, expectedParameters, spec.parameters)
        )
        _ <- Either.cond(
          spec.time == expectedTime,
          (),
          StudyResultError.SpecificationTime(design, expectedTime, spec.time)
        )
        _ <- Either.cond(
          reduced.diagnostics.policy.render == policy,
          (),
          StudyResultError.Policy(design, policy, reduced.diagnostics.policy)
        )
        _ <- source.rows
          .collectFirst {
            case row if layout.phase(row.left) != focal =>
              StudyResultError.Phase(row.left, focal, layout.phase(row.left))
            case row if layout.phase(row.right) != reference =>
              StudyResultError.Phase(row.right, reference, layout.phase(row.right))
          }
          .toLeft(())
      yield ()
    if declaredEstimate != estimate then
      Left(StudyResultError.ScaleEstimate(declaredEstimate, estimate))
    else
      scale.estimation
        .collectFirst {
          case (key, Right(mass)) if gridParams(mass.grid) != grid =>
            StudyResultError.MassGrid(key, grid, gridParams(mass.grid))
        }
        .toLeft(())
        .flatMap(_ => analysis(StudyDesign.Matched))
        .flatMap(_ => analysis(StudyDesign.Control))
        .flatMap(_ =>
          scale.excludedPhases
            .collectFirst {
              case key if layout.phase(key) == focal || layout.phase(key) == reference =>
                StudyResultError
                  .Phase(key, "neither " + focal + " nor " + reference, layout.phase(key))
            }
            .toLeft(())
        )

  private def gridParams[U <: Unit2D](grid: Grid[U]): Vector[Provenance.Param] =
    Vector(
      Provenance.Param.Text(grid.id.name),
      Provenance.Param.Num(grid.nx.toDouble),
      Provenance.Param.Num(grid.ny.toDouble)
    )

/** A saved study describes exhaustive matched and different-stimulus controls
  * within each participant. Scale results remain separate; no implicit pooling.
  *
  * `geometry` says where maps live (the whole admission frame, or a half-open
  * window of it with an off-window policy); `scales` are the estimation scales
  * as declared, in frame units or in degrees, and `estimates` are the same
  * scales resolved to frame units through the plan's one `angularScale`.
  */
final class StudyPlan[K, U <: Unit2D, P, S, D] private (
    val input: ArtifactRef[StudyInput[K, U]],
    val layout: StudyLayout[K],
    val geometry: StudyGeometry[U],
    val focalPhase: String,
    val referencePhase: String,
    val weight: Weight,
    val scales: Vector[StudyScale[U]],
    val angularScale: Option[LinearAngularScale[U]],
    val estimates: Vector[StudyEstimate[U]],
    val policy: FailurePolicy,
    val method: StudyMethod[P, U, S, D],
    val parameters: P
)(using unit: UnitLabel[U]):
  /** The grid every density lies on. */
  def grid: Grid[U] = geometry.grid

  /** True when the plan means exactly what a version-1 saved study meant:
    * the whole admission frame and scales declared in frame units.
    */
  def isVersion1: Boolean =
    geometry.isInstanceOf[StudyGeometry.WholeFrame[?]] && angularScale.isEmpty &&
      scales.forall {
        case StudyScale.Native(_)  => true
        case StudyScale.Angular(_) => false
      }

  def inspect: Either[DescriptorError, RecipeInspection] = RecipeDescriptors.study(this)

  /** Typed availability report; see [[Preflight.study]]. */
  def preflight(
      available: Option[StudyInput[K, U]],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): StudyReport[K, U] = Preflight.study(this, available, budget)

  /** Fields in the version-1 order; a later field appears only when the plan
    * departs from the version-1 meaning, so an unchanged plan keeps its
    * description, its equality and the results it reconstructs.
    */
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
      } ++
      StudyWindowing.description(geometry) ++
      angularScale.toVector.map(s =>
        "angularScale" -> Vector(Text(s.frame.id.name), Num(s.unitsPerDegree))
      ) ++
      scales.zipWithIndex.collect { case (s @ StudyScale.Angular(_), i) =>
        s"scale.$i" -> (Text("degrees") +: s.angularParameters.flatMap { case (k, v) =>
          Vector(Text(k), v)
        })
      }

  override def equals(other: Any): Boolean = other match
    case that: StudyPlan[?, ?, ?, ?, ?] => description == that.description
    case _                              => false
  override def hashCode: Int = description.hashCode

  def diff(that: StudyPlan[K, U, P, S, D]): Vector[PlanChange] =
    PlanChange.between(description, that.description)

  def prerequisites(available: Option[StudyInput[K, U]]): Vector[PlanError] = available match
    case None => Vector(PlanError.MissingArtifact(input.digest))
    case Some(value) if value.reference != input =>
      Vector(PlanError.ArtifactMismatch(input.digest, value.reference.digest))
    case _ => Vector.empty

  /** Per-trial counts of fixations outside the plan's analysis window, in
    * input order, for every trial in the admission frame; empty for a
    * whole-frame plan. Derived from the input and the plan alone.
    */
  def windowTallies(available: StudyInput[K, U]): Vector[(K, WindowTally)] =
    StudyWindowing.tallies(geometry, available.trials.rows.map(t => t.key -> t.value))

  def run(available: StudyInput[K, U]): Either[PlanError, StudyResult[K, U, S, D]] =
    prepare(available).flatMap(_.run)

  def prepare(
      available: StudyInput[K, U],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[PlanError, PreparedStudy[K, U, P, S, D]] =
    prerequisites(Some(available)).headOption match
      case Some(error) => Left(error)
      case None        => PreparedStudy.build(this, available, budget)

  private[plan] def runPrepared(
      available: StudyInput[K, U],
      prepare: (K, Scanpath[U]) => Either[StudyFailure[K], PointMeasure[U]],
      context: Vector[(String, Provenance.Param)]
  ): Either[PlanError, StudyResult[K, U, S, D]] =
    this.prepare(available).flatMap(_.execute(prepare, context))

  /** The same plan with other phases and weighting, as a temporal repetition
    * runs it; geometry and scales are kept.
    */
  private[plan] def withPhases(
      focal: String,
      reference: String,
      weighting: Weight
  ): Either[PlanError, StudyPlan[K, U, P, S, D]] =
    StudyPlan.configure(
      input,
      layout,
      geometry,
      focal,
      reference,
      weighting,
      scales,
      angularScale,
      policy,
      method,
      parameters
    )

  /** One scale's method specification; identical for pure and resumable execution. */
  private[plan] def specification(
      estimate: StudyEstimate[U],
      context: Vector[(String, Provenance.Param)]
  ): Either[PlanError, EvaluationSpec] =
    val params = Vector("weight" -> Provenance.Param.Text(weight.toString)) ++
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

  /** One trial's density at one scale; a whole operation per trial. */
  private[plan] def estimateTrial(
      work: PreparedStudy[K, U, P, S, D],
      estimate: StudyEstimate[U],
      prepare: (K, Scanpath[U]) => Either[StudyFailure[K], PointMeasure[U]],
      index: Int
  ): (K, Either[StudyFailure[K], Mass[U]]) =
    val trial = work.input.trials.rows(index)
    val mass  = for
      _         <- work.frameChecks(index)
      _         <- work.windowChecks(index)
      whole     <- prepare(trial.key, trial.value)
      occupancy <- StudyWindowing.restrict(geometry, trial.key, whole)
      mass      <- estimate match
        case StudyEstimate.Anisotropic(x, y, edges) =>
          Smoother
            .anisotropic(x, y, edges)
            .density(occupancy, grid)
            .left
            .map(StudyFailure.Estimation(trial.key, _))
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
    trial.key -> mass

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

  /** The version-1 study: the whole frame of `grid`, scales in frame units. */
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
    configure(
      input,
      layout,
      StudyGeometry.WholeFrame(grid),
      focalPhase,
      referencePhase,
      weight,
      estimates.map(StudyScale.Native(_)),
      None,
      policy,
      method,
      parameters
    )

  /** A study with an explicit geometry and scales. An angular scale is
    * resolved through `angularScale`, whose frame must be the geometry's
    * admission frame: one units-per-degree value per plan.
    */
  def configure[K, U <: Unit2D, P, S, D](
      input: ArtifactRef[StudyInput[K, U]],
      layout: StudyLayout[K],
      geometry: StudyGeometry[U],
      focalPhase: String,
      referencePhase: String,
      weight: Weight,
      scales: Vector[StudyScale[U]],
      angularScale: Option[LinearAngularScale[U]],
      policy: FailurePolicy,
      method: StudyMethod[P, U, S, D],
      parameters: P
  )(using UnitLabel[U]): Either[PlanError, StudyPlan[K, U, P, S, D]] =
    for
      _ <- Either.cond(
        focalPhase.trim.nonEmpty && referencePhase.trim.nonEmpty && focalPhase != referencePhase,
        (),
        PlanError.InvalidPhases(focalPhase, referencePhase)
      )
      _ <- Either.cond(scales.nonEmpty, (), PlanError.EmptyScales(0))
      _ <- angularScale.traverse(s =>
        Agreement.frames(s.frame, geometry.admission).left.map(PlanError.Geometry.apply)
      )
      estimates <- scales.zipWithIndex.traverse { case (scale, index) =>
        scale.resolve(angularScale, index)
      }
      _ <- Either.cond(
        scales.distinct.size == scales.size && estimates.distinct.size == estimates.size,
        (),
        PlanError.DuplicateScales(estimates.map(_.name))
      )
      _ <- estimates.traverse { estimate =>
        EvaluationSpec
          .of(
            method.id.name,
            method.id.version.toString,
            method.parameters(parameters).map { case (k, v) =>
              s"method.$k" -> v
            } ++
              estimate.parameters.map { case (k, v) => s"estimate.$k" -> v },
            method.difference.components,
            EvaluationGeometry.onGrid(geometry.grid),
            EvaluationTime.OrderFree
          )
          .left
          .map(PlanError.Specification.apply)
      }
    yield new StudyPlan(
      input,
      layout,
      geometry,
      focalPhase,
      referencePhase,
      weight,
      scales,
      angularScale,
      estimates,
      policy,
      method,
      parameters
    )
