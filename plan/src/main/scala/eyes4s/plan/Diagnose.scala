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

import cats.data.NonEmptyVector
import eyes4s.aoi.AoiError
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.surface.*
import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror

/** The projection of one typed error family into diagnostics: the single way
  * an application turns an error, refusal or finding into a [[Diagnostic]].
  *
  * An instance exists for every public error enum of the shipped modules;
  * `Diagnostic.of(error)` finds it. `K` is the key type of the trials the
  * diagnostics name: `Nothing` for a family that names none, the study key
  * for a keyed family, and [[ErasedKey]] where the codec carries an error of a
  * key schema it cannot state statically. The instances for the codec's own
  * families are in `eyes4s.codec.CodecDiagnostics`, for io's in
  * `eyes4s.io.IoDiagnostics` and for the laws module's in
  * `eyes4s.laws.LawsDiagnostics`; import their `given`s. The trait is
  * sealed: every instance projects a cataloged family.
  */
sealed trait Diagnose[-E, +K]:
  /** The code table of this family. */
  def family: DiagnosticFamily

  def apply(error: E): Diagnostic[K]

object Diagnose:
  import DiagnosticCatalog as C

  private[eyes4s] def instance[E, K](of: DiagnosticFamily)(
      f: E => Diagnostic[K]
  ): Diagnose[E, K] =
    new Diagnose[E, K]:
      val family: DiagnosticFamily       = of
      def apply(error: E): Diagnostic[K] = f(error)

  // ---------------------------------------------------------------- study, recording, admission

  given plan: Diagnose[PlanError, Nothing]            = instance(C.plan)(Projections.plan)
  given studyFailure[K]: Diagnose[StudyFailure[K], K] =
    instance(C.studyFailure)(Projections.failure[K])
  given studyResult[K]: Diagnose[StudyResultError[K], K] =
    instance(C.studyResult)(Projections.result[K])
  given temporal: Diagnose[TemporalStudyError, Nothing] =
    instance(C.temporal)(Projections.temporal)
  given recordingPlan: Diagnose[RecordingPlanError, Nothing] =
    instance(C.recordingPlan)(Projections.recordingPlan)
  given recordingInput: Diagnose[RecordingInputError, Nothing] =
    instance(C.recordingInput)(Projections.recordingInput)
  given recordingResult: Diagnose[RecordingResultError, Nothing] =
    instance(C.recordingResult)(Projections.recordingResult)
  given temporalResult[K]: Diagnose[TemporalResultError[K], K] =
    instance(C.temporalResult)(Projections.temporalResult[K])
  given reduction[K]: Diagnose[ReductionError[K], K] =
    instance(C.reduction)(Projections.reduction[K])
  given reconstruction[K]: Diagnose[ReconstructionError[K], K] =
    instance(C.reconstruction)(Projections.reconstruction[K])
  given contrast[K]: Diagnose[ContrastError[K], K] =
    instance(C.contrast)(Projections.contrast[K])
  given contrastRow[K]: Diagnose[ContrastRowError[K], K] =
    instance(C.contrastRow)(Projections.contrastRow[K])
  given studyFinding[K, U <: Unit2D]: Diagnose[StudyFinding[K, U], K] =
    instance(C.studyFinding)(Projections.studyFinding[K, U])
  given recordingFinding: Diagnose[RecordingFinding, Nothing] =
    instance(C.recordingFinding)(Projections.recordingFinding)
  given temporalFinding[K, U <: Unit2D]: Diagnose[TemporalFinding[K, U], K] =
    instance(C.temporalFinding)(Projections.temporalFinding[K, U])
  given budget: Diagnose[BudgetError, Nothing]       = instance(C.budget)(Projections.budget)
  given preflight[K]: Diagnose[PreflightError[K], K] =
    instance(C.preflight)(Projections.preflight[K])
  given analysisFinding[K]: Diagnose[AnalysisFinding[K], K] =
    instance(C.analysisFinding)(Projections.analysisFinding[K])
  given admissionReason: Diagnose[AdmissionReason, Nothing] =
    instance(C.admissionReason)(Projections.admissionReason)
  given quarantine: Diagnose[QuarantineCause, Nothing] =
    instance(C.quarantine)(Projections.quarantine)
  given admission: Diagnose[AdmissionError, Nothing] =
    instance(C.admission)(Projections.admission)
  given inventory: Diagnose[InventoryError, Nothing] =
    instance(C.inventory)(Projections.inventory)
  given inspection[K]: Diagnose[InspectionError[K], K] =
    instance(C.inspection)(Projections.inspection[K])

  /** A ledger refusal carries the admission code of its error, with the
    * trials it concerns named by key and linked to their records.
    */
  given ledgerRefusal[K]: Diagnose[LedgerRefusal[K], K] =
    instance(C.admission)(Projections.ledgerRefusal[K])

  // ---------------------------------------------------------------- lower-level causes

  given contrastCompatibility: Diagnose[ContrastCompatibilityError, Nothing] =
    instance(C.contrastCompatibility)(CauseDiagnostics.contrastCompatibility)
  given difference: Diagnose[DifferenceError, Nothing] =
    instance(C.difference)(CauseDiagnostics.difference)
  given scoreMean: Diagnose[ScoreMeanError, Nothing] =
    instance(C.scoreMean)(CauseDiagnostics.scoreMean)
  given evaluationSpec: Diagnose[EvaluationSpecError, Nothing] =
    instance(C.evaluationSpec)(CauseDiagnostics.evaluationSpecError)
  given pairSchedule: Diagnose[PairScheduleError, Nothing] =
    instance(C.pairSchedule)(CauseDiagnostics.pairSchedule)
  given comparisonWork: Diagnose[ComparisonWorkError, Nothing] =
    instance(C.comparisonWork)(CauseDiagnostics.comparisonWork)
  given compare: Diagnose[CompareError, Nothing] = instance(C.compare)(CauseDiagnostics.compare)
  given comparisonValue: Diagnose[ComparisonValueError, Nothing] =
    instance(C.comparisonValue)(CauseDiagnostics.comparisonValue)
  given estimate: Diagnose[EstimateError, Nothing] =
    instance(C.estimate)(CauseDiagnostics.estimate)
  given surface: Diagnose[SurfaceError, Nothing] = instance(C.surface)(CauseDiagnostics.surface)
  given geometry: Diagnose[GeometryError, Nothing] =
    instance(C.geometry)(CauseDiagnostics.geometry)
  given time: Diagnose[TimeError, Nothing] = instance(C.time)(CauseDiagnostics.time)
  given windowOccupancy: Diagnose[WindowOccupancyError, Nothing] =
    instance(C.windowOccupancy)(CauseDiagnostics.windowOccupancy)
  given syncEvidence: Diagnose[SyncEvidenceError, Nothing] =
    instance(C.syncEvidence)(CauseDiagnostics.syncEvidence)
  given core: Diagnose[CoreError, Nothing]           = instance(C.core)(CauseDiagnostics.core)
  given recording: Diagnose[RecordingError, Nothing] =
    instance(C.recording)(CauseDiagnostics.recordingData)
  given scanpath: Diagnose[ScanpathError, Nothing] =
    instance(C.scanpath)(CauseDiagnostics.scanpath)
  given event: Diagnose[EventError, Nothing] = instance(C.event)(CauseDiagnostics.event)
  given detectionSupport: Diagnose[DetectionSupportError, Nothing] =
    instance(C.detectionSupport)(CauseDiagnostics.detectionSupport)
  given detectorDefinition: Diagnose[DetectorDefinitionError, Nothing] =
    instance(C.detectorDefinition)(CauseDiagnostics.detectorDefinition)
  given detectionResult: Diagnose[DetectionResultError, Nothing] =
    instance(C.detectionResult)(CauseDiagnostics.detectionResult)
  given detectionFailure: Diagnose[DetectionFailure, Nothing] =
    instance(C.detectionFailure)(CauseDiagnostics.detectionFailure)
  given kinematics: Diagnose[KinematicsError, Nothing] =
    instance(C.kinematics)(CauseDiagnostics.kinematics)
  given configuration: Diagnose[ConfigurationError, Nothing] =
    instance(C.configuration)(CauseDiagnostics.configuration)
  given aoi: Diagnose[AoiError, Nothing]               = instance(C.aoi)(CauseDiagnostics.aoi)
  given descriptor: Diagnose[DescriptorError, Nothing] =
    instance(C.descriptor)(CauseDiagnostics.descriptor)

  /** A form field's refusal; `Refused` keeps the domain error's diagnostic. */
  given formField[E, K](using underlying: Diagnose[E, K]): Diagnose[FieldError[E], K] =
    instance(C.formField)(e => CauseDiagnostics.formField(e)(underlying(_)))

  /** A form field's refusal as a host holds it from `ParameterSet.validate`,
    * with its domain error type erased. Not a `given`, so it never competes
    * with [[formField]] for a statically typed error.
    */
  val reportedFormField: Diagnose[FieldError[Any], Nothing] =
    instance(C.formField)(CauseDiagnostics.formFieldReported)

  given studyRecipe: Diagnose[StudyRecipeError, Nothing] =
    derived(
      C.studyRecipe,
      (e: StudyRecipeError) =>
        Vector(Locus.Field(StudyForm.formField(e.field).fold(e.field.toString)(_.value)))
    )(_.message)
  given methodsFact: Diagnose[FactError, Nothing]       = derived(C.methodsFact)(_.message)
  given studyAdvisory: Diagnose[StudyAdvisory, Nothing] =
    derived(
      C.studyAdvisory,
      (a: StudyAdvisory) =>
        Vector(
          Locus.Field(StudyForm.ids.scales.value),
          Locus.Scale(a match
            case StudyAdvisory.SigmaBelowCells(i, _, _)  => i
            case StudyAdvisory.SigmaNearUniform(i, _, _) => i)
        )
    )(_.message)

  // ---------------------------------------------------------------- derived families

  given timeline: Diagnose[TimelineError, Nothing]         = derived(C.timeline)(_.message)
  given moving: Diagnose[MovingError, Nothing]             = derived(C.moving)(_.message)
  given timeQuantity: Diagnose[TimeQuantityError, Nothing] =
    derived(C.timeQuantity)(_.message)
  given occupancy: Diagnose[OccupancyError, Nothing]     = derived(C.occupancy)(_.message)
  given replication: Diagnose[ReplicationError, Nothing] =
    derived(C.replication)(_.message)
  given temporalSupport: Diagnose[TemporalSupportError, Nothing] =
    derived(C.temporalSupport)(_.message)
  given algorithmMetadata: Diagnose[AlgorithmMetadataError, Nothing] =
    derived(C.algorithmMetadata)(_.message)
  given ekEstimation: Diagnose[EkEstimationError, Nothing] =
    derived(C.ekEstimation)(_.message)
  given merge: Diagnose[MergeError, Nothing] =
    derived(C.merge, (e: MergeError) => Vector(Locus.Recording(mergeSource(e))))(_.message)
  given densityLookup: Diagnose[DensityLookupError, Nothing] =
    derived(C.densityLookup)(_.message)
  given densityPointFailure: Diagnose[DensityPointFailure, Nothing] =
    derived(C.densityPointFailure)(_.message)
  given iqrBandwidth: Diagnose[IqrBandwidthError, Nothing] =
    derived(C.iqrBandwidth)(_.message)
  given smootherCard: Diagnose[SmootherCardError, Nothing] =
    derived(C.smootherCard)(_.message)
  given comparisonConfiguration: Diagnose[ComparisonConfigurationError, Nothing] =
    derived(C.comparisonConfiguration)(_.message)
  given crqaParameter: Diagnose[CrqaParameterError, Nothing] =
    derived(C.crqaParameter)(_.message)
  given crqa: Diagnose[CrqaError, Nothing] = derived(C.crqa)(_.message)
  given fixationComparison: Diagnose[FixationComparisonError, Nothing] =
    derived(C.fixationComparison)(_.message)
  given overlapFailure: Diagnose[OverlapFailure, Nothing] =
    derived(C.overlapFailure)(_.message)
  given mapScaleFailure: Diagnose[MapScaleFailure, Nothing] =
    derived(C.mapScaleFailure)(_.message)
  given mapComparison: Diagnose[MapComparisonError, Nothing] =
    derived(C.mapComparison)(_.message)
  given decomposition: Diagnose[DecompositionError, Nothing] =
    derived(C.decomposition)(_.message)
  given pairing: Diagnose[PairingError, Nothing]                 = derived(C.pairing)(_.message)
  given session: Diagnose[SessionError, Nothing]                 = derived(C.session)(_.message)
  given reductionPolicy: Diagnose[ReductionPolicyError, Nothing] =
    derived(C.reductionPolicy)(_.message)
  given stageMeter: Diagnose[StageMeterError, Nothing] = derived(C.stageMeter)(_.message)
  given studyRun: Diagnose[StudyRunError, Nothing]     = derived(C.studyRun)(_.message)
  given workQuanta: Diagnose[WorkQuantaError, Nothing] = derived(C.workQuanta)(_.message)
  given evaluationWork: Diagnose[EvaluationWorkError, Nothing] =
    derived(C.evaluationWork)(_.message)
  given repetitionMean: Diagnose[RepetitionMeanError, Nothing] =
    derived(C.repetitionMean)(_.message)
  given leastSquares: Diagnose[LeastSquaresError, Nothing] =
    derived(C.leastSquares)(_.message)
  given template: Diagnose[TemplateError, Nothing]           = derived(C.template)(_.message)
  given rng: Diagnose[RngError, Nothing]                     = derived(C.rng)(_.message)
  given pointSampling: Diagnose[PointSamplingError, Nothing] =
    derived(C.pointSampling)(_.message)
  given recipeParameter: Diagnose[RecipeParameterError, Nothing] =
    derived(C.recipeParameter)(_.message)
  given repetitionPlan: Diagnose[RepetitionPlanError, Nothing] =
    derived(C.repetitionPlan)(_.message)
  given diagnosticCode: Diagnose[DiagnosticCodeError, Nothing] =
    derived(C.diagnosticCode)(_.message)
  given massLevel: Diagnose[MassLevelError, Nothing] = derived(C.massLevel)(_.message)

  given fixationEntropy: Diagnose[FixationEntropyError, Nothing] =
    derived(C.fixationEntropy)(_.message)

  given sourceIdentity: Diagnose[SourceIdentityError, Nothing] =
    derived(SourceDiagnostics.identity)(_.message)
  given importSpec: Diagnose[ImportSpecError, Nothing] =
    derived(SourceDiagnostics.importDescription)(_.message)

  /** An epoch error names its trial by key; the mark kind of its selector is
    * the application's own value and is carried as text.
    */
  given epoch[T, M]: Diagnose[EpochError[T, M], T] =
    given DiagnosticOperand[T, T]               = DiagnosticOperand.key[T]
    val occurrence                              = summon[DiagnosticOperand[Occurrence, Nothing]]
    given DiagnosticOperand[MarkSelector[M], T] = DiagnosticOperand.of[MarkSelector[M]] {
      selector =>
        Operand.Fields(
          Vector(
            "kind"       -> Operand.Text(selector.kind.toString),
            "occurrence" -> occurrence(selector.occurrence)
          )
        )
    }
    derived[EpochError[T, M], T](
      C.epoch,
      (e: EpochError[T, M]) => Vector(Locus.Trial(epochTrial(e)))
    )(_.message)

  private def epochTrial[T, M](e: EpochError[T, M]): T = e match
    case EpochError.Clock(trial, _, _)             => trial
    case EpochError.AnchorMatches(trial, _, _)     => trial
    case EpochError.AnchorOverflow(trial, _, _, _) => trial
    case EpochError.DurationOverflow(trial, _, _)  => trial
    case EpochError.NonDivisible(trial, _, _, _)   => trial
    case EpochError.BinLimit(trial, _, _, _, _)    => trial

  private def mergeSource(e: MergeError): RecordingRef = e match
    case MergeError.Interval(source, _, _, _) => source
    case MergeError.Range(source, _, _, _)    => source
    case MergeError.Event(source, _, _, _)    => source
    case MergeError.SourceSupport(source, _)  => source

  // ---------------------------------------------------------------- derivation

  /** The instance of an enum family whose projection needs no custom subject
    * beyond `locate`: each field becomes the operand its [[DiagnosticOperand]]
    * gives, under the field's own name and in declaration order, and a
    * wrapped error keeps its subject. The code is the family's code for the
    * case's ordinal, so a family whose labels match the compiler's case list
    * gives every case a cataloged code.
    */
  private[eyes4s] inline def derived[E, K](
      family: DiagnosticFamily,
      locate: E => Vector[Locus[K]] = (_: E) => Vector.empty
  )(message: E => String)(using mirror: Mirror.SumOf[E]): Diagnose[E, K] =
    new Derived[E, K](
      family,
      locate,
      message,
      mirror.ordinal,
      () => Derivation.cases[mirror.MirroredElemTypes, K]
    )

  private[eyes4s] final class Derived[E, K](
      val family: DiagnosticFamily,
      locate: E => Vector[Locus[K]],
      message: E => String,
      ordinal: E => Int,
      table: () => Vector[Vector[DiagnosticOperand[Any, K]]]
  ) extends Diagnose[E, K]:
    // Built on first use: a family that wraps its own errors refers to its
    // own instance, which must be initialised before the table is read.
    private lazy val cases = table()

    def apply(error: E): Diagnostic[K] =
      val index    = ordinal(error)
      val product  = error.asInstanceOf[Product]
      val values   = product.productIterator.toVector
      val operands = product.productElementNames.toVector
        .zip(values.zip(cases(index)).map((value, field) => field(value)))
      val inner = operands.flatMap {
        case (_, Operand.Cause(diagnostic))   => diagnostic.subject
        case (_, Operand.Causes(diagnostics)) => diagnostics.flatMap(_.subject)
        case _                                => Vector.empty
      }
      val prefix = locate(error)
      Diagnostic(
        family.code(index),
        family.severity,
        prefix ++ inner.distinct.filterNot(prefix.contains),
        operands,
        Vector.empty,
        message(error)
      )

  /** Compile-time traversal of an enum's cases and a case's fields. Cases are
    * taken four at a time, so a family of up to 128 cases stays within the
    * compiler's limit of successive inlines.
    */
  private[eyes4s] object Derivation:
    inline def cases[T <: Tuple, K]: Vector[Vector[DiagnosticOperand[Any, K]]] =
      inline erasedValue[T] match
        case _: EmptyTuple                 => Vector.empty
        case _: (a *: b *: c *: d *: rest) =>
          Vector(fields[a, K], fields[b, K], fields[c, K], fields[d, K]) ++ cases[rest, K]
        case _: (c *: rest) => fields[c, K] +: cases[rest, K]

    inline def fields[C, K]: Vector[DiagnosticOperand[Any, K]] = summonFrom {
      case product: Mirror.ProductOf[C] => operands[product.MirroredElemTypes, K]
    }

    inline def operands[T <: Tuple, K]: Vector[DiagnosticOperand[Any, K]] =
      inline erasedValue[T] match
        case _: EmptyTuple  => Vector.empty
        case _: (f *: rest) =>
          summonInline[DiagnosticOperand[f, K]].asInstanceOf[DiagnosticOperand[Any, K]] +:
            operands[rest, K]

/** How one field of an error case becomes a typed [[Operand]]. Instances
  * follow the catalog's conventions: counts are integers, measured values are
  * exact reals, instants and spans are microseconds, identifiers are names,
  * library vocabulary is a token, free text is text, nested errors are causes
  * through their own [[Diagnose]] instance, and any other product or sum is
  * decomposed into named fields.
  */
private[eyes4s] trait DiagnosticOperand[A, +K]:
  def apply(value: A): Operand[K]

private[eyes4s] object DiagnosticOperand extends DiagnosticOperandCauses:
  import DiagnosticSupport.*

  private[eyes4s] def of[A](f: A => Operand[Nothing]): DiagnosticOperand[A, Nothing] =
    new DiagnosticOperand[A, Nothing]:
      def apply(value: A): Operand[Nothing] = f(value)

  /** A field holding a trial key of the family's own key type. */
  private[eyes4s] def key[K]: DiagnosticOperand[K, K] =
    new DiagnosticOperand[K, K]:
      def apply(value: K): Operand[K] = Operand.Key(value)

  given int: DiagnosticOperand[Int, Nothing]               = of(DiagnosticSupport.int)
  given long: DiagnosticOperand[Long, Nothing]             = of(DiagnosticSupport.long)
  given bigInt: DiagnosticOperand[BigInt, Nothing]         = of(Operand.Integer(_))
  given byte: DiagnosticOperand[Byte, Nothing]             = of(b => Operand.Integer(BigInt(b)))
  given double: DiagnosticOperand[Double, Nothing]         = of(real)
  given string: DiagnosticOperand[String, Nothing]         = of(text)
  given char: DiagnosticOperand[Char, Nothing]             = of(c => text(c.toString))
  given boolean: DiagnosticOperand[Boolean, Nothing]       = of(b => token(b.toString))
  given bigDecimal: DiagnosticOperand[BigDecimal, Nothing] = of(d => text(d.toString))
  given ints: DiagnosticOperand[Vector[Int], Nothing]      = of(DiagnosticSupport.ints)
  given strings: DiagnosticOperand[Vector[String], Nothing] = of(names)
  given stringSet: DiagnosticOperand[Set[String], Nothing]  =
    of(values => names(values.toVector.sorted))
  given span: DiagnosticOperand[Span, Nothing]                 = of(DiagnosticSupport.span)
  given instant: DiagnosticOperand[Instant, Nothing]           = of(DiagnosticSupport.instant)
  given positiveSpan: DiagnosticOperand[PositiveSpan, Nothing] =
    of(value => Operand.Micros(value.toMicros))
  given nonNegativeSpan: DiagnosticOperand[NonNegativeSpan, Nothing] =
    of(value => Operand.Micros(value.toMicros))
  given nonNegativeLong: DiagnosticOperand[NonNegativeLong, Nothing] =
    of(value => DiagnosticSupport.long(value.toLong))
  given contentHash: DiagnosticOperand[ContentHash, Nothing]       = of(h => artifact(h.render))
  given evaluationInfo: DiagnosticOperand[EvaluationInfo, Nothing] =
    of(DiagnosticSupport.evaluation)
  given sourceInterpretation: DiagnosticOperand[SourceInterpretation, Nothing] = of {
    case SourceInterpretation.LegacyUnspecified  => token("LegacyUnspecified")
    case declared: SourceInterpretation.Declared =>
      fields(
        "kind"          -> token("Declared"),
        "format"        -> token(declared.format.toString),
        "parser"        -> definition(declared.parser),
        "optionsSchema" -> token(declared.optionsSchema.toString),
        "options"       -> artifact(declared.options.render)
      )
  }
  given frameId: DiagnosticOperand[FrameId, Nothing]           = of(frame)
  given clockId: DiagnosticOperand[ClockId, Nothing]           = of(clock)
  given gridId: DiagnosticOperand[GridId, Nothing]             = of(grid)
  given recording: DiagnosticOperand[RecordingRef, Nothing]    = of(recordingRef)
  given detectorRef: DiagnosticOperand[DetectorRef, Nothing]   = of(detector)
  given definitionId: DiagnosticOperand[DefinitionId, Nothing] = of(definition)
  given algorithmId: DiagnosticOperand[AlgorithmId, Nothing]   = of(id => name(id.value))
  given sessionKey: DiagnosticOperand[SessionKey, Nothing]     = of(k => name(k.value))
  given predictorId: DiagnosticOperand[PredictorId, Nothing]   = of(id => name(id.value))
  given interval: DiagnosticOperand[Interval, Nothing]         = of(DiagnosticSupport.interval)
  given sampleRange: DiagnosticOperand[SampleRange, Nothing]   = of(range)
  given window: DiagnosticOperand[Window, Nothing]             = of(w =>
    fields("from" -> DiagnosticSupport.span(w.from), "until" -> DiagnosticSupport.span(w.until))
  )
  given temporalSupport: DiagnosticOperand[TemporalSupport, Nothing] =
    of(support => token(support.render))
  given provenance: DiagnosticOperand[Provenance, Nothing]           = of(lineage)
  given params: DiagnosticOperand[Vector[Provenance.Param], Nothing] =
    of(DiagnosticSupport.params)

private[eyes4s] trait DiagnosticOperandCauses extends DiagnosticOperandCollections:
  /** A nested error: a cause projected through its own family. */
  given cause[E, K](using diagnose: Diagnose[E, K]): DiagnosticOperand[E, K] =
    new DiagnosticOperand[E, K]:
      def apply(value: E): Operand[K] = Operand.Cause(diagnose(value))

  given causes[E, K](using diagnose: Diagnose[E, K]): DiagnosticOperand[Vector[E], K] =
    new DiagnosticOperand[Vector[E], K]:
      def apply(value: Vector[E]): Operand[K] = Operand.Causes(value.map(diagnose(_)))

  given nonEmptyCauses[E, K](using
      diagnose: Diagnose[E, K]
  ): DiagnosticOperand[NonEmptyVector[E], K] =
    new DiagnosticOperand[NonEmptyVector[E], K]:
      def apply(value: NonEmptyVector[E]): Operand[K] =
        Operand.Causes(value.toVector.map(diagnose(_)))

private[eyes4s] trait DiagnosticOperandCollections extends DiagnosticOperandStructures:
  given option[A, K](using inner: DiagnosticOperand[A, K]): DiagnosticOperand[Option[A], K] =
    new DiagnosticOperand[Option[A], K]:
      def apply(value: Option[A]): Operand[K] = value.fold(Operand.Absent)(inner(_))

  given vector[A, K](using inner: DiagnosticOperand[A, K]): DiagnosticOperand[Vector[A], K] =
    new DiagnosticOperand[Vector[A], K]:
      def apply(value: Vector[A]): Operand[K] = Operand.Items(value.map(inner(_)))

private[eyes4s] trait DiagnosticOperandStructures:
  /** Any other product or sum: a case without fields is a token, a product is
    * its named fields, and a case of a sum with fields starts with its `kind`.
    */
  inline given structure[A](using mirror: Mirror.Of[A]): DiagnosticOperand[A, Nothing] =
    inline mirror match
      case sum: Mirror.SumOf[A] =>
        DiagnosticOperandStructures.Sum[A](
          sum.ordinal,
          Diagnose.Derivation.cases[sum.MirroredElemTypes, Nothing]
        )
      case product: Mirror.ProductOf[A] =>
        DiagnosticOperandStructures.Product[A](
          Diagnose.Derivation.operands[product.MirroredElemTypes, Nothing]
        )

private[eyes4s] object DiagnosticOperandStructures:
  /** A sum's operand: a field-less case is its name as a token; a case with
    * fields is those fields after a leading `kind` naming the case.
    */
  final class Sum[A](
      ordinal: A => Int,
      cases: Vector[Vector[DiagnosticOperand[Any, Nothing]]]
  ) extends DiagnosticOperand[A, Nothing]:
    def apply(value: A): Operand[Nothing] =
      val product = value.asInstanceOf[scala.Product]
      val fields  = named(product, cases(ordinal(value)))
      if fields.isEmpty then Operand.Token(product.productPrefix)
      else Operand.Fields(("kind" -> Operand.Token(product.productPrefix)) +: fields)

  /** A product's operand: its fields by name, in declaration order. */
  final class Product[A](fields: Vector[DiagnosticOperand[Any, Nothing]])
      extends DiagnosticOperand[A, Nothing]:
    def apply(value: A): Operand[Nothing] =
      Operand.Fields(named(value.asInstanceOf[scala.Product], fields))

  def named(
      product: scala.Product,
      fields: Vector[DiagnosticOperand[Any, Nothing]]
  ): Vector[(String, Operand[Nothing])] =
    product.productElementNames.toVector
      .zip(product.productIterator.toVector.zip(fields).map((value, field) => field(value)))
