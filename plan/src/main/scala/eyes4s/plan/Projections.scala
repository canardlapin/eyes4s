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

/** The projections behind the study, recording, preflight, admission and
  * inspection [[Diagnose]] instances. Applications project through
  * `Diagnostic.of` and the instances; these functions are their bodies.
  *
  * Each projection is total over its enum and keeps every field as a named
  * operand. A wrapper case (a scale, repetition or refusal around another
  * error) prefixes its own locus to the wrapped error's subject, so the
  * diagnostic still names the innermost failing object.
  */
private[eyes4s] object Projections:
  import DiagnosticSupport.*
  import DiagnosticCatalog as C

  private def inherit[K](prefix: Vector[Locus[K]], inner: Diagnostic[K]): Vector[Locus[K]] =
    prefix ++ inner.subject

  // ---------------------------------------------------------------- plan and run

  def plan(e: PlanError): Diagnostic[Nothing] =
    import PlanError.*
    e match
      case InvalidDefinition(id, version) =>
        diagnostic[Nothing](C.plan, e, e.message)(text(id), int(version))
      case InvalidArtifact(digest) => diagnostic[Nothing](C.plan, e, e.message)(text(digest))
      case MissingArtifact(digest) =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Artifact(digest)))(artifact(digest))
      case ArtifactMismatch(expected, actual) =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Artifact(expected)))(
          artifact(expected),
          artifact(actual)
        )
      case InvalidPhases(focal, reference) =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Field("phases")))(
          name(focal),
          name(reference)
        )
      case EmptyScales(count) =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Field("estimates")))(int(count))
      case DuplicateScales(values) =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Field("estimates")))(names(values))
      case Specification(underlying) =>
        val inner = CauseDiagnostics.evaluationSpecError(underlying)
        diagnostic(C.plan, e, e.message, inner.subject)(cause(inner))
      case Schedule(underlying) =>
        val inner = CauseDiagnostics.pairSchedule(underlying)
        diagnostic(C.plan, e, e.message, inner.subject)(cause(inner))
      case StudyWorkBudget(focal, reference, scales, maximum) =>
        diagnostic[Nothing](C.plan, e, e.message)(
          int(focal),
          int(reference),
          int(scales),
          long(maximum)
        )
      case ChangedPreparedPlan(method, layout) =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Definition(method)))(
          definition(method),
          definition(layout)
        )
      case ComparisonWork(underlying) =>
        val inner = CauseDiagnostics.comparisonWork(underlying)
        diagnostic(C.plan, e, e.message, inner.subject)(cause(inner))
      case UnsupportedExecution(method, capability) =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Definition(method)))(
          definition(method),
          token(capability.toString)
        )
      case MissingAngularScale(scale) =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Scale(scale)))(int(scale))
      case Geometry(underlying) =>
        val inner = CauseDiagnostics.geometry(underlying)
        diagnostic(C.plan, e, e.message, inner.subject)(cause(inner))
      case InvalidOccurrence(value) => diagnostic[Nothing](C.plan, e, e.message)(int(value))
      case BlankKeyField(field)     =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Field(field)))(name(field))
      case OccurrenceUnavailable(layout, matched) =>
        diagnostic(C.plan, e, e.message, Vector(Locus.Definition(layout)))(
          definition(layout),
          token(matched.toString)
        )
      case MatchItemConflict(groups) =>
        diagnostic(C.plan, e, e.message, groups.flatten.map(Locus.TrialDigest(_)))(
          Operand.Items(groups.map(names))
        )
      case MatchedCardinality(matched, focal, groups) =>
        diagnostic(
          C.plan,
          e,
          e.message,
          (focal ++ groups.flatten).map(Locus.TrialDigest(_))
        )(
          token(matched.toString),
          names(focal),
          Operand.Items(groups.map(names))
        )
      case UnmatchedFocalRefused(focal) =>
        diagnostic(C.plan, e, e.message, focal.map(Locus.TrialDigest(_)))(names(focal))
      case InvalidWindowTally(screen, window, total, screenMicros, windowMicros, totalMicros) =>
        diagnostic[Nothing](C.plan, e, e.message)(
          int(screen),
          int(window),
          int(total),
          long(screenMicros),
          long(windowMicros),
          long(totalMicros)
        )
      case InitialFixations(underlying) =>
        val inner = RevisionDiagnostics.initialFixation(underlying)
        diagnostic(C.plan, e, e.message, inner.subject)(cause(inner))

  def failure[K](e: StudyFailure[K]): Diagnostic[K] =
    import StudyFailure.*
    def trial(key: K, inner: Diagnostic[Nothing]) =
      diagnostic(C.studyFailure, e, e.message, inherit(Vector(Locus.Trial(key)), inner))(
        Operand.Key(key),
        cause(inner)
      )
    e match
      case Frame(key, underlying)      => trial(key, CauseDiagnostics.geometry(underlying))
      case Occupancy(key, underlying)  => trial(key, CauseDiagnostics.surface(underlying))
      case Temporal(key, underlying)   => trial(key, temporal(underlying))
      case Estimation(key, underlying) => trial(key, CauseDiagnostics.estimate(underlying))
      case Comparison(left, right, underlying) =>
        val inner = CauseDiagnostics.compare(underlying)
        diagnostic(
          C.studyFailure,
          e,
          e.message,
          inherit(Vector(Locus.Pair(left, right)), inner)
        )(Operand.Key(left), Operand.Key(right), cause(inner))
      case OffWindow(key, tally) =>
        diagnostic(C.studyFailure, e, e.message, Vector(Locus.Trial(key)))(
          Operand.Key(key),
          windowTally(tally)
        )
      case InitialFixations(key, underlying) =>
        trial(key, RevisionDiagnostics.initialFixation(underlying))

  def result[K](e: StudyResultError[K]): Diagnostic[K] =
    import StudyResultError.*
    def at(loci: Locus[K]*)                                      = loci.toVector
    def steps(values: Vector[Provenance.Step]): Operand[Nothing] =
      Operand.Items(
        values.map(step =>
          fields("operation" -> name(step.operation), "params" -> parameters(step.params))
        )
      )
    def design(value: StudyDesign) = token(value.toString)
    e match
      case Description(field, found) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Field(field)))(
          name(field),
          params(found)
        )
      case InputMismatch(reference, described) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Artifact(reference)))(
          artifact(reference),
          params(described)
        )
      case LayoutMismatch(expected, described) =>
        diagnostic[K](C.studyResult, e, e.message)(definition(expected), definition(described))
      case ScaleCount(declared, found) =>
        diagnostic[K](C.studyResult, e, e.message)(int(declared), int(found))
      case ScaleEstimate(declared, found) =>
        diagnostic[K](C.studyResult, e, e.message)(params(declared), params(found))
      case MassGrid(key, declared, found) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Trial(key)))(
          Operand.Key(key),
          params(declared),
          params(found)
        )
      case MassProvenance(key, expected, found) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Trial(key)))(
          Operand.Key(key),
          steps(expected),
          steps(found)
        )
      case FailureKey(key, stored) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Trial(key)))(
          Operand.Key(key),
          cause(failure(stored))
        )
      case PairFailure(left, right, stored) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Pair(left, right)))(
          Operand.Key(left),
          Operand.Key(right),
          cause(failure(stored))
        )
      case OrphanKey(key) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Trial(key)))(Operand.Key(key))
      case OrphanPair(left, right) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Pair(left, right)))(
          Operand.Key(left),
          Operand.Key(right)
        )
      case SourceIdentity(value) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Design(value)))(design(value))
      case ContrastAnalyses(value) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Design(value)))(design(value))
      case ProvenanceInputs(value, declared, expected) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Design(value)))(
          design(value),
          artifact(declared),
          artifact(expected)
        )
      case MissingSpecification(value, info) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Design(value)))(
          design(value),
          evaluation(info)
        )
      case SpecificationMethod(value, expected, method, revision) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Design(value)))(
          design(value),
          definition(expected),
          name(method),
          name(revision)
        )
      case SpecificationParameters(value, expected, found) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Design(value)))(
          design(value),
          parameters(expected),
          parameters(found)
        )
      case Policy(value, declared, found) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Design(value)))(
          design(value),
          token(declared),
          token(found.render)
        )
      case Phase(key, expected, found) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Trial(key)))(
          Operand.Key(key),
          name(expected),
          name(found)
        )
      case Reconstruction(underlying) =>
        val inner = reconstruction(underlying)
        diagnostic(C.studyResult, e, e.message, inner.subject)(cause(inner))
      case Scale(index, underlying) =>
        val inner = result(underlying)
        diagnostic(C.studyResult, e, e.message, inherit(at(Locus.Scale(index)), inner))(
          int(index),
          cause(inner)
        )
      case SpecificationTime(value, expected, found) =>
        diagnostic(C.studyResult, e, e.message, at(Locus.Design(value)))(
          design(value),
          evaluationTime(expected),
          evaluationTime(found)
        )

  def temporal(e: TemporalStudyError): Diagnostic[Nothing] =
    import TemporalStudyError.*
    def wrap(inner: Diagnostic[Nothing]) =
      diagnostic(C.temporal, e, e.message, inner.subject)(cause(inner))
    e match
      case Input(underlying)     => wrap(plan(underlying))
      case Time(underlying)      => wrap(CauseDiagnostics.time(underlying))
      case Occupancy(underlying) => wrap(CauseDiagnostics.windowOccupancy(underlying))
      case InvalidWindow(window, from, until) =>
        diagnostic(C.temporal, e, e.message, Vector(Locus.Window(window)))(
          name(window),
          Operand.Micros(from),
          Operand.Micros(until)
        )
      case AnchorOverflow(window, anchor, from, until) =>
        diagnostic(C.temporal, e, e.message, Vector(Locus.Window(window)))(
          name(window),
          Operand.Micros(anchor),
          Operand.Micros(from),
          Operand.Micros(until)
        )
      case InvalidRepetition(repetition, focal, reference) =>
        diagnostic(C.temporal, e, e.message, Vector(Locus.Repetition(repetition)))(
          name(repetition),
          name(focal),
          name(reference)
        )
      case WindowNames(values) =>
        diagnostic(C.temporal, e, e.message, Vector(Locus.Field("windows")))(names(values))
      case RepetitionNames(values) =>
        diagnostic(C.temporal, e, e.message, Vector(Locus.Field("repetitions")))(names(values))
      case DuplicateEpochs(digests) =>
        diagnostic[Nothing](C.temporal, e, e.message)(names(digests))
      case DuplicateTrials(digests) =>
        diagnostic[Nothing](C.temporal, e, e.message)(names(digests))
      case UnknownEpochs(digests) =>
        diagnostic[Nothing](C.temporal, e, e.message)(names(digests))
      case MissingEpoch(digest) =>
        diagnostic(C.temporal, e, e.message, Vector(Locus.TrialDigest(digest)))(name(digest))
      case Weighting(weight) =>
        diagnostic(C.temporal, e, e.message, Vector(Locus.Field("weight")))(
          token(weight.toString)
        )

  def recordingPlan(e: RecordingPlanError): Diagnostic[Nothing] =
    import RecordingPlanError.*
    def wrap(inner: Diagnostic[Nothing]) =
      diagnostic(C.recordingPlan, e, e.message, inner.subject)(cause(inner))
    def staged(stage: String, inner: Diagnostic[Nothing]) =
      diagnostic(C.recordingPlan, e, e.message, inner.subject)(name(stage), cause(inner))
    def source(ref: RecordingRef) = Vector(Locus.Recording(ref))
    e match
      case Input(underlying)            => wrap(plan(underlying))
      case Geometry(underlying)         => wrap(CauseDiagnostics.geometry(underlying))
      case Time(underlying)             => wrap(CauseDiagnostics.time(underlying))
      case Synchronization(underlying)  => wrap(CauseDiagnostics.syncEvidence(underlying))
      case Core(stage, underlying)      => staged(stage, CauseDiagnostics.core(underlying))
      case Recording(stage, underlying) =>
        staged(stage, CauseDiagnostics.recordingData(underlying))
      case DetectorDefinition(underlying) =>
        wrap(CauseDiagnostics.detectorDefinition(underlying))
      case Detection(underlying) => wrap(CauseDiagnostics.detectionResult(underlying))
      case Areas(underlying)     => wrap(CauseDiagnostics.aoi(underlying))
      case MissingViewing(ref)   =>
        diagnostic(C.recordingPlan, e, e.message, source(ref))(recordingRef(ref))
      case MissingSynchronization(from, to) =>
        diagnostic[Nothing](C.recordingPlan, e, e.message)(clock(from), clock(to))
      case InvalidSource(ref) =>
        diagnostic[Nothing](C.recordingPlan, e, e.message)(text(ref.value))
      case InvalidArea(id, label) =>
        diagnostic[Nothing](C.recordingPlan, e, e.message)(text(id), text(label))
      case AreaNames(ids) => diagnostic[Nothing](C.recordingPlan, e, e.message)(names(ids))
      case AreaWarp(id, corner) =>
        diagnostic(C.recordingPlan, e, e.message, Vector(Locus.Area(id)))(
          name(id),
          token(corner)
        )
      case Cardinality(ref, before, after) =>
        diagnostic(C.recordingPlan, e, e.message, source(ref))(
          recordingRef(ref),
          int(before),
          int(after)
        )
      case Parameters(method, values) =>
        diagnostic(C.recordingPlan, e, e.message, Vector(Locus.Definition(method)))(
          definition(method),
          parameters(values)
        )

  def recordingInput(e: RecordingInputError): Diagnostic[Nothing] =
    import RecordingInputError.*
    e match
      case EmptySource(ref) =>
        diagnostic[Nothing](C.recordingInput, e, e.message)(text(ref.value))
      case SynchronizationTargetIsSource(id) =>
        diagnostic[Nothing](C.recordingInput, e, e.message)(clock(id))
      case NoSynchronizationMarks(from, to) =>
        diagnostic[Nothing](C.recordingInput, e, e.message)(clock(from), clock(to))
      case Synchronization(underlying) =>
        val inner = CauseDiagnostics.syncEvidence(underlying)
        diagnostic(C.recordingInput, e, e.message, inner.subject)(cause(inner))
      case PlanDisagreement(field, declared, evidence) =>
        diagnostic(C.recordingInput, e, e.message, Vector(Locus.Field(field)))(
          name(field),
          text(declared),
          text(evidence)
        )
      case BinocularChannels(ref) =>
        diagnostic(C.recordingInput, e, e.message, Vector(Locus.Recording(ref)))(
          recordingRef(ref)
        )
      case Plan(underlying) =>
        val inner = recordingPlan(underlying)
        diagnostic(C.recordingInput, e, e.message, inner.subject)(cause(inner))

  def recordingResult(e: RecordingResultError): Diagnostic[Nothing] =
    import RecordingResultError.*
    e match
      case Plan(underlying) =>
        val inner = recordingPlan(underlying)
        diagnostic(C.recordingResult, e, e.message, inner.subject)(cause(inner))
      case Stage(stage, field, expected, found) =>
        diagnostic(C.recordingResult, e, e.message, Vector(Locus.Field(s"$stage.$field")))(
          name(stage),
          name(field),
          text(expected),
          text(found)
        )

  def temporalResult[K](e: TemporalResultError[K]): Diagnostic[K] =
    import TemporalResultError.*
    def trial(key: K) = Vector(Locus.Trial(key))
    e match
      case CellCount(expected, found) =>
        diagnostic[K](C.temporalResult, e, e.message)(int(expected), int(found))
      case CellLayout(index, repetition, window, foundRepetition, foundWindow) =>
        diagnostic(
          C.temporalResult,
          e,
          e.message,
          Vector(Locus.Repetition(repetition), Locus.Window(window))
        )(int(index), name(repetition), name(window), name(foundRepetition), name(foundWindow))
      case Repetition(repetition, underlying) =>
        val inner = temporal(underlying)
        diagnostic(
          C.temporalResult,
          e,
          e.message,
          inherit(Vector(Locus.Repetition(repetition)), inner)
        )(name(repetition), cause(inner))
      case Plan(changes) =>
        diagnostic[K](C.temporalResult, e, e.message)(
          Operand.Items(
            changes.map(change =>
              fields(
                "field"  -> name(change.field),
                "before" -> params(change.before),
                "after"  -> params(change.after)
              )
            )
          )
        )
      case Result(underlying) =>
        val inner = result(underlying)
        diagnostic(C.temporalResult, e, e.message, inner.subject)(cause(inner))
      case OccupancyKeys(expected, found) =>
        diagnostic[K](C.temporalResult, e, e.message)(keys(expected), keys(found))
      case Boundary(key, expected, found) =>
        diagnostic(C.temporalResult, e, e.message, trial(key))(
          Operand.Key(key),
          token(expected.toString),
          token(found.toString)
        )
      case Width(key, expected, found) =>
        diagnostic(C.temporalResult, e, e.message, trial(key))(
          Operand.Key(key),
          Operand.Integer(expected),
          Operand.Integer(found)
        )
      case Epoch(key, expected, found) =>
        diagnostic(C.temporalResult, e, e.message, trial(key))(
          Operand.Key(key),
          optional(expected.map(name)),
          optional(found.map(name))
        )
      case Anchor(key, expectedClock, expectedMicros, foundClock, foundMicros) =>
        diagnostic(C.temporalResult, e, e.message, trial(key))(
          Operand.Key(key),
          clock(expectedClock),
          Operand.Integer(expectedMicros),
          clock(foundClock),
          Operand.Integer(foundMicros)
        )
      case Density(key, expected, found) =>
        diagnostic(C.temporalResult, e, e.message, trial(key))(
          Operand.Key(key),
          optional(expected.map(artifact)),
          artifact(found)
        )
      case Failure(key, stored) =>
        diagnostic(C.temporalResult, e, e.message, trial(key))(
          Operand.Key(key),
          cause(failure(stored))
        )
      case Cell(repetition, window, underlying) =>
        val inner = temporalResult(underlying)
        diagnostic(
          C.temporalResult,
          e,
          e.message,
          inherit(Vector(Locus.Repetition(repetition), Locus.Window(window)), inner)
        )(name(repetition), name(window), cause(inner))

  // ---------------------------------------------------------------- reduction and contrast

  def reduction[K](e: ReductionError[K]): Diagnostic[K] =
    import ReductionError.*
    def trial(key: K) = Vector(Locus.Trial(key))
    e match
      case NoSelectedScores(key) =>
        diagnostic(C.reduction, e, e.message, trial(key))(Operand.Key(key))
      case AmbiguousKey(key, indices) =>
        diagnostic(C.reduction, e, e.message, trial(key))(Operand.Key(key), ints(indices))
      case FailedScores(key, successful, failed) =>
        diagnostic(C.reduction, e, e.message, trial(key))(
          Operand.Key(key),
          int(successful),
          int(failed)
        )
      case InsufficientSuccessful(key, required, successful, failed) =>
        diagnostic(C.reduction, e, e.message, trial(key))(
          Operand.Key(key),
          int(required),
          int(successful),
          int(failed)
        )
      case MeanFailure(key, underlying) =>
        diagnostic(C.reduction, e, e.message, trial(key))(
          Operand.Key(key),
          cause(CauseDiagnostics.scoreMean(underlying))
        )

  def reconstruction[K](e: ReconstructionError[K]): Diagnostic[K] =
    import ReconstructionError.*
    def trial(key: K) = Vector(Locus.Trial(key))
    def outcome(value: Option[Either[ReductionError[K], Unit]]): Operand[K] = value match
      case None              => Operand.Absent
      case Some(Right(()))   => token("score")
      case Some(Left(error)) => cause(reduction(error))
    def failed(value: Option[ReductionError[K]]): Operand[K] =
      optional(value.map(error => cause(reduction(error))))
    e match
      case PairCounts(eligible, selected) =>
        diagnostic[K](C.reconstruction, e, e.message)(long(eligible), int(selected))
      case RowCount(expected, actual) =>
        diagnostic[K](C.reconstruction, e, e.message)(int(expected), int(actual))
      case Denominator(key, successful, failed, contributing) =>
        diagnostic(C.reconstruction, e, e.message, trial(key))(
          Operand.Key(key),
          int(successful),
          int(failed),
          int(contributing)
        )
      case ResultKey(expected, found) =>
        diagnostic(C.reconstruction, e, e.message, trial(expected))(
          Operand.Key(expected),
          Operand.Key(found)
        )
      case ResultCounts(key, outcome, successful, failed) =>
        diagnostic(C.reconstruction, e, e.message, trial(key))(
          Operand.Key(key),
          cause(reduction(outcome)),
          int(successful),
          int(failed)
        )
      case DuplicateEntry(key, indices) =>
        diagnostic(C.reconstruction, e, e.message, trial(key))(Operand.Key(key), ints(indices))
      case ReportCount(field, expected, found) =>
        diagnostic(C.reconstruction, e, e.message, Vector(Locus.Field(field)))(
          name(field),
          long(expected),
          long(found)
        )
      case FailedKeys(expected, found) =>
        diagnostic(C.reconstruction, e, e.message, Vector(Locus.Trials(expected ++ found)))(
          keys(expected),
          keys(found)
        )
      case ProvenanceConflict(stage, declared, derived) =>
        diagnostic(C.reconstruction, e, e.message, Vector(Locus.Field(stage)))(
          name(stage),
          lineage(declared),
          lineage(derived)
        )
      case ContrastDomain(expected, found) =>
        diagnostic[K](C.reconstruction, e, e.message)(keys(expected), keys(found))
      case ContrastRowShape(key, matched, control, difference) =>
        diagnostic(C.reconstruction, e, e.message, trial(key))(
          Operand.Key(key),
          outcome(matched),
          outcome(control),
          difference.fold(error => cause(contrastRow(error)), _ => token("difference"))
        )
      case ContrastOperand(key, operand) =>
        diagnostic(C.reconstruction, e, e.message, trial(key))(
          Operand.Key(key),
          token(operand.toString)
        )
      case Incompatible(issues) =>
        diagnostic[K](C.reconstruction, e, e.message)(
          Operand.Causes(issues.toVector.map(CauseDiagnostics.contrastCompatibility))
        )
      case Orientation(expected, found) =>
        diagnostic[K](C.reconstruction, e, e.message)(
          token(expected.toString),
          token(found.toString)
        )
      case KeyDomain(expected, found) =>
        diagnostic[K](C.reconstruction, e, e.message)(keys(expected), keys(found))
      case KeyDenominator(key, expectedSuccessful, expectedFailed, successful, failed) =>
        diagnostic(C.reconstruction, e, e.message, trial(key))(
          Operand.Key(key),
          int(expectedSuccessful),
          int(expectedFailed),
          int(successful),
          int(failed)
        )
      case OutcomeShape(key, expected, found) =>
        diagnostic(C.reconstruction, e, e.message, trial(key))(
          Operand.Key(key),
          failed(expected),
          failed(found)
        )

  def contrast[K](e: ContrastError[K]): Diagnostic[K] =
    import ContrastError.*
    e match
      case Incompatible(issues) =>
        diagnostic[K](C.contrast, e, e.message)(
          Operand.Causes(issues.toVector.map(CauseDiagnostics.contrastCompatibility))
        )
      case EmptyDomain(matched, control) =>
        diagnostic[K](C.contrast, e, e.message)(int(matched), int(control))
      case IndistinguishableOrdering(first, second) =>
        diagnostic(C.contrast, e, e.message, Vector(Locus.Trials(Vector(first, second))))(
          Operand.Key(first),
          Operand.Key(second)
        )

  def contrastRow[K](e: ContrastRowError[K]): Diagnostic[K] =
    import ContrastRowError.*
    def trial(key: K)                                        = Vector(Locus.Trial(key))
    def failed(value: Option[ReductionError[K]]): Operand[K] =
      optional(value.map(error => cause(reduction(error))))
    e match
      case MissingOperands(key, missing) =>
        diagnostic(C.contrastRow, e, e.message, trial(key))(
          Operand.Key(key),
          Operand.Items(missing.map(operand => token(operand.toString)))
        )
      case ReductionFailures(key, matched, control) =>
        diagnostic(C.contrastRow, e, e.message, trial(key))(
          Operand.Key(key),
          failed(matched),
          failed(control)
        )
      case Arithmetic(key, underlying) =>
        diagnostic(C.contrastRow, e, e.message, trial(key))(
          Operand.Key(key),
          cause(CauseDiagnostics.difference(underlying))
        )

  // ---------------------------------------------------------------- preflight

  private def finding[K](
      projected: Diagnostic[K],
      severity: Severity,
      category: FindingClass,
      remedy: Remedy
  ): Diagnostic[K] =
    val level = severity match
      case Severity.Blocker => DiagnosticSeverity.Error
      case Severity.Warning => DiagnosticSeverity.Warning
    projected.copy(severity = level, category = Some(category), remedy = Some(remedy))

  def budget(e: BudgetError): Diagnostic[Nothing] =
    import BudgetError.*
    e match
      case CandidateVisits(focal, reference, scales, maximum) =>
        diagnostic[Nothing](C.budget, e, e.message)(
          int(focal),
          int(reference),
          int(scales),
          long(maximum)
        )
      case Schedule(underlying) =>
        val inner = CauseDiagnostics.pairSchedule(underlying)
        diagnostic(C.budget, e, e.message, inner.subject)(cause(inner))

  def studyFinding[K, U <: Unit2D](f: StudyFinding[K, U]): Diagnostic[K] =
    import StudyFinding.*
    def method(id: DefinitionId) = Vector(Locus.Definition(id))
    def trial(key: K)            = Vector(Locus.Trial(key))
    val projected: Diagnostic[K] = f match
      case UndescribedMethod(id) =>
        diagnostic(C.studyFinding, f, f.message, method(id))(definition(id))
      case InconsistentDescriptor(id, underlying) =>
        diagnostic(C.studyFinding, f, f.message, method(id))(
          definition(id),
          cause(CauseDiagnostics.descriptor(underlying))
        )
      case MissingArtifact(expected) =>
        diagnostic(C.studyFinding, f, f.message, Vector(Locus.Artifact(expected.digest)))(
          artifact(expected.digest)
        )
      case ArtifactMismatch(expected, actual) =>
        diagnostic(C.studyFinding, f, f.message, Vector(Locus.Artifact(expected.digest)))(
          artifact(expected.digest),
          artifact(actual.digest)
        )
      case OverBudget(underlying) =>
        val inner = budget(underlying)
        diagnostic(C.studyFinding, f, f.message, inner.subject)(cause(inner))
      case Refused(underlying) =>
        val inner = plan(underlying)
        diagnostic(C.studyFinding, f, f.message, inner.subject)(cause(inner))
      case FrameMismatch(key, underlying) =>
        diagnostic(C.studyFinding, f, f.message, trial(key))(
          Operand.Key(key),
          cause(CauseDiagnostics.geometry(underlying))
        )
      case DuplicateTrial(key, side, positions) =>
        diagnostic(C.studyFinding, f, f.message, trial(key))(
          Operand.Key(key),
          token(side.toString),
          ints(positions)
        )
      case UnmatchedFocal(key) =>
        diagnostic(C.studyFinding, f, f.message, trial(key))(Operand.Key(key))
      case UncontrolledFocal(key) =>
        diagnostic(C.studyFinding, f, f.message, trial(key))(Operand.Key(key))
      case OffWindowFixations(key, tally, policy) =>
        diagnostic(C.studyFinding, f, f.message, trial(key))(
          Operand.Key(key),
          windowTally(tally),
          token(policy.toString)
        )
      case NoFixationInWindow(key, tally) =>
        diagnostic(C.studyFinding, f, f.message, trial(key))(
          Operand.Key(key),
          windowTally(tally)
        )
      case MatchedCardinality(key, references, matched) =>
        diagnostic(C.studyFinding, f, f.message, trial(key))(
          Operand.Key(key),
          keys(references),
          token(matched.toString)
        )
      case AmbiguousReferences(references, matched) =>
        diagnostic(C.studyFinding, f, f.message, Vector(Locus.Trials(references)))(
          keys(references),
          token(matched.toString)
        )
      case UnmatchedFocalRefused(key) =>
        diagnostic(C.studyFinding, f, f.message, trial(key))(Operand.Key(key))
      case MatchItemConflict(conflicting) =>
        diagnostic(C.studyFinding, f, f.message, Vector(Locus.Trials(conflicting)))(
          keys(conflicting)
        )
      case NoFixationKept(key, tally) =>
        diagnostic(C.studyFinding, f, f.message, trial(key))(
          Operand.Key(key),
          fields(
            "dropped"         -> int(tally.dropped),
            "total"           -> int(tally.total),
            "droppedDuration" -> span(tally.droppedDuration),
            "totalDuration"   -> span(tally.totalDuration)
          )
        )
    finding(projected, f.severity, f.category, f.remedy)

  def recordingFinding(f: RecordingFinding): Diagnostic[Nothing] =
    import RecordingFinding.*
    def method(id: DefinitionId)       = Vector(Locus.Definition(id))
    def source(ref: RecordingRef)      = Vector(Locus.Recording(ref))
    val projected: Diagnostic[Nothing] = f match
      case UndescribedMethod(id) =>
        diagnostic(C.recordingFinding, f, f.message, method(id))(definition(id))
      case InconsistentDescriptor(id, underlying) =>
        diagnostic(C.recordingFinding, f, f.message, method(id))(
          definition(id),
          cause(CauseDiagnostics.descriptor(underlying))
        )
      case MissingArtifact(expected) =>
        diagnostic(C.recordingFinding, f, f.message, Vector(Locus.Artifact(expected.digest)))(
          artifact(expected.digest)
        )
      case ArtifactMismatch(expected, actual) =>
        diagnostic(C.recordingFinding, f, f.message, Vector(Locus.Artifact(expected.digest)))(
          artifact(expected.digest),
          artifact(actual.digest)
        )
      case FrameMismatch(ref, underlying) =>
        diagnostic(C.recordingFinding, f, f.message, source(ref))(
          recordingRef(ref),
          cause(CauseDiagnostics.geometry(underlying))
        )
      case ClockMismatch(ref, underlying) =>
        diagnostic(C.recordingFinding, f, f.message, source(ref))(
          recordingRef(ref),
          cause(CauseDiagnostics.time(underlying))
        )
      case MissingViewing(ref) =>
        diagnostic(C.recordingFinding, f, f.message, source(ref))(recordingRef(ref))
      case MissingSynchronization(from, to) =>
        diagnostic[Nothing](C.recordingFinding, f, f.message)(clock(from), clock(to))
      case Refused(underlying) =>
        val inner = recordingPlan(underlying)
        diagnostic(C.recordingFinding, f, f.message, inner.subject)(cause(inner))
      case Synchronization(underlying) =>
        val inner = CauseDiagnostics.syncEvidence(underlying)
        diagnostic(C.recordingFinding, f, f.message, inner.subject)(cause(inner))
      case AngularFrame(id, underlying) =>
        diagnostic[Nothing](C.recordingFinding, f, f.message)(
          frame(id),
          cause(CauseDiagnostics.geometry(underlying))
        )
      case AreaWarp(area, corner) =>
        diagnostic(C.recordingFinding, f, f.message, Vector(Locus.Area(area)))(
          name(area),
          token(corner.toString)
        )
      case DetectorDefinition(id, underlying) =>
        diagnostic(C.recordingFinding, f, f.message, method(id))(
          definition(id),
          cause(CauseDiagnostics.detectorDefinition(underlying))
        )
    finding(projected, f.severity, f.category, f.remedy)

  def temporalFinding[K, U <: Unit2D](f: TemporalFinding[K, U]): Diagnostic[K] =
    import TemporalFinding.*
    def trial(key: K)            = Vector(Locus.Trial(key))
    val projected: Diagnostic[K] = f match
      case MissingArtifact(expected) =>
        diagnostic(C.temporalFinding, f, f.message, Vector(Locus.Artifact(expected.digest)))(
          artifact(expected.digest)
        )
      case ArtifactMismatch(expected, actual) =>
        diagnostic(C.temporalFinding, f, f.message, Vector(Locus.Artifact(expected.digest)))(
          artifact(expected.digest),
          artifact(actual.digest)
        )
      case Refused(underlying) =>
        val inner = temporal(underlying)
        diagnostic(C.temporalFinding, f, f.message, inner.subject)(cause(inner))
      case Study(underlying) =>
        val inner = studyFinding(underlying)
        diagnostic(C.temporalFinding, f, f.message, inner.subject)(cause(inner))
      case RepetitionPlan(repetition, underlying) =>
        val inner = plan(underlying)
        diagnostic(
          C.temporalFinding,
          f,
          f.message,
          inherit(Vector(Locus.Repetition(repetition)), inner)
        )(name(repetition), cause(inner))
      case Repetition(repetition, underlying) =>
        val inner = studyFinding(underlying)
        diagnostic(
          C.temporalFinding,
          f,
          f.message,
          inherit(Vector(Locus.Repetition(repetition)), inner)
        )(name(repetition), cause(inner))
      case MissingEpoch(key) =>
        diagnostic(C.temporalFinding, f, f.message, trial(key))(Operand.Key(key))
      case CoverageClock(key, underlying) =>
        diagnostic(C.temporalFinding, f, f.message, trial(key))(
          Operand.Key(key),
          cause(CauseDiagnostics.time(underlying))
        )
      case WindowResolution(key, window, underlying) =>
        val inner = temporal(underlying)
        diagnostic(
          C.temporalFinding,
          f,
          f.message,
          merge(Vector(Locus.Window(window), Locus.Trial(key)), inner)
        )(Operand.Key(key), name(window), cause(inner))
      case NoObservedCoverage(key, window) =>
        diagnostic(
          C.temporalFinding,
          f,
          f.message,
          Vector(Locus.Window(window), Locus.Trial(key))
        )(Operand.Key(key), name(window))
    finding(projected, f.severity, f.category, f.remedy)

  /** Any preflight finding, keys typed. A study or temporal finding in a
    * `PreflightFinding[K]` carries keys of type `K` (the finding type is
    * covariant in its key through `PreflightFinding`), and no projection
    * depends on the unit, so the unchecked type arguments are sound. They
    * stay sound only while no finding projection needs an `Ordering`, a type
    * test or any other evidence for `K` or `U`.
    */
  def preflightFinding[K](f: PreflightFinding[K]): Diagnostic[K] = f match
    case study: StudyFinding[K @unchecked, Unit2D @unchecked] =>
      studyFinding[K, Unit2D](study)
    case recording: RecordingFinding => recordingFinding(recording)
    case temporal: TemporalFinding[K @unchecked, Unit2D @unchecked] =>
      temporalFinding[K, Unit2D](temporal)

  def preflight[K](e: PreflightError[K]): Diagnostic[K] =
    import PreflightError.*
    def family(value: RecipeFamily) = token(value.toString)
    e match
      case ChangedPlan(value, changes) =>
        diagnostic[K](C.preflight, e, e.message)(
          family(value),
          Operand.Items(
            changes.map(change =>
              fields(
                "field"  -> name(change.field),
                "before" -> params(change.before),
                "after"  -> params(change.after)
              )
            )
          )
        )
      case ChangedInput(value, reported, actual) =>
        diagnostic(C.preflight, e, e.message, Vector(Locus.Artifact(reported.digest)))(
          family(value),
          artifact(reported.digest),
          artifact(actual.digest)
        )
      case NotReady(value, blockers) =>
        diagnostic[K](C.preflight, e, e.message)(
          family(value),
          Operand.Causes(blockers.map(preflightFinding))
        )
      case Refused(underlying) =>
        val inner = plan(underlying)
        diagnostic(C.preflight, e, e.message, inner.subject)(cause(inner))

  // ---------------------------------------------------------------- admission

  def admissionReason(e: AdmissionReason): Diagnostic[Nothing] =
    import AdmissionReason.*
    val d = diagnostic[Nothing](C.admissionReason, e, e.message)
    e match
      case Width(expected, actual)            => d(int(expected), int(actual))
      case Key(reason)                        => d(text(reason))
      case Number(column, value, requirement) => d(name(column), text(value), text(requirement))
      case Time(onset, duration, unit, reason) =>
        d(text(onset), text(duration), token(unit), text(reason))
      case Position(x, y, id)          => d(real(x), real(y), frame(id))
      case Event(reason)               => d(text(reason))
      case Quarantined(records, value) =>
        val inner = quarantine(value)
        diagnostic(C.admissionReason, e, e.message, Locus.Records(records) +: inner.subject)(
          ints(records),
          cause(inner)
        )

  def quarantine(e: QuarantineCause): Diagnostic[Nothing] =
    import QuarantineCause.*
    def at(index: Int) = Vector(Locus.Fixation(index))
    e match
      case RejectedRecords   => diagnostic[Nothing](C.quarantine, e, e.message)()
      case DuplicateOrdinals => diagnostic[Nothing](C.quarantine, e, e.message)()
      case NoFixations       => diagnostic[Nothing](C.quarantine, e, e.message)()
      case Overlap(index, previous, current) =>
        diagnostic(C.quarantine, e, e.message, at(index))(
          int(index),
          text(previous),
          text(current)
        )
      case WrongClock(index, expected, actual) =>
        diagnostic(C.quarantine, e, e.message, at(index))(
          int(index),
          name(expected),
          name(actual)
        )
      case InvalidTransition(index, reason) =>
        diagnostic(C.quarantine, e, e.message, at(index))(int(index), text(reason))
      case InvalidExtent(reason) =>
        diagnostic[Nothing](C.quarantine, e, e.message)(text(reason))
      case UnmappableFixation(index, from, to, x, y) =>
        diagnostic(C.quarantine, e, e.message, at(index))(
          int(index),
          frame(from),
          frame(to),
          real(x),
          real(y)
        )
      case CorrectionConflict(first, second) =>
        diagnostic[Nothing](C.quarantine, e, e.message)(int(first), int(second))
      case ItemConflict(items) => diagnostic[Nothing](C.quarantine, e, e.message)(names(items))
      case OccurrenceConflict(occurrences) =>
        diagnostic[Nothing](C.quarantine, e, e.message)(ints(occurrences))
      case NotInInventory(participant, phase, trial, occurrence) =>
        diagnostic[Nothing](C.quarantine, e, e.message)(
          name(participant),
          name(phase),
          name(trial),
          int(occurrence)
        )
      case InventoryItemConflict(inventory, records) =>
        diagnostic[Nothing](C.quarantine, e, e.message)(name(inventory), names(records))

  def inventory(e: InventoryError): Diagnostic[Nothing] =
    import InventoryError.*
    def record(number: Int) = Vector(Locus.Record(number))
    e match
      case Width(number, expected, actual) =>
        diagnostic(C.inventory, e, e.message, record(number))(
          int(number),
          int(expected),
          int(actual)
        )
      case Field(number, column, value, requirement) =>
        diagnostic(C.inventory, e, e.message, record(number))(
          int(number),
          name(column),
          text(value),
          text(requirement)
        )
      case Conflict(participant, phase, trial, numbers, columns) =>
        diagnostic(C.inventory, e, e.message, Vector(Locus.Records(numbers)))(
          name(participant),
          name(phase),
          name(trial),
          ints(numbers),
          names(columns)
        )
      case DuplicateAttribute(values) =>
        diagnostic[Nothing](C.inventory, e, e.message)(names(values))
      case DuplicateTrial(participant, phase, trial, occurrence) =>
        diagnostic[Nothing](C.inventory, e, e.message)(
          name(participant),
          name(phase),
          name(trial),
          int(occurrence)
        )
      case RecordOrder(trial, numbers) =>
        diagnostic(C.inventory, e, e.message, Vector(Locus.Records(numbers)))(
          name(trial),
          ints(numbers)
        )
      case SharedRecord(number, trials) =>
        diagnostic(C.inventory, e, e.message, record(number))(int(number), names(trials))
      case AbsentMismatch(trial, disposition, numbers) =>
        diagnostic(C.inventory, e, e.message, Vector(Locus.Records(numbers)))(
          name(trial),
          token(disposition),
          ints(numbers)
        )
      case AttributeRecord(number) =>
        diagnostic(C.inventory, e, e.message, record(number))(int(number))
      case UnknownRecord(trial, number) =>
        diagnostic(C.inventory, e, e.message, record(number))(name(trial), int(number))
      case ForeignRecord(trial, number, found) =>
        diagnostic(C.inventory, e, e.message, record(number))(
          name(trial),
          int(number),
          name(found)
        )
      case UnclaimedRecord(number, trial) =>
        diagnostic(C.inventory, e, e.message, record(number))(int(number), name(trial))
      case DispositionMismatch(trial, disposition, number, found) =>
        diagnostic(C.inventory, e, e.message, record(number))(
          name(trial),
          token(disposition),
          int(number),
          text(found)
        )
      case ItemMismatch(trial, number, expected, actual) =>
        diagnostic(C.inventory, e, e.message, record(number))(
          name(trial),
          int(number),
          name(expected),
          name(actual)
        )
      case NoTrialProjection(layout) =>
        diagnostic(C.inventory, e, e.message, Vector(Locus.Definition(layout)))(
          definition(layout)
        )
      case RecordItems(trial, disposition, items) =>
        diagnostic[Nothing](C.inventory, e, e.message)(
          name(trial),
          token(disposition),
          names(items)
        )
      case AttributeNames(owner, declared, found) =>
        diagnostic[Nothing](C.inventory, e, e.message)(
          name(owner),
          names(declared),
          names(found)
        )
      case AttributeKindMismatch(owner, attribute, declared, found) =>
        diagnostic[Nothing](C.inventory, e, e.message)(
          name(owner),
          name(attribute),
          token(declared),
          token(found)
        )

  def admission(e: AdmissionError): Diagnostic[Nothing] =
    import AdmissionError.*
    def record(number: Int) = Vector(Locus.Record(number))
    e match
      case NonPositiveRecord(number) =>
        diagnostic(C.admission, e, e.message, record(number))(int(number))
      case RecordOrder(index, previous, number) =>
        diagnostic(C.admission, e, e.message, record(number))(
          int(index),
          int(previous),
          int(number)
        )
      case NegativeOrdinal(number, value) =>
        diagnostic(C.admission, e, e.message, record(number))(int(number), int(value))
      case DuplicateOrdinal(numbers, value) =>
        diagnostic(C.admission, e, e.message, Vector(Locus.Records(numbers)))(
          ints(numbers),
          int(value)
        )
      case QuarantineScope(number, numbers) =>
        diagnostic(C.admission, e, e.message, record(number))(int(number), ints(numbers))
      case QuarantineAdmitted(number, admitted) =>
        diagnostic(C.admission, e, e.message, record(number))(int(number), int(admitted))
      case QuarantinedKeyAdmitted(quarantined, admitted) =>
        diagnostic(C.admission, e, e.message, record(quarantined))(
          int(quarantined),
          int(admitted)
        )
      case OutcomeMismatch(outcome, rejected) =>
        diagnostic[Nothing](C.admission, e, e.message)(token(outcome.toString), int(rejected))
      case AmbiguousTrial(indices) =>
        diagnostic(C.admission, e, e.message, indices.map(Locus.InputTrial(_)))(ints(indices))
      case UnknownTrial(numbers) =>
        diagnostic(C.admission, e, e.message, Vector(Locus.Records(numbers)))(ints(numbers))
      case UnadmittedTrial(index) =>
        diagnostic(C.admission, e, e.message, Vector(Locus.InputTrial(index)))(int(index))
      case FixationCount(index, fixations, records) =>
        diagnostic(C.admission, e, e.message, Vector(Locus.InputTrial(index)))(
          int(index),
          int(fixations),
          int(records)
        )
      case OutsideFrameRecord(number, policy) =>
        diagnostic(C.admission, e, e.message, record(number))(
          int(number),
          token(policy.toString)
        )
      case CorrectionConflict(number, first, second) =>
        diagnostic(C.admission, e, e.message, record(number))(
          int(number),
          int(first),
          int(second)
        )
      case Inventory(underlying) =>
        val inner = inventory(underlying)
        diagnostic(C.admission, e, e.message, inner.subject)(cause(inner))
      case UninventoriedCause(number, value) =>
        val inner = quarantine(value)
        diagnostic(C.admission, e, e.message, record(number))(int(number), cause(inner))

  /** A ledger refusal resolved against its input: the admission error's
    * code and operands, with the trials it concerns named by key and linked
    * to their records. An input position stays in the subject only where the
    * key cannot tell occurrences apart (a repeated key).
    */
  def ledgerRefusal[K](refusal: LedgerRefusal[K]): Diagnostic[K] =
    val inner  = admission(refusal.error)
    val trials = refusal.trials match
      case Vector()    => Vector.empty
      case Vector(key) => Vector(Locus.Trial(key))
      case keys        => Vector(Locus.Trials(keys))
    val located = refusal.error match
      case AdmissionError.AmbiguousTrial(_) => inner.subject
      case _ => inner.subject.filterNot { case Locus.InputTrial(_) => true; case _ => false }
    inner.copy(subject = trials ++ located, sources = refusal.links)

  // ---------------------------------------------------------------- inspection

  def inspection[K](e: InspectionError[K]): Diagnostic[K] =
    import InspectionError.*
    def reference(ref: ResultRef[K]): Operand[K] =
      val kind = token(ref.productPrefix)
      ref match
        case ResultRef.Estimation(scale, key) =>
          fields("kind" -> kind, "scale" -> int(scale), "key" -> Operand.Key(key))
        case ResultRef.PairRow(scale, design, focal, other) =>
          fields(
            "kind"      -> kind,
            "scale"     -> int(scale),
            "design"    -> token(design.toString),
            "focal"     -> Operand.Key(focal),
            "reference" -> Operand.Key(other)
          )
        case ResultRef.Reduction(scale, design, key) =>
          fields(
            "kind"   -> kind,
            "scale"  -> int(scale),
            "design" -> token(design.toString),
            "key"    -> Operand.Key(key)
          )
        case ResultRef.ContrastRow(scale, key) =>
          fields("kind" -> kind, "scale" -> int(scale), "key" -> Operand.Key(key))
        case ResultRef.Occupancy(repetition, window, key) =>
          fields(
            "kind"       -> kind,
            "repetition" -> name(repetition),
            "window"     -> name(window),
            "key"        -> Operand.Key(key)
          )
        case ResultRef.Event(from, until) =>
          fields("kind" -> kind, "from" -> int(from), "until" -> int(until))
        case ResultRef.InCell(repetition, window, inner) =>
          fields(
            "kind"       -> kind,
            "repetition" -> name(repetition),
            "window"     -> name(window),
            "ref"        -> reference(inner)
          )
    e match
      case UnknownScale(index, scales) =>
        diagnostic(C.inspection, e, e.message, Vector(Locus.Scale(index)))(
          int(index),
          int(scales)
        )
      case UnknownCell(repetition, window) =>
        diagnostic(
          C.inspection,
          e,
          e.message,
          Vector(Locus.Repetition(repetition), Locus.Window(window))
        )(name(repetition), name(window))
      case UnknownReference(ref) =>
        diagnostic(C.inspection, e, e.message, ref.loci)(reference(ref))
      case DuplicateReference(ref) =>
        diagnostic(C.inspection, e, e.message, ref.loci)(reference(ref))
      case InvalidPageSize(requested, maximum) =>
        diagnostic[K](C.inspection, e, e.message)(int(requested), int(maximum))
      case Sources(refusal) =>
        val inner = ledgerRefusal(refusal)
        diagnostic(C.inspection, e, e.message, inner.subject)(cause(inner))
          .linked(inner.sources)
      case InputMismatch(result, sources) =>
        diagnostic(C.inspection, e, e.message, Vector(Locus.Artifact(result.digest)))(
          artifact(result.digest),
          artifact(sources.digest)
        )
      case Components(underlying) =>
        diagnostic[K](C.inspection, e, e.message)(
          cause(CauseDiagnostics.descriptor(underlying))
        )
      case PlanMismatch(changes) =>
        diagnostic[K](C.inspection, e, e.message)(
          Operand.Items(
            changes.map(change =>
              fields(
                "field"  -> name(change.field),
                "before" -> params(change.before),
                "after"  -> params(change.after)
              )
            )
          )
        )
      case ReductionMembership(ref, selected, members, contributing, contributors) =>
        diagnostic(C.inspection, e, e.message, ref.loci)(
          reference(ref),
          int(selected),
          int(members),
          int(contributing),
          int(contributors)
        )
      case Orientation(scale, design, found) =>
        diagnostic(
          C.inspection,
          e,
          e.message,
          Vector(Locus.Scale(scale), Locus.Design(design))
        )(int(scale), token(design.toString), token(found.toString))
      case NoContrast(scale) =>
        diagnostic(C.inspection, e, e.message, Vector(Locus.Scale(scale)))(int(scale))
