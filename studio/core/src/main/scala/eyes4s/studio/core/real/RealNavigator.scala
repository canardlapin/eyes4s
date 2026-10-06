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

package eyes4s.studio.core.real

import cats.effect.kernel.{Concurrent, Ref}
import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.compare.Similarity
import eyes4s.design.SignedDifference
import eyes4s.kernel.Unit2D
import eyes4s.plan.{
  CoordinateProvenance,
  FixationRef,
  ListingOffset,
  PageSize,
  ResultInspection,
  ResultNavigation,
  ResultRef,
  ScanpathPosition,
  StudyInspection,
  TrialKey as CoreKey
}
import eyes4s.results.{Report, ReportNavigation, ReportRef as NativeReportRef, Role}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ReportingId, ReportingSpec, SourceRole}
import eyes4s.studio.core.execution.StudyInputArtifact
import eyes4s.studio.core.navigation.{
  ChainLevel,
  FixationSourceContext,
  MissingSource,
  NavigationError,
  Page,
  PairMaps,
  ReportRef,
  StudyNavigator,
  UsedBy,
  UsedByRole
}
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, ScaleIndex, StudioRef}

/** Native report/result membership and source navigation over retained library values. */
final class RealNavigator[F[_]] private[real] (
    initialReports: Map[ReportingId, ReportingSpec],
    held: RunId => F[Either[BackendError, RealStudyBackend.RealRun]],
    prepared: AnalysisRevision => F[Either[BackendError, RealPrepared]],
    state: Ref[F, RealNavigator.State]
)(using F: Concurrent[F])
    extends StudyNavigator[F]:
  import RealNavigator.*

  private def unavailable(message: String): NavigationError =
    NavigationError.Backend(BackendError.Unavailable(DiagnosticLocus.Artifact(message)))

  /** The latest explicitly requested spec binds navigation for its id and scale. */
  private[real] def requestedReport(run: RunId, spec: ReportingSpec, scale: Int): F[Unit] =
    state.update(s => s.copy(bindings = s.bindings.updated((run, spec.id, scale), spec)))

  /** Cache the actual native report; finishing an older request cannot rebind its id. */
  private[real] def evaluatedReport(
      run: RunId,
      spec: ReportingSpec,
      scale: Int,
      report: Report[CoreKey]
  ): F[Unit] =
    state.update(s => s.copy(reports = s.reports.updated((run, spec, scale), report)))

  /** Authoritative document edits replace id bindings without discarding old reports. */
  private[real] def synchronizeReporting(specifications: Vector[ReportingSpec]): F[Unit] =
    val current = specifications.map(spec => spec.id -> spec).toMap
    state.update { s =>
      val previous = s.defaults.getOrElse(initialReports)
      val changed  =
        (previous.keySet ++ current.keySet).filter(id => previous.get(id) != current.get(id))
      s.copy(
        defaults = Some(current),
        bindings = s.bindings.filter { case ((_, id, _), _) => !changed(id) }
      )
    }

  private def report(
      run: RunId,
      id: ReportingId,
      scale: Int
  ): F[Either[NavigationError, Report[CoreKey]]] =
    state.get.flatMap { snapshot =>
      snapshot.bindings
        .get((run, id, scale))
        .orElse(snapshot.defaults.getOrElse(initialReports).get(id)) match
        case None =>
          F.pure(Left(unavailable(s"${run.label} has no reporting specification ${id.value}.")))
        case Some(spec) =>
          snapshot.reports.get((run, spec, scale)) match
            case Some(report) => F.pure(Right(report))
            case None         =>
              held(run)
                .map(_.flatMap(RealReports.native(run, spec, scale, _)))
                .flatTap {
                  case Left(_)      => F.unit
                  case Right(value) => evaluatedReport(run, spec, scale, value)
                }
                .map(_.leftMap(NavigationError.Backend(_)))
    }

  private def context(work: RealPrepared): F[Either[NavigationError, SourceContext]] =
    state.get.flatMap { snapshot =>
      snapshot.contexts.find(_.work eq work) match
        case Some(found) => F.pure(Right(found))
        case None        =>
          F.pure(contextOf(work)).flatTap {
            case Left(_)      => F.unit
            case Right(found) => state.update(s => s.copy(contexts = s.contexts :+ found))
          }
    }

  private def loaded(run: RunId): F[Either[NavigationError, Loaded]] =
    state.get.flatMap(_.inspections.get(run) match
      case Some(found) => F.pure(Right(found))
      case None        =>
        held(run)
          .flatMap {
            case Left(error)  => F.pure(Left(NavigationError.Backend(error)))
            case Right(value) =>
              context(value.prepared).map(_.flatMap { context =>
                val work = value.prepared
                ResultInspection
                  .study(
                    work.plan,
                    value.result,
                    work.admitted.input,
                    Some(work.admitted.evidence)
                  )
                  .leftMap(e => unavailable(s"${run.label} inspection: ${e.message}"))
                  .map(inspection => Loaded(work, inspection, context))
              })
          }
          .flatTap {
            case Left(_)      => F.unit
            case Right(found) =>
              state.update(s => s.copy(inspections = s.inspections.updated(run, found)))
          })

  private def nativeCell(
      report: Report[CoreKey],
      cell: ReportRef.Cell
  ): Either[NavigationError, NativeReportRef.Cell] =
    ReportNavigation
      .cells(report)
      .find(c =>
        c.scale == cell.scale.value && role(c.role) == cell.role && group(c)
          .map(_.label) == cell.group.map(_.label)
      )
      .toRight(
        NavigationError
          .UnknownGroup(cell, ReportNavigation.cells(report).flatMap(group).distinct)
      )

  def cells(
      run: RunId,
      reporting: ReportingId,
      scale: ScaleIndex,
      request: PageRequest
  ): F[Either[NavigationError, Page[ReportRef.Cell]]] =
    report(run, reporting, scale.value).map(_.map { native =>
      Page.of(
        ReportNavigation
          .cells(native)
          .filter(_.role == Role.Difference)
          .map(c => ReportRef.Cell(run, reporting, scale, group(c), role(c.role))),
        request
      )
    })

  def participants(
      cell: ReportRef.Cell,
      request: PageRequest
  ): F[Either[NavigationError, Page[ReportRef.Participant]]] =
    report(cell.run, cell.reporting, cell.scale.value).map(_.flatMap { native =>
      for
        where   <- nativeCell(native, cell)
        members <- ReportNavigation
          .participants(native, where)
          .leftMap(e => unavailable(e.message))
      yield Page.of(members.map(p => ReportRef.Participant(cell, p.participant)), request)
    })

  def queries(
      participant: ReportRef.Participant,
      request: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]] =
    val cell = participant.cell
    report(cell.run, cell.reporting, cell.scale.value).flatMap {
      case Left(error)   => F.pure(Left(error))
      case Right(native) =>
        loaded(cell.run).map(_.flatMap { found =>
          for
            where <- nativeCell(native, cell)
            who   <- NativeReportRef
              .participant(where, participant.participant)
              .leftMap(e => unavailable(e.message))
            refs <- ReportNavigation.queries(native, who, found.work.plan.layout).leftMap {
              case eyes4s.results.ReportNavigationError.NotInCell(_, _, _, _) =>
                NavigationError.NotInCell(participant.participant, cell)
              case other => unavailable(other.message)
            }
            converted <- refs.traverse(ref => studio(cell.run, ref))
          yield Page.of(converted, request)
        })
    }

  private def core(
      found: Loaded,
      run: RunId,
      address: ResultAddress
  ): Either[NavigationError, ResultRef[CoreKey]] =
    def key(key: TrialKey): Either[NavigationError, CoreKey] =
      found.context.keys
        .get(key)
        .toRight(NavigationError.Backend(BackendError.UnknownReference(run, address)))
    address match
      case ResultAddress.ContrastRow(scale, query) =>
        key(query).map(ResultRef.ContrastRow(scale, _))
      case ResultAddress.PairRow(scale, design, query, reference) =>
        (key(query), key(reference)).mapN((q, r) =>
          ResultRef.PairRow(scale, RealResults.studyDesign(design), q, r)
        )
      case ResultAddress.Estimation(scale, trial) =>
        key(trial).map(ResultRef.Estimation(scale, _))
      case ResultAddress.Reduction(scale, design, query) =>
        key(query).map(ResultRef.Reduction(scale, RealResults.studyDesign(design), _))

  private def studio(run: RunId, ref: ResultRef[CoreKey]): Either[NavigationError, StudioRef] =
    def index(scale: Int) = ScaleIndex.of(scale).leftMap(e => unavailable(e.message))
    ref match
      case ResultRef.ContrastRow(scale, key) =>
        index(scale).map(StudioRef.QueryContrast(run, _, RealResults.key(key)))
      case ResultRef.PairRow(scale, design, query, reference) =>
        index(scale).map(
          StudioRef.Pair(
            run,
            _,
            PairDesign.of(design),
            RealResults.key(query),
            RealResults.key(reference)
          )
        )
      case ResultRef.Estimation(scale, key) =>
        index(scale).map(StudioRef.TrialMap(run, _, RealResults.key(key)))
      case _ => Left(unavailable(s"${run.label} has unsupported navigation reference $ref."))

  def pairs(
      contrast: StudioRef,
      design: PairDesign,
      request: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]] = contrast match
    case StudioRef.QueryContrast(run, scale, query) =>
      loaded(run).map(_.flatMap { found =>
        for
          ref    <- core(found, run, ResultAddress.ContrastRow(scale.value, query))
          offset <- ListingOffset.of(request.offset).leftMap(e => unavailable(e.message))
          size   <- PageSize.of(request.size).leftMap(e => unavailable(e.message))
          page   <- ResultNavigation
            .pairs(found.inspection, ref, RealResults.studyDesign(design), offset, size)
            .leftMap {
              case eyes4s.plan.NavigationError.NoReduction(_, _) =>
                NavigationError.NoPairs(contrast, design)
              case other => unavailable(other.message)
            }
          refs <- page.entries.traverse(studio(run, _))
        yield Page(refs, page.offset.value, page.total, page.next.map(_.value))
      })
    case other => F.pure(Left(NavigationError.WrongLevel(other, ChainLevel.Query)))

  def maps(pair: StudioRef): F[Either[NavigationError, PairMaps]] = pair match
    case StudioRef.Pair(run, scale, design, query, reference) =>
      loaded(run).map(_.flatMap { found =>
        for
          ref  <- core(found, run, ResultAddress.PairRow(scale.value, design, query, reference))
          maps <- ResultNavigation
            .maps(found.inspection, ref)
            .leftMap(e => unavailable(e.message))
          query     <- studio(run, maps._1)
          reference <- studio(run, maps._2)
        yield PairMaps(query, reference)
      })
    case other => F.pure(Left(NavigationError.WrongLevel(other, ChainLevel.Pair)))

  def fixations(
      map: StudioRef,
      request: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]] = map match
    case StudioRef.TrialMap(run, scale, trial) =>
      loaded(run).flatMap {
        case Left(error)  => F.pure(Left(error))
        case Right(found) =>
          val listed = for
            ref       <- core(found, run, ResultAddress.Estimation(scale.value, trial))
            fixations <- ResultNavigation
              .fixations(found.inspection, found.context.provenance, ref)
              .leftMap(e => unavailable(e.message))
            refs <- fixations.traverse(f =>
              FixationIndex
                .of(f.number.value)
                .leftMap(e => unavailable(e.message))
                .map(StudioRef.Fixation(RealResults.key(f.key), _))
            )
          yield Page.of(refs, request)
          F.pure(listed).flatTap {
            case Left(_)     => F.unit
            case Right(page) => remember(found.context, page.entries)
          }
      }
    case other => F.pure(Left(NavigationError.WrongLevel(other, ChainLevel.Map)))

  /** Called when the backend serves trial fixations directly, before any run exists. */
  private[real] def rememberTrial(
      revision: AnalysisRevision,
      refs: Vector[StudioRef]
  ): F[Either[NavigationError, Unit]] =
    prepared(revision).flatMap {
      case Left(error) => F.pure(Left(NavigationError.Backend(error)))
      case Right(work) =>
        context(work).flatMap {
          case Left(error)  => F.pure(Left(error))
          case Right(found) => remember(found, refs).as(Right(()))
        }
    }

  private def remember(context: SourceContext, refs: Vector[StudioRef]): F[Unit] =
    state.update(s =>
      s.copy(fixations = refs.foldLeft(s.fixations) { (all, ref) =>
        val existing = all.getOrElse(ref, Vector.empty)
        all.updated(
          ref,
          if existing.exists(_.work eq context.work) then existing else existing :+ context
        )
      })
    )

  def record(fixation: StudioRef): F[Either[NavigationError, StudioRef]] = fixation match
    case StudioRef.Fixation(trial, number) =>
      state.get.map { snapshot =>
        val contexts = snapshot.fixations.getOrElse(fixation, Vector.empty)
        if contexts.isEmpty then Left(NavigationError.UnboundFixation(fixation))
        else
          contexts
            .traverse { context =>
              for
                key <- context.keys
                  .get(trial)
                  .toRight(NavigationError.Source(fixation, MissingSource.UnknownTrial(trial)))
                position <- ScanpathPosition
                  .of(number.value - 1)
                  .leftMap(e => unavailable(e.message))
                record <- ResultNavigation
                  .record(context.provenance, FixationRef(key, position))
                  .leftMap {
                    case eyes4s.plan.NavigationError.NoRecord(_, _, reason) =>
                      NavigationError.Source(
                        fixation,
                        MissingSource.of(reason, RealResults.key)
                      )
                    case other => unavailable(other.message)
                  }
                ordinal <- RecordNumber.of(record.value).leftMap(e => unavailable(e.message))
              yield FixationSourceContext(
                context.work.revision,
                context.work.dataset,
                context.source,
                context.input,
                ordinal
              )
            }
            .flatMap { sources =>
              val identities = sources.map(c => (c.source, c.input, c.record)).distinct
              if identities.size > 1 then
                Left(NavigationError.AmbiguousFixation(fixation, sources))
              else
                sources.headOption
                  .map(source =>
                    StudioRef
                      .SourceRecord(trial, Some(number), SourceRole.Fixations, source.record)
                  )
                  .toRight(NavigationError.UnboundFixation(fixation))
            }
      }
    case other => F.pure(Left(NavigationError.WrongLevel(other, ChainLevel.Fixation)))

  private def using(map: StudioRef): F[Either[NavigationError, (RunId, Loaded, CoreKey, Int)]] =
    map match
      case StudioRef.TrialMap(run, scale, trial) =>
        loaded(run).map(_.flatMap { found =>
          core(found, run, ResultAddress.Estimation(scale.value, trial)).flatMap { ref =>
            found.inspection.estimation(ref).leftMap(e => unavailable(e.message)).flatMap { _ =>
              found.context.keys
                .get(trial)
                .map(key => (run, found, key, scale.value))
                .toRight(
                  NavigationError.Backend(
                    BackendError
                      .UnknownReference(run, ResultAddress.Estimation(scale.value, trial))
                  )
                )
            }
          }
        })
      case other => F.pure(Left(NavigationError.WrongLevel(other, ChainLevel.Map)))

  def usedByCounts(map: StudioRef): F[Either[NavigationError, UsedBy]] =
    using(map).map(_.flatMap { (_, found, key, scale) =>
      found.inspection.scale(scale).leftMap(e => unavailable(e.message)).map { inspected =>
        val counts = inspected.usedByCounts(key)
        UsedBy(counts.asQuery, counts.asMatched, counts.asControl)
      }
    })

  def usedBy(
      map: StudioRef,
      role: UsedByRole,
      request: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]] =
    using(map).map(_.flatMap { (run, found, key, scale) =>
      found.inspection.scale(scale).leftMap(e => unavailable(e.message)).flatMap { inspected =>
        val roles = inspected.usedBy(key)
        val refs  = role match
          case UsedByRole.AsQuery   => roles.asQuery
          case UsedByRole.AsMatched => roles.asMatched
          case UsedByRole.AsControl => roles.asControl
        refs.traverse(studio(run, _)).map(Page.of(_, request))
      }
    })

  private def contextOf(work: RealPrepared): Either[NavigationError, SourceContext] =
    for
      coordinates <- CoordinateProvenance
        .of(work.plan, work.admitted.input, Some(work.admitted.evidence))
        .leftMap(e => unavailable(e.message))
      source <- work.admitted.spec.sources.fixations
        .map(_.bytes)
        .toRight(unavailable(s"${work.revision.label} has no fixation source identity."))
      canonical <- work.inputs.input
        .digest(work.admitted.input)
        .leftMap(e => unavailable(e.message))
      input <- CanonicalDigest
        .parse[StudyInputArtifact](canonical.sha256.hex)
        .leftMap(e => unavailable(e.message))
    yield SourceContext(
      work,
      coordinates,
      source,
      input,
      work.admitted.input.trials.rows.map(r => RealResults.key(r.key) -> r.key).toMap
    )

object RealNavigator:
  private[real] final case class SourceContext(
      work: RealPrepared,
      provenance: CoordinateProvenance[CoreKey, Unit2D.Px],
      source: ByteDigest,
      input: CanonicalDigest[StudyInputArtifact],
      keys: Map[TrialKey, CoreKey]
  )
  private[real] final case class Loaded(
      work: RealPrepared,
      inspection: StudyInspection[CoreKey, Unit2D.Px, Similarity, SignedDifference],
      context: SourceContext
  )
  private[real] final case class State(
      reports: Map[(RunId, ReportingSpec, Int), Report[CoreKey]],
      bindings: Map[(RunId, ReportingId, Int), ReportingSpec],
      inspections: Map[RunId, Loaded],
      fixations: Map[StudioRef, Vector[SourceContext]],
      contexts: Vector[SourceContext],
      defaults: Option[Map[ReportingId, ReportingSpec]]
  )
  private[real] def empty[F[_]: Concurrent]: F[Ref[F, State]] =
    Ref.of[F, State](State(Map.empty, Map.empty, Map.empty, Map.empty, Vector.empty, None))
  private def group(cell: NativeReportRef.Cell): Option[Response] =
    cell.group.levels.headOption.map((_, value) => Response(value))
  private def role(role: Role): ReportRole = role match
    case Role.Matched    => ReportRole.Matched
    case Role.Control    => ReportRole.Control
    case Role.Difference => ReportRole.Difference
