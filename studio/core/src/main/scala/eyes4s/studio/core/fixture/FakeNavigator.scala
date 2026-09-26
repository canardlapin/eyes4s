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

package eyes4s.studio.core.fixture

import cats.Monad
import cats.data.EitherT
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ReportingId, SourceRole}
import eyes4s.studio.core.navigation.*
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, ScaleIndex, StudioRef}

/** The provenance chain's down functions over the mock study (ticket S3.4),
  * as `FakeStudyBackend` serves them: summary cells and participant means
  * from fixture.json, pairs from its queries, and each admitted golden
  * trial's scanpath as fixations.csv data records (generated at build time
  * from fixtures/studio-golden; `GoldenInventory.scanpaths`).
  *
  * The fake computes no science. It knows one grouping, by retrieval
  * response, and serves it for every reporting spec. A query's control
  * references follow FIXTURE.md's stated pool (same participant, admitted
  * encoding trials of other items), and the fake serves them only where that
  * pool has exactly the `controls` count fixture.json records for the query;
  * otherwise its control pairs are `NoPairs`. Pairs belong to contributing
  * queries, the ones with a contrast row. `scored` refuses a run the fixture
  * has no scores for; `status` is the backend's own reading of a query.
  */
final class FakeNavigator[F[_]] private[fixture] (
    study: MockStudy,
    scored: RunId => F[Either[BackendError, Unit]],
    status: MockQuery => QueryStatus
)(using F: Monad[F])
    extends StudyNavigator[F]:
  import FakeNavigator.*

  private val summary                         = study.summary
  private val byKey: Map[TrialKey, MockQuery] = study.queries.map(q => q.key -> q).toMap
  private val inventory: Set[TrialKey]        = study.inventory.map(_.trial).toSet
  private val groups: Vector[Response]        = summary.groups.map(_.label)

  private type Step[A] = EitherT[F, NavigationError, A]

  private def pure[A](a: Either[NavigationError, A]): Step[A] = EitherT.fromEither[F](a)

  /** The run is scored and `scale` is one of its scales. */
  private def scoredAt(run: RunId, scale: Int): Step[Unit] =
    EitherT(scored(run)).leftMap(NavigationError.Backend(_)).flatMap { _ =>
      pure(
        Either.cond(
          summary.scales.indices.contains(scale),
          (),
          NavigationError.Backend(BackendError.Unavailable(DiagnosticLocus.Scale(scale)))
        )
      )
    }

  def cells(
      run: RunId,
      reporting: ReportingId,
      scale: ScaleIndex,
      page: PageRequest
  ): F[Either[NavigationError, Page[ReportRef.Cell]]] =
    scoredAt(run, scale.value)
      .map(_ => Page.of(groups.map(g => ReportRef.Cell(run, reporting, scale, Some(g))), page))
      .value

  private def inCell(cell: ReportRef.Cell): Step[Vector[ParticipantSummary]] =
    scoredAt(cell.run, cell.scale.value).flatMap { _ =>
      pure(cell.group match
        case Some(g) if !groups.contains(g) => Left(NavigationError.UnknownGroup(cell, groups))
        case group                          =>
          Right(summary.participants.filter { p =>
            group.forall(g => p.groups.exists(m => m.label == g && m.n > 0))
          }))
    }

  def participants(
      cell: ReportRef.Cell,
      page: PageRequest
  ): F[Either[NavigationError, Page[ReportRef.Participant]]] =
    inCell(cell)
      .map(ps => Page.of(ps.map(p => ReportRef.Participant(cell, p.participant)), page))
      .value

  def queries(
      participant: ReportRef.Participant,
      page: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]] =
    val cell = participant.cell
    inCell(cell).flatMap { members =>
      pure(
        Either.cond(
          members.exists(_.participant == participant.participant),
          Page.of(
            study.queries
              .filter(q =>
                q.participant == participant.participant &&
                  cell.group.forall(_ == q.response) && contributing(status(q))
              )
              .map(q => StudioRef.QueryContrast(cell.run, cell.scale, q.key)),
            page
          ),
          NavigationError.NotInCell(participant.participant, cell)
        )
      )
    }.value

  /** The admitted encoding trials of the query's participant with another
    * item (FIXTURE.md's control pool), when their number is the query's
    * recorded `controls`.
    */
  private def controlsOf(q: MockQuery): Option[Vector[TrialKey]] =
    val pool = study.inventory.collect {
      case e
          if e.trial.participant == q.participant && e.trial.phase == Phase.Encoding &&
            e.disposition == TrialDisposition.Admitted && e.item != q.item =>
        e.trial
    }
    Option.when(q.controls.contains(pool.size))(pool)

  /** Every contributing query with its matched reference and controls. */
  private lazy val designs: Vector[(MockQuery, Option[Vector[TrialKey]])] =
    study.queries.filter(q => contributing(status(q))).map(q => q -> controlsOf(q))

  /** The query of a contrast ref, scored at its scale. */
  private def scoredQuery(contrast: StudioRef): Step[(RunId, ScaleIndex, MockQuery)] =
    contrast match
      case StudioRef.QueryContrast(run, scale, key) =>
        for
          _ <- scoredAt(run, scale.value)
          q <- pure(
            byKey
              .get(key)
              .toRight(
                NavigationError.Backend(
                  BackendError
                    .UnknownReference(run, ResultAddress.ContrastRow(scale.value, key))
                )
              )
          )
          _ <- pure(
            Either.cond(
              contributing(status(q)),
              (),
              NavigationError.NoContrast(contrast, status(q))
            )
          )
        yield (run, scale, q)
      case other => pure(Left(NavigationError.WrongLevel(other, ChainLevel.Query)))

  def pairs(
      contrast: StudioRef,
      design: PairDesign,
      page: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]] =
    scoredQuery(contrast).flatMap { (run, s, q) =>
      val references: Either[NavigationError, Vector[TrialKey]] = design match
        case PairDesign.Matched => Right(Vector(q.matchedKey))
        case PairDesign.Control =>
          controlsOf(q).toRight(NavigationError.NoPairs(contrast, design))
      pure(references.map { all =>
        Page.of(all.map(r => StudioRef.Pair(run, s, design, q.key, r)), page)
      })
    }.value

  def maps(pair: StudioRef): F[Either[NavigationError, PairMaps]] = pair match
    case StudioRef.Pair(run, scale, _, focal, reference) =>
      scoredAt(run, scale.value).map { _ =>
        PairMaps(
          StudioRef.TrialMap(run, scale, focal),
          StudioRef.TrialMap(run, scale, reference)
        )
      }.value
    case other => F.pure(Left(NavigationError.WrongLevel(other, ChainLevel.Pair)))

  /** The trial's scanpath records, or why it has none. */
  private def scanpath(
      subject: StudioRef,
      key: TrialKey
  ): Either[NavigationError, Vector[Int]] =
    Scanpaths
      .get(key)
      .toRight(
        NavigationError.Source(
          subject,
          if inventory.contains(key) then MissingSource.NotAdmitted(key)
          else MissingSource.UnknownTrial(key)
        )
      )

  def fixations(
      map: StudioRef,
      page: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]] =
    map match
      case StudioRef.TrialMap(run, scale, key) =>
        scoredAt(run, scale.value).flatMap { _ =>
          pure(scanpath(map, key).map { records =>
            Page.of(
              records.indices.toVector.flatMap(i =>
                FixationIndex.of(i + 1).toOption.map(StudioRef.Fixation(key, _))
              ),
              page
            )
          })
        }.value
      case other => F.pure(Left(NavigationError.WrongLevel(other, ChainLevel.Map)))

  def record(fixation: StudioRef): F[Either[NavigationError, StudioRef]] =
    F.pure(fixation match
      case StudioRef.Fixation(key, index) =>
        scanpath(fixation, key).flatMap { records =>
          records
            .lift(index.value - 1)
            .flatMap(n => RecordNumber.of(n).toOption)
            .map(n => StudioRef.SourceRecord(key, Some(index), SourceRole.Fixations, n))
            .toRight(
              NavigationError.Source(
                fixation,
                MissingSource.FixationOutOfRange(key, index.value, records.size)
              )
            )
        }
      case other => Left(NavigationError.WrongLevel(other, ChainLevel.Fixation)))

  /** Every pair of the map's run and scale that uses its trial in `role`. */
  private def using(map: StudioRef): Step[(RunId, ScaleIndex, TrialKey)] = map match
    case StudioRef.TrialMap(run, scale, key) =>
      scoredAt(run, scale.value).map(_ => (run, scale, key))
    case other => pure(Left(NavigationError.WrongLevel(other, ChainLevel.Map)))

  private def pairsUsing(
      run: RunId,
      scale: ScaleIndex,
      key: TrialKey,
      role: UsedByRole
  ): Vector[StudioRef] =
    def pair(q: MockQuery, design: PairDesign, reference: TrialKey) =
      StudioRef.Pair(run, scale, design, q.key, reference)
    role match
      case UsedByRole.AsQuery =>
        designs.collect {
          case (q, controls) if q.key == key =>
            pair(q, PairDesign.Matched, q.matchedKey) +:
              controls.getOrElse(Vector.empty).map(pair(q, PairDesign.Control, _))
        }.flatten
      case UsedByRole.AsMatched =>
        designs.collect {
          case (q, _) if q.matchedKey == key => pair(q, PairDesign.Matched, key)
        }
      case UsedByRole.AsControl =>
        designs.collect {
          case (q, Some(controls)) if controls.contains(key) => pair(q, PairDesign.Control, key)
        }

  def usedByCounts(map: StudioRef): F[Either[NavigationError, UsedBy]] =
    using(map).map { (run, scale, key) =>
      def n(role: UsedByRole) = pairsUsing(run, scale, key, role).size
      UsedBy(n(UsedByRole.AsQuery), n(UsedByRole.AsMatched), n(UsedByRole.AsControl))
    }.value

  def usedBy(
      map: StudioRef,
      role: UsedByRole,
      page: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]] =
    using(map).map((run, scale, key) => Page.of(pairsUsing(run, scale, key, role), page)).value

object FakeNavigator:

  private def contributing(status: QueryStatus): Boolean = status match
    case QueryStatus.Contributing(_, _, _) => true
    case _                                 => false

  /** Each admitted golden trial's scanpath as data record numbers. */
  private[fixture] lazy val Scanpaths: Map[TrialKey, Vector[Int]] =
    GoldenInventory.scanpaths.linesIterator.flatMap { line =>
      line.split("\t", -1).toList match
        case List(p, phase, trial, occurrence, records) =>
          occurrence.toIntOption.map { occ =>
            TrialKey(p, Phase(phase), trial, occ) ->
              records.split(',').toVector.filter(_.nonEmpty).flatMap(_.toIntOption)
          }
        case _ => None
    }.toMap
