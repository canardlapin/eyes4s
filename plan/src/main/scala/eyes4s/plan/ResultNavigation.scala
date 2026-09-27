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

import eyes4s.kernel.Unit2D

/** The levels of the provenance chain an application walks, from a
  * report's summary down to a source record. The first two are report
  * levels (`eyes4s-results`); the rest are addressed here.
  */
enum NavigationLevel derives CanEqual:
  case Summary, Participant, QueryContrast, Pair, Map, Fixation, Record

/** One fixation of a trial: the trial key and its position in the scanpath,
  * counted from 0. [[number]] is the display form, counted from 1.
  */
final case class FixationRef[K](key: K, position: ScanpathPosition) derives CanEqual:
  def number: FixationNumber = position.number

/** Why a navigation step was refused. Every case names its operands. */
enum NavigationError[+K] derives CanEqual:
  case NegativeOffset(value: Int)

  /** The reference is not at the level the step starts from. */
  case WrongLevel(ref: ResultRef[K], expected: NavigationLevel)

  /** The inspection has no such item, or refused the lookup. */
  case Inspection(underlying: InspectionError[K])

  /** The query contrast has no stored reduction for this design. */
  case NoReduction(contrast: ResultRef[K], design: StudyDesign)

  /** The inspection and the provenance describe different inputs. */
  case InputMismatch(inspection: String, provenance: String)

  /** The provenance refused the trial or fixation. */
  case Provenance(underlying: ProvenanceError[K])

  /** No record of this fixation is known; `reason` says why. */
  case NoRecord(key: K, position: ScanpathPosition, reason: MissingSource[K])

  /** The ledger lists no such record. */
  case UnknownRecord(record: DataRecord)

  /** The record was rejected, so it supplied no fixation. */
  case NotAdmitted(record: DataRecord)

  def message: String = this match
    case NegativeOffset(v)             => s"A listing offset counts from 0; $v is negative."
    case WrongLevel(ref, expected)     => s"$ref is not a ${expected.toString} reference."
    case Inspection(underlying)        => underlying.message
    case NoReduction(contrast, design) =>
      s"$contrast has no stored ${design.toString.toLowerCase} reduction."
    case InputMismatch(inspection, provenance) =>
      s"The inspection describes input $inspection; the provenance describes $provenance."
    case Provenance(underlying)          => underlying.message
    case NoRecord(key, position, reason) =>
      s"No record of fixation ${position.number.value} of $key is known: $reason."
    case UnknownRecord(record) => s"The ledger lists no data record ${record.value}."
    case NotAdmitted(record)   =>
      s"Data record ${record.value} was rejected, so it supplied no fixation."

object NavigationError:
  /** A result reference as an operand: its kind, then its fields. */
  private[eyes4s] given resultRefOperand[K]: DiagnosticOperand[ResultRef[K], K] =
    new DiagnosticOperand[ResultRef[K], K]:
      def apply(value: ResultRef[K]): Operand[K] = Projections.resultRef(value)

  /** Why no source is known, as an operand: its case, then its fields. */
  private[eyes4s] given missingOperand[K]: DiagnosticOperand[MissingSource[K], K] =
    new DiagnosticOperand[MissingSource[K], K]:
      def apply(value: MissingSource[K]): Operand[K] =
        import MissingSource.*
        def kind(fields: (String, Operand[K])*): Operand[K] =
          Operand.Fields(("kind" -> Operand.Token(value.productPrefix)) +: fields.toVector)
        def int(n: Int): Operand[K] = Operand.Integer(BigInt(n))
        value match
          case NoLedger                          => Operand.Token(value.productPrefix)
          case UnknownTrial(key)                 => kind("key" -> Operand.Key(key))
          case FixationOutOfRange(key, index, n) =>
            kind("key" -> Operand.Key(key), "index" -> int(index), "fixations" -> int(n))
          case NotSourceSupported(key) => kind("key" -> Operand.Key(key))
          case AmbiguousTrial(key, n)  =>
            kind("key" -> Operand.Key(key), "occurrences" -> int(n))
          case UnknownDigest(digest)         => kind("digest" -> Operand.Text(digest))
          case CollidingDigest(digest, keys) =>
            kind("digest" -> Operand.Text(digest), "keys" -> Operand.Keys(keys))
          case UnknownInputTrial(index, trials) =>
            kind("index" -> int(index), "trials" -> int(trials))
          case MissingSource.NotAdmitted(key, records) =>
            kind(
              "key"     -> Operand.Key(key),
              "records" -> Operand.Integers(records.map(BigInt(_)))
            )

  given diagnose[K]: Diagnose[NavigationError[K], K] =
    given DiagnosticOperand[K, K] = DiagnosticOperand.key[K]
    Diagnose.derived[NavigationError[K], K](DiagnosticCatalog.navigation, subject[K])(
      _.message
    )

  /** The item, trial, fixation or record a refusal names. */
  private def subject[K](error: NavigationError[K]): Vector[Locus[K]] = error match
    case WrongLevel(ref, _)    => ref.loci
    case NoReduction(ref, _)   => ref.loci
    case NoRecord(key, p, _)   => Vector(Locus.Trial(key), Locus.Fixation(p.value))
    case UnknownRecord(record) => Vector(Locus.Record(record.csv.value))
    case NotAdmitted(record)   => Vector(Locus.Record(record.csv.value))
    case _                     => Vector.empty

/** The provenance chain below a report: query contrast > pair > map >
  * fixation > record, and back up. Each step down is refused with a typed
  * [[NavigationError]] rather than guessed; each step up is determined by the
  * reference alone, except a map's pair, which the path taken to it names.
  */
object ResultNavigation:
  private def contrastOf[K](ref: ResultRef[K]): Option[(Int, K)] = ref match
    case ResultRef.ContrastRow(scale, key) => Some(scale -> key)
    case ResultRef.InCell(_, _, inner)     => contrastOf(inner)
    case _                                 => None

  private def pairOf[K](ref: ResultRef[K]): Option[(Int, StudyDesign, K, K)] = ref match
    case ResultRef.PairRow(scale, design, focal, reference) =>
      Some((scale, design, focal, reference))
    case ResultRef.InCell(_, _, inner) => pairOf(inner)
    case _                             => None

  private def mapOf[K](ref: ResultRef[K]): Option[(Int, K)] = ref match
    case ResultRef.Estimation(scale, key) => Some(scale -> key)
    case ResultRef.InCell(_, _, inner)    => mapOf(inner)
    case _                                => None

  /** A reference a listing holds, checked without building its entry. */
  private def listed[K](found: Boolean, ref: ResultRef[K]): Either[NavigationError[K], Unit] =
    Either.cond(found, (), NavigationError.Inspection(InspectionError.UnknownReference(ref)))

  /** The same cell wrapping as `like`, around `ref`. */
  private def within[K](like: ResultRef[K], ref: ResultRef[K]): ResultRef[K] = like match
    case ResultRef.InCell(repetition, window, inner) =>
      ResultRef.InCell(repetition, window, within(inner, ref))
    case _ => ref

  /** One page of the pairs of a query contrast's `design` reduction: the
    * stored pairs whose query trial is the contrast's. The page's `total` is
    * known without building any pair entry.
    */
  def pairs[K, U <: Unit2D, S, D](
      inspection: StudyInspection[K, U, S, D],
      contrast: ResultRef[K],
      design: StudyDesign,
      offset: ListingOffset,
      size: PageSize
  ): Either[NavigationError[K], OffsetPage[ResultRef[K]]] =
    for
      (scale, key) <- contrastOf(contrast).toRight(
        NavigationError.WrongLevel(contrast, NavigationLevel.QueryContrast)
      )
      entry <- inspection.contrastRow(contrast).left.map(NavigationError.Inspection.apply)
      _     <- (design match
        case StudyDesign.Matched => entry.matched
        case StudyDesign.Control => entry.control
      ).toRight(NavigationError.NoReduction(contrast, design))
      found <- inspection.scale(scale).left.map(NavigationError.Inspection.apply)
    yield
      val refs = found.pairsOfQuery(design, key)
      OffsetPage.of(refs.size, offset, size)(refs)

  /** The two maps a pair compares: its query's, then its reference's. */
  def maps[K, U <: Unit2D, S, D](
      inspection: StudyInspection[K, U, S, D],
      pair: ResultRef[K]
  ): Either[NavigationError[K], (ResultRef[K], ResultRef[K])] =
    for
      (scale, design, focal, reference) <- pairOf(pair).toRight(
        NavigationError.WrongLevel(pair, NavigationLevel.Pair)
      )
      found <- inspection.scale(scale).left.map(NavigationError.Inspection.apply)
      _     <- listed(found.pairs(design).contains(pair), pair)
      query = within(pair, ResultRef.Estimation(scale, focal))
      other = within(pair, ResultRef.Estimation(scale, reference))
      _ <- listed(found.estimation.contains(query), query)
      _ <- listed(found.estimation.contains(other), other)
    yield query -> other

  /** The fixations of a map's trial, in scanpath order. */
  def fixations[K, U <: Unit2D, S, D](
      inspection: StudyInspection[K, U, S, D],
      provenance: CoordinateProvenance[K, U],
      map: ResultRef[K]
  ): Either[NavigationError[K], Vector[FixationRef[K]]] =
    for
      (_, key) <- mapOf(map).toRight(NavigationError.WrongLevel(map, NavigationLevel.Map))
      _        <- inspection.estimation(map).left.map(NavigationError.Inspection.apply)
      _        <- Either.cond(
        inspection.input.digest == provenance.sources.input.digest,
        (),
        NavigationError.InputMismatch(
          inspection.input.digest,
          provenance.sources.input.digest
        )
      )
      positions <- provenance.positions(key).left.map(NavigationError.Provenance.apply)
    yield positions.map(FixationRef(key, _))

  /** The source record that supplied a fixation. */
  def record[K, U <: Unit2D](
      provenance: CoordinateProvenance[K, U],
      fixation: FixationRef[K]
  ): Either[NavigationError[K], DataRecord] =
    provenance
      .fixation(fixation.key, fixation.position)
      .left
      .map(NavigationError.Provenance.apply)
      .flatMap(
        _.record.left.map(NavigationError.NoRecord(fixation.key, fixation.position, _))
      )

  // ------------------------------------------------------------ up

  /** The query contrast a pair belongs to: its query trial's contrast row. */
  def queryOf[K](pair: ResultRef[K]): Either[NavigationError[K], ResultRef[K]] =
    pairOf(pair)
      .map((scale, _, focal, _) => within(pair, ResultRef.ContrastRow(scale, focal)))
      .toRight(NavigationError.WrongLevel(pair, NavigationLevel.Pair))

  /** The fixation a source record supplied. */
  def fixationOf[K, U <: Unit2D](
      provenance: CoordinateProvenance[K, U],
      record: DataRecord
  ): Either[NavigationError[K], FixationRef[K]] =
    provenance.sources.entryAt(record) match
      case None =>
        Left(NavigationError.UnknownRecord(record))
      case Some(SourceRecord(_, Disposition.Rejected(_, _, _))) =>
        Left(NavigationError.NotAdmitted(record))
      case Some(SourceRecord(number, Disposition.Admitted(key, _))) =>
        provenance.sources
          .trial(key)
          .toOption
          .flatMap(_.admitted.find(_.record == number))
          .map(source => FixationRef(key, new ScanpathPosition(source.index)))
          .toRight(NavigationError.NotAdmitted(record))
