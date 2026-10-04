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

package eyes4s.studio.core.assets

import eyes4s.codec.ByteDigest
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.bundle.{InputKind, InputStatus}
import eyes4s.studio.core.document.{Source, StudioDocument}

/** What the project store holds for one dataset source (ticket S2.5): its
  * recorded bytes, none, other bytes at its address, or bytes the bundle
  * withheld or that could not be read. Only [[Present]] can be run.
  */
enum SourceState derives CanEqual:
  case Present
  case Missing
  case Changed(found: ByteDigest)
  case Withheld
  case Unreadable(reason: String)

  def blocks: Boolean = this != Present

/** One source of one dataset revision and what the store holds for it. */
final case class SourceFinding(dataset: DatasetRevision, source: Source, state: SourceState)
    derives CanEqual:
  /** The source's file name, as the project stores it. */
  def name: String = source.path.value.split('/').last

/** What is known of the project's stored inputs (ticket S2.5). Only a
  * window with no project store ([[NoProject]]: the sources are not stored
  * copies, as in the fixture backend) runs without a check; every other
  * state blocks until a check has found the sources present (fail closed).
  */
enum InputCheck derives CanEqual:
  /** No project store: there is nothing stored to check. */
  case NoProject

  /** A project whose inputs have not been checked yet. */
  case Unchecked

  /** The state of every input the project lists, by digest. */
  case Checked(statuses: Vector[InputStatus])

  /** The check itself failed (a store error, or a port that cannot check). */
  case CheckFailed(reason: String)

/** Why a dataset revision may not be run or previewed (ticket S2.5). */
enum SourceBlock derives CanEqual:
  /** The project's stored inputs have not been checked yet. */
  case Unchecked

  /** The check failed, with its reason. */
  case CheckFailed(reason: String)

  /** These sources are not present as recorded. */
  case Damaged(findings: Vector[SourceFinding])

/** The stored state of every dataset source of a document (ticket S2.5): a
  * source is matched to the listed input of its role whose digest is the
  * one the source records (a revision's source is its exact bytes, so two
  * revisions that import the same file name with other bytes never share
  * an input), and the store's bytes at that input's address are checked
  * against the digest. A source whose input is not listed is missing. A
  * source that is not present blocks its revision's runs and previews; it
  * is repaired by re-copying its exact bytes, or replaced by a new pending
  * revision, which runs once admitted. Stimulus images are listed apart:
  * they do not block.
  */
final case class SourceCheck private (
    check: InputCheck,
    findings: Vector[SourceFinding],
    images: Vector[InputStatus]
) derives CanEqual:

  /** `dataset`'s sources, in its source order. */
  def of(dataset: DatasetRevision): Vector[SourceFinding] =
    findings.filter(_.dataset == dataset)

  /** `dataset`'s sources that are not present as recorded. */
  def blocking(dataset: DatasetRevision): Vector[SourceFinding] =
    of(dataset).filter(_.state.blocks)

  /** Why `dataset` may not be run or previewed, if it may not. */
  def block(dataset: DatasetRevision): Option[SourceBlock] = check match
    case InputCheck.NoProject           => None
    case InputCheck.Unchecked           => Some(SourceBlock.Unchecked)
    case InputCheck.CheckFailed(reason) => Some(SourceBlock.CheckFailed(reason))
    case InputCheck.Checked(_)          =>
      Option(blocking(dataset)).filter(_.nonEmpty).map(SourceBlock.Damaged(_))

  /** The finding for `dataset`'s `source`, if it was checked. */
  def find(dataset: DatasetRevision, source: Source): Option[SourceFinding] =
    of(dataset).find(_.source == source)

  /** The stored images among `digests` (one dataset's) whose bytes changed
    * or went missing; a withheld image is neither.
    */
  def imagesUnstored(digests: Set[ByteDigest]): Vector[InputStatus] =
    images.filter(s => digests.contains(s.entry.sha256))

object SourceCheck:

  def of(document: StudioDocument, check: InputCheck): SourceCheck = check match
    case InputCheck.Checked(statuses) =>
      val findings = for
        spec   <- document.datasets
        source <- spec.sources.entries
      yield
        val kind  = InputKind.Source(source.role)
        val entry = statuses.find(s =>
          s.entry.kind == kind && s.entry.sha256 == source.bytes &&
            s.entry.name.forall(_ == source.path.value.split('/').last)
        )
        SourceFinding(spec.id, source, entry.fold(SourceState.Missing)(stateOf))
      SourceCheck(
        check,
        findings,
        statuses.filter(s => s.entry.kind == InputKind.StimulusImage && unstored(s))
      )
    case other => SourceCheck(other, Vector.empty, Vector.empty)

  private def unstored(s: InputStatus): Boolean = s match
    case InputStatus.Present(_) | InputStatus.Withheld(_) => false
    case _                                                => true

  private def stateOf(status: InputStatus): SourceState = status match
    case InputStatus.Present(_)           => SourceState.Present
    case InputStatus.Withheld(_)          => SourceState.Withheld
    case InputStatus.Missing(_)           => SourceState.Missing
    case InputStatus.Changed(_, found)    => SourceState.Changed(found)
    case InputStatus.Unreadable(_, error) => SourceState.Unreadable(error.message)
