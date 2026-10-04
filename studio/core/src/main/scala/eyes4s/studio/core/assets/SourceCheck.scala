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

/** Whether the project's stored inputs were checked, and what was found. */
enum InputCheck derives CanEqual:
  /** Not checked (no project, or not yet): nothing is known to block. */
  case Unchecked

  /** The state of every input the project lists, by digest. */
  case Checked(statuses: Vector[InputStatus])

/** The stored state of every dataset source of a document (ticket S2.5): a
  * source is matched to the input of its role whose digest it records, and
  * the store's bytes at that input's address are checked against the
  * digest. A source whose input is not listed is missing. A source that is
  * not present blocks the runs of its revision; it is repaired by re-copying
  * its exact bytes, or replaced by a new pending revision, which runs once
  * admitted. Stimulus images are listed apart: they do not block runs.
  */
final case class SourceCheck private (
    findings: Vector[SourceFinding],
    images: Vector[InputStatus]
) derives CanEqual:

  /** `dataset`'s sources, in its source order. */
  def of(dataset: DatasetRevision): Vector[SourceFinding] =
    findings.filter(_.dataset == dataset)

  /** `dataset`'s sources that block its runs. */
  def blocking(dataset: DatasetRevision): Vector[SourceFinding] =
    of(dataset).filter(_.state.blocks)

  /** The finding for `dataset`'s `source`, if it was checked. */
  def find(dataset: DatasetRevision, source: Source): Option[SourceFinding] =
    of(dataset).find(_.source == source)

object SourceCheck:

  /** Nothing checked: nothing blocks. */
  val unchecked: SourceCheck = SourceCheck(Vector.empty, Vector.empty)

  def of(document: StudioDocument, check: InputCheck): SourceCheck = check match
    case InputCheck.Unchecked         => unchecked
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
        findings,
        statuses.filter(s => s.entry.kind == InputKind.StimulusImage && !present(s))
      )

  private def present(s: InputStatus): Boolean = s match
    case InputStatus.Present(_) => true
    case _                      => false

  private def stateOf(status: InputStatus): SourceState = status match
    case InputStatus.Present(_)           => SourceState.Present
    case InputStatus.Withheld(_)          => SourceState.Withheld
    case InputStatus.Missing(_)           => SourceState.Missing
    case InputStatus.Changed(_, found)    => SourceState.Changed(found)
    case InputStatus.Unreadable(_, error) => SourceState.Unreadable(error.message)
