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

package eyes4s.studio.app.data

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.app.text.Messages
import eyes4s.studio.core.document.Perspective

/** The words of Data's setup steps (overhaul v4, `data.html`). */
enum DataStepsTextId derives CanEqual:
  case Sources, Columns, TrialIdentity, Geometry, Admission
  case SourcesSub, SourcesMissingSub, ColumnsSub, TrialIdentitySub, GeometrySub, AdmissionSub
  case AdmittedSub, Spoken, Done, Problems

object DataStepsText:
  def english(id: DataStepsTextId): String =
    import DataStepsTextId.*
    id match
      case Sources           => "Sources"
      case Columns           => "Columns"
      case TrialIdentity     => "Trial identity"
      case Geometry          => "Geometry"
      case Admission         => "Admission"
      case SourcesSub        => "{0} inputs"
      case SourcesMissingSub => "{0} inputs · {1}"
      case ColumnsSub        => "the fixation table's columns"
      case TrialIdentitySub  => "participant, phase, trial, occurrence"
      case GeometrySub       => "screen, image frame, px/°"
      case AdmissionSub      => "review, then admit {0}"
      case AdmittedSub       => "{0} admitted"
      case Spoken            => "Step {0}, {1}: {2}"
      case Done              => "done"
      case Problems          => "{0} to repair"

  def apply(id: DataStepsTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

/** A step's state at its right: problems to repair, done, or nothing yet. */
enum StepState derives CanEqual:
  case Problems(count: Int)
  case Done
  case Open

/** One setup step: its number, words, state, whether the trail is on it,
  * and where it goes.
  */
final case class DataStepVM(
    section: DataSection,
    number: Int,
    title: String,
    sub: String,
    state: StepState,
    current: Boolean,
    spoken: String,
    open: Intent
) derives CanEqual

/** Data's setup as five steps (overhaul v4): the sources, the columns, the
  * trial identity, the geometry and the admission. A step opens its
  * section; the trail's section is the current step.
  */
object DataSteps:
  import DataStepsTextId.*

  /** The section the Data trail is on, if any. */
  def section(m: AppModel): Option[DataSection] =
    m.navigation.trail(Perspective.Data).reverseIterator.collectFirst {
      case Place.DataView(s) => s
    }

  def vm(m: AppModel, sources: SourcesVM): Vector[DataStepVM] =
    val dataset = m.document.datasets.lastOption
    dataset.toVector.flatMap { d =>
      val current  = section(m)
      val missing  = sources.missing.map(_.files.size).getOrElse(0)
      val admitted = d.decision.isAdmitted
      def step(
          s: DataSection,
          n: Int,
          title: DataStepsTextId,
          sub: String,
          state: StepState
      ) =
        val words = DataStepsText(title)
        val tail  = state match
          case StepState.Problems(k) => DataStepsText(Problems, k.toString)
          case StepState.Done        => DataStepsText(Done)
          case StepState.Open        => sub
        DataStepVM(
          s,
          n,
          words,
          sub,
          state,
          current.contains(s),
          DataStepsText(Spoken, n.toString, words, tail),
          Intent.Navigate(
            Location(Perspective.Data, Vector(Place.Dataset(d.id), Place.DataView(s)))
          )
        )
      val inputs     = sources.sources.size.toString
      val sourcesSub = sources.missing.fold(DataStepsText(SourcesSub, inputs))(mv =>
        DataStepsText(SourcesMissingSub, inputs, mv.title.toLowerCase)
      )
      Vector(
        step(
          DataSection.Sources,
          1,
          Sources,
          sourcesSub,
          if missing > 0 then StepState.Problems(missing) else StepState.Done
        ),
        step(DataSection.ColumnMapping, 2, Columns, DataStepsText(ColumnsSub), StepState.Done),
        step(
          DataSection.TrialMetadata,
          3,
          TrialIdentity,
          DataStepsText(TrialIdentitySub),
          StepState.Done
        ),
        step(DataSection.Geometry, 4, Geometry, DataStepsText(GeometrySub), StepState.Done),
        step(
          DataSection.Admission,
          5,
          Admission,
          if admitted then DataStepsText(AdmittedSub, d.id.label)
          else DataStepsText(AdmissionSub, d.id.label),
          if admitted then StepState.Done else StepState.Open
        )
      )
    }
