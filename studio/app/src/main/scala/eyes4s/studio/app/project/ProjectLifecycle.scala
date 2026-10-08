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

package eyes4s.studio.app.project

import eyes4s.studio.core.backend.{JobId, RunId}

/** A request made from File, or by closing the native window. */
enum ProjectOperation derives CanEqual:
  case New, Open, Close, Quit

/** Close admission uses the current window, including work still running. */
final case class ProjectWork(run: RunId, job: Option[JobId]) derives CanEqual

final case class ProjectCloseFacts(title: String, named: Boolean, edited: Boolean,
    activeWork: Boolean, work: Set[ProjectWork] = Set.empty) derives CanEqual:
  /** A queued run receiving its job id is the same admitted work; a new
    * job for an already-running run needs fresh admission.
    */
  def hasNewWork(previous: ProjectCloseFacts): Boolean =
    (activeWork && !previous.activeWork) || work.exists(w => !previous.work.exists(p =>
      p.run == w.run && (w.job.isEmpty || p.job.isEmpty || p.job == w.job)
    ))
  def needsAdmission: Boolean = edited || activeWork

/** Untitled work cannot be saved here: Save As is a separate lifecycle slice. */
enum ProjectCloseChoice derives CanEqual:
  case KeepOpen, CloseWithoutSaving, SaveAndClose

final case class ProjectOperationId private (value: BigInt) derives CanEqual

object ProjectOperationId:
  private[project] def next(value: BigInt): ProjectOperationId = new ProjectOperationId(value)

enum ProjectLifecyclePhase derives CanEqual:
  case Preparing
  case Confirming(facts: ProjectCloseFacts)
  case Saving(facts: ProjectCloseFacts)
  case Committing

/** What the host performs after the pure state transition. */
enum ProjectLifecycleAction derives CanEqual:
  case Ignored, Save, Commit, KeepOpen
  case Ask(facts: ProjectCloseFacts)

final case class ProjectLifecycle private (
    generation: BigInt,
    operation: Option[(ProjectOperationId, ProjectOperation, ProjectLifecyclePhase)],
    stopped: Boolean
) derives CanEqual:
  def owns(id: ProjectOperationId): Boolean = !stopped && operation.exists(_._1 == id)
  def busy: Boolean = operation.isDefined

  /** Only one operation owns a candidate. Callers cancel before starting another. */
  def begin(request: ProjectOperation): (ProjectLifecycle, Option[ProjectOperationId]) =
    if stopped || busy then (this, None)
    else
      val next = generation + 1
      val id = ProjectOperationId.next(next)
      (copy(generation = next, operation = Some((id, request, ProjectLifecyclePhase.Preparing))), Some(id))

  /** A candidate is ready, or Close/Quit needs no candidate. */
  def prepared(id: ProjectOperationId, facts: ProjectCloseFacts): (ProjectLifecycle, ProjectLifecycleAction) =
    operation match
      case Some((`id`, request, ProjectLifecyclePhase.Preparing)) if !stopped =>
        if facts.needsAdmission then
          (copy(operation = Some((id, request, ProjectLifecyclePhase.Confirming(facts)))), ProjectLifecycleAction.Ask(facts))
        else (copy(operation = Some((id, request, ProjectLifecyclePhase.Committing))), ProjectLifecycleAction.Commit)
      case _ => (this, ProjectLifecycleAction.Ignored)

  def choose(id: ProjectOperationId, choice: ProjectCloseChoice): (ProjectLifecycle, ProjectLifecycleAction) =
    operation match
      case Some((`id`, request, ProjectLifecyclePhase.Confirming(facts))) if !stopped =>
        choice match
          case ProjectCloseChoice.KeepOpen => (copy(operation = None), ProjectLifecycleAction.KeepOpen)
          case ProjectCloseChoice.CloseWithoutSaving =>
            (copy(operation = Some((id, request, ProjectLifecyclePhase.Committing))), ProjectLifecycleAction.Commit)
          case ProjectCloseChoice.SaveAndClose if facts.named =>
            (copy(operation = Some((id, request, ProjectLifecyclePhase.Saving(facts)))), ProjectLifecycleAction.Save)
          case _ => (this, ProjectLifecycleAction.Ignored)
      case _ => (this, ProjectLifecycleAction.Ignored)

  /** A failed save keeps the window. Edits arriving during a save need new admission. */
  def saved(id: ProjectOperationId, success: Boolean, facts: ProjectCloseFacts): (ProjectLifecycle, ProjectLifecycleAction) =
    operation match
      case Some((`id`, request, ProjectLifecyclePhase.Saving(admitted))) if !stopped =>
        if !success then (copy(operation = None), ProjectLifecycleAction.KeepOpen)
        else if facts.edited || facts.hasNewWork(admitted) then
          (copy(operation = Some((id, request, ProjectLifecyclePhase.Confirming(facts)))), ProjectLifecycleAction.Ask(facts))
        else (copy(operation = Some((id, request, ProjectLifecyclePhase.Committing))), ProjectLifecycleAction.Commit)
      case _ => (this, ProjectLifecycleAction.Ignored)

  def cancel(id: ProjectOperationId): ProjectLifecycle =
    if owns(id) then copy(operation = None) else this

  def finished(id: ProjectOperationId): ProjectLifecycle =
    operation match
      case Some((`id`, ProjectOperation.Quit, ProjectLifecyclePhase.Committing)) =>
        copy(operation = None, stopped = true)
      case Some((`id`, _, ProjectLifecyclePhase.Committing)) => copy(operation = None)
      case _ => this

  def shutdown: ProjectLifecycle = copy(operation = None, stopped = true)

object ProjectLifecycle:
  val empty: ProjectLifecycle = ProjectLifecycle(BigInt(0), None, false)

/** Text used by the native close admission; no storage details leak into the title. */
object ProjectLifecycleText:
  def operation(value: ProjectOperation): String = value match
    case ProjectOperation.New => "New project"
    case ProjectOperation.Open => "Open project"
    case ProjectOperation.Close => "Close project"
    case ProjectOperation.Quit => "Close Eyes Studio"

  def admission(facts: ProjectCloseFacts): String =
    val edits = if facts.edited then
      if facts.named then "This project has unsaved changes. Any autosave recovery is kept if you close without saving."
      else "This untitled project's unsaved changes will be lost."
    else ""
    val work = if facts.activeWork then "Active work will stop." else ""
    Vector(edits, work).filter(_.nonEmpty).mkString("\n\n")
