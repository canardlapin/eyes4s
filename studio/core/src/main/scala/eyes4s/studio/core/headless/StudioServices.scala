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

package eyes4s.studio.core.headless

import eyes4s.studio.core.backend.{AdmissionSummary, BackendError, DatasetRevision}
import eyes4s.studio.core.execution.{ExecutionEffect, ExecutionError, ExecutionEvent}
import eyes4s.studio.core.navigation.StudyNavigator

/** What a headless shell asks of studio's services when it performs the
  * app's effects (ticket S3.6), in studio-core's own terms: studio-app's
  * driver maps each `AppEffect` onto these, and names no effect library.
  * [[HeadlessSession]] implements it over `FakeStudyBackend`.
  */
trait StudioServices[F[_]]:

  /** Verify a dataset revision: the backend's admission summary. */
  def admission(dataset: DatasetRevision): F[Either[BackendError, AdmissionSummary]]

  /** The admission of `dataset` verified for `content` (protocol 1.9). */
  def verify(
      dataset: DatasetRevision,
      content: eyes4s.codec.CanonicalDigest[eyes4s.studio.core.document.DatasetRevisionSpec]
  ): F[Either[BackendError, AdmissionSummary]]

  /** Submit, cancel or require a run on the execution service. */
  def execute(effect: ExecutionEffect): F[Either[ExecutionError, Unit]]

  /** Every execution event published since the last call, in order. */
  def events: F[Vector[ExecutionEvent]]

  /** The provenance chain's down functions (S3.4). */
  def navigator: StudyNavigator[F]
