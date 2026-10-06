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

package eyes4s.studio.core.artifacts

import eyes4s.studio.core.document.RunRef
import eyes4s.studio.core.session.ProjectSession

/** Host-owned archive reads. None means absent or legacy; a present native
  * archive that fails verification must return a refusal, never None.
  */
trait NativeArtifactSource[F[_]]:
  def load(
      ref: RunRef,
      budget: NativeArtifactBudget = NativeArtifactBudget.Default
  ): F[Either[NativeArtifactError, Option[NativeArtifactPackage]]]

object NativeArtifactSource:
  def project[F[_]](session: ProjectSession[F]): NativeArtifactSource[F] =
    new NativeArtifactSource[F]:
      def load(ref: RunRef, budget: NativeArtifactBudget) =
        session.findNativeArtifacts(ref, budget)
