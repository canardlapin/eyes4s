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

import eyes4s.studio.core.session.ProjectSession

/** Acknowledges verified native bytes before their document binding is published
  * (bead q-native-stored-completion). A failed store publishes no binding facts.
  */
trait NativeArtifactSink[F[_]]:
  def store(
      artifacts: NativeArtifactPackage
  ): F[Either[NativeArtifactError, NativeBindingFacts]]

  def storeCompletion(
      artifacts: NativeCompletion
  )(using F: cats.Applicative[F]): F[Either[NativeArtifactError, NativeCompletionFacts]] =
    artifacts match
      case NativeCompletion.Timed(value) =>
        F.map(store(value))(_.map(NativeCompletionFacts.Timed.apply))
      case NativeCompletion.Locations(value) =>
        F.pure(
          Left(
            NativeArtifactError.persistence(
              value.facts.run,
              "store spatial completion",
              "this host cannot store spatial run files"
            )
          )
        )

object NativeArtifactSink:
  def project[F[_]](session: ProjectSession[F]): NativeArtifactSink[F] =
    new NativeArtifactSink[F]:
      def store(artifacts: NativeArtifactPackage) = session.storeNativeArtifacts(artifacts)

      override def storeCompletion(artifacts: NativeCompletion)(using cats.Applicative[F]) =
        session.storeNativeCompletion(artifacts)
