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

package eyes4s.studio.core.execution

import cats.Monad
import cats.syntax.all.*
import eyes4s.studio.core.artifacts.{
  NativeArtifactError,
  NativeArtifactProvider,
  NativeArtifactSink,
  NativeBindingFacts
}
import eyes4s.studio.core.backend.RunId

/** The host acknowledges the exact verified package, including result and
  * parsed-source identities, before its binding can reach the application
  * (bead q-native-stored-completion).
  */
private[studio] object NativeArtifactDelivery:
  def store[F[_]: Monad](
      provider: NativeArtifactProvider[F],
      sink: NativeArtifactSink[F],
      run: RunId
  ): F[Either[NativeArtifactError, NativeBindingFacts]] =
    provider.nativeArtifacts(run).flatMap {
      case Left(error)     => Monad[F].pure(Left(error))
      case Right(verified) =>
        sink
          .store(verified)
          .map(_.flatMap { found =>
            Either.cond(
              found == verified.facts,
              found,
              NativeArtifactError.InvalidFacts(
                run,
                "stored package acknowledgement",
                s"Verified package declared ${verified.facts}; host acknowledged $found."
              )
            )
          })
    }
