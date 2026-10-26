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

package eyes4s.studio.core.real

import cats.effect.kernel.Concurrent
import cats.effect.std.Mutex
import cats.syntax.all.*

/** Report sources share a lazily computed canonical binding. Queue concurrent
  * evaluations as fibers so waiting on that binding never occupies every
  * compute worker and starves independent trial or image reads.
  */
private[real] final class RealReportGate[F[_]] private (mutex: Mutex[F])(using
    F: Concurrent[F]
):
  def evaluate[A](body: => A): F[A] = mutex.lock.surround(F.unit.map(_ => body))

private[real] object RealReportGate:
  def create[F[_]: Concurrent]: F[RealReportGate[F]] =
    Mutex[F].map(new RealReportGate(_))
