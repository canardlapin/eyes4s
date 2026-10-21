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

package eyes4s.studio.desktop.harness

import cats.effect.IO
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.ExecutionContext.Implicits.global

class UnevaluatedEffectsFxSuite extends StudioFxSuite:
  test("the normal test transformer refuses lazy IO without executing it") {
    val ran = AtomicBoolean(false)
    munitValueTransform(IO(ran.set(true))).failed.map { error =>
      val reasons = Iterator
        .iterate(error)(_.getCause)
        .takeWhile(_ != null)
        .map(_.getMessage)
        .mkString("; ")
      assert(reasons.contains("unevaluated IO"), reasons)
      assert(!ran.get)
    }
  }

  // This passes only if the real fixture rejects its lazy result. With the
  // guard removed, MUnit silently accepts IO.unit and this canary fails.
  fxStage.test("an FX fixture cannot silently accept an IO result".fail) { _ =>
    IO.unit
  }
