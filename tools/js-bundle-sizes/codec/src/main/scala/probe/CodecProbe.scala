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

package probe

import eyes4s.codec.DomainCodecs
import eyes4s.kernel.{Frame, Unit2D}
import eyes4s.plan.DefinitionId

object CodecProbe:
  def main(args: Array[String]): Unit =
    require(args.isEmpty, "The probe takes no arguments")
    val encoded = for
      frame  <- Frame.screen("bundle-probe", 2, 2)
      schema <- DefinitionId.of("probe.frame", 1)
      json   <- DomainCodecs.frame[Unit2D.Px](schema).encode(frame)
    yield json
    println(
      encoded.fold(error => throw new IllegalArgumentException(error.toString), _.noSpaces)
    )
