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

package eyes4s.laws

import eyes4s.codec.*
import java.nio.file.{Files, Path}

/** Explicit regeneration of the two synthetic location archive fixtures. */
object WriteLocationFixtures:
  def main(args: Array[String]): Unit =
    require(args.length == 1, "WriteLocationFixtures codec-resource-directory")
    val directory = Path.of(args(0))
    val plan      = LocationComparisonCodecLawFixture.plan(7L)
    val run       = LocationComparisonRun(plan, plan.run)
    val documents = Vector(
      "location-comparison-plan-v1.json"   -> LocationComparisonResultCodec.plans.encode(plan),
      "location-comparison-result-v1.json" -> LocationComparisonResultCodec.codec.encode(run)
    )
    documents.foreach { (name, result) =>
      val json = result.fold(e => throw IllegalArgumentException(e.message), identity)
      Files.writeString(directory.resolve(name), json.spaces2 + "\n")
    }
