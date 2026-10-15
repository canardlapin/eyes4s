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

package eyes4s.codec

object PrintDecompositionFixtures:
  def main(args: Array[String]): Unit =
    import DecompositionCodecExamples.*
    def get[E, A](value: Either[E, A]): A = value.toOption.get
    specs.foreach { spec =>
      val p = plan(spec); val archive = get(DecompositionArchive.run(p))
      Vector(
        s"decomposition-${spec.method}-plan-v1.json"   -> get(codec.plans.encode(p)),
        s"decomposition-${spec.method}-result-v1.json" -> get(codec.results.encode(archive))
      ).foreach { (name, json) =>
        println("DECOMPOSITION_FIXTURE " + name + " " + json.noSpaces)
      }
    }
