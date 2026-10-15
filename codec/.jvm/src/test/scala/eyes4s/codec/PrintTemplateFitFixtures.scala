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

object PrintTemplateFitFixtures:
  def main(args: Array[String]): Unit =
    import TemplateFitCodecExamples.*
    def get[E, A](value: Either[E, A]): A = value.toOption.get
    val fixed    = get(TemplateFitArchive.run(TemplateFitCodecExamples.fixed()))
    val mean     = get(TemplateFitArchive.run(meanMap))
    val imported = get(TemplateFitArchive.run(TemplateFitCodecExamples.imported))
    Vector(
      "fixed-template-plan-v1.json"            -> get(fixedCodec.plans.encode(fixed.plan)),
      "fixed-template-result-v1.json"          -> get(fixedCodec.results.encode(fixed)),
      "fixed-template-imported-plan-v1.json"   -> get(fixedCodec.plans.encode(imported.plan)),
      "fixed-template-imported-result-v1.json" -> get(fixedCodec.results.encode(imported)),
      "mean-map-template-plan-v1.json"         -> get(mapCodec.plans.encode(mean.plan)),
      "mean-map-template-result-v1.json"       -> get(mapCodec.results.encode(mean))
    ).foreach { (name, json) => println("TEMPLATE_FIXTURE " + name + " " + json.noSpaces) }
