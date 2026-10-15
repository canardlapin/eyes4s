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

object PrintScanpathStudyFixtures:
  def main(args: Array[String]): Unit =
    import ScanpathStudyCodecExamples.*
    def get[E, A](value: Either[E, A]): A = value.toOption.get
    Vector("multimatch", "overlap", "transport").zip(methods).foreach { (name, method) =>
      val p           = plan(method)
      val savedPlan   = get(codec.plans.encode(p))
      val savedResult = get(codec.results.encode(get(ScanpathStudyArchive.run(p))))
      println(s"SCANPATH_STUDY_FIXTURE scanpath-study-$name-plan-v1.json ${savedPlan.noSpaces}")
      println(
        s"SCANPATH_STUDY_FIXTURE scanpath-study-$name-result-v1.json ${savedResult.noSpaces}"
      )
    }
