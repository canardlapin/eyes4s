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

/** Compact mirror of study-v4.json. */
object StudyV4Mirrors:
  val studyVersionFour: String =
    """{"schema":{"name":"eyes4s.study","version":4},"value":{"layout":{"name":"eyes4s.participant-phase-trial-occurrence","version":1},"keySchema":{"name":"eyes4s.trial-key","version":1},"method":{"name":"eyes4s.cosine","version":1},"input":"fedcba9876543210","frame":{"id":"v2-screen","unit":"px","xMin":0.0,"yMin":0.0,"xMax":16.0,"yMax":12.0,"yAxis":"Down"},"gridId":"v2-screen@8x6","nx":8,"ny":6,"focalPhase":"retrieval","referencePhase":"encoding","weight":"Duration","policy":{"kind":"requireAll"},"parameters":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"pairing":{"matched":{"kind":"select","choice":{"kind":"at","occurrence":2}},"controls":"allOccurrences","unmatched":"refuse"},"geometry":{"kind":"wholeFrame"},"scales":[{"kind":"native","estimate":{"kind":"binned"}}],"angularScale":null,"initialFixations":{"kind":"keepAll"},"expectedUnmatched":{"column":"old_new","values":["lure","new"],"assignments":[{"participant":"p1","phase":"retrieval","trial":"r1","occurrence":1,"value":"lure"},{"participant":"p1","phase":"retrieval","trial":"r2","occurrence":2,"value":"old"}]}}}"""
