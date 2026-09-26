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

/** Compact mirror of study-v3.json. */
object StudyV3Mirrors:
  val studyVersionThree: String =
    """{"schema":{"name":"eyes4s.study","version":3},"value":{"layout":{"name":"eyes4s.participant-stimulus-phase","version":1},"keySchema":{"name":"eyes4s.study-key","version":1},"method":{"name":"eyes4s.cosine","version":1},"input":"0123456789abcdef","frame":{"id":"v2-image","unit":"px","xMin":0.0,"yMin":0.0,"xMax":8.0,"yMax":6.0,"yAxis":"Down"},"gridId":"v2-image@4x3","nx":4,"ny":3,"focalPhase":"recall","referencePhase":"encode","weight":"Duration","policy":{"kind":"requireAll"},"parameters":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"pairing":{"matched":{"kind":"requireOne"},"controls":"sameSelection","unmatched":"reportNoMatch"},"geometry":{"kind":"windowed","admission":{"id":"v2-screen","unit":"px","xMin":0.0,"yMin":0.0,"xMax":16.0,"yMax":12.0,"yAxis":"Down"},"window":{"id":"v2-image","xMin":4.0,"yMin":3.0,"xMax":12.0,"yMax":9.0},"offWindow":"exclude"},"scales":[{"kind":"degrees","estimate":{"kind":"gaussian","sigma":0.5,"edges":"Truncate"}},{"kind":"native","estimate":{"kind":"binned"}}],"angularScale":{"frame":"v2-screen","unitsPerDegree":2.0},"initialFixations":{"kind":"dropLeadingNearCross","cross":{"x":8.0,"y":6.0},"radiusDegrees":1.5}}}"""
