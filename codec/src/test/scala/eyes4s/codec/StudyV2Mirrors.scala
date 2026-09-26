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

/** Compact mirrors of study-v2.json and admission-ledger-v2.json. */
object StudyV2Mirrors:
  val studyVersionTwo: String =
    """{"schema":{"name":"eyes4s.study","version":2},"value":{"layout":{"name":"eyes4s.participant-stimulus-phase","version":1},"keySchema":{"name":"eyes4s.study-key","version":1},"method":{"name":"eyes4s.cosine","version":1},"input":"0123456789abcdef","frame":{"id":"v2-image","unit":"px","xMin":0.0,"yMin":0.0,"xMax":8.0,"yMax":6.0,"yAxis":"Down"},"gridId":"v2-image@4x3","nx":4,"ny":3,"focalPhase":"recall","referencePhase":"encode","weight":"Duration","policy":{"kind":"requireAll"},"parameters":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"geometry":{"kind":"windowed","admission":{"id":"v2-screen","unit":"px","xMin":0.0,"yMin":0.0,"xMax":16.0,"yMax":12.0,"yAxis":"Down"},"window":{"id":"v2-image","xMin":4.0,"yMin":3.0,"xMax":12.0,"yMax":9.0},"offWindow":"exclude"},"scales":[{"kind":"degrees","estimate":{"kind":"gaussian","sigma":0.5,"edges":"Truncate"}},{"kind":"native","estimate":{"kind":"binned"}}],"angularScale":{"frame":"v2-screen","unitsPerDegree":2.0}}}"""
  val ledgerVersionTwo: String =
    """{"schema":{"name":"eyes4s.admission-ledger","version":2},"value":{"keySchema":{"name":"eyes4s.study-key","version":1},"source":{"label":"v2.csv","records":"e352c084702f33cb"},"header":["participant","image","phase","fixation","x","y","onset","duration","n"],"outcome":"complete","records":[{"record":2,"kind":"admitted","key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"p1","stimulus":"a","phase":"encode"}},"ordinal":0},{"record":3,"kind":"admitted","key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"p1","stimulus":"a","phase":"encode"}},"ordinal":1},{"record":4,"kind":"admitted","key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"p2","stimulus":"a","phase":"encode"}},"ordinal":0}],"offScreen":"excludeRecord","corrections":[{"scope":{"kind":"participant","participant":"p2"},"correction":{"kind":"flipY"}},{"scope":{"kind":"trial","key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"p1","stimulus":"a","phase":"encode"}}},"correction":{"kind":"translate","dx":1.0,"dy":-0.5}}],"outsideFrame":[{"record":3,"x":17.0,"y":2.5,"frame":"v2-screen"}]}}"""
