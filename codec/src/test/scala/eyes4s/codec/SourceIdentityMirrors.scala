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

/** Frozen documents shared by the JVM and JavaScript compatibility suites. */
private[codec] object SourceIdentityMirrors:
  val importSpec: String =
    """{"schema":{"name":"eyes4s.import-spec","version":1},"value":{"keySchema":{"name":"eyes4s.study-key","version":1},"keys":{"kind":"study","participant":"participant","stimulus":"item","phase":"phase"},"columns":{"ordinal":"n","x":"x","y":"y","onset":"onset","duration":"duration","samples":{"kind":"positiveColumn","column":"samples"},"attributes":[]},"frame":{"id":"source-frame","unit":"px","xMin":0.0,"yMin":0.0,"xMax":100.0,"yMax":100.0,"yAxis":"Down"},"timeUnit":"Milliseconds","rounding":"NearestMicrosecond","policy":{"offScreen":"ExcludeRecord","corrections":[]},"decision":"ReviewExclusions","inventory":null,"digest":"36995201760330b5"}}"""
  val inventorySpec: String =
    """{"schema":{"name":"eyes4s.inventory-import-spec","version":1},"value":{"participant":"participant","phase":"phase","trial":"trial","occurrence":null,"item":"item","attributes":[]}}"""
  val sourceRef: String =
    """{"schema":{"name":"eyes4s.source-ref","version":1},"value":{"label":"fixations.csv","records":"e5740769eab149c6","interpretation":{"kind":"declared","format":"FixationCsv","parser":{"name":"eyes4s.fixation-csv-parser","version":1},"optionsSchema":"FixationCsvV1","options":"36995201760330b5","identityVersion":"eyes4s.source-identity/1","identity":"068718ec24fd232f"}}}"""
  val inventoryRef: String =
    """{"schema":{"name":"eyes4s.source-ref","version":1},"value":{"label":"trials.csv","records":"e1f5405aea80983e","interpretation":{"kind":"declared","format":"TrialInventoryCsv","parser":{"name":"eyes4s.trial-inventory-csv-parser","version":1},"optionsSchema":"TrialInventoryCsvV1","options":"3a6a492490fede29","identityVersion":"eyes4s.source-identity/1","identity":"e14cf26d5ad7c128"}}}"""
  val ledgerV4: String =
    """{"schema":{"name":"eyes4s.admission-ledger","version":4},"value":{"keySchema":{"name":"eyes4s.study-key","version":1},"source":{"label":"empty.csv","records":"df0f52b97b77fd3b","interpretation":{"kind":"declared","format":"FixationCsv","parser":{"name":"eyes4s.fixation-csv-parser","version":1},"optionsSchema":"FixationCsvV1","options":"36995201760330b5","identityVersion":"eyes4s.source-identity/1","identity":"8f6e78db3722138f"}},"header":["x"],"outcome":"complete","records":[],"offScreen":"excludeRecord","corrections":[],"outsideFrame":[],"inventory":null}}"""
