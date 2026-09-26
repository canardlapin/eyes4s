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

package eyes4s.studio.core.document

/** The pinned wire form of every [[DocumentSamples]] sample and the story
  * moments' scientific hashes, document schema version 1.
  */
object DocumentPins:
  val pins: Map[String, String] = Map(
    "admission.default" ->
      """{"offScreen":{"ExcludeRecord":{}},"corrections":[]}""",
    "admission.rules" ->
      """{"offScreen":{"QuarantineTrial":{}},"corrections":[{"target":{"AllTrials":{}},"correction":{"FlipY":{}}},{"target":{"Participant":{"participant":"P05"}},"correction":{"Translate":{"offset":{"dx":-2.5,"dy":4.0}}}},{"target":{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}},"correction":{"FlipX":{}}}]}""",
    "analysis.rev4" ->
      """{"id":4,"dataset":3,"plan":{"Unbound":{}},"recipe":{"phases":{"focal":"Retrieval","reference":"Encoding"},"grid":{"columns":64,"rows":48},"scales":[0.5,1.0,2.0,4.0],"matched":{"RequireOne":{}},"controls":{"SameSelection":{}},"unmatched":{"ReportNoMatch":{}},"initialFixations":{"KeepAll":{}}},"studio":{"preset":{"EncodingRetrieval":{}},"name":"Encoding → retrieval reinstatement","description":""}}""",
    "binding.Bound" ->
      """{"Bound":{"sha256":"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"}}""",
    "binding.Unbound" ->
      """{"Unbound":{}}""",
    "change.Controls" ->
      """{"Controls":{"before":{"SameSelection":{}},"after":{"AllOccurrences":{}}}}""",
    "change.Grid" ->
      """{"Grid":{"before":{"columns":64,"rows":48},"after":{"columns":32,"rows":24}}}""",
    "change.InitialFixations" ->
      """{"InitialFixations":{"before":{"KeepAll":{}},"after":{"DropLeadingNearCross":{"radius":1.5}}}}""",
    "change.Matched" ->
      """{"Matched":{"before":{"RequireOne":{}},"after":{"Select":{"pick":{"At":{"occurrence":2}}}}}}""",
    "change.Phases" ->
      """{"Phases":{"before":{"focal":"Retrieval","reference":"Encoding"},"after":{"focal":"Encoding","reference":"Encoding"}}}""",
    "change.Scales" ->
      """{"Scales":{"before":[0.5,1.0,2.0,4.0],"after":[0.5,1.0,2.0,4.0,8.0]}}""",
    "change.Unmatched" ->
      """{"Unmatched":{"before":{"ReportNoMatch":{}},"after":{"Refuse":{}}}}""",
    "controls.AllOccurrences" ->
      """{"AllOccurrences":{}}""",
    "dataset.r3" ->
      """{"id":3,"parent":2,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"admission":{"offScreen":{"ExcludeRecord":{}},"corrections":[]},"decision":{"Admitted":{"ledger":{"Unbound":{}},"inventory":{"Unbound":{}}}}}""",
    "decision.Admitted" ->
      """{"Admitted":{"ledger":{"Unbound":{}},"inventory":{"Unbound":{}}}}""",
    "decision.Pending" ->
      """{"Pending":{}}""",
    "document.t1" ->
      """{"schema":{"name":"eyes4s.studio.document","version":1},"value":{"analyses":[{"dataset":2,"id":3,"plan":{"Unbound":{}},"recipe":{"controls":{"SameSelection":{}},"grid":{"columns":64,"rows":48},"initialFixations":{"KeepAll":{}},"matched":{"RequireOne":{}},"phases":{"focal":"Retrieval","reference":"Encoding"},"scales":[0.5,1.0,2.0,4.0],"unmatched":{"ReportNoMatch":{}}},"studio":{"description":"","name":"Encoding → retrieval reinstatement","preset":{"EncodingRetrieval":{}}}}],"datasets":[{"admission":{"corrections":[],"offScreen":{"ExcludeRecord":{}}},"decision":{"Admitted":{"inventory":{"Unbound":{}},"ledger":{"Unbound":{}}}},"geometry":{"image":{"height":768,"left":448,"top":156,"width":1024},"pixelsPerDegree":35.0,"screen":{"height":1080,"width":1920}},"id":2,"mapping":[{"column":"participant","role":{"Participant":{}}},{"column":"phase","role":{"Phase":{}}},{"column":"trial","role":{"Trial":{}}},{"column":"ordinal","role":{"Ordinal":{}}},{"column":"sample_count","role":{"SampleCount":{}}},{"column":"x","role":{"X":{}}},{"column":"y","role":{"Y":{}}},{"column":"onset_ms","role":{"Onset":{}}},{"column":"duration_ms","role":{"Duration":{}}}],"parent":null,"sources":[{"bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","path":"inputs/fixations.csv","role":{"Fixations":{}},"semantic":null},{"bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","path":"inputs/trials.csv","role":{"Trials":{}},"semantic":null}],"units":{"time":null}},{"admission":{"corrections":[],"offScreen":{"ExcludeRecord":{}}},"decision":{"Pending":{}},"geometry":{"image":{"height":768,"left":448,"top":156,"width":1024},"pixelsPerDegree":35.0,"screen":{"height":1080,"width":1920}},"id":3,"mapping":[{"column":"participant","role":{"Participant":{}}},{"column":"phase","role":{"Phase":{}}},{"column":"trial","role":{"Trial":{}}},{"column":"occurrence","role":{"Occurrence":{}}},{"column":"ordinal","role":{"Ordinal":{}}},{"column":"sample_count","role":{"SampleCount":{}}},{"column":"x","role":{"X":{}}},{"column":"y","role":{"Y":{}}},{"column":"onset_ms","role":{"Onset":{}}},{"column":"duration_ms","role":{"Duration":{}}}],"parent":2,"sources":[{"bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","path":"inputs/fixations.csv","role":{"Fixations":{}},"semantic":null},{"bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","path":"inputs/trials.csv","role":{"Trials":{}},"semantic":null}],"units":{"time":{"Milliseconds":{}}}}],"draft":null,"figures":[{"id":2,"panels":[{"letter":"A","scale":{"At":{"sigma":2.0}},"selection":{"AllQueries":{}},"title":"Participant D by response"}],"reporting":"by-retrieval-response","run":5}],"presentation":{"layouts":[],"mapOpacity":0.6,"perspective":{"Data":{}},"shownRun":5,"stage":{"Dark":{}},"theme":{"Light":{}},"underlay":false},"reporting":[{"filters":[],"groupBy":"response","id":"by-retrieval-response","minimumPerGroup":null,"name":"By retrieval response","weighting":{"ParticipantMeans":{}}}],"runs":[{"analysis":3,"archive":{"Unbound":{}},"dataset":2,"id":5,"state":{"Completed":{}}}]}}""",
    "draft.rev5" ->
      """{"id":5,"base":4,"changes":[{"Scales":{"before":[0.5,1.0,2.0,4.0],"after":[0.5,1.0,2.0,4.0,8.0]}}]}""",
    "figure.1" ->
      """{"id":1,"run":7,"reporting":"by-retrieval-response","panels":[{"letter":"A","title":"Encoding gaze","scale":{"Unscaled":{}},"selection":{"Trial":{"key":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}}},{"letter":"B","title":"Retrieval gaze","scale":{"Unscaled":{}},"selection":{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}},{"letter":"C","title":"Density maps","scale":{"At":{"sigma":2.0}},"selection":{"QueryWithReferences":{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}},{"letter":"D","title":"Participant D by response","scale":{"At":{"sigma":2.0}},"selection":{"AllQueries":{}}},{"letter":"E","title":"Scale profile by response","scale":{"AllScales":{}},"selection":{"AllQueries":{}}}]}""",
    "figure.2" ->
      """{"id":2,"run":5,"reporting":"by-retrieval-response","panels":[{"letter":"A","title":"Participant D by response","scale":{"At":{"sigma":2.0}},"selection":{"AllQueries":{}}}]}""",
    "filter.Keep" ->
      """{"Keep":{"attribute":"response","values":["Remembered"]}}""",
    "filter.OutsideWindowAtMost" ->
      """{"OutsideWindowAtMost":{"share":0.25}}""",
    "geometry" ->
      """{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0}""",
    "grid" ->
      """{"columns":64,"rows":48}""",
    "initial.DropFirst" ->
      """{"DropFirst":{}}""",
    "initial.DropLeadingNearCross" ->
      """{"DropLeadingNearCross":{"radius":1.5}}""",
    "initial.KeepAll" ->
      """{"KeepAll":{}}""",
    "lifecycle.Cancelled.none" ->
      """{"Cancelled":{"at":null}}""",
    "lifecycle.Failed" ->
      """{"Failed":{}}""",
    "mapping.r3" ->
      """[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}]""",
    "matched.MeanOfAll" ->
      """{"MeanOfAll":{}}""",
    "matched.RequireOne" ->
      """{"RequireOne":{}}""",
    "matched.SameOccurrence" ->
      """{"SameOccurrence":{}}""",
    "matched.Select.At" ->
      """{"Select":{"pick":{"At":{"occurrence":2}}}}""",
    "matched.Select.First" ->
      """{"Select":{"pick":{"First":{}}}}""",
    "matched.Select.Last" ->
      """{"Select":{"pick":{"Last":{}}}}""",
    "panel-scale.At" ->
      """{"At":{"sigma":8.0}}""",
    "presentation.default" ->
      """{"perspective":{"Data":{}},"theme":{"Light":{}},"stage":{"Dark":{}},"mapOpacity":0.6,"underlay":false,"shownRun":null,"layouts":[]}""",
    "presentation.layouts" ->
      """{"perspective":{"Figures":{}},"theme":{"Dark":{}},"stage":{"Mid":{}},"mapOpacity":0.4,"underlay":true,"shownRun":7,"layouts":[{"perspective":{"Data":{}},"layout":"{\"root\":\"data\"}"},{"perspective":{"Figures":{}},"layout":"{\"root\":\"figures\"}"}]}""",
    "preset.Custom" ->
      """{"Custom":{}}""",
    "preset.Recognition" ->
      """{"Recognition":{}}""",
    "recipe.rev4" ->
      """{"phases":{"focal":"Retrieval","reference":"Encoding"},"grid":{"columns":64,"rows":48},"scales":[0.5,1.0,2.0,4.0],"matched":{"RequireOne":{}},"controls":{"SameSelection":{}},"unmatched":{"ReportNoMatch":{}},"initialFixations":{"KeepAll":{}}}""",
    "reporting.by-response" ->
      """{"id":"by-retrieval-response","name":"By retrieval response","groupBy":"response","filters":[],"minimumPerGroup":null,"weighting":{"ParticipantMeans":{}}}""",
    "reporting.strict" ->
      """{"id":"strict","name":"Remembered, window ≤ 25%","groupBy":"response","filters":[{"Keep":{"attribute":"response","values":["Remembered"]}},{"OutsideWindowAtMost":{"share":0.25}}],"minimumPerGroup":3,"weighting":{"PooledQueries":{}}}""",
    "run.Cancelled" ->
      """{"id":6,"analysis":4,"dataset":3,"state":{"Cancelled":{"at":{"Comparing":{}}}},"archive":{"Unbound":{}}}""",
    "run.Completed" ->
      """{"id":7,"analysis":4,"dataset":3,"state":{"Completed":{}},"archive":{"Unbound":{}}}""",
    "run.Running" ->
      """{"id":8,"analysis":5,"dataset":3,"state":{"Running":{"job":1}},"archive":{"Unbound":{}}}""",
    "scales" ->
      """[0.5,1.0,2.0,4.0]""",
    "sigma" ->
      """2.0""",
    "source-role.Fixations" ->
      """{"Fixations":{}}""",
    "source-role.Trials" ->
      """{"Trials":{}}""",
    "source.semantic" ->
      """{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":"0123456789abcdef"}""",
    "sources" ->
      """[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}]""",
    "time-unit.Microseconds" ->
      """{"Microseconds":{}}""",
    "time-unit.Seconds" ->
      """{"Seconds":{}}""",
    "units.ms" ->
      """{"time":{"Milliseconds":{}}}""",
    "units.undeclared" ->
      """{"time":null}""",
    "unmatched.Refuse" ->
      """{"Refuse":{}}"""
  )

  val science: Map[String, String] = Map(
    "t1" -> "sha256:332d9916639cf7ea51bda6722d3bc11be81ff5ba59d131a9aa4be62819fa5390",
    "t2" -> "sha256:c5ea22f1c1a459a96b7a3970e1c876cb8ad06f830d426557b4ed964515d0655d",
    "t3" -> "sha256:5a28115e88ee8e75926c9d178c31c8a5dd5afc20a6ee05d3597ec66e0a047e75"
  )
