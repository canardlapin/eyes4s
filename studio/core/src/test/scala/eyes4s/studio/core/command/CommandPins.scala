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

package eyes4s.studio.core.command

/** The pinned wire form of every [[CommandSamples]] command and journal
  * entry, and of the story session's journal lines (`studio.journal`
  * version 1).
  */
object CommandPins:
  /** `ImportSources` as journals wrote it before S5.2 (no attributes). */
  val importSourcesV1: String =
    """{"ImportSources":{"parent":2,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0}}}"""

  val pins: Map[String, String] = Map(
    "AddCorrection" ->
      """{"AddCorrection":{"dataset":3,"index":0,"rule":{"target":{"AllTrials":{}},"correction":{"FlipY":{}}}}}""",
    "AddPanel" ->
      """{"AddPanel":{"figure":1,"index":5,"panel":{"letter":"F","title":"Participant D by response","scale":{"At":{"sigma":2.0}},"selection":{"AllQueries":{}}}}}""",
    "Admit" ->
      """{"Admit":{"dataset":3,"verified":"e0904bd6169b75691383b1ee7236ed8da0e01d12def025e95c68ce77834ce789","policy":{"ReviewExclusions":{}},"ledger":{"Unbound":{}},"inventory":{"Unbound":{}}}}""",
    "BindFigure" ->
      """{"BindFigure":{"figure":1,"run":5,"reporting":"by-retrieval-response"}}""",
    "BindPlan" ->
      """{"BindPlan":{"revision":4,"plan":"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef","input":"00112233445566ff"}}""",
    "CancelRun" ->
      """{"CancelRun":{"run":8}}""",
    "ChangeRecipe" ->
      """{"ChangeRecipe":{"change":{"Grid":{"before":{"columns":64,"rows":48},"after":{"columns":32,"rows":24}}}}}""",
    "ChangeRecipes" ->
      """{"ChangeRecipes":{"changes":[{"Grid":{"before":{"columns":64,"rows":48},"after":{"columns":32,"rows":24}}},{"Weighting":{"before":{"Duration":{}},"after":{"Uniform":{}}}}]}}""",
    "CreateFigure" ->
      """{"CreateFigure":{"run":7,"reporting":"by-retrieval-response","panels":[{"letter":"A","title":"Encoding gaze","scale":{"Unscaled":{}},"selection":{"Trial":{"key":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}}}]}}""",
    "DeleteFigure" ->
      """{"DeleteFigure":{"figure":1}}""",
    "DiscardDataset" ->
      """{"DiscardDataset":{"dataset":3}}""",
    "DiscardDraft" ->
      """{"DiscardDraft":{}}""",
    "ImportSources" ->
      """{"ImportSources":{"parent":2,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"attributes":[],"admission":null,"inventory":null}}""",
    "ImportSources.admission" ->
      """{"ImportSources":{"parent":3,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"attributes":[],"admission":{"offScreen":{"QuarantineTrial":{}},"corrections":[{"target":{"AllTrials":{}},"correction":{"FlipY":{}}}]},"inventory":null}}""",
    "ImportSources.attributes" ->
      """{"ImportSources":{"parent":2,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"attributes":[{"column":"Pupil","kind":{"Number":{}}}],"admission":null,"inventory":null}}""",
    "ReviseDataset" ->
      """{"ReviseDataset":{"dataset":3,"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Seconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"attributes":[{"column":"Pupil","kind":{"Number":{}}}],"inventory":null}}""",
    "PutReporting" ->
      """{"PutReporting":{"spec":{"id":"by-retrieval-response","name":"By retrieval response","groupBy":"response","filters":[],"minimumPerGroup":null,"weighting":{"ParticipantMeans":{}}}}}""",
    "RebaseDraft" ->
      """{"RebaseDraft":{"dataset":2}}""",
    "RecordRunOutcome" ->
      """{"RecordRunOutcome":{"run":8,"state":{"Cancelled":{"at":{"Comparing":{}}}},"archive":{"Unbound":{}}}}""",
    "RemoveCorrection" ->
      """{"RemoveCorrection":{"dataset":3,"index":0}}""",
    "RemovePanel" ->
      """{"RemovePanel":{"figure":1,"panel":"E"}}""",
    "RemoveReporting" ->
      """{"RemoveReporting":{"reporting":"by-retrieval-response"}}""",
    "RestoreDataset" ->
      """{"RestoreDataset":{"dataset":{"id":3,"parent":2,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0},"admission":{"offScreen":{"ExcludeRecord":{}},"corrections":[]},"decision":{"Pending":{}},"inventory":{"bindings":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Item":{}},"column":"item"},{"role":{"Response":{}},"column":"response"}],"attributes":[{"column":"display_kind","kind":{"Text":{}}},{"column":"image_file","kind":{"Text":{}}}]}}}}""",
    "RestoreDraft" ->
      """{"RestoreDraft":{"draft":{"id":5,"base":4,"dataset":null,"changes":[{"Scales":{"before":[0.5,1.0,2.0,4.0],"after":[0.5,1.0,2.0,4.0,8.0]}}]}}}""",
    "RestoreFigure" ->
      """{"RestoreFigure":{"figure":{"id":1,"run":7,"reporting":"by-retrieval-response","panels":[{"letter":"A","title":"Encoding gaze","scale":{"Unscaled":{}},"selection":{"Trial":{"key":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}}},{"letter":"B","title":"Retrieval gaze","scale":{"Unscaled":{}},"selection":{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}},{"letter":"C","title":"Density maps","scale":{"At":{"sigma":2.0}},"selection":{"QueryWithReferences":{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}},{"letter":"D","title":"Participant D by response","scale":{"At":{"sigma":2.0}},"selection":{"AllQueries":{}}},{"letter":"E","title":"Scale profile by response","scale":{"AllScales":{}},"selection":{"AllQueries":{}}}]}}}""",
    "ResumeVerification" ->
      """{"ResumeVerification":{"dataset":3,"content":"e0904bd6169b75691383b1ee7236ed8da0e01d12def025e95c68ce77834ce789"}}""",
    "RetitlePanel" ->
      """{"RetitlePanel":{"figure":1,"panel":"A","title":"Encoding gaze · P17"}}""",
    "SaveAndRun" ->
      """{"SaveAndRun":{"studio":null}}""",
    "SaveAndRun.studio" ->
      """{"SaveAndRun":{"studio":{"preset":{"Custom":{}},"name":"σ 8° added","description":"rerun"}}}""",
    "SaveLayout" ->
      """{"SaveLayout":{"perspective":{"Figures":{}},"layout":"{\"root\":\"figures\"}"}}""",
    "SaveLayout.clear" ->
      """{"SaveLayout":{"perspective":{"Data":{}},"layout":null}}""",
    "SetGeometry" ->
      """{"SetGeometry":{"dataset":3,"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.0}}}""",
    "SetMapOpacity" ->
      """{"SetMapOpacity":{"opacity":0.4}}""",
    "SetMapping" ->
      """{"SetMapping":{"dataset":3,"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}]}}""",
    "SetOffScreenPolicy" ->
      """{"SetOffScreenPolicy":{"dataset":3,"policy":{"QuarantineTrial":{}}}}""",
    "SetPanelScale" ->
      """{"SetPanelScale":{"figure":1,"panel":"A","scale":{"At":{"sigma":2.0}}}}""",
    "SetPanelSelection" ->
      """{"SetPanelSelection":{"figure":1,"panel":"A","selection":{"AllQueries":{}}}}""",
    "SetPerspective" ->
      """{"SetPerspective":{"perspective":{"Compare":{}}}}""",
    "SetStage" ->
      """{"SetStage":{"stage":{"Mid":{}}}}""",
    "SetTheme" ->
      """{"SetTheme":{"theme":{"Dark":{}}}}""",
    "SetUnderlay" ->
      """{"SetUnderlay":{"shown":true}}""",
    "SetUnits" ->
      """{"SetUnits":{"dataset":3,"units":{"time":{"Seconds":{}}}}}""",
    "ShowRun" ->
      """{"ShowRun":{"run":7}}""",
    "ShowRun.none" ->
      """{"ShowRun":{"run":null}}""",
    "StartDraft" ->
      """{"StartDraft":{"base":4,"dataset":null,"changes":[{"Scales":{"before":[0.5,1.0,2.0,4.0],"after":[0.5,1.0,2.0,4.0,8.0]}}]}}""",
    "VerifyDataset" ->
      """{"VerifyDataset":{"dataset":3}}""",
    "WithdrawVerification" ->
      """{"WithdrawVerification":{"dataset":3}}""",
    "entry.Apply" ->
      """{"Apply":{"command":{"SetTheme":{"theme":{"Dark":{}}}}}}""",
    "entry.Redo" ->
      """{"Redo":{}}""",
    "entry.RedoView" ->
      """{"RedoView":{}}""",
    "entry.Undo" ->
      """{"Undo":{}}""",
    "entry.UndoView" ->
      """{"UndoView":{}}""",
    "journal.0" ->
      """{"schema":{"name":"studio.journal","version":1},"value":{"Start":{"base":"bdc86c34ae4af5a2862be64ee7bcbacf8178e3cf605f051d8d3ddc39a78b9a09"}}}""",
    "journal.1" ->
      """{"schema":{"name":"studio.journal","version":1},"value":{"Entry":{"entry":{"Apply":{"command":{"SetPanelSelection":{"figure":1,"panel":"A","selection":{"AllQueries":{}}}}}},"seq":1}}}""",
    "journal.2" ->
      """{"schema":{"name":"studio.journal","version":1},"value":{"Entry":{"entry":{"Apply":{"command":{"SetTheme":{"theme":{"Dark":{}}}}}},"seq":2}}}""",
    "journal.3" ->
      """{"schema":{"name":"studio.journal","version":1},"value":{"Entry":{"entry":{"Undo":{}},"seq":3}}}""",
    "journal.4" ->
      """{"schema":{"name":"studio.journal","version":1},"value":{"Entry":{"entry":{"Redo":{}},"seq":4}}}""",
    "journal.5" ->
      """{"schema":{"name":"studio.journal","version":1},"value":{"Checkpoint":{"science":"e4e6bc3bfde395e2a1ed95122caa50886e8bbd545f30c7f60874ca2569b26148","seq":4}}}""",
    "journal.6" ->
      """{"schema":{"name":"studio.journal","version":1},"value":{"Entry":{"entry":{"UndoView":{}},"seq":5}}}""",
    "journal.7" ->
      """{"schema":{"name":"studio.journal","version":1},"value":{"Entry":{"entry":{"Apply":{"command":{"SaveAndRun":{"studio":null}}}},"seq":6}}}"""
  )

  def journalKeys: Set[String] = pins.keySet.filter(_.startsWith("journal."))
