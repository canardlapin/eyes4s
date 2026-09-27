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

package eyes4s.studio.core.backend

/** The pinned wire form of every [[ProtocolSamples]] sample, protocol 1.3. */
object ProtocolPins:
  val pins: Map[String, String] = Map(
    "error.InventoryRefused" ->
      """{"InventoryRefused":{"dataset":4,"issues":[{"Conflict":{"trial":{"participant":"P01","phase":"Encoding","trial":"enc_01"},"records":[2,9],"columns":["response"]}},{"Width":{"record":5,"expected":8,"actual":7}},{"Field":{"record":6,"column":"occurrence","value":"x","requirement":"a positive integer occurrence"}},{"Other":{"kind":"DuplicateAttribute","text":"Attribute names [a] are declared more than once."}}]}}""",
    "error.DuplicateSubscription" ->
      """{"DuplicateSubscription":{"request":41}}""",
    "error.Malformed" ->
      """{"Malformed":{"excerpt":"{\"id\":3,\"body\":{\"Runz\":{}}}","reason":"no such request"}}""",
    "request.Unsubscribe" ->
      """{"Unsubscribe":{"subscription":41}}""",
    "response.Unsubscribed" ->
      """{"Unsubscribed":{"subscription":41,"active":true}}""",
    "cause.CorrectionConflict" ->
      """{"code":"quarantine.correction-conflict","message":"correction rules 0 and 1 both apply to the trial","first":0,"second":1}""",
    "cause.DuplicateOrdinals" ->
      """{"code":"quarantine.duplicate-ordinals","message":"duplicate fixation ordinals"}""",
    "cause.InvalidExtent" ->
      """{"code":"quarantine.invalid-extent","message":"scanpath extent: empty","reason":"empty"}""",
    "cause.InvalidTransition" ->
      """{"code":"quarantine.invalid-transition","message":"transition into fixation 2: negative","index":2,"reason":"negative"}""",
    "cause.InventoryItemConflict" ->
      """{"code":"quarantine.inventory-item-conflict","message":"the trial's records name items beach-008, but the inventory declares 'beach-007'","inventory":"beach-007","records":["beach-008"]}""",
    "cause.ItemConflict" ->
      """{"code":"quarantine.item-conflict","message":"the trial's records name different match items a, b","items":["a","b"]}""",
    "cause.NoFixations" ->
      """{"code":"quarantine.no-fixations","message":"a scanpath needs at least one fixation"}""",
    "cause.NotInInventory" ->
      """{"code":"quarantine.not-in-inventory","message":"trial P01/Encoding/enc_21#1 is not in the trial inventory","participant":"P01","phase":"Encoding","trial":"enc_21","occurrence":1}""",
    "cause.OccurrenceConflict" ->
      """{"code":"quarantine.occurrence-conflict","message":"the trial's records name different occurrences 1, 2","occurrences":[1,2]}""",
    "cause.Other" ->
      """{"code":"quarantine.future-cause","message":"a cause from a newer backend"}""",
    "cause.Overlap" ->
      """{"code":"quarantine.overlap","message":"fixation 3 at [110.0ms, 300.0ms) on c begins before the previous fixation [70.0ms, 150.0ms) on c ends","index":3,"previous":"[70.0ms, 150.0ms) on c","current":"[110.0ms, 300.0ms) on c"}""",
    "cause.RejectedRecords" ->
      """{"code":"quarantine.rejected-records","message":"one or more source rows were rejected"}""",
    "cause.UnmappableFixation" ->
      """{"code":"quarantine.unmappable-fixation","message":"fixation 4 at (-5.5, 2.5) cannot be mapped from screen to image","index":4,"from":"screen","to":"image","x":-5.5,"y":2.5}""",
    "cause.WrongClock" ->
      """{"code":"quarantine.wrong-clock","message":"fixation 1 is on clock b, expected a","index":1,"expected":"a","actual":"b"}""",
    "disposition.Absent" ->
      """{"Absent":{}}""",
    "disposition.Admitted" ->
      """{"Admitted":{}}""",
    "disposition.NoFixations" ->
      """{"NoFixations":{}}""",
    "envelope.frame" ->
      """{"version":{"major":1,"minor":3},"id":41,"body":{"Event":{"event":{"Finished":{"outcome":{"Completed":{"job":1,"run":8,"last":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}}}}}}""",
    "envelope.request" ->
      """{"version":{"major":1,"minor":3},"id":41,"body":{"Subscribe":{"id":1}}}""",
    "error.AlreadyRunning" ->
      """{"AlreadyRunning":{"revision":5,"job":1}}""",
    "error.NoResult" ->
      """{"NoResult":{"run":5,"state":{"Stale":{}}}}""",
    "error.Unavailable" ->
      """{"Unavailable":{"subject":{"Dataset":{"dataset":2}}}}""",
    "error.UnknownDataset" ->
      """{"UnknownDataset":{"dataset":9,"known":[2,3]}}""",
    "error.UnknownJob" ->
      """{"UnknownJob":{"job":9,"known":[]}}""",
    "error.UnknownReference" ->
      """{"UnknownReference":{"run":7,"address":{"PairRow":{"scale":2,"design":{"Control":{}},"focal":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}}}}""",
    "error.UnknownRevision" ->
      """{"UnknownRevision":{"revision":9,"known":[4]}}""",
    "error.UnknownRun" ->
      """{"UnknownRun":{"run":9,"known":[7]}}""",
    "error.UnsupportedVersion" ->
      """{"UnsupportedVersion":{"requested":{"major":2,"minor":0},"supported":{"major":1,"minor":0}}}""",
    "event.Advanced" ->
      """{"Advanced":{"progress":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}""",
    "event.Finished.Cancelled" ->
      """{"Finished":{"outcome":{"Cancelled":{"job":1,"run":8,"last":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}}}""",
    "event.Finished.Completed" ->
      """{"Finished":{"outcome":{"Completed":{"job":1,"run":8,"last":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}}}""",
    "event.Finished.Failed" ->
      """{"Finished":{"outcome":{"Failed":{"job":1,"run":8,"diagnostics":[{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map"}],"last":null}}}}""",
    "frame.Event" ->
      """{"Event":{"event":{"Advanced":{"progress":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}}}""",
    "frame.Response" ->
      """{"Response":{"response":{"Refused":{"error":{"UnknownRun":{"run":9,"known":[7]}}}}}}""",
    "inspection.Contrast" ->
      """{"Contrast":{"address":{"ContrastRow":{"scale":2,"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}},"m":0.73,"b":0.35,"d":0.38}}""",
    "inspection.Pair" ->
      """{"Pair":{"address":{"PairRow":{"scale":2,"design":{"Control":{}},"focal":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}},"referenceItem":"street-112","score":0.61}}""",
    "inspection.Reduction" ->
      """{"Reduction":{"address":{"Reduction":{"scale":2,"design":{"Control":{}},"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}},"value":0.35,"members":19}}""",
    "inspection.Unscored" ->
      """{"Unscored":{"address":{"Estimation":{"scale":0,"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}},"status":{"NotAdmitted":{"disposition":{"Absent":{}}}}}}""",
    "locus.Address" ->
      """{"Address":{"address":{"PairRow":{"scale":2,"design":{"Control":{}},"focal":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}}}}""",
    "locus.Area" ->
      """{"Area":{"id":"face"}}""",
    "locus.Artifact" ->
      """{"Artifact":{"digest":"abc"}}""",
    "locus.Dataset" ->
      """{"Dataset":{"dataset":2}}""",
    "locus.Definition" ->
      """{"Definition":{"name":"eyes4s.study","version":1}}""",
    "locus.Design" ->
      """{"Design":{"design":{"Matched":{}}}}""",
    "locus.Entry" ->
      """{"Entry":{"name":"result"}}""",
    "locus.Event" ->
      """{"Event":{"index":4}}""",
    "locus.Field" ->
      """{"Field":{"name":"x"}}""",
    "locus.Fixation" ->
      """{"Fixation":{"index":6}}""",
    "locus.InputTrial" ->
      """{"InputTrial":{"index":3}}""",
    "locus.Job" ->
      """{"Job":{"job":1}}""",
    "locus.Line" ->
      """{"Line":{"source":"s.asc","line":12}}""",
    "locus.Participant" ->
      """{"Participant":{"name":"P17"}}""",
    "locus.Group" ->
      """{"Group":{"levels":[{"term":"response","level":"Remembered"}]}}""",
    "locus.Pair" ->
      """{"Pair":{"focal":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}}""",
    "locus.Path" ->
      """{"Path":{"path":".a"}}""",
    "locus.Record" ->
      """{"Record":{"number":7214}}""",
    "locus.Recording" ->
      """{"Recording":{"source":"rec"}}""",
    "locus.Records" ->
      """{"Records":{"numbers":[1,2]}}""",
    "locus.Relation" ->
      """{"Relation":{"kind":"input","source":"result"}}""",
    "locus.Repetition" ->
      """{"Repetition":{"name":"r1"}}""",
    "locus.Revision" ->
      """{"Revision":{"revision":3}}""",
    "locus.Run" ->
      """{"Run":{"run":5}}""",
    "locus.Sample" ->
      """{"Sample":{"index":5}}""",
    "locus.Samples" ->
      """{"Samples":{"from":5,"until":9}}""",
    "locus.Scale" ->
      """{"Scale":{"index":2}}""",
    "locus.Trial" ->
      """{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}""",
    "locus.TrialDigest" ->
      """{"TrialDigest":{"digest":"d1"}}""",
    "locus.Trials" ->
      """{"Trials":{"keys":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}]}}""",
    "locus.Window" ->
      """{"Window":{"name":"w"}}""",
    "query-status.Contributing" ->
      """{"Contributing":{"m":[0.41],"b":[0.22],"d":[0.19]}}""",
    "query-status.Failed" ->
      """{"Failed":{"diagnostic":{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map"}}}""",
    "query-status.NoMatch" ->
      """{"NoMatch":{"diagnostic":{"code":"study-finding.unmatched-focal","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map"}}}""",
    "query-status.NotAdmitted" ->
      """{"NotAdmitted":{"disposition":{"Absent":{}}}}""",
    "request.Admission" ->
      """{"Admission":{"dataset":3}}""",
    "request.Cancel" ->
      """{"Cancel":{"id":1}}""",
    "request.Inspect" ->
      """{"Inspect":{"run":8,"address":{"PairRow":{"scale":2,"design":{"Control":{}},"focal":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}}}}""",
    "request.Job" ->
      """{"Job":{"id":1}}""",
    "request.Jobs" ->
      """{"Jobs":{}}""",
    "request.Ledger" ->
      """{"Ledger":{"dataset":3,"page":{"offset":10,"size":20}}}""",
    "request.Outcome" ->
      """{"Outcome":{"id":1}}""",
    "request.Preview" ->
      """{"Preview":{"revision":5}}""",
    "request.PreviewRows" ->
      """{"PreviewRows":{"revision":5,"page":{"offset":10,"size":20}}}""",
    "request.ProvenanceOf" ->
      """{"ProvenanceOf":{"run":8,"address":{"PairRow":{"scale":2,"design":{"Control":{}},"focal":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}}}}""",
    "request.Queries" ->
      """{"Queries":{"run":8,"page":{"offset":10,"size":20}}}""",
    "request.Result" ->
      """{"Result":{"run":8}}""",
    "request.Runs" ->
      """{"Runs":{}}""",
    "request.Submit" ->
      """{"Submit":{"revision":5}}""",
    "request.Subscribe" ->
      """{"Subscribe":{"id":1}}""",
    "response.Admission" ->
      """{"Admission":{"summary":{"dataset":3,"state":{"Admitted":{}},"inventory":{"Joined":{"trials":960,"absent":6}},"admitted":937,"quarantined":[{"code":"quarantine.overlap","trials":6}],"noFixations":5,"fixationRecords":11520,"window":{"outsideWindow":543,"outsideScreen":0,"total":11311,"trialsOutsideWindow":409,"trialsOutsideScreen":0,"trials":937,"untallied":0,"sourceRecords":11520,"outsideWindowMicros":159142000,"outsideScreenMicros":0,"totalMicros":3282108000},"items":259,"imagesFound":257,"missingImages":[{"item":"forest-044","participants":["P01","P24"],"encodingTrials":2}],"history":"onset declared ms"}}}""",
    "response.Inspected" ->
      """{"Inspected":{"inspection":{"Contrast":{"address":{"ContrastRow":{"scale":2,"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}},"m":0.73,"b":0.35,"d":0.38}}}}""",
    "response.Job" ->
      """{"Job":{"status":{"job":1,"run":8,"revision":5,"dataset":3,"state":{"Running":{"progress":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}}}}""",
    "response.Jobs" ->
      """{"Jobs":{"jobs":[{"job":1,"run":8,"revision":5,"dataset":3,"state":{"Queued":{}}}]}}""",
    "response.Ledger" ->
      """{"Ledger":{"page":{"dataset":3,"page":{"offset":10,"total":480,"next":30},"entries":[{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"beach-042","response":"Remembered","disposition":{"Admitted":{}},"outsideFrame":[{"record":12,"x":-5.5,"y":2.5,"frame":"screen"}]}]}}}""",
    "response.Outcome" ->
      """{"Outcome":{"job":1,"outcome":{"Cancelled":{"job":1,"run":8,"last":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}}}""",
    "response.Preview" ->
      """{"Preview":{"summary":{"revision":5,"dataset":3,"scales":["8°"],"focalTrials":480,"referenceTrials":480,"requestedQueries":480,"eligibleQueries":457,"candidatePairsPerScale":230400,"pairRowsPerScale":8969,"pairRows":44845}}}""",
    "response.PreviewRows" ->
      """{"PreviewRows":{"page":{"revision":5,"page":{"offset":10,"total":480,"next":30},"rows":[{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"beach-042","response":"Remembered","matched":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1},"controls":19,"eligibility":{"Eligible":{}}},{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"b","response":"Forgotten","matched":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1},"controls":null,"eligibility":{"QueryNotAdmitted":{"disposition":{"NoFixations":{}}}}},{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"b","response":"Forgotten","matched":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1},"controls":null,"eligibility":{"NoMatch":{"diagnostic":{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map"}}}}]}}}""",
    "response.ProvenanceOf" ->
      """{"ProvenanceOf":{"provenance":{"address":{"PairRow":{"scale":2,"design":{"Control":{}},"focal":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}},"trail":[{"Run":{"run":7}},{"Analysis":{"revision":4}},{"Dataset":{"dataset":3}},{"Scale":{"index":2,"label":"2°"}},{"Design":{"design":{"Control":{}}}},{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"beach-042"}}]}}}""",
    "response.Queries" ->
      """{"Queries":{"page":{"run":8,"page":{"offset":10,"total":480,"next":30},"rows":[{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"beach-042","response":"Remembered","matched":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1},"controls":19,"status":{"Contributing":{"m":[0.41],"b":[0.22],"d":[0.19]}}}]}}}""",
    "response.Refused" ->
      """{"Refused":{"error":{"UnknownRun":{"run":9,"known":[7]}}}}""",
    "response.Result" ->
      """{"Result":{"summary":{"run":7,"revision":4,"dataset":3,"scales":["2°"],"pairRowsPerScale":8969,"pairRows":35876,"eligibleQueries":457,"contrasts":{"requested":480,"queryNotAdmitted":14,"noMatch":9,"failed":3,"contributing":454},"grandD":0.26,"grandDByScale":[0.26],"groups":[{"attribute":"response","label":"Remembered","n":24,"d":0.3,"dByScale":[0.3]}],"pairedN":24,"groupNMinimum":2,"groupNMaximum":17,"participants":[{"participant":"P17","requested":20,"contributing":19,"failed":0,"noMatch":0,"notAdmitted":1,"all":{"m":0.73,"b":0.35,"d":0.38,"dByScale":[0.38]},"groups":[{"label":"Forgotten","n":2,"m":0.64,"b":0.32,"d":0.32}]}]}}}""",
    "response.Runs" ->
      """{"Runs":{"runs":[{"run":7,"revision":4,"dataset":3,"state":{"Current":{}}}]}}""",
    "run-state.Cancelled" ->
      """{"Cancelled":{"at":{"Comparing":{}}}}""",
    "run-state.Completed" ->
      """{"Completed":{}}""",
    "run-state.Current" ->
      """{"Current":{}}""",
    "run-state.Failed" ->
      """{"Failed":{}}""",
    "run-state.Running" ->
      """{"Running":{"job":1}}""",
    "run-state.Stale" ->
      """{"Stale":{}}""",
    "segment.Comparing" ->
      """{"Comparing":{"scale":1,"design":{"Matched":{}}}}""",
    "segment.Contrasting" ->
      """{"Contrasting":{"scale":3}}""",
    "segment.Estimating" ->
      """{"Estimating":{"scale":0}}""",
    "segment.Reducing" ->
      """{"Reducing":{"scale":2,"design":{"Control":{}}}}""",
    "state.Finished" ->
      """{"Finished":{"outcome":{"Completed":{"job":1,"run":8,"last":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}}}""",
    "state.Queued" ->
      """{"Queued":{}}""",
    "state.Running" ->
      """{"Running":{"progress":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}""",
    "total.AtMost" ->
      """{"AtMost":{"units":2}}""",
    "total.Counting" ->
      """{"Counting":{}}""",
    "total.Exact" ->
      """{"Exact":{"units":1}}""",
    "total.Unknown" ->
      """{"Unknown":{}}"""
  )
