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

/** The pinned wire form of every [[ProtocolSamples]] sample, protocol 1.12. */
object ProtocolPins:
  val trialFixationsV17: String =
    """{"TrialFixationsOf":{"fixations":{"revision":4,"dataset":3,"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"fixations":[{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":1,"record":7209,"x":960.5,"y":540.25,"onsetMs":0.5,"durationMs":212.5,"placement":{"InMap":{}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":2,"record":7210,"x":1500.5,"y":540.75,"onsetMs":230.5,"durationMs":180.25,"placement":{"OutsideWindow":{"policy":{"Exclude":{}}}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":3,"record":7211,"x":-4.5,"y":20.25,"onsetMs":420.5,"durationMs":96.5,"placement":{"OutsideScreen":{}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":4,"record":7212,"x":600.5,"y":400.5,"onsetMs":530.5,"durationMs":140.5,"placement":{"DroppedInitial":{}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":5,"record":7213,"x":610.5,"y":410.5,"onsetMs":680.5,"durationMs":160.5,"placement":{"OutsideWindow":{"policy":{"FailTrial":{}}}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":6,"record":7214,"x":900.5,"y":500.5,"onsetMs":860.5,"durationMs":120.5,"placement":{"TrialFailed":{"tally":{"outsideScreen":1,"outsideWindow":2,"total":6,"outsideScreenMicros":96500,"outsideWindowMicros":341000,"totalMicros":"9007199254740993"}}}}]}}}"""
  val pins: Map[String, String] = Map(
    "response.MapGridOf" ->
      """{"MapGridOf":{"grid":{"run":8,"scale":2,"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"sigmaDegrees":1.5,"region":{"left":448.5,"top":156.5,"right":450.5,"bottom":158.5},"columns":2,"rows":2,"order":{"TopFirst":{}},"cellDegrees":{"width":0.25,"height":0.25},"cells":[0.1,0.2,0.3,0.4],"levels":[{"coverage":0.5,"threshold":0.3},{"coverage":0.9,"threshold":0.1}]}}}""",
    "request.MapGridOf" ->
      """{"MapGridOf":{"run":8,"scale":2,"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}""",
    "error.NoDensity" ->
      """{"NoDensity":{"run":7,"address":{"Estimation":{"scale":2,"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}},"cause":{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"no fixation lies in the map","affected":[],"category":null,"remedy":null}}}""",
    "error.SourceDigestMismatch" ->
      """{"SourceDigestMismatch":{"dataset":3,"source":"fixations.csv","recorded":"abababababababababababababababababababababababababababababababab","read":"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"}}""",
    "error.RunDatasetMismatch" ->
      """{"RunDatasetMismatch":{"run":8,"revision":4,"recorded":2,"current":3}}""",
    "error.ResultDigestMismatch" ->
      """{"ResultDigestMismatch":{"run":8,"recorded":"abababababababababababababababababababababababababababababababab","recomputed":"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"}}""",
    "error.AdmissionRefused" ->
      """{"AdmissionRefused":{"dataset":3,"source":"fixations.csv","reason":"Missing ordinal column."}}""",
    "response.ReportOf" ->
      """{"ReportOf":{"report":{"run":8,"reporting":"by-retrieval-response","scale":2,"cells":[{"group":"Forgotten","role":{"Difference":{}},"estimate":0.15,"absence":null,"participants":22,"queries":124,"failed":3,"ref":{"ReportCell":{"run":8,"reporting":"by-retrieval-response","scale":2,"group":{"Level":{"response":"Forgotten"}},"role":{"Difference":{}}}}},{"group":"Forgotten","role":{"Matched":{}},"estimate":null,"absence":{"EmptyGroup":{}},"participants":0,"queries":0,"failed":0,"ref":{"ReportCell":{"run":8,"reporting":"by-retrieval-response","scale":2,"group":{"Level":{"response":"Forgotten"}},"role":{"Matched":{}}}}},{"group":null,"role":{"Control":{}},"estimate":null,"absence":{"Undefined":{"reason":{"NotFinite":{"operation":"mean","n":2}}}},"participants":2,"queries":2,"failed":0,"ref":{"ReportCell":{"run":8,"reporting":"by-retrieval-response","scale":2,"group":{"Whole":{}},"role":{"Control":{}}}}}],"participants":[{"group":"Forgotten","role":{"Difference":{}},"participant":"P01","queries":6,"value":0.12,"absence":null,"ref":{"ReportParticipant":{"run":8,"reporting":"by-retrieval-response","scale":2,"group":{"Level":{"response":"Forgotten"}},"role":{"Difference":{}},"participant":"P01"}}},{"group":"Forgotten","role":{"Matched":{}},"participant":"P05","queries":1,"value":null,"absence":{"Failed":{"code":"study-failure.off-window","message":"11 of 11 fixations outside"}},"ref":{"ReportParticipant":{"run":8,"reporting":"by-retrieval-response","scale":2,"group":{"Level":{"response":"Forgotten"}},"role":{"Matched":{}},"participant":"P05"}}}],"contrasts":[{"role":{"Difference":{}},"minuend":"Remembered","subtrahend":"Forgotten","estimate":0.04,"absence":null,"pairedN":22,"unpaired":[{"participant":"P09","present":"Remembered","missing":"Forgotten"}],"ref":{"ReportContrast":{"run":8,"reporting":"by-retrieval-response","scale":2,"role":{"Difference":{}},"minuend":"Remembered","subtrahend":"Forgotten"}}}],"dropped":[{"group":"Forgotten","participant":"P17","queries":2,"required":3,"ref":{"ReportParticipant":{"run":8,"reporting":"by-retrieval-response","scale":2,"group":{"Level":{"response":"Forgotten"}},"role":{"Difference":{}},"participant":"P17"}}}],"queryRanges":[{"role":{"Difference":{}},"fewest":2,"most":17,"ref":{"ReportQueryRange":{"run":8,"reporting":"by-retrieval-response","scale":2,"role":{"Difference":{}}}}}],"tallies":[{"role":{"Difference":{}},"count":{"OutsideWindowFiltered":{}},"value":9,"ref":{"ReportTally":{"run":8,"reporting":"by-retrieval-response","scale":2,"role":{"Difference":{}},"count":{"OutsideWindowFiltered":{}}}}},{"role":{"Difference":{}},"count":{"OutsideWindowUnknown":{}},"value":0,"ref":{"ReportTally":{"run":8,"reporting":"by-retrieval-response","scale":2,"role":{"Difference":{}},"count":{"OutsideWindowUnknown":{}}}}}]}}}""",
    "request.ReportOf" ->
      """{"ReportOf":{"run":8,"reporting":{"id":"by-retrieval-response","name":"By retrieval response","groupBy":"response","filters":[{"OutsideWindowAtMost":{"share":0.25}}],"minimumPerGroup":3,"weighting":{"ParticipantMeans":{}},"contrast":{"minuend":"Remembered","subtrahend":"Forgotten"}},"scale":2}}""",
    "error.ReportRefused" ->
      """{"ReportRefused":{"run":8,"refusal":{"UndeclaredCovariate":{"reporting":"by-retrieval-response","covariate":"confidence","declared":["response"]}}}}""",
    "response.PairRowsOf" ->
      """{"PairRowsOf":{"page":{"run":8,"scale":2,"page":{"offset":0,"total":8969,"next":3},"rows":[{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"design":{"Matched":{}},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1},"referenceItem":"beach-042","score":{"Scored":{"score":0.73}},"queryWindow":{"trial":{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}},"tally":{"outsideScreen":0,"outsideWindow":1,"total":12,"outsideScreenMicros":0,"outsideWindowMicros":120000,"totalMicros":3000000}},"referenceWindow":{"trial":{"Trial":{"key":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}},"tally":{"outsideScreen":0,"outsideWindow":1,"total":13,"outsideScreenMicros":0,"outsideWindowMicros":90000,"totalMicros":3000000}}},{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"design":{"Control":{}},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_11","occurrence":1},"referenceItem":"street-112","score":{"NotServed":{}},"queryWindow":{"trial":{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}},"tally":{"outsideScreen":0,"outsideWindow":1,"total":12,"outsideScreenMicros":0,"outsideWindowMicros":120000,"totalMicros":3000000}},"referenceWindow":null},{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"design":{"Control":{}},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_11","occurrence":1},"referenceItem":"street-112","score":{"Failed":{"diagnostic":{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}}},"queryWindow":null,"referenceWindow":null}]}}}""",
    "request.PairRowsOf" ->
      """{"PairRowsOf":{"run":8,"scale":2,"page":{"offset":10,"size":20}}}""",
    "error.UnknownScale" ->
      """{"UnknownScale":{"run":8,"scale":4,"scales":["0.5°","1°","2°","4°"]}}""",
    "error.SourceRecordsRefused" ->
      """{"SourceRecordsRefused":{"revision":4,"error":{"RangeInvalid":{"from":0,"count":501,"limit":500}}}}""",
    "error.ContentMismatch" ->
      """{"ContentMismatch":{"dataset":3,"requested":"abababababababababababababababababababababababababababababababab","held":"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"}}""",
    "request.Verify" ->
      """{"Verify":{"dataset":3,"content":"abababababababababababababababababababababababababababababababab"}}""",
    "error.ContentNotHeld" ->
      """{"ContentNotHeld":{"dataset":9,"requested":"abababababababababababababababababababababababababababababababab"}}""",
    "error.PlacementRefused" ->
      """{"PlacementRefused":{"dataset":3,"reason":"it has no fixation source"}}""",
    "request.PlacementOf" ->
      """{"PlacementOf":{"spec":{"id":3,"parent":2,"sources":[{"role":{"Fixations":{}},"path":"inputs/fixations.csv","bytes":"19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2","semantic":null},{"role":{"Trials":{}},"path":"inputs/trials.csv","bytes":"0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99","semantic":null}],"mapping":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Ordinal":{}},"column":"ordinal"},{"role":{"SampleCount":{}},"column":"sample_count"},{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},{"role":{"Onset":{}},"column":"onset_ms"},{"role":{"Duration":{}},"column":"duration_ms"}],"units":{"time":{"Milliseconds":{}}},"geometry":{"screen":{"width":1920,"height":1080},"image":{"left":448,"top":156,"width":1024,"height":768},"pixelsPerDegree":35.5},"admission":{"offScreen":{"ExcludeRecord":{}},"corrections":[]},"decision":{"Pending":{}},"inventory":{"bindings":[{"role":{"Participant":{}},"column":"participant"},{"role":{"Phase":{}},"column":"phase"},{"role":{"Trial":{}},"column":"trial"},{"role":{"Occurrence":{}},"column":"occurrence"},{"role":{"Item":{}},"column":"item"},{"role":{"Response":{}},"column":"response"}],"attributes":[{"column":"display_kind","kind":{"Text":{}}},{"column":"image_file","kind":{"Text":{}}}]}}}}""",
    "response.PlacementOf" ->
      """{"PlacementOf":{"preview":{"dataset":3,"records":[{"record":7214,"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"rawX":1148.25,"rawY":456.75,"rule":null,"correctedX":1148.25,"correctedY":456.75,"imageX":700.25,"imageY":300.75,"placement":{"Inside":{}},"degrees":[5.375,2.375]},{"record":7215,"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"rawX":-40.5,"rawY":500.25,"rule":0,"correctedX":1960.5,"correctedY":500.25,"imageX":1512.5,"imageY":344.25,"placement":{"OutsideScreen":{}},"degrees":null}],"unplaced":[{"record":7216,"reason":"x is not a number"}],"trials":[{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"records":2,"outsideWindow":0,"outsideScreen":1}],"density":{"columns":2,"rows":1,"counts":[1.5,0.5],"placed":2}}}}""",
    "request.SourceRecordsOf" ->
      """{"SourceRecordsOf":{"revision":4,"from":7214,"count":60}}""",
    "response.SourceRecordsOf" ->
      """{"SourceRecordsOf":{"page":{"revision":4,"dataset":3,"source":{"role":{"Fixations":{}},"path":"fixations.csv","bytes":"abababababababababababababababababababababababababababababababab","semantic":null},"pixelsPerDegree":35.5,"scaleSource":{"Recipe":{}},"total":11520,"from":7214,"count":3,"rows":[{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"record":7214,"fixation":6,"ordinal":6,"onsetMs":2160.5,"durationMs":412.5,"samples":206,"screen":{"x":1148.5,"y":456.5},"image":{"x":700.5,"y":300.5,"insideImage":true},"degrees":{"x":5.375,"y":2.385},"placement":{"InMap":{}},"line":"P17,Retrieval,ret_07,1,6,1148.5,456.5,2160.5,412.5,206"},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"record":7215,"fixation":null,"ordinal":7,"onsetMs":2650.5,"durationMs":200.5,"samples":0,"screen":{"x":120.5,"y":80.5},"image":{"x":-327.5,"y":-75.5,"insideImage":false},"degrees":{"x":-24.25,"y":13.75},"placement":null,"line":"P17,Retrieval,ret_07,1,7,120.5,80.5,2650.5,200.5,0"},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"record":7216,"fixation":null,"ordinal":null,"onsetMs":null,"durationMs":null,"samples":null,"screen":null,"image":null,"degrees":null,"placement":null,"line":"P17,Retrieval,ret_07,1,x,,,,,"}]}}}""",
    "error.InventoryRefused" ->
      """{"InventoryRefused":{"dataset":4,"issues":[{"Conflict":{"trial":{"participant":"P01","phase":"Encoding","trial":"enc_01"},"records":[2,9],"columns":["response"]}},{"Width":{"record":5,"expected":8,"actual":7}},{"Field":{"record":6,"column":"occurrence","value":"x","requirement":"a positive integer occurrence"}},{"Other":{"kind":"DuplicateAttribute","text":"Attribute names [a] are declared more than once."}}]}}""",
    "error.UnknownTrial" ->
      """{"UnknownTrial":{"dataset":3,"trial":{"participant":"P99","phase":"Encoding","trial":"enc_01","occurrence":1}}}""",
    "error.TrialViewRefused" ->
      """{"TrialViewRefused":{"error":{"TrialFails":{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"positions":[4,5]}}}}""",
    "response.PreviewAccepted" ->
      """{"PreviewAccepted":{}}""",
    "error.UnknownPreview" ->
      """{"UnknownPreview":{"preview":9,"known":[1]}}""",
    "error.PreviewWorkNotReady" ->
      """{"PreviewWorkNotReady":{"preview":1,"design":{"Control":{}},"visitedScheduleWork":219486}}""",
    "error.PreviewNotReady" ->
      """{"PreviewNotReady":{"preview":1,"completedParticipants":3,"totalParticipants":24}}""",
    "error.StalePreview" ->
      """{"StalePreview":{"preview":1,"captured":{"revision":5,"dataset":3,"plan":{"Unbound":{}},"input":{"Unbound":{}}},"current":{"revision":5,"dataset":4,"plan":{"Unbound":{}},"input":{"Unbound":{}}}}}""",
    "error.TamperedPreview" ->
      """{"TamperedPreview":{"supplied":{"id":1,"stamp":{"revision":5,"dataset":3,"plan":{"Unbound":{}},"input":{"Unbound":{}}},"candidates":{"focalTrials":466,"referenceTrials":471,"participants":24,"candidatePairsPerScale":219486,"requestedQueries":480,"queriesNotAdmitted":14,"byDesignQueries":null},"counts":{"eligiblePairsPerScale":8969,"eligiblePairs":44845,"eligibleQueries":457,"unmatchedQueries":9,"ambiguousMatches":1},"diagnostics":[{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}],"recipe":null},"retained":{"id":1,"stamp":{"revision":5,"dataset":3,"plan":{"Unbound":{}},"input":{"Unbound":{}}},"candidates":{"focalTrials":466,"referenceTrials":471,"participants":24,"candidatePairsPerScale":219486,"requestedQueries":480,"queriesNotAdmitted":14,"byDesignQueries":null},"counts":{"eligiblePairsPerScale":8969,"eligiblePairs":44845,"eligibleQueries":457,"unmatchedQueries":9,"ambiguousMatches":0},"diagnostics":[{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}],"recipe":null}}}""",
    "request.PreviewCounting" ->
      """{"PreviewCounting":{"revision":5,"budget":24}}""",
    "request.ContinuePreview" ->
      """{"ContinuePreview":{"preview":1,"budget":24}}""",
    "request.SubmitPreview" ->
      """{"SubmitPreview":{"ready":{"id":1,"stamp":{"revision":5,"dataset":3,"plan":{"Unbound":{}},"input":{"Unbound":{}}},"candidates":{"focalTrials":466,"referenceTrials":471,"participants":24,"candidatePairsPerScale":219486,"requestedQueries":480,"queriesNotAdmitted":14,"byDesignQueries":null},"counts":{"eligiblePairsPerScale":8969,"eligiblePairs":44845,"eligibleQueries":457,"unmatchedQueries":9,"ambiguousMatches":0},"diagnostics":[{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}],"recipe":null}}}""",
    "preview-event.Initial" ->
      """{"Initial":{"id":1,"stamp":{"revision":5,"dataset":3,"plan":{"Unbound":{}},"input":{"Unbound":{}}},"candidates":{"focalTrials":466,"referenceTrials":471,"participants":24,"candidatePairsPerScale":219486,"requestedQueries":480,"queriesNotAdmitted":14,"byDesignQueries":null}}}""",
    "preview-event.CountingWork" ->
      """{"CountingWork":{"id":1,"design":{"Matched":{}},"workUnits":24,"visitedScheduleWork":96}}""",
    "preview-event.Counting" ->
      """{"Counting":{"id":1,"progress":{"completedParticipants":1,"totalParticipants":24}}}""",
    "preview-event.Ready" ->
      """{"Ready":{"ready":{"id":1,"stamp":{"revision":5,"dataset":3,"plan":{"Unbound":{}},"input":{"Unbound":{}}},"candidates":{"focalTrials":466,"referenceTrials":471,"participants":24,"candidatePairsPerScale":219486,"requestedQueries":480,"queriesNotAdmitted":14,"byDesignQueries":null},"counts":{"eligiblePairsPerScale":8969,"eligiblePairs":44845,"eligibleQueries":457,"unmatchedQueries":9,"ambiguousMatches":0},"diagnostics":[{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}],"recipe":null}}}""",
    "frame.Preview" ->
      """{"Preview":{"event":{"Ready":{"ready":{"id":1,"stamp":{"revision":5,"dataset":3,"plan":{"Unbound":{}},"input":{"Unbound":{}}},"candidates":{"focalTrials":466,"referenceTrials":471,"participants":24,"candidatePairsPerScale":219486,"requestedQueries":480,"queriesNotAdmitted":14,"byDesignQueries":null},"counts":{"eligiblePairsPerScale":8969,"eligiblePairs":44845,"eligibleQueries":457,"unmatchedQueries":9,"ambiguousMatches":0},"diagnostics":[{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}],"recipe":null}}}}}""",
    "error.DuplicateSubscription" ->
      """{"DuplicateSubscription":{"request":41}}""",
    "error.Malformed" ->
      """{"Malformed":{"excerpt":"{\"id\":3,\"body\":{\"Runz\":{}}}","reason":"no such request"}}""",
    "request.Unsubscribe" ->
      """{"Unsubscribe":{"subscription":41}}""",
    "response.Unsubscribed" ->
      """{"Unsubscribed":{"subscription":41,"active":true}}""",
    "request.TrialFixationsOf" ->
      """{"TrialFixationsOf":{"revision":4,"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}""",
    "request.TrialPreviewOf" ->
      """{"TrialPreviewOf":{"revision":4,"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}""",
    "response.TrialFixationsOf" ->
      """{"TrialFixationsOf":{"fixations":{"revision":4,"dataset":3,"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"fixations":[{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":1,"record":7209,"x":960.5,"y":540.25,"onsetMs":0.5,"durationMs":212.5,"placement":{"InMap":{}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":2,"record":7210,"x":1500.5,"y":540.75,"onsetMs":230.5,"durationMs":180.25,"placement":{"OutsideWindow":{"policy":{"Exclude":{}}}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":3,"record":7211,"x":-4.5,"y":20.25,"onsetMs":420.5,"durationMs":96.5,"placement":{"OutsideScreen":{}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":4,"record":7212,"x":600.5,"y":400.5,"onsetMs":530.5,"durationMs":140.5,"placement":{"DroppedInitial":{}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":5,"record":7213,"x":610.5,"y":410.5,"onsetMs":680.5,"durationMs":160.5,"placement":{"OutsideWindow":{"policy":{"FailTrial":{}}}}},{"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"position":6,"record":7214,"x":900.5,"y":500.5,"onsetMs":860.5,"durationMs":120.5,"placement":{"TrialFailed":{"tally":{"outsideScreen":1,"outsideWindow":2,"total":6,"outsideScreenMicros":96500,"outsideWindowMicros":341000,"totalMicros":"9007199254740993"}}}}],"extent":{"LegacyMissing":{"owner":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}}}}""",
    "response.TrialPreviewOf" ->
      """{"TrialPreviewOf":{"preview":{"revision":4,"trial":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"sigmaDegrees":2.5,"region":{"left":448.5,"top":156.5,"right":1472.5,"bottom":924.5},"columns":3,"rows":2,"order":{"TopFirst":{}},"cells":[0.1,0.25,null,0.3,0.2,0.15],"levels":[0.2,0.12]}}}""",
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
      """{"version":{"major":1,"minor":16},"id":41,"body":{"Event":{"event":{"Finished":{"outcome":{"Completed":{"job":1,"run":8,"last":{"job":1,"run":8,"step":2,"segment":{"Comparing":{"scale":2,"design":{"Control":{}}}},"meter":{"kind":{"Comparing":{}},"unit":{"Pairs":{}},"done":3005,"total":{"Exact":{"units":8512}}},"totals":{"completedMaps":2811,"totalMaps":{"Exact":{"units":4685}},"completedPairs":21400,"totalPairs":{"Exact":{"units":44845}}}}}}}}}}}""",
    "envelope.request" ->
      """{"version":{"major":1,"minor":16},"id":41,"body":{"Subscribe":{"id":1}}}""",
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
    "error.ResultPending" ->
      """{"ResultPending":{"run":8,"job":1}}""",
    "error.ResultDeferred" ->
      """{"ResultDeferred":{"run":8,"activeJob":2}}""",
    "error.ResultRecomputationCancelled" ->
      """{"ResultRecomputationCancelled":{"run":8,"job":1}}""",
    "error.ResultRecomputationFailed" ->
      """{"ResultRecomputationFailed":{"run":8,"job":1,"diagnostics":[{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}]}}""",
    "error.ResultReadClosed" ->
      """{"ResultReadClosed":{"run":8,"job":1}}""",
    "error.RegistryRefused" ->
      """{"RegistryRefused":{"locus":{"Revision":{"revision":4}},"reason":"Saved recipe changed."}}""",
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
      """{"Finished":{"outcome":{"Failed":{"job":1,"run":8,"diagnostics":[{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}],"last":null}}}}""",
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
    "query-status.FailedAtScales" ->
      """{"FailedAtScales":{"diagnostics":[{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"},{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"failed at the second scale","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}]}}""",
    "query-status.Failed" ->
      """{"Failed":{"diagnostic":{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}}}""",
    "query-status.NoMatch" ->
      """{"NoMatch":{"diagnostic":{"code":"study-finding.unmatched-focal","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}}}""",
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
      """{"Preview":{"summary":{"revision":5,"dataset":3,"scales":["8°"],"focalTrials":466,"referenceTrials":471,"requestedQueries":480,"eligibleQueries":457,"candidatePairsPerScale":219486,"pairRowsPerScale":8969,"pairRows":44845}}}""",
    "response.PreviewRows" ->
      """{"PreviewRows":{"page":{"revision":5,"page":{"offset":10,"total":480,"next":30},"rows":[{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"beach-042","response":"Remembered","matched":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1},"controls":19,"eligibility":{"Eligible":{}}},{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"b","response":"Forgotten","matched":null,"controls":null,"eligibility":{"QueryNotAdmitted":{"disposition":{"NoFixations":{}}}}},{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"b","response":"Forgotten","matched":null,"controls":null,"eligibility":{"NoMatch":{"diagnostic":{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}}}}]}}}""",
    "response.ProvenanceOf" ->
      """{"ProvenanceOf":{"provenance":{"address":{"PairRow":{"scale":2,"design":{"Control":{}},"focal":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}},"trail":[{"Run":{"run":7}},{"Recomputed":{"eyes4sVersion":"0.1.0"}},{"Analysis":{"revision":4}},{"Dataset":{"dataset":3}},{"Scale":{"index":2,"label":"2°"}},{"Design":{"design":{"Control":{}}}},{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"beach-042"}}]}}}""",
    "response.Queries" ->
      """{"Queries":{"page":{"run":8,"page":{"offset":10,"total":480,"next":30},"rows":[{"query":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"item":"beach-042","response":"Remembered","matched":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1},"controls":19,"status":{"Contributing":{"m":[0.41],"b":[0.22],"d":[0.19]}}}]}}}""",
    "response.Refused" ->
      """{"Refused":{"error":{"UnknownRun":{"run":9,"known":[7]}}}}""",
    "response.Result" ->
      """{"Result":{"summary":{"run":7,"revision":4,"dataset":3,"scales":["2°"],"pairRowsPerScale":8969,"pairRows":35876,"eligibleQueries":457,"contrasts":{"requested":480,"queryNotAdmitted":14,"noMatch":9,"failed":3,"contributing":454},"participants":[{"participant":"P17","requested":20,"contributing":19,"failed":0,"noMatch":0,"notAdmitted":1}]}}}""",
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
      """{"Unknown":{}}""",
    "envelope.restored" ->
      """{"version":{"major":1,"minor":18},"id":42,"body":{"Response":{"response":{"ProvenanceOf":{"provenance":{"address":{"PairRow":{"scale":2,"design":{"Control":{}},"focal":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},"reference":{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}}},"trail":[{"Run":{"run":8}},{"Restored":{"manifest":"abababababababababababababababababababababababababababababababab"}}]}}}}}}""",
    "error.ArchiveRestoreRefused" ->
      """{"ArchiveRestoreRefused":{"run":8,"cause":{"code":"study-failure.off-window","level":{"Error":{}},"origin":{"EyesCore":{}},"subject":[{"Trial":{"key":{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1}}}],"message":"empty map","affected":[{"participant":"P17","phase":"Retrieval","trial":"ret_07","occurrence":1},{"participant":"P17","phase":"Encoding","trial":"enc_03","occurrence":1}],"category":"DataDependent","remedy":"ReviewAnalysisWindow"}}}""",
    "error.ResultRestoring" ->
      """{"ResultRestoring":{"run":8}}""",
    "provenance.Restored" ->
      """{"Restored":{"manifest":"abababababababababababababababababababababababababababababababab"}}"""
  )
