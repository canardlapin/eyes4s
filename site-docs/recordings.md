# An acquired recording to AOI results

This example uses the checked-in `data/adserp.csv` (90 rows) from AdSERP's MIT-licensed
Zenodo record 15236546, `pupil-data/p049-b4-t6.csv`, source rows 231–320. Its exact SHA-256 is
`57dc3230c1e2a701017011a9652e215fcb09b7f1b63a16fe00ca0ed1ca9639d0`.
It includes an actual invalid-tracking run. No sample has been simulated or replaced in the file.

The **analysis configuration below is illustrative**, not recovered calibration: viewing distance,
display dimensions and rectangular AOIs are analyst choices. The two marks define a relative
clock by subtracting the excerpt's start; zero residual is not independent hardware-sync accuracy.
For your experiment, supply measured geometry and genuine common-event marks.

## Reuse one checked configuration

```scala mdoc:silent
import eyes4s.io.*
import eyes4s.codec.*
import eyes4s.plan.DefinitionId
import scala.util.Using
import scala.io.Source

val configuration = for
  start <- WorkflowSyncMark.of("excerpt-start", 1678716023627L, 0L)
  end <- WorkflowSyncMark.of("excerpt-end", 1678716024223L, 596L)
  left <- WorkflowAoi.of("left", "Left band", 200, 0, 320, 180)
  right <- WorkflowAoi.of("right", "Right band", 320, 0, 500, 180)
  plan <- AdserpWorkflowPlan.of(
    sourceName = "AdSERP/p049-b4-t6/rows-231-320.csv",
    participant = "p049", trial = "b4-t6-excerpt",
    conditions = Vector("task" -> "SERP"),
    displayWidthPixels = 1280, displayHeightPixels = 1024,
    viewingDistanceMillimetres = 600,
    displayWidthMillimetres = 530, displayHeightMillimetres = 300,
    trackerClock = "gazepoint-unix", analysisClock = "excerpt-relative",
    synchronizationModel = SynchronizationModel.OffsetOnly,
    synchronizationMarks = Vector(start, end),
    interpolationMaxGapMilliseconds = 150,
    velocityThresholdDegreesPerSecond = 30,
    minimumEventDurationMilliseconds = 20,
    areas = Vector(left, right),
    sourceMetadata = Vector("dataset" -> "AdSERP", "license" -> "MIT",
      "zenodoRecord" -> "15236546", "configuration" -> "illustrative"))
yield plan

val contents = Using(Source.fromResource("adserp.csv"))(_.mkString).toEither
val result = for
  text <- contents
  plan <- configuration
  output <- PsychologyWorkflow.run(plan, text)
yield output

assert(result.exists(r =>
  r.imported.sourceDigest.hex == "57dc3230c1e2a701017011a9652e215fcb09b7f1b63a16fe00ca0ed1ca9639d0" &&
  r.report.acceptedRows == 90 && r.report.interpolatedSamples == 18 &&
  r.report.detectedEvents == 9))
```

## Read the report before the CSV

```scala mdoc
result.map(r => (r.report.acceptedRows, r.report.rejectedRows,
  r.report.interpolatedSamples, r.report.detectedEvents))
```

Accepted rows are not all measured, valid gaze: invalid tracking remains represented, and later
interpolation is separately marked. Inspect `result.report`, `warnings`, `provenance`, detection
failures and AOI accounting. `result.csv` contains tidy AOI rows with their support and exclusions;
the complete stage values remain available for expert inspection.

The public facade performs admission, clock mapping, visual-angle conversion, gap interpolation,
I-VT detection and AOI measurement. It does not hide the scientific choices in defaults.

## Save the analysis separately from its data

`PsychologyWorkflow.prepare` returns admitted data and a pure `RecordingPlan`. Use
`RecordingCodecs.ivt` to persist that plan, then `PsychologyWorkflow.runAnalysis` on the restored
plan and admitted data. Missing input, geometry or synchronization evidence is a typed failure.
The plan does not archive the raw CSV; retain the source and its digest alongside it.

```scala mdoc:silent
val restoredResult = for
  text <- contents
  configurationValue <- configuration
  prepared <- PsychologyWorkflow.prepare(configurationValue, text)
  schema <- DefinitionId.of("guide.recording", 1)
  method <- DefinitionId.of("eyes4s.recording.ivt", 1)
  parameters <- DefinitionId.of("guide.ivt-parameters", 1)
  persistence = RecordingCodecs.ivt(schema, method, parameters)
  json <- persistence.codec.encode(prepared.analysis)
  restored <- persistence.codec.decode(json)
  output <- PsychologyWorkflow.runAnalysis(prepared.study, prepared.imported, restored)
yield output.csv
assert(restoredResult.isRight && restoredResult == result.map(_.csv))
```

## Choose a different detector

`RecordingPlan.of` also accepts `RecordingMethod.idt` with `IdtParameters`, or
`RecordingMethod.engbertKliegl` with `EkParameters`. These use the same synchronization,
angular conversion, preprocessing and AOI stages; the AdSERP convenience facade above still
selects I-VT. Each method has a descriptor for inspection and a matching persistence codec.

```scala mdoc:silent
import eyes4s.plan.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

val detectorChoices = for
  schema <- DefinitionId.of("guide.recording", 1)
  idtId <- DefinitionId.of("guide.recording.idt", 1)
  idtSchema <- DefinitionId.of("guide.idt-parameters", 1)
  ekId <- DefinitionId.of("guide.recording.engbert-kliegl", 1)
  ekSchema <- DefinitionId.of("guide.ek-parameters", 1)
  extent <- Extent.of[Deg](0.5, 0.8)
  minimum <- MinimumEventDuration.of(Span.micros(60000))
  thresholds <- EkThresholds.of(6.0, 8.0)
  count <- EkMinimumSamples.of(3)
yield (
  RecordingCodecs.idt(schema, idtId, idtSchema), IdtParameters(extent, minimum),
  RecordingCodecs.engbertKliegl(schema, ekId, ekSchema), EkParameters(thresholds, count))

assert(detectorChoices.exists { case (idt, ip, ek, ep) =>
  idt.parameters.encode(ip).flatMap(idt.parameters.decode) == Right(ip) &&
  ek.parameters.encode(ep).flatMap(ek.parameters.decode) == Right(ep)
})
```

Pass the chosen codec's `method` and matching parameters to `RecordingPlan.of`, then save and
restore with its `codec`. I-DT uses **per-axis angular ranges**, not a radial threshold.
The EK numbers above are illustrative fixed thresholds in degrees/second, not a multiplier.
For data-derived thresholds, run `EkThresholds.estimate` explicitly on the intended angular
trial or calibration samples and store that result in `EkParameters`; replay does not refit it.
Retain the calibration input and estimation settings separately if needed for provenance.

EK requires regular timestamps for its five-point velocity calculation. Irregular windows
produce typed detection failures; interpolated observations do not become measured velocities.
Both methods retain the existing synchronous whole-operation execution contract. Saving a
method does not make it resumable, and preflight does not certify sample-level EK suitability.

For other delimited layouts use `DelimitedSchema`. For EyeLink use the repository's
[ASC format guide](https://github.com/canardlapin/eyes4s/blob/main/docs/formats/eyelink-asc.md).
EDF requires `edf2asc`; this acquired CSV example is not licensed EDF-converter certification.
