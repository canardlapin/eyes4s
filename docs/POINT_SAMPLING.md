# Static density along a fixation path

`PointSamplingPlan` evaluates supplied static template fields along each focal scanpath at
explicit query instants. It uses the shared onset-step trajectory, not interpolation or a
windowed duration-occupancy map. The latter remains `TemporalStudyPlan` and keeps its saved schema.

The complete public workflows are executed on JVM and Scala.js in
[PointSamplingSuite](../plan/src/test/scala/eyes4s/plan/PointSamplingSuite.scala) and
[PointSamplingCodecSuite](../codec/src/test/scala/eyes4s/codec/PointSamplingCodecSuite.scala).

1. Supply `StudyInput[K, U]`, static `Trials[K, Unit, Signed[U]]` templates and a registered
   `StudyLayout[K]`. Participant and stimulus projections identify templates; the full source
   key identifies an occurrence. Repeated presentations need distinct full keys.
2. Construct `PointSamplingSpec.of` with a nominal query clock, exact microsecond `Instant`s,
   optional checked `PointBins`, normalization, lookup and trajectory endpoint policies,
   control selection, and separate control/time-bin failure policies. Queries may be repeated,
   unsorted or empty. Order and cardinality survive every failure.
3. Call `PointSamplingPlan.of(...).run`. Missing templates, ambiguous projected template keys,
   duplicate full source keys, incompatible frames/clocks and missing trajectory positions
   remain typed failures. No arbitrary first template is selected. Unresolved source occurrences
   remain result rows but are excluded from the control candidate population.
4. Persist through `PointSamplingCodec` with separate versioned plan/result schemas and a typed
   key codec. `PointSamplingArchive.run(plan)` binds the result to its complete recipe.
   Decoding reconstructs checked inputs, checks registrations and hashes, replays the analysis,
   and compares every archived result field. Save keys and registrations with stable meanings;
   a changed projection requires a new registration identity/version.

## Time, cells and bins

`TrajectoryEndpoint.OnsetRange` includes the final fixation onset and excludes queries after it,
regardless of that fixation's duration. At an intermediate onset the new fixation supplies the
position. `HoldLastOnset` is an explicit alternative. A clock disagreement is an error, never an
implicit conversion. Reference fixture milliseconds are converted exactly to native microseconds.

`DensityLookupPolicy.NearestClampedRIndex` rounds the one-based cell index to even and clamps
outside coordinates. `ContainingCell` instead uses half-open cells and reports outside points.
The four named normalizations operate on the entire field before lookup. Constant fields retain
their raw values only for sample-z-score normalization; maximum and sum still normalize them.

Point bins have strictly increasing exact microsecond boundaries. `PointBinEndpoint.HalfOpen`
excludes the final boundary; `IncludeFinalEndpoint` assigns it to the last bin, reproducing
`cut(right=FALSE, include.lowest=TRUE)`. This endpoint policy is distinct from `EpochPlan`'s
short-final-bin policy. `unbinned` records query indices outside requested bins. With no bins,
it is empty because no bin admission was requested. Empty/all-missing bins remain results with
requested, successful and contributing counts. No NaN represents a missing score.

## Controls and the estimand

The candidate population is the multiset of admitted **source occurrences**, each mapped to its
matched template. Selection stays within participant and excludes every same-stimulus occurrence
before applying the cap. Two distinct source keys matched to template A therefore supply two
candidate occurrences when evaluating B, and neither is eligible when evaluating A. Every selected
control template is sampled along the focal source's path, not its own source's path.

`PointControlSelection.Disabled` is different from enabled selection with no eligible candidates.
The latter retains empty-control failures. `Selection.BottomK` uses the existing keyed sampler,
exact 64-bit seed and sample identity. Membership is independent of input ordering and nests as
the cap increases. It does not reproduce R's PRNG draws. The fixture retains R's cap-one indices
and verifies those resulting means independently.

At each time, `PointSamplingMean` averages successful selected controls under `controlPolicy`.
Then each bin averages those time means under `binPolicy`; the observed bin mean is computed
separately. The difference is observed minus control. With heterogeneous missing controls,
the pinned nested mean is 11/3 while pooling all available control-time values gives 4. Raw
control values, selected occurrence/template keys, query indices and all denominators remain
available. `RequireAll` refuses an incomplete level; `SuccessfulOnly(minimum)` makes the omission
policy explicit. The supported method is the named/versioned arithmetic-mean recipe
`eyes4s.static-density-onset-sampling-arithmetic-mean/1`; arbitrary R callbacks are not persisted.

## Reference evidence and limits

[point-sampling.json](../tools/r-parity/fixtures/point-sampling.json) records public
`sample_density_time` calls at the locked eyesim revision, all normalization modes, disabled,
finite and exhaustive controls, and boundary cases. An independent Python onset-step, grid-index
and arithmetic oracle verifies every scalar and bin mean at `OracleTolerance = 1e-12`.
The asymmetric 2-by-2 fields and distinct source paths detect axis, tie and wrong-path errors.

The reference silently drops an unmatched source and picks the first duplicate template. Native
results retain both situations as failures. R allows missing field cells; native `Signed` rejects
nonfinite cells on admission. The heterogeneous missing-cell fixture therefore qualifies the
two-stage reducer separately, not an end-to-end native missing-field feature. Empty queries,
empty sources, missing/no-eligible controls and final endpoints are retained in the raw evidence.
Temporal R selection excludes all true-match copies **before** sampling; this differs from the
reference template/fixation similarity facades described in [fixation sampling](FIXATION_SAMPLING.md).

The [saved artifact](../codec/src/test/resources/eyes4s/point-sampling-v1.json) is generated by the
public codec example and pinned after equal JVM/Scala.js JSON values (integral-double text may differ). Integer times and
seeds use exact decimal strings; results have explicit success/failure tags. Portable content
hashes detect changes but are not cryptographic authentication.

```sh
python3 tools/r-parity/generate_point_sampling.py --eyesim /path/to/eyesim --check
sbt 'planJVM/testOnly *PointSamplingSuite' 'planJS/testOnly *PointSamplingSuite' \
    'codecJVM/testOnly *PointSamplingCodecSuite' 'codecJS/testOnly *PointSamplingCodecSuite'
```
