# Eyes4s / pymovements comparison contract

Protocol version 1, source inventory refreshed on 2026-10-05; comparator review dated 2026-09-27.
Epic: `bd-01M3HYEG72WCCNE6P84PWKBGCV`.
Inventory slice: `bd-01M3HYF5BJ3CXAXG70BS13ZG2W` (PM1.1).

The objective is demonstrated leadership in **all five areas**: complete research
workflows, API design and researcher usability, performance, scientific
visualization, and independent empirical qualification. A strength in one area
does not offset a missing or failed area. This document defines the comparison;
it does not assert that eyes4s has already achieved the objective.

## Pins and authority

The authoritative inventory is
[`tools/pymovements/manifest.json`](../../tools/pymovements/manifest.json).
It records public entry points, source hashes and anchors, supported or required
platforms, scientific conventions, existing fixture/test pointers, missing
evidence, and Mote owners. The table below is generated from that file. An owner
is the next accountable **ticket**, not a claim that an agent is currently
working on it. Initial slices do not discharge their feature's remaining scope.

The comparator is [pymovements 0.28.0](https://pypi.org/project/pymovements/0.28.0/),
released 2026-09-08, with source commit
[`fe333f3afa862ae3fc7056047a1a929798cfe18b`](https://github.com/pymovements/pymovements/tree/fe333f3afa862ae3fc7056047a1a929798cfe18b).
PyPI's current release and the Git tag were checked at intake. The published
wheel is pinned by its immutable URL and SHA-256:
`4600f990173faeb9cccfdfc4da627ac81981eca6f1d4c511056294a5b96054ff`.
Member hashes bind the reviewed Python APIs to that wheel; inspection did not
install or execute it. The [stable documentation](https://pymovements.readthedocs.io/en/stable/reference/index.html)
helps navigation but is not an immutable evidence pin.

The eyes4s source snapshot is
`cba9edd54e48275e4db9aec8d8f09e93636e46c5`. The source hashes are checked against
the local checkout. They must be refreshed by reviewing changed entry points,
not by blindly rehashing to make a check pass. No absence claim here is a proof
that no private or experimental implementation exists elsewhere.

Before final epic acceptance, recheck the current pymovements release and source
identity. Preserve the original baseline. If a newer release exists, record its
capability delta, run affected comparisons under the same protocol, and retain
both results. A changed pin, narrowed required scope, relaxed superiority
threshold, or waived platform needs an explicit owner decision in Mote. A
moving `stable` URL must never silently replace the comparator.

## Four independent evidence levels

1. **Implementation**: source is present, partial, or absent for the row's stated
   capability. API/test source and historical receipts are inventory evidence.
   A partial implementation may still be a useful scientific primitive.
2. **Workflow**: a reproducible public consumer executes the stated task through
   admission, analysis, diagnostics, saved/reloaded results and applicable export.
   Existing tests are pointers to run, not proof of a current paired journey.
3. **Empirical qualification**: an independently justified protocol evaluates
   the relevant scientific, usability, performance or visual outcome. Oracle
   conformance, typed invariants, screenshots and corpus admission each establish
   narrower facts. They cannot substitute for this evaluation.
4. **Release availability**: a clean consumer can install the exact published
   artifact and reproduce the capability on every required platform. A local
   source build or unreleased Studio branch is insufficient.

The last three levels start `pending`. Positive states (`verified`, `qualified`,
`available`) require hashed JSON receipts naming the capability, dimension,
reviewed source commit, protocol digest, exact command, runtime/platforms,
input/config/output hashes and a passed result. Release receipts additionally
name artifact coordinates and hashes. The protocol digest excludes the generated
inventory table, so recording a result does not change its evaluation criteria.
These are provenance requirements, not
an automatic scientific adjudication: reviewers must inspect raw results and
the frozen criteria. The checker checks receipt structure and binding; it does
not rerun commands, fetch artifacts or prove the truth of a `passed` field.

A row can be accepted only with implementation present, all three evidence
levels passed, and no remaining gap. Every open row names an open next owner.
The five Mote gates and root epic remain open while required rows remain open.
Do not remove required rows to make a percentage or aggregate score improve.

## Common tasks and fair scientific comparison

The common research journey is: obtain licensed data; admit samples and metadata;
inspect quality; convert units and preprocess; detect events; compute AOI/reading
or comparative measures; inspect figures; export and reopen the analysis.
Reading and quality reports are required tasks. The corpus and fixture manifest
must include their cases; the existing human detector corpus lacks reading.

Each row's manifest names conventions and candidate fixtures. PM2.1 and PM3.1
will freeze actual input/member hashes and task configurations before collecting
comparison outcomes; a planned fixture description is not an admitted dataset.
Use the same data, licensed subsets, scientific estimands, error policies and
available tuning information. Keep rejected rows/events visible. Preserve the
repository's static units, nominal frame/clock identity, pure/effect boundaries,
typed failures, named tolerances and law/mutation requirements.

For I-VT, I-DT and microsaccades, record units, velocity stencil, threshold
estimation, minimum duration, invalid support and event-boundary mapping.
pymovements offsets identify the last included timestamp; eyes4s uses half-open
support. The adapter must retain original boundaries alongside normalized ones.
For I-DT, sum-of-axis-range dispersion differs from per-axis extent thresholds,
and the violating-sample policy differs. Equal numeric parameters do not make
these algorithms equivalent. Use a documented common subset or report the
scientific difference; never change the scorer to hide it. The existing
[0.26.2 conformance fixtures](../../tools/detector-conformance/README.md) remain
historical evidence until a reviewed 0.28.0 adapter establishes current behavior.

For QC, name the denominator, eye, validity/blink/interpolation policies, precision
estimand and units. For reading, name word order, regression/skip conventions,
first-pass termination and interruption handling. For densities and comparisons,
fix normalization, coordinate domain, grid, bandwidth, alignment and empty-support
policy. A plotting heatmap is not automatically the same density estimand.

## Acceptance work under the five gates

- **PM1 — workflow completeness.** Execute the entire common journey, including
  reading and QC. Reuse existing BIDS, dynamic AOI and real Studio backend work.
  New bounded slices cover dataset admission, sample-loss QC, ordered reading
  AOIs/first pass, pupil blink candidates, resampling, fixed-parameter I-HMM, and
  IPC interchange. Their bodies require successors for incomplete feature scope.
- **PM2 — design and usability.** Preserve the typed architecture and qualify the
  first-analysis public consumer. Freeze researcher tasks, starting knowledge,
  completion/error measures and success thresholds before evaluating. An agent
  running a snippet is a developer smoke check, not researcher usability evidence.
- **PM3 — performance.** Freeze numeric per-workload budgets and meaningful speed
  and memory improvement thresholds **before** optimization/evaluation. Include
  CSV/ASC admission, geometry/preprocessing/detection (including I-DT scaling),
  density, comparison, full result materialization/export, streaming and Studio
  response. Separate cold startup, warm computation, end-to-end cost, peak RSS,
  allocation/GC and retained memory. Record hardware, runtime/compiler, threads,
  cache policy, warm-up, repetitions, uncertainty and failures/timeouts/OOM.
  Outputs must satisfy the scientific contract before timings count. The first
  baseline is CSV-to-I-VT; optimization follows a measured bottleneck. Real Studio
  p95 latency uses the existing S10.6 fixture/10x workload and its explicit budgets.
  Eyes4s-only operations get absolute budgets. Historical ASC measurements and
  asymptotic source inspection are leads, not current comparative wins.
- **PM4 — visualization.** Freeze same-data specimens for scanpaths, traces/events,
  heatmaps, AOI/reading, main sequence, QC and study comparisons, with physical
  output sizes, scale conventions and an independent review rubric. Include gaps,
  off-screen samples, overplotting and misleading-scale counterexamples. Separate
  actual SVG/PDF/PNG renders from designs, static quality from interactive task
  success, and handler-to-snapshot timings from input-to-display latency.
- **PM5 — empirical qualification.** Use the existing human-corpus/scorer owner.
  Freeze timing/label adapters, matching, held-out splits, tuning, uncertainty
  and adequacy/improvement thresholds before scoring. Evaluate required event
  classes and stimulus families, including reading, plus noise/dropout/rate
  degradation. Use independent participant/recording units for uncertainty.
  Report unavailable reference quantities, losses and failed strata. Corpus
  admission and reference-code agreement are not human-data validity.

PM3.1 owns numeric performance thresholds; PM4.1 owns the visual rubric; PM5.1
and the existing benchmark own scoring/adapters. This inventory deliberately
does not invent those numbers after seeing results. Until their protocols and
actual receipts exist, no comparative superiority claim is accepted.

## Capability inventory

`pending` means this comparison has no accepted current receipt. It does not
erase existing code or prior project test results. Platform lists in the manifest
state the required acceptance scope, including planned platforms for absent work;
they are not release claims. API links, scientific conventions, fixture pointers
and the exact remaining gap for each row live in the manifest.

<!-- comparison-table:start -->

| Capability | Targets | Implementation | Workflow / empirical / release | Next owners |
|---|---|---|---|---|
| `geometry` — Typed geometry, units and identities | PM2, PM5 | present | pending / pending / pending | `bd-01M3HYF5KM139C8NXG35NXRYEP` |
| `datasets` — Public dataset access | PM1, PM2 | partial | pending / pending / pending | `bd-01M3J2FA8E3CPC5GW6JFHKTCS2` |
| `csv-asc` — CSV and ASC admission to event results | PM1, PM2 | present | pending / pending / pending | `bd-01M3HYF5KM139C8NXG35NXRYEP`<br>`bd-01M3HYF63RDKD2VSQ9PFAXEKAN` |
| `ipc-bids` — IPC/Feather and BIDS interchange | PM1 | absent | pending / pending / pending | `bd-01M3J2FHKPFGQR40D4V6H5XBR7`<br>`bd-01M004XM43TEWJENBNBX29GWK1` |
| `preprocessing` — Velocity, interpolation and smoothing | PM1, PM5 | present | pending / pending / pending | `bd-01M3HYF6VRDVEJ9C7H1PRYAJ3E`<br>`bd-01M3HYF5KM139C8NXG35NXRYEP` |
| `resampling` — Regular-grid resampling | PM1 | absent | pending / pending / pending | `bd-01M3J2FF59TXXH53NXY1FZNVMG` |
| `ivt` — I-VT detection | PM1, PM5 | present | pending / pending / pending | `bd-01M3HYF6VRDVEJ9C7H1PRYAJ3E` |
| `idt` — I-DT detection and scaling | PM1, PM3, PM5 | present | pending / pending / pending | `bd-01M3HYF6VRDVEJ9C7H1PRYAJ3E`<br>`bd-01M3HYF5VMV818PJMJDKMJBK8P` |
| `microsaccades` — Engbert-Kliegl microsaccades | PM1, PM5 | present | pending / pending / pending | `bd-01M3HYF6VRDVEJ9C7H1PRYAJ3E` |
| `blink` — Pupil-based blink detection | PM1, PM5 | absent | pending / pending / pending | `bd-01M3J2FDY7D9BQ09NYJCPM3FVT` |
| `ihmm` — I-HMM detection | PM1, PM5 | absent | pending / pending / pending | `bd-01M3J2FGC4DWFJ7K7TZFEJ5TZ6` |
| `qc` — Sample quality reports | PM1, PM5 | partial | pending / pending / pending | `bd-01M3J2FBFQJHEVV5SQBM2EQF28` |
| `reading` — Word-level reading measures | PM1, PM5 | partial | pending / pending / pending | `bd-01M3J2FCPZX828NSHW4Q8TZVV2` |
| `aoi` — AOI assignment, dwell and transitions | PM1, PM5 | present | pending / pending / pending | `bd-01M02N4DQ4M2PGVY2Y6DK48JT3`<br>`bd-01M3HYF5KM139C8NXG35NXRYEP` |
| `study` — Typed analyses, contrasts and provenance | PM2, PM5 | present | pending / pending / pending | `bd-01M3HYF5KM139C8NXG35NXRYEP` |
| `distributions` — Density, bandwidth and multiscale measures | PM1, PM3, PM5 | present | pending / pending / pending | `bd-01M3HYF5VMV818PJMJDKMJBK8P`<br>`bd-01M3HYF41AZGMBR922GN3H6VY4` |
| `comparison` — Scanpath and distribution comparison | PM1, PM3, PM5 | present | pending / pending / pending | `bd-01M3HYF5VMV818PJMJDKMJBK8P`<br>`bd-01M3HYF41AZGMBR922GN3H6VY4` |
| `first-analysis` — Install-to-first-result research journey | PM2 | partial | pending / pending / pending | `bd-01M3HYF5KM139C8NXG35NXRYEP` |
| `persistence` — Saved analysis and replay | PM2, PM1 | present | pending / pending / pending | `bd-01M3HYF5KM139C8NXG35NXRYEP`<br>`bd-01M3DPFHY9YW5BPSJQ8VBBXHVN` |
| `studio` — Real Studio analysis and persistence | PM1, PM2 | partial | pending / pending / pending | `bd-01M3DPFHY9YW5BPSJQ8VBBXHVN`<br>`bd-01M3DPG3KTRQNN6HKWRHYQTT0C` |
| `scanpath-heatmap` — Scanpath and heatmap figures | PM4 | partial | pending / pending / pending | `bd-01M3HYF6KTBTJDGSG8R300DVJC`<br>`bd-01M3HYF4TV51MQA36X1QFYG9WA` |
| `trace-events` — Trace and event-overlay figures | PM4 | absent | pending / pending / pending | `bd-01M3J2FJTQFV6RF1ZVJZVW34DM` |
| `reading-qc-main-sequence` — Reading, QC and main-sequence figures | PM4 | absent | pending / pending / pending | `bd-01M3HYF6KTBTJDGSG8R300DVJC`<br>`bd-01M3J2FJTQFV6RF1ZVJZVW34DM` |
| `publication` — Publication export and interactive task quality | PM4, PM2 | partial | pending / pending / pending | `bd-01M3HYF6KTBTJDGSG8R300DVJC`<br>`bd-01M3HYF4TV51MQA36X1QFYG9WA` |
| `performance` — Paired CPU and memory performance | PM3 | partial | pending / pending / pending | `bd-01M3HYF5VMV818PJMJDKMJBK8P`<br>`bd-01M3HYF63RDKD2VSQ9PFAXEKAN` |
| `responsiveness` — Studio latency and bounded memory | PM3, PM4 | partial | pending / pending / pending | `bd-01M3DPG25V0DGFR2BPANBDWMQF`<br>`bd-01M3HYF5VMV818PJMJDKMJBK8P` |
| `empirical` — Independent scientific qualification | PM5 | partial | pending / pending / pending | `bd-01M3HYF6VRDVEJ9C7H1PRYAJ3E`<br>`bd-01M02N4CCS9425KPD4S0A0S9HX` |
| `streaming` — Batch/stream equivalence and bounded execution | PM3, PM2 | present | pending / pending / pending | `bd-01M3HYF5KM139C8NXG35NXRYEP`<br>`bd-01M3HYF5VMV818PJMJDKMJBK8P` |

<!-- comparison-table:end -->

## Verification and next execution

Run the [inventory checker and its tests](../../tools/pymovements/README.md).
Default checking is offline; final inventory acceptance also checks the exact
wheel and the live shared Mote store. A stale source reference or a closed next
owner requires review and a successor, not an automatic exemption.

After PM1.1, PM2.1 (first consumer), PM3.1 (performance protocol), PM4.1
(specimens/rubric), and the new bounded capability tasks can proceed. PM5.1 also
needs the existing human-benchmark protocol/admission dependency. The sequence
for performance remains protocol → paired baseline → measured optimization.
The comparison epic does not change the core eyesim baseline's release scope.
