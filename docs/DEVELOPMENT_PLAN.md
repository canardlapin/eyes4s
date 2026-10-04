# Development plan: a coherent analysis foundation

Established 2026-09-08 from the owner's direction in [Vision and mission](VISION.md).
This is the active construction order. The architecture and numbered requirements remain in
[eyes4s.md](../eyes4s.md) and [PRD.md](../PRD.md); their older version numbers are historical planning
labels, not evidence of completion. Mote records dependencies and acceptance evidence.

## Outcome

Complete a scientific core that subsumes the [eyesim baseline](EYESIM_CAPABILITIES.md), offers a
pleasant direct API, and supports independently developed methods and a separate analysis UI.
The next substantial milestone is all of those properties demonstrated together. A saved toy plan
or a larger method inventory alone does not meet it.

Three executable research journeys anchor the work:

1. **Existing fixations to a study contrast.** Admit fixation tables with participant, stimulus,
   phase, geometry, and timing; construct scanpaths and weighted maps; compare matched templates
   and explicit controls; return per-pair scores, reduced results, contrasts, and tidy exports.
2. **Raw recording to an interpretable result.** Extend the real AdSERP workflow through validated
   synchronization, preprocessing, detection, and AOI measurement. Preserve source rows, missing
   support, excluded time, diagnostics, and the distinction between measured and derived samples.
3. **Repetition across time and scale.** Reuse the same study representation for repetition designs,
   temporal windows, and multiple bandwidths; retain window/scale identity and explicit reduction
   policies. Add a basic template-model example with training-only fitting and held-out evaluation.

The first two journeys must also run as saved plans; the third must be expressible through the
same plan vocabulary or a demonstrated registered extension. Existing fixations and raw samples
are equally legitimate starting points.

## Current position

The [public guide](../site-docs/index.md) is the reader entry point; the baseline manifest remains
the scientific inventory, and `tools/r-parity/check_baseline.py` prints its current counts. The
tracker, not this page, records each epic's live state:

| Epic | State | Remaining acceptance |
|---|---|---|
| Relational analysis (`bd-01KYD6CFWNAN0Y3TR59FJV8VMG`) | Closed | — Typed relations and designs, shared between/within scheduling, keyed sampling, explicit reductions and contrasts, repetition, and native OLS, NNLS, simplex and partial-association surface models are delivered. |
| Complete baseline (`bd-01M214CX2CTADNR6Q9D032Y1FB`) | Closed | — `check_baseline.py` reports no implementation gap; the remaining cases are verified equivalences or recorded intentional divergences ([PARITY.md](../PARITY.md)). |
| Psychology-facing API and docs (`bd-01M02N4EKAEA6EBB3TE0G4TYMP`) | Open | Fresh-consumer proof across all journeys, Scaladoc invariant coverage, rendered-browser review and hosted CI evidence. |
| Core readiness for the desktop app (`bd-01M3DH0R9WB8P7HYVSYMJEQJYG`) | Open | The analysis families and contracts in [the core readiness plan](plans/CORE_READINESS_PLAN.md). |
| Coherent roadmap (`bd-01M02N4AVNKKTEV3SA5PR0H4S8`) | Open | Foundation acceptance (M6, `bd-01M214CXGNS6HJQG8AM4EQXXF8`): documentation completeness, the downstream consumer, recording workload limits and supported-runtime release evidence. |

`PsychologyWorkflow` and `RecordingPlan` retain synchronization, angle conversion, preprocessing,
detection and AOI operations; `TemporalStudyPlan` retains explicit windows and clipped duration
support. Their bounded operation sets are not generic archives or proof that every method is
serializable. Template fitting runs natively on the shared QR solver; R is an offline parity
oracle only, never a workflow step. [`Session`](SESSION.md) is a checked container with total
accessors.

`RepetitionDesign` is a thin constructor over the existing pair/evaluate/reduce machinery. Its
explicit within-participant estimand differs from eyesim's measured phase-only grouping, which
PARITY.md records as a divergence.

Licensed EyeLink verification remains a separate programme with external inputs, not a
prerequisite for pure core work. Each implementation slice must establish current test evidence;
this roadmap does not turn historical test counts or symbol presence into release acceptance.

## Milestones and acceptance

### M1 — Turn the baseline into an executable contract

Start with `bd-01M214C09ZJH32HJWCT6BN13TD`, reusing `v-parity`, `doc-parity`, and `doc-migration`.

- Enumerate the methods, parameter conventions, and ordinary failure cases behind each row of the
  capability map. Pin the eyesim revision, required dependencies, input fixtures, and regeneration
  scripts. Separate matching estimands from intentional differences.
- Specify expected outputs for the three journeys before introducing new convenience APIs.
  Include duplicate/unmatched trial keys, empty groups, failed comparisons, non-overlapping time
  support, and incompatible frames/clocks. Name the estimand and realized denominator.
- Produce analytic or exhaustive small cases alongside reference outputs. Ordinary Scala tests
  consume committed fixtures without starting R or accessing vendor services.
- Attach an implementation task to every remaining baseline gap. Recheck older open items against
  their actual acceptance criteria, preserving any unfulfilled scientific obligations.

**Exit:** the baseline is finite and reviewable; every required case has a target, an evidence
method, and either verified coverage or a specific gap. `PARITY.md` reports measured results only.
Unexplained discrepancies remain open; historical assertions about eyesim are rechecked at the
pinned revision.

### M2 — Complete the scientific workflows

Integration gate: `bd-01M214CX2CTADNR6Q9D032Y1FB`. Reuse `x-contrast`, surface-totality work
`bd-01KYFTQ7P9VXC3SR4TBYV02P6H`, relational laws `bd-01KYD6T48ZREC657WRGSAHM1R3`, and surface
modeling `bd-01KYD6SYK02ZRV99MG7FX939ZS`.

- Finish typed contrasts over compatible reduced results, preserving pair orientation, method
  identity, failure policy, key coverage, and the meaning of each score component. Do not assume
  every bounded score is closed under subtraction or that every score supports a mean.
- Provide the missing composition for matched, repetitive, multiscale, and temporal analyses.
  Convenience functions construct ordinary typed designs; they do not contain alternate samplers
  or hidden scientific branches.
- Complete fixation admission and comparison/contrast export where the existing I/O supports only
  samples or AOI results. Preserve source timing without inventing a recording reference.
- Define the template-model data boundary: response and predictors, fitted parameters, folds,
  training-only transformations, and typed predictions/residuals. Reuse the existing surface-fit
  work; keep solver-specific dependencies optional. Deliver at least one basic fit, either through
  an optional adapter or a runnable export/reimport workflow, and name unsupported model variants.
- Keep clocks, observed timing, missing support, spatial normalization, and grid/scale compatibility
  explicit throughout. Extend existing contracts only where a required workflow exposes a gap.

**Exit:** every required capability-map row has a runnable public workflow with independent
scientific evidence. All three journeys retain keys, failures, exclusions, and denominator
accounting. Reference discrepancies are explained and tested; no required workflow remains a
promise backed only by primitives.

### M3 — Make the direct library a pleasure to use

Use `bd-01M02N4EKAEA6EBB3TE0G4TYMP` and its existing documentation work. Begin alongside M2;
API examples should shape implementation before signatures settle.

- Build a compiled guide for each research journey. Present ordinary choices first and connect
  each convenience call to the underlying domain operations. Collect repeated laboratory setup
  into reusable, validated configuration rather than another set of scalar arguments.
- Use `PsychologyWorkflow` as a working example, then factor reusable pure descriptions into the
  appropriate domain/design/plan modules. Source loading, writing, and cancellation stay in the
  effectful adapters. Keep direct lower-level composition available.
- Standardize discoverable ways to inspect parameters, results, diagnostics, warnings, provenance,
  and export schemas. Preserve domain-specific result types; avoid a universal bag of fields.
- Add a migration guide organized by scientific task and a developer guide for a custom method.
  Correct stale module READMEs and architecture examples as each area is verified.

**Exit:** a fresh consumer project can execute the guides using only public APIs. Examples have
no private test helpers, unsafe extraction, invented raw samples, or undocumented setup. A reader
can identify the estimand, change one scientific choice, inspect its configuration, and obtain
useful data or an operand-bearing error. Numerical equivalence between convenience and explicit
composition is checked. Documentation builds in CI.

### M4 — Make analyses inspectable and persistent

Use `bd-01KYDZ7GGMYKTXYWNRR40Q8X9M`, `pl-detect`, `pl-analysis`, `pl-registry`, `cd-codecs`,
`cd-roundtrip`, `cd-blobs`, `pl-display`, `pl-diff`, `x-session`, and `app-prereq`. `ms-v065`
remains the integration gate. Design the seam during M1/M2; implement the first saved journey
once its concrete operation/result contracts are established. Do not wait for every baseline
method before testing a small complete round trip.

- Settle ownership of versioned schema contracts and typed node registration without a plan/codec
  dependency cycle. Parameters remain domain values. Serializable projections/selectors name
  registered behavior; arbitrary functions and a display label cannot stand in for persistence.
- Start with detection and one analysis journey. Interpret plans through existing `Machine`,
  comparison, pairing, and reduction operations. Add nodes from the other journeys as required.
- Give each plan an inspectable structure, parameters with units, stable operation identities,
  structural comparison, and useful prerequisites. Missing viewing geometry, input artifacts,
  observed anchors, or adequate synchronization must be discoverable before expensive execution.
- Preserve shared frame/clock identity across decoding, validate duplicates and incompatible
  schemas, and reference large payloads by typed content hashes. Resolve artifacts in `io`/`fs2`;
  do not make pure decoding load files or access the network.
- Separate plan/schema version, method version, input digest, and result representation. Pin a
  previous schema fixture and demonstrate one migration or a precise incompatibility response.
  Never silently reinterpret an old plan using changed defaults.
- Keep exact content verification separate from numerical reproducibility. Specify which values
  are exact and which use named tolerances; compare raw numerical outputs as well as exports.

**Exit:** direct, interpreted, and saved/reloaded forms of the designated journeys agree under
their declared contracts on JVM and Scala.js. Codecs have round-trip and malformed-input tests,
including user-defined keys/metadata/markers. Missing plugins, versions, artifacts, and prerequisites
produce actionable typed failures. A changed plan produces a meaningful structural diff.

### M5 — Prove modular development and application consumption

Status: closed on the integrated UI-G0/UI-G1 consumer evidence. Long-recording responsiveness
is measured in [execution responsiveness](EXECUTION_RESPONSIVENESS.md).

The [UI-consumer infrastructure plan](UI_FOUNDATION_PLAN.md) breaks the execution, scientific
serialization and method/preflight boundary into scoped tickets under the existing Mote owners;
the shared work/pair schedule, domain codecs, typed descriptors, preflight and bounded preview are
implemented. A fixation-only gate (G0) proved the first app journey before the external-consumer
gate (G1) covered the bounded shipped study, recording and temporal routes; neither closes the
broader baseline, Session or all-method coverage requirements in this plan. Follow the live
tracker for the remaining consumer work.

Use `bd-01M214CXBZN8R7R4P0M5ACY25H`, the application-boundary integration work
`bd-01M02N4GM03BF98C3PSJS6D4WA`, and `app-progress`/`app-measureinfo`/`app-errors`.

- Build an isolated consumer project against packaged artifacts, without access to implementation
  source or package-private helpers. Add a custom comparison and one detector or smoother with
  typed parameters, typed results, and published law/conformance suites.
- Register a custom plan node and its codec, then persist and run it through the ordinary
  interpreter. Test duplicate registration, unknown versions, invalid parameters, and extension
  removal. Registration must not require modifying the core ADT for each downstream method.
- Build a small headless consumer that lists methods and their prerequisites, inspects/diffs
  plans, resolves inputs, runs an analysis, and consumes results suitable for presentation. Verify
  FS2 progress and cancellation, resource cleanup, and equivalence with uninterrupted pure runs.
- Write a module-author guide covering dependency placement, algorithm metadata, versioning,
  published tests, and failure behavior. Record any pressure revealed by the two different
  extension families before declaring the contracts stable.

**Exit:** the external consumer demonstrates extension and application contracts on the supported
platforms without changes to core interpreters. A UI repository could build on those contracts;
no GUI, remote service, or plugin marketplace is part of this milestone.

### M6 — Accept and stabilize the foundation

Use `bd-01M214CXGNS6HJQG8AM4EQXXF8`, then the existing release audit and publication tasks.

- Require M2–M5 acceptance evidence and a capability map with no unresolved baseline gaps. Run the
  existing compiler, law, negative-type, mutation, boundary, formatting, workflow-generation,
  documentation, and supported Java/JVM/Scala.js CI checks appropriate to the final candidate.
- Run one representative recording/study workload and record input size, time, and memory on a
  named environment. Set practical limits from that workload before optimizing. Preserve the
  tested scientific results across any performance changes.
- Publish compatibility and schema-evolution policies, module dependencies, support limits, and
  migration guidance. Reconcile every v1.0 acceptance requirement with current evidence.
- Distinguish local checks, hosted checks, numerical conformance, and real-device/vendor evidence.
  The designated golden workflows retain exact cross-runtime checks. Broader numerical claims
  follow the explicit reproducibility contracts.

**Exit:** a release candidate satisfies the scientific baseline, direct-user experience,
persistence, extension, and operational acceptance criteria. The separate UI application does not
block this milestone. Publishing artifacts remains a distinct release action.

## Construction order and the next implementation slices

M1 starts first. M2 and M3 advance together; M4 begins with its seam design during M1 and then
follows concrete workflow contracts. M5 consumes the completed persistence boundary. M6 requires
all preceding acceptance evidence. Documentation and error quality are part of each slice.

The contrast, fixation-table, saved-study, multiscale-extension, repetition and template slices
are delivered within the boundaries recorded in [saved studies](SAVED_STUDIES.md),
[fixation studies](FIXATION_STUDIES.md), [extending studies](EXTENDING_STUDIES.md),
[repetition studies](REPETITION_STUDIES.md), [template fitting](TEMPLATE_FITTING.md) and
[PARITY.md](../PARITY.md).

The next coherent slices are:

1. **Core readiness for the desktop app:** the analysis families, descriptors and contracts the
   application needs, in the order of [the core readiness plan](plans/CORE_READINESS_PLAN.md).
2. **Consumer and docs proof:** complete public-example migration and API Scaladoc, obtain
   rendered browser review, run the fresh-consumer proof across all journeys, and distinguish
   configured CI from a passing hosted run.
3. **Foundation acceptance:** capture a representative recording workload with practical time
   and memory limits ([execution responsiveness](EXECUTION_RESPONSIVENESS.md)) and the
   supported-runtime evidence M6 requires.

Reuse live task ownership and attach any genuinely new gap to the existing consumer and acceptance
gates rather than creating another roadmap.

## Scope discipline

Complete the shared core before pursuing a long list of specialized methods. Reading, additional
saliency measures, adaptive detection, pupillometry, visual-world analysis, and advanced replay or
transport models are subsequent domain modules, each with a concrete scientific task and evidence
plan. A module may expose a necessary shared contract, but does not justify importing its entire
implementation into the kernel.

Maintain I/O correctness and the real ASC support requirement. The licensed EDF programme and
new vendor formats do not gate pure plans, codecs, or usability work. Preserve their existing
issues and evidence instead of declaring unfinished verification complete. General model fitting
and visualization stay in optional integrations or consumers; the basic template workflow is
native, so no baseline workflow depends on an external solver.

## Tracker reconciliation

Roadmap epic: `bd-01M02N4AVNKKTEV3SA5PR0H4S8`. Existing tasks are reused; the four new integration
items above capture baseline definition, baseline completion, downstream proof, and final acceptance.
The adopted direction is recorded in closed decision `bd-01M214TX04A7GNRHP3ECBXQETE`.

Remove milestone-wide prerequisites that do not represent actual code dependencies: DetectPlan
must not wait for licensed EyeLink certification; current documentation must not wait for all
codecs; the application boundary must not wait for new vendor formats or every advanced detector.
Retain concrete scientific dependencies, including Session/AOI prerequisites and the generic-codec
seam. Old EyeLink and advanced-breadth milestones remain open with their own acceptance criteria.

The foundation acceptance gate requires the baseline, usability, persistence, downstream-proof,
and portable real-ASC items. It becomes an additional prerequisite of the existing release
acceptance audit, so the new direction cannot be satisfied by closing only the old version milestones. Follow `mote ready`
and this construction order; do not infer implementation completion from a milestone's name.
