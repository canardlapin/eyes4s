# Core readiness for the desktop app

Established 2026-09-25 from the owner's direction: finish the core computational framework so the
desktop application can start, with 90–95% of eyesim's analyses available through eyes4s. The
host is JavaFX; plots and graphics use Intaglio where it makes sense. UI design proceeds in a
separate session. This plan covers the core only.

Scope decisions recorded in Mote:

- The parity target is the eyesim baseline. GazeWeave is noted and deferred
  (`bd-01M3DGCG45SC5WSNZGMFE940GE`); latent transforms stay outside the baseline (`eyesim-transform`).
- The parity pin follows the latest local eyesim `master`.
- The real-ASC end-to-end gate (`v-asc-e2e`) blocks publication, not the UI start or foundation
  acceptance.

## Definition of done for an analysis family

The app can only offer an analysis that satisfies the same contracts as the study, recording and
temporal plans. Each family must provide:

1. a serializable plan and codec, registered as a recipe family with an archive/manifest role;
2. a `MethodDescriptor` whose parameters carry typed bounds, defaults and units;
3. preflight and error diagnostics with catalog codes, keyed to the app's entities;
4. a resumable `Stepwise` cursor executed by the shared fs2 runner with progress and cancellation;
5. results that project into the shared result-table layer;
6. parity or oracle evidence, and a conformance test that enumerates every family and fails when
   one is missing a contract.

## Phases

| Phase | Goal | Contents | Tickets |
|---|---|---|---|
| CR0 | Land and verify | Commit the 2026-09-19 tranche, fresh-context review fixes, CI blockers, eyesim pin move, push, hosted CI green | `bd-01M2RKFZVHASF2GRKGP8DT6C45` |
| CR1 | Close the baseline | `eyesim-entropy`, `v-parity` zero-gap run, every map method available to `template_similarity`, matched/control scanpath plan | `eyesim-entropy`, `v-parity`, baseline epic |
| CR2 | Consolidate before registering | One comparison-method registry that keeps `Metric`/`Kernel` typing, eyesim-compatibility variants in their own object, one template-fitting family, one pure result-table layer outside `io`/`fs2`, remove uninhabited public cases | new |
| CR3 | Durable persistence | Upcast registry on `VersionedCodec`, one artifact-identity scheme, canonical wire forms | new |
| CR4 | Uniform family contracts | Apply the definition of done to repetition, point sampling, epochs, templates and decomposition, map, scanpath and fixation comparison | new; absorbs `pl-analysis` |
| CR5 | Diagnostics and typed access | `Diagnose[E]` instances for every public error enum, keyed preflight, io diagnostics, typed `LoadedStudy` | `bd-01M2SBT6HYM8WT3VHND10TSRGF`, `bd-01M2SG4718MMQ8SDDMC0K13T33`, `bd-01M2SG47CCG30E22SY3M4WA7DR` |
| CR6 | Form-grade descriptors | Typed numeric bounds, defaults, unit enums, raw-input JSON, no path-dependent types at the host boundary | new |
| CR7 | Fixation-level AOI | Fixation and scanpath dwell, entries and visits, after the owner's visit decision | `bd-01M2R18YTC8BF4T1890QX991RW` |
| CR8 | Interactive bounds | Cursors or work bounds on quadratic measures (distance correlation, fixation transport), measured envelopes | new |
| CR9 | Hygiene | Move EyeLink evidence apparatus out of `io` main, replace dated tracker logs with current-state docs. Keep the full Scala.js cross-build: Studio must remain swappable to a Scala.js client (owner, 2026-09-26) | new |

## Studio requirements

The UI-design session (Eyes Studio, a separate `studio` module) ranked what it needs from the core
on 2026-09-25. Its MVP uses only the fixation-study (reinstatement) family; other families can
follow. Studio owns the document model, undo, commands, selection and view state.

| Item | Need | Ticket |
|---|---|---|
| UI-A (blocker) | Analysis window inside the admission frame with an off-window policy, px/° as a recorded plan parameter, frame corrections recorded in the admission ledger, per-trial off-window counts | `bd-01M3DH2F9QK95DYA2VBHJ5FA9Y` |
| UI-B (blocker) | Preflight finding or policy for matched cardinality ≠ 1; occurrence-aware `StudyLayout` with codec | `bd-01M3DH2FRNZG7PFZ57SJ632Y18` |
| UI-C (blocker) | `ReportSpec`: a pure, serializable reduction over `StudyResult` with typed trial covariates | `bd-01M3DH2GCC15HC73EGC0G8GBS3` |
| UI-D | Progress stages, stale-result rejection by plan revision and input digest, exact preview counts | `bd-01M3DH2GZ458TTHRH2GBE0C49M` |
| UI-E | Density grids and geometry readable from result archives; recomputable-density payloads | `bd-01M3DH2HGXSF41GET6C3WAHWTX` |
| UI-F | Initial-fixation policy; structural `plan.diff` for recipe inspection | `bd-01M3DH2J1G3Z52RY0ESABW87WX` |
| UI-G | Navigable result provenance, paged inspection, typed `ResultRef` | `bd-01M3DH2JHHC5HKFD41DZS0W2GE` |
| UI-H | `FixationCsv` sample counts, attribute pass-through, declared time units, trial inventory | `bd-01M3DH2K0K354FKKPX0VKNPC0Y` |

## Order

1. CR0 (land and verify).
2. The study-family blockers: UI-A, UI-B, then UI-C together with the result-table part of CR2.
3. CR3, CR6 with UI-F, CR5 with keyed `affectedTrials`, UI-D and UI-E.
4. CR1, UI-G and UI-H.
5. The rest of CR2, then CR4 for the remaining families, then CR7–CR9.

The app can begin against the study family as soon as step 2 lands; the family conformance test
states which families satisfy the definition of done.

## Stop doing

- Adding public API variants solely for eyesim bit-parity; they belong in the compatibility object.
- Landing a method without the definition of done above.
- Adding dated process logs or chronological "Update" layers to `docs/`; git holds the history.
- Growing per-method evidence overhead faster than method value.
