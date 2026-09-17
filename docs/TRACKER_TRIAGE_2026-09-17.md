# Tracker triage, 2026-09-17

Review of all 93 open Mote tickets at commit `6218627` plus the uncommitted
X1/S1/M1 tranche. Every assessment was checked against code, tests and docs,
not ticket text alone. Verified evidence for the tranche: 2,176 library tests
(JVM and JS), 16 + 16 isolated-consumer tests, format/header/boundary checks.
MiMa is a no-op because no previous artifact is configured.

Tiers are ordered by what should happen first. Within a tier, items are ordered
by value per unit of effort. Effort: S under an hour, M half a day, L multi-day.

## Tier 0 — tracker hygiene (no code)

Close as already done, with evidence:

| id | evidence |
|---|---|
| aoi-seq | `aoi/Aoi.scala` AoiTransition counts; AoiSuite asserts them; ScanMatch consumes label sequences |
| pl-diff | `PlanChange` and `diff` in all three plan families |
| pl-registry | StudyRegistry, RecordingRegistry, MissingMethod, consumer proof |
| pl-display | MethodDescriptor, RecipeDescriptors, docs/METHOD_DESCRIPTORS.md (UI-M1) |
| v-cross-platform | CrossPlatformDeterminismSuite, EyeLinkParityPlatform, ConsumerSuite on both runtimes |
| bd-01M039S1EVKN325F2RDXGDEKR3 | docs/formats/eyelink-asc.md has status, support matrix, failure examples |

Merge (close the source, add a note on the target, move dependents):

| source | target | residual |
|---|---|---|
| io-export | eyesim-export | Arrow export only |
| cd-roundtrip | UI-S6 | manifest/payload codecs |
| cd-blobs | UI-S5 | manifest and verifying resolver |
| app-errors | UI-M4 | trial key and source line on PlanError |
| ms-v05 | bd-01M214CX2CTADNR6Q9D032Y1FB | its exit items are already that epic's blockers |
| bd-01KYDZ7GGMYKTXYWNRR40Q8X9M (seam) | cd-codecs | Trials/Timeline conditional codecs |

Rewrite to the residual scope:

| id | residual | new priority |
|---|---|---|
| UI-M2 | thin preview facade, reduction-policy field, truth-table tests; the schedule itself shipped in X1 | p2 |
| pl-analysis | shared plan interface and remaining journey nodes; the monolithic ADT is superseded by three concrete plan families | p2 |
| pl-detect | I-DT and microsaccade as RecordingMethod values; I-VT done | p2 |
| app-measureinfo | Smoother algorithm card; everything else done | p2 |
| aoi-set | AOI laws and mutants in the laws module; implementation done | p2 |
| v-generators | Scanpath generator only | p3 |
| bd-01M02N4E54KR43Q5JSSMCV4G2E | byte-budget preflight and banded variants; MultiMatch conformance done | p2 |
| doc-parity | bandwidth, Ops semantics, permutation baseline, signed-mass rows | p1 |
| doc-spec-sync | bound to a dated sync of eyes4s.md against PreparedStudy, MethodDescriptor, PairSchedule, DomainCodecs, RecordingPlan | p2 |
| doc-scaladoc | name plan, codec, aoi as the thin modules | p2 |
| io-csv | Mirror-derived key and metadata reader | p3 |
| bd-01M02N4DQ4M2PGVY2Y6DK48JT3 | split dynamic AOIs from pupillometry | p2 |
| ms-v10 | cite DEVELOPMENT_PLAN M6 as its exit | p2 |
| rel-audit | title says A-27, body A-23 | p2 |

Dependency edges to fix:

- Drop every edge that blocks on pl-analysis (pl-diff, pl-display, pl-registry, cd-codecs, app-progress, app-prereq, app-measureinfo, app-errors); the work was delivered against concrete plans.
- Drop milestone-to-feature inversions: aoi-set, io-csv, io-export blocked by ms-v05; v-coverage, v-cross-platform, v-generators blocked by ms-v065.
- Reverse rel-mima and rel-publish: MiMa needs a first published artifact.
- Drop UI-M3's dependency on UI-M2; preflight needs M1 descriptors, not previews.
- Repoint doc-migration from the closed pinning ticket to bd-01M214CX2CTADNR6Q9D032Y1FB.
- Mark v-asc-e2e blocked: it needs a redistributable real ASC file that does not exist in the repo.
- bd-01M214CXBZN8R7R4P0M5ACY25H should block on UI-G1, not on the whole app-boundary epic.
- Drop cd-codecs's dependencies on the closed x-trials and bd-01KYDZ6D3 tickets.

## Tier 1 — conditions on the X1/S1/M1 tranche and the plan

| item | effort | why first |
|---|---|---|
| Commit the tranche | S | 1,615 tracker ops and all code are uncommitted; nothing else should land on top of it |
| Unify pairing paths: make `PairConstruction.between` delegate to `DirectedPairSchedule` or add a generated-input equivalence law | S | X1's completion criterion says one pairing path; two exist |
| Add the StudyGuide before/after parity test X1 proposed | M | parity currently rests on unchanged io suites only |
| Revise docs/UI_FOUNDATION_PLAN.md: fixation-only intermediate gate (X4 + S2 + S4 + S5 + M4), relax X4's hard dependency on X2, rescope M2, remove stale "open and unassigned" wording, note that the default pair budget is unbounded | M | the remaining 14 tickets are sequenced by this document |
| Record an owner decision on the JavaFX/Intaglio host choice that UI_APP_VISION.md assumes | S | the plan silently adopts an undecided choice |

## Tier 2 — ready, high value, small or medium

| id | effort | note |
|---|---|---|
| eyesim-transform | S | Warp/Perspective exist; only R fixtures missing |
| bd-01KYFTQ7P9VXC3SR4TBYV02P6H surface sampling totality | S | `Surface.at` and `Grid.indexAt` still unchecked |
| app-measureinfo residual (Smoother card) | S | |
| v-generators residual (Scanpath generator) | S | |
| bd-01KYD6T48ZREC657WRGSAHM1R3 relation truth tables and mutants | M | ms-v05 exit item; PairDesignSuite has no truth table |
| UI-X3 bounded comparison and reduction | M | ready; base budget type exists in StudyWork |
| UI-M3 preflight reports | M | ready once the M2 edge is dropped |
| eyesim-entropy, eyesim-scanpath, eyesim-admission | M each | code exists; fixtures and pinning missing |
| aoi-set residual (laws and mutants) | M | |
| doc-parity rewrite | M | |
| bd-01M037KSYC771CKG0XS9FAQK7P dependency graph | S | GitHub admin toggle; Security workflow failing since 2026-09-15 |

## Tier 3 — ready or nearly ready, large

UI-S2 then S3, S4, S5, S6; UI-X2 (only after measuring whether pair-level
quanta suffice), X4, X5, X6; UI-M4; UI-G1; eyesim-compare, eyesim-kde,
eyesim-repetition, eyesim-template, eyesim-sampling, eyesim-temporal,
eyesim-export; v-parity as the umbrella; doc-mdoc, doc-migration,
doc-scaladoc, doc-spec-sync; bd-01KYD6SYK02ZRV99MG7FX939ZS surface
decomposition; bd-01KYDZ80ANFH3HW11946E4QTR8 epoch selectors; v-coverage;
bd-01M0FGQTV4FH0561V1VV4M2947 validation artifact generator;
bd-01M02N4CCS9425KPD4S0A0S9HX annotated detector benchmark;
bd-01M004XKS6PS89KTQSSCHJE86W storage profile.

## Tier 4 — blocked on external inputs or deliberately deferred

- EyeLink vendor-oracle work (bd-01M039RY73, bd-01M039S0WW, bd-01M039RWM9, epic bd-01M039NWZN): needs a licensed EDF API and real EDF files.
- v-asc-e2e: needs a redistributable real ASC file.
- Visual-world epic and its three children (bd-01KYDZ8V3, 933F, 9BYP, 9JQ7): no activity since 2026-07-26; VISION.md defers it.
- BIDS ingest (bd-01M004XM43), large-recording engine (bd-01M02N4FXY), capability incubator (bd-01M02N4H0D), Phase 3 epic (bd-01M02N4FAW): v1.1 or later.
- x-session: the UI plan routes bounded studies around it.
- Release gates (rel-publish, rel-mima, rel-audit, bd-01M214CXGNS6 acceptance): blocked on the foundation acceptance chain.

## Epics and milestones

No epic has every child done. The relational epic is 7 of 10 closed and its
body still reads as pre-implementation. The EyeLink epic is 16 of 22 closed and
the rest is externally gated. The app-boundary epic is 3 of 19 closed and
active. ms-v065 should rise to p1 since its plan-family children are done in
code; ms-v05 folds into the baseline epic; ms-v10 is an alias for M6.
