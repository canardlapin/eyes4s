# Branch and worktree consolidation · 2026-10-05

Intake main was `09e6255ad6233900f3848398f6320f4fae077131`. Of 236 local
branches, 188 were already ancestors. The 48 remaining refs are accounted for below.
All original branches and worktrees are preserved. Recovery used isolated checkouts
and retained complete coupled core/app/viz/desktop changes. Generated API metadata
is re-recorded on the combined tree rather than copied from historical branches.

This record describes source disposition. Gate results and final SHA/tree identities
are recorded in the landing evidence; source recovery alone is not qualification.

| Intake branch | Disposition |
|---|---|
| `codex-audit-candidate-inventory` | Implemented on main by 11be6f3c and 1dee4e21; newer audit fixes retained. |
| `codex-ledger-bounded` | Archive: unfinished replay coordinator foundation; no production caller, coupled S2.2/S2.3 acceptance open. Shared S2.1 extraction recovered separately. |
| `codex-pymovements-asc-adapters` | Landed in the first verified batch, main 8ab15dc3. |
| `codex-pymovements-asc-fixture-compat` | Landed in the first verified batch, main 8ab15dc3. |
| `codex-pymovements-capability-contract` | Landed in the first verified batch, main 8ab15dc3. |
| `codex-pymovements-csv-header-index` | Landed in the first verified batch, main 8ab15dc3. |
| `codex-pymovements-csv-ivt-baseline` | Landed in the first verified batch, main 8ab15dc3. |
| `codex-pymovements-interference-gate` | Landed in the first verified batch, main 8ab15dc3. |
| `codex-pymovements-performance-protocol` | Landed in the first verified batch, main 8ab15dc3. |
| `codex-source-ui-e-integration` | Exact patch already on main (git cherry: no unique patch). |
| `p0-addpanel` | Landed in the first verified batch, main 8ab15dc3. |
| `p0-bundle-dup` | Landed in the first verified batch, main 8ab15dc3. |
| `p0-bundledoc` | Recovered in the bound-report, bundle and table integration (ea1ef248); current protocol and UI retained. |
| `p0-figc` | Recovered in the reviewed import/density line (0ee8ba55), including coupled dirty figure work. |
| `p0-ledger-s2` | Archive: unfinished replay coordinator foundation; no production caller, coupled S2.2/S2.3 acceptance open. Shared S2.1 extraction recovered separately. |
| `p0-pairwindow` | Recovered in the bound-report, bundle and table integration (ea1ef248); current protocol and UI retained. |
| `p0-pinned` | Recovered in the bound-report, bundle and table integration (ea1ef248); current protocol and UI retained. |
| `p0-report` | Recovered in the bound-report, bundle and table integration (ea1ef248); current protocol and UI retained. |
| `p0-s07b` | Covered by newer main/recovered source and golden journey integration; stale generated metadata was not copied. |
| `p0-s101` | Covered by newer main/recovered source and golden journey integration; stale generated metadata was not copied. |
| `p0-s105` | Covered by newer main/recovered source and golden journey integration; stale generated metadata was not copied. |
| `p0-s105b` | Accessibility source and dirty work recovered; main defect handling retained. Debug keyboard probe remains historical. |
| `p0-s25` | Superseded by the recovered reviewed m-line; no unique behavior retained only on this older implementation. |
| `p0-s25m` | Recovered in the reviewed import/density line (0ee8ba55), including coupled dirty figure work. |
| `p0-s37` | Real backend source and dirty work recovered; typed identity, provenance and resource-lifecycle gaps repaired. S3.7 remains open. |
| `p0-s37-s5` | Real backend source and dirty work recovered; typed identity, provenance and resource-lifecycle gaps repaired. S3.7 remains open. |
| `p0-s37-s5-prerebase` | Historical pre-rebase line; current real-backend work is p0-s37-s5. |
| `p0-s46` | Old S4.6 line replaced by p0-s46-r. |
| `p0-s46-r` | Semantic scene descriptions recovered. |
| `p0-s54f` | Superseded by the recovered reviewed m-line; no unique behavior retained only on this older implementation. |
| `p0-s54fm` | Recovered in the reviewed import/density line (0ee8ba55), including coupled dirty figure work. |
| `p0-s55f` | Superseded by the recovered reviewed m-line; no unique behavior retained only on this older implementation. |
| `p0-s55fm` | Recovered in the reviewed import/density line (0ee8ba55), including coupled dirty figure work. |
| `p0-s55p` | Superseded by the recovered reviewed m-line; no unique behavior retained only on this older implementation. |
| `p0-s55pm` | Recovered in the reviewed import/density line (0ee8ba55), including coupled dirty figure work. |
| `p0-s56d` | Unique density integration recovered through 22939f90 and 8691f0a7; newer m-line retained. |
| `p0-s56dm` | Recovered in the reviewed import/density line (0ee8ba55), including coupled dirty figure work. |
| `p0-s62-slice1` | Both patches already on main (git cherry: +0 -2). |
| `p0-s64e` | Source-record table superseded by main cc956e9c and later protocol integration. |
| `p0-s65` | Old protocol 1.7 implementation; main carries reviewed 1.7 and later 1.9/1.10 integration. |
| `p0-s65e` | Alias of old p0-s64e source-table tip. |
| `p0-s66` | Original linked-selection implementation replaced by main bc93ba2b and later review fixes. |
| `p0-s71` | Recovered preset and unmatched-policy line plus dirty fixes; explicit expected-unmatched selector remains an open separate ticket. |
| `p0-s85` | Recovered in the bound-report, bundle and table integration (ea1ef248); current protocol and UI retained. |
| `p0-tablerole` | Old table-role patch replaced by p0-tablerole-r/p0-pinned. |
| `p0-tablerole-r` | Recovered in the bound-report, bundle and table integration (ea1ef248); current protocol and UI retained. |
| `p0-unmatched` | Recovered preset and unmatched-policy line plus dirty fixes; explicit expected-unmatched selector remains an open separate ticket. |
| `worktree-agent-ae3b7633183de2c2a` | Archive: obsolete replacement API. Sound first-entry and membership-run properties recovered additively into shipped AoiLaws.accounting and mutation-tested. |

## Historical archives

The ledger foundation is preserved at `archive/20261005/ledger-replay-foundation`
(original `34e506ecc9143f619f21e59134d491a5f76c9f2f`). This includes the
`codex-ledger-bounded` ancestry. The old AOI replacement is preserved at
`archive/20261005/aoi-law-design` (original `8c317c292ff647ee9688d4bd67ce514cb0622673`).
Archive marker commits retain the original tree and parent exactly and skip CI;
they do not claim a qualified build or successful landing.

## Dirty work

The seven source worktrees `p0-figc`, `p0-ledger-s21`, `p0-pairrows`, `p0-s105b`,
`p0-s37`, `p0-s71` and `p0-s84` were recovered with integration fixes. Original
worktree heads, index patches and 457 dirty-file hashes were captured before editing.
Original bytes are retained; debug probes and stale audit outputs remain historical.

## Boundaries retained

- S3.7 is partial: native density, pair windows and ungrouped reports bind retained
  library values. Grouped reports refuse without persisted contrast direction.
  Legacy summaries and remaining preview/query/trial APIs still need adapters.
- S2.1 shared interpretation is recovered. The unfinished ledger replay coordinator
  remains archived pending coupled S2.2/S2.3 implementation and acceptance.
- AOI entry/run laws extend the shipped sample-time API; the obsolete replacement
  and broader entity-trace design are not merged. Both production mutants fail
  on JVM and Scala.js.
- Accessibility K2–K4 recovery does not close the recorded native focus/menu limits.
- The explicit expected-unmatched selector remains open; preset recovery does not
  certify item-collision behavior that needs that selector.
- Performance tooling recovery makes no full-round performance superiority claim.

## Tracker portability

Mote's authoritative state is the versioned `.mote/FORMAT.json` and append-only
`.mote/ops/` history. Recovered historical operations are included after byte-collision
checks. Cloning the pushed repository and installing Mote carries tickets and decision
notes to another computer. `.mote/local/` is machine-local identity/cache state.
Mote tickets are repository data; they are not GitHub Issues.

## Local landing evidence

Library and studio source were frozen at `d3beca9c5b2c6a1f843dc56a4fb0b04fe3575ef8`
(tree `cc443b09ea5381ebc97ecdbf86d4e8b728906a11`). The header, format, generated
workflow and JVM/Scala.js `-Werror` checks passed. The recorded API audit covers
3,650 runtime entries and 540 abstractions, with zero uncovered entries. `testAll`
passed 6,396 tests; `checkBoundaries` passed. Generated inventory, evidence and
provenance were committed together; their fingerprint still matches the library
sources, tests and build after the consumer corrections.

Studio core, app and visualization tests passed on both platforms. The final
visualization runs passed 228 tests per platform, and the complete headless desktop
run passed 559 tests. JavaScript linking, studio style and studio boundaries passed.
The combined studio evidence has 3,590 passes and two existing ignored app tests.
Integration repairs include exact report-reply correlation, served-reference Explain
routes, truthful weighting labels, actual density raster/contour export, pending-table
focus behavior, rejection of overtaken dock-focus events, and served-control readiness
before the accessibility Tab-order audit.

The standard packaged-consumer gate passed at
`ffa670fcca6931159c6ecd3b6c84286f18c286b5`
(tree `a3c87d908b6e29718108bc4725d04708cec15d44`): 206 JVM and 195 Scala.js tests,
26 packaged artifacts, packaged-source identity checks, independent fixture oracles,
fresh-process reloads and archive reruns. Consumer assertions now retain the S0.7b
owner decision excluding controls for unmatched queries. Publication runs each
module separately and uses `TieredStopAtLevel=1` for its JVM: default JDK 25.0.1
optimization failed inside Scaladoc's signature builder even with sequential tasks,
while unchanged Plan documentation regenerated successfully under C1. Consumer
execution retains its normal JVM settings; documentation and tests remain enabled.

These are local qualification results. They do not establish hosted CI, full-round
performance superiority, or closure of the native/backend limitations above.
