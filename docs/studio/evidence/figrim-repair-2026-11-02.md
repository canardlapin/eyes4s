# FIGRIM repair verification — 2026-11-02

## Implemented scope

1. New timed projects can create the reporting specification from Results;
   the operation is saved and undoable. Report is reachable through the normal workflow.
2. Spatial FIGRIM bundles use the main `.eyes` document, import window, analysis
   history, executor, Results, Report and export. The five source files retain
   their byte and semantic identities. Timing is neither required nor fabricated.
3. Saved native results restore from their archive. Passive result reads never
   submit a new analysis; unavailable archives require an explicit rerun.
   Spatial restore retention is bounded, and unstored results are preserved.
   Oversized timed archives are refused by an explicit capacity check.
4. Unmapped optional display columns no longer require a stimulus registry.
   Declared display mappings still receive their normal validation.

Spatial support currently implements the FIGRIM profile: 1000 × 1000 pixels,
10 × 10 uniform-count bins, cosine comparisons of supplied focal/matched/control
triples, and an equal-query descriptive mean. Observer slots remain source labels;
no timing, person identities, standard errors or confidence intervals are inferred.
This witness does not validate the earlier timing-based, 6,116,044-pair recipe.

Related regression coverage includes zero-pair preflight refusal, blank response
preservation, exact source-record selection, bounded missing-trial notices,
spatial command undo/redo, and journal compatibility. Exact undo preserves legacy
family representation and original revision identities, including gaps.

## Executed checks

All runs used JDK 25 and `GITHUB_ACTIONS=true` for strict compilation.

| Check | Result |
|---|---|
| Studio core full suites | JVM 1,180 passed; JavaScript 1,087 passed |
| Studio app full suites | 829 passed on each platform |
| Studio viz full suites | 326 passed on each platform |
| Desktop broad sweep | 902 passed; three failures corrected and their suites rerun successfully; three ignored |
| Desktop focused reruns | AppShell and Keymap passed; DataFirstRun 7 passed |
| Final spatial import and main-window journey | 3 passed, including saved Results/Report and cold reopen |
| Spatial plan and report | 10 plan and 4 report tests passed on each platform |
| Import, CSV, export and diagnostics | JVM 40 passed; JavaScript 35 passed |
| Spatial codecs | 6 passed on each platform |
| Codec laws and schema registry | JVM 16 passed; JavaScript 6 passed |
| Root JVM/JS test compilation | Passed with warnings as errors |
| JavaScript application link | Passed |
| Studio style, module boundaries, headers, formatting and workflow checks | Passed |
| Compiler inventory preparation | Passed; diagnostic coverage used the current source fingerprint |

The bundled example explicitly computes and stores its initial archive before
installation. It took 32–52 seconds during focused checks. The first-copy test now
allows bounded native I/O time; ordinary UI transition waits remain 20 seconds.
The final copy/close/reopen journey passed. This is not a startup performance claim.

## Full-data native witness

`SpatialMemoryCourt` wrote a real `FileProjectStore` / `ProjectSession` project and
read it in a fresh JVM with `-Xmx950m` (maximum heap 998,244,352 bytes).
The final cold read used the repaired retention and archive-loading code.

- 30,012 trial maps; 182,532 admitted locations.
- 19,718 explicit comparison triples; 142,010 occupied bins.
- Zero backend jobs started and zero external source-port reads on reopen.
- Final cold restore plus export completed in 15,721 ms.
- Saved and restored `scores.tsv`, `bins.tsv` and `location-run.json` were byte-identical.
- Independent NumPy 2.4.3 verification passed: exact bins, maximum score error
  5.551115123125783e-16 against an absolute tolerance of 1e-12.

The full dataset was exercised through native persistence and the backend. The
headless window journey used a small asymmetric fixture. These are separate proofs.

Output SHA-256 values:

```text
scores.tsv        2c8456dc1fcd6c03c5d9d2f049b6fc1146ef1ce614a46b6c83c565283d3ba5b8
bins.tsv          ff9ce935c8a09bc04c1fd2ec388b726aa4d73a62f9de5bac132664e5566e3e28
location-run.json cec7c8c7ecf6c2541b7779ddbc79b9cf227cc4b61e2bdb26cf765c48aceeb49b
```

## Evidence and remaining limits

Local logs are preserved in the primary checkout under
`.mote/artifacts/studio-real-data-repair-20261101/`. The checkpoint includes the
changed files, patch, hashes, independent verifier and verification receipts.
Principal log names:

- `eyes4s-spatial-app-all-02.log` — app/viz and broad desktop sweep.
- `eyes4s-spatial-core-desktop-final-01.log` — full core suites and pane/keyboard fixes.
- `eyes4s-spatial-example-final-03.log` — example lifecycle, style/boundaries and final full-data cold read.
- `eyes4s-spatial-strict-final-01.log` — strict compilation, diagnostics, codecs/laws and final spatial UI journey.
- `eyes4s-spatial-library-final-01.log` — spatial plan and report tests.
- `eyes4s-spatial-api-prepare-final-01.log` — compiler inventory preparation.

Live native computer-use testing remains unavailable on this machine's macOS 14.3;
the current helper requires 14.4 or later. No installed application was replaced.
No full release/landing gate, hosted CI, merge or push is claimed by this receipt.
