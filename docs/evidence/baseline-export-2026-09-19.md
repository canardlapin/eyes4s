# Baseline export evidence, 2026-09-19

The implementation is an uncommitted overlay on base commit
`131970d78fd0a7d18ed72e4b8ccf1d2a5a136279`; this is local evidence, not a published candidate or
hosted CI run. Source and schema files are recorded by the per-ticket parity manifest.

- The compiled `BaselineExportGuide` exercises every finite family in `ResultFamily` on JVM and
  Scala.js; its scientific assertions use absolute `NumericTolerance = 1e-12`.
- `BaselineExportMain` emitted 22 CSV/JSON-metadata/Arrow triples and an index under
  `/tmp/eyes4s-baseline-exports`. `tools/result-export/receipt-v1.json` records every emitted file's
  byte length and SHA-256. `schema-v1.json` pins ordered field types, units and nullability.
- Base R independently verified scalar scores, named component order, counts, missing values,
  quoted Unicode identities, exact 64-bit times, native coefficients and held-out predictions.
- PyArrow 19.0.1 (CPython 3.12.11) independently read Arrow Java 19.0.0 IPC streams and checked every
  cell against CSV, including nulls, Int64 widths, field units/labels and embedded metadata. It
  independently recomputed the table SHA-256 with UTF-16 length framing and UTF-8 bytes.
- Changing the first scalar score from 2 to 99 in a separate artifact copy caused the base-R
  analytic check and Python content-identity check to fail. Failed copies/logs remain under
  `/tmp/eyes4s-export-corruption-probe`, `/tmp/eyes4s-export-r-negative.log`, and
  `/tmp/eyes4s-export-arrow-negative.log`; valid artifacts were not changed.
- Arrow tests cover successful multi-batch and empty streams, exact large integers, nullability,
  invalid budgets, failed writes, stream closure and zero retained allocator bytes. Writer
  shutdown can itself fail; a separate stream finalizer closes the output regardless.
- All example table identities except learned coefficient/prediction outputs agree on JVM/JS.
  Those two retain a one-ulp coefficient difference (`2.1213203435596424` versus
  `2.121320343559642`); no forced rounding hides it. Learned input identities agree after fixing
  the use of formatted grid descriptions. The pinned learned recipe reopens on both platforms.
- The full gate exposed unregistered repetition/point fixtures. Their conventional schemas now
  have registry entries, real decoder routes and published `CodecLaws` checks. Changed-control
  and dropped-query codec mutants are rejected; the registry guard was not relaxed.

Focused logs: `/tmp/eyes4s-export-focused-final.log`, `/tmp/eyes4s-added-codec-laws.log`.
The complete repository gate and offline regeneration are recorded in the Mote completion note
only after they finish successfully. No remote deployment or release qualification is claimed.
