# Executable documentation inventory

`tools/doc-examples.json` classifies every Markdown page under `docs/` and `site-docs/`, plus
README. Current public guides are checked even when they are outside the rendered site; internal
plans and historical evidence are explicitly classified. New pages require review.

- Site Scala fences execute through mdoc. The checker refuses compile-only fences and requires
  current generated HTML. Assertions pin numerical results and refused inputs.
- README and ASC import fences are copied verbatim by `tools/documentation-examples.py` into a
  generated external-package MUnit suite. It executes checked geometry, finite control design,
  byte admission, temporary-file import and both documented CLI programs.
- Linked Scala examples map to exact successful test-case names on each declared platform.
  The checker rejects missing, renamed, failed, skipped or stale test evidence. The human-reviewed
  mapping identifies the caller; it is not a claim of statement or public-API coverage.
- Extension examples execute in the existing isolated artifact consumer. Its verifier checks
  packaged source identity, fresh-process reconstruction and independent numerical targets on JVM
  and Scala.js. Cached evidence is tied to the exact library/consumer source fingerprint.

Run `sbt testAll docs/tlSite`, then `python3 tools/check-docs.py --run-consumer`. The consumer is
built under `target/study-consumer`. `python3 tools/check-docs.py` rechecks existing matching
evidence without rerunning the isolated consumer. It refuses a changed candidate, including a
changed oracle fixture. Per push, each CI matrix project checks only its own reports
(`--platform jvm` or `--platform js`, with `--skip-consumer`); the scheduled `evidence.yml`
workflow rebuilds and checks the consumer.

Incomplete fragments with undefined setup were replaced by links to complete executed programs.
The schema declaration pattern and old quadratic assembly excerpt are explicitly illustrative or
historical text. They are not executable usage promises. API invocation coverage belongs to
`v-coverage`; symbol documentation belongs to `doc-scaladoc`.
