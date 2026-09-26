# Executable and rendered documentation qualification

Base commit: `131970d78fd0a7d18ed72e4b8ccf1d2a5a136279`, with the reviewed working-tree changes.
This is local candidate evidence, not a committed release or hosted CI run. No site was deployed.

The maintained inventory is `tools/doc-examples.json`; its execution contract is
[EXAMPLE_COVERAGE](../EXAMPLE_COVERAGE.md). `docs/tlSite` executes the site examples and renders
only the curated public pages. Five external fences are emitted verbatim into
`docconsumer.DocumentationExamplesSuite`, whose five successful tests also run both documented
CLI programs. All linked source programs have successful named test evidence. The isolated
consumer passed 192 JVM and 181 Scala.js tests against freshly published local artifacts; its
exact source hashes, classpaths and numerical receipts are preserved in
[consumer-receipt.json](documentation-2026-09-19/consumer-receipt.json).

The review repaired an uncompilable README `Length.mm` example, stale migration/landing-page
claims, and consumer progress assertions that omitted the newly bounded assembly phases.
The phase test still checks the original exact totals, requires all seven assembly phases,
checks positive increments no greater than the work quantum, and compares the completed result
with the pure analysis. No numerical tolerance was widened.

Successful commands and local logs:

- `sbt ioJVM/testOnly docconsumer.DocumentationExamplesSuite docs/tlSite`:
  `/tmp/eyes4s-doc-build2.log` (five tests; mdoc has zero errors).
- Final navigation/reference rebuild: `/tmp/eyes4s-doc-build3.log`.
- `python3 tools/check-docs.py --run-consumer`: `/tmp/eyes4s-doc-check4.log`.
- Added-unverified-fence and renamed-test-mapping mutants both failed the intended checks;
  originals were restored. [Mutation receipt](documentation-2026-09-19/mutations.json).

An owned Playwright headless Chromium reviewed every page at 1440×1000 and 390×844, exercised
mobile navigation, and checked local link destinations, page errors and page overflow. Code
wraps within its panel on mobile. A long inline call was shortened after visual review.
The local HTTP server, browser and contexts were closed; the browser guard reported no automated
browser processes before and after the run. The [render receipt](documentation-2026-09-19/receipt.json)
and [desktop export](documentation-2026-09-19/1440-exports.html.png),
[mobile template](documentation-2026-09-19/390-code-templates.html.png), and
[mobile export](documentation-2026-09-19/390-code-exports.html.png) screenshots preserve the review.
External repository links describe the source paths; this local review does not claim unpublished
working-tree changes are already present at their hosted destinations.
