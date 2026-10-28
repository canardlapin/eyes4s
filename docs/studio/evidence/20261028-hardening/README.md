# Archive and timeline hardening, 2026-10-28

Implementation and targeted regression evidence; the combined release gate is pending.
The source candidate and final qualification will be recorded separately.

The timeline uses the declared origin/end when a trial extent is served. Otherwise
its viewport and playback cover the first observed onset through the last observed
fixation end. Raw timestamps, plot/table source values and half-open brushes stay
unchanged. Restart returns to the first observed onset. This is observed coverage,
not event-relative timing or an inferred trial duration.

[The synthetic native screenshot](observed-coverage.png) uses the real CSV backend
and production Intaglio timeline at onset 600000 ms. It was reviewed for the visible
playhead, coverage warning, inspector and source/table agreement. The declared
5000 ms tail has separate native tests. These are Monocle captures, not physical
Retina acceptance. Unknown-extent board examples can now begin after zero; this
intentional deviation avoids implying an unobserved interval.

The density archive prepares the canonical geometry/selection-context SHA prefix
once per exact grid geometry, then forks independent continuations for provenance
and cells. Storage versions and digests remain unchanged. Portable regressions cover
padding boundaries, independent finish/reset, signed-zero geometry and the pinned
legacy map digest. The [benchmark](density-prefix-benchmark.json) measures hashing
only; end-to-end archive time is a separate measurement.

The native writer checks the exact retained plan/input/ledger bytes plus packed
cells as a lower bound before result hashing and packing. It retains the existing
row, cell, entry and byte limits. A whole-cohort single archive is still unsupported
under those limits; no compression ratio or hash speedup qualifies larger capacity.

The owner deferred manual VoiceOver acceptance beyond v1 on 2026-10-28
(`a11y-voiceover-post-v1`). Keyboard, contrast, focus, roles and table twins remain
v1 requirements. Physical Retina, installed/Finder/clean-no-JDK/Gatekeeper and
researcher usability acceptance remain unqualified.
