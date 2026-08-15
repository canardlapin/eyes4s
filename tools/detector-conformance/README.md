# Detector conformance fixtures

This directory contains executable, pinned evidence for eyes4s' paper-named
I-VT and I-DT detectors. The oracle is pymovements 0.26.2 at commit
`6753fdf8b81da40b890576dd6b369edb81243b06` (MIT), not code copied from the
eyes4s implementations.

Generate the fixture from a clean environment with:

```sh
uv run tools/detector-conformance/generate_reference.py
```

The script prints deterministic JSON to stdout. Its check mode compares the
parsed checked-in artifact with freshly generated oracle output:

```sh
uv run tools/detector-conformance/generate_reference.py --check
```

Updating the fixture requires reviewing both the pinned oracle revision and
the scientific convention mapping.

## Conventions and deliberate differences

pymovements reports the timestamp of the last classified sample as `offset`
and computes `duration = offset - onset`. eyes4s intervals are half-open, so a
regular one-millisecond fixture ending on timestamp 5 has support `[onset, 6)`.
The JSON retains both forms rather than silently rewriting the oracle.

The `idt-violating-sample-policy` fixture is deliberately non-zero. In the
pinned pymovements implementation, the point that first exceeds the dispersion
threshold is included in the completed fixation. eyes4s excludes it and begins
the next candidate there, matching its named
`ViolatingSampleStartsNextCandidate` deviation. The checked error vector makes
that difference auditable instead of calling the two outputs identical.

Engbert-Kliegl uses a separate independent oracle under
`tools/engbert-kernels/`; the Scala conformance court combines its physical
velocity evidence with these event-boundary fixtures on JVM and JavaScript.
