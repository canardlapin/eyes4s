# Reading one archived density

`DensityArchiveCodec` writes a completed fixation study under `eyes4s.study-result`.
Its default, `DensityStorage.Recomputable`, stores the result metadata and a
SHA-256 identity for each successful map. `Inline` retains the version-1 wire
representation. `Packed` stores float64 little-endian arrays in one verified
chunk per participant, across every scale. It never uses float32.

Construct `DensityArchiveCodec` with the result codec, such as
`StudyResultCodecs.cosine[Px]`. Its `encode(completedResult)` returns an `Either`;
a successful value contains the bundle. Encode `bundle.archive` with the archive
codec to obtain the saved document.
The [executed archive examples](../codec/src/test/scala/eyes4s/codec/DensityArchiveSuite.scala)
include complete fixtures for encoding, opening and reading each storage mode.

Opening a document returns `StudyResultArchive`, a parsed archive candidate.
Opening and constructing its `DensityReader` call no payload provider and do
not estimate any maps. A candidate is not a validated `StudyResult`: call
`materialize` to read every map and apply the completed-result reconstruction
checks, including the pair rows, reductions and contrasts.

Decode the saved document with the archive codec. For a recomputable archive,
construct its provider with `recomputer(savedPlan, savedInput, archive)`, pass it
as `Some(recompute)` to `archive.densityReader`, then request
`density(scaleIndex, trialKey)`. The linked examples check the selected map's
cells against the completed result and verify that opening the archive invokes
no provider. Applications can retain `CodecError` and `DensityError` in their
own typed error sum, or render their `message` values.

A read addresses one scale and full trial key. Repeated full keys produce
`AmbiguousKey`; a stored failed estimate produces `Failed`. Neither silently
selects an occurrence. Recomputing a map reuses the plan's actual initial-fixation,
window, weighting, smoother and bandwidth rules. It does not construct pair
schedules or evaluate comparisons. The saved description and supplied input
reference must match, and the result's per-map SHA-256 must match exactly.
A mismatch reports the scale, key and expected and actual digests. This does not
promise bit-for-bit recomputation between arbitrary numerical implementations;
a numerical difference is a refusal, never an accepted substitute.

For packed archives, supply `PayloadRef => Option[VerifiedPayload]` to
`densityReader` or `materialize`. The provider verifies the complete chunk's
SHA-256 and length with the existing payload API. The reader checks the requested
reference and decodes only the selected row from shape `[maps, ny, nx]`.
The provider owns file access and any verified-chunk cache; the codec is pure.
Missing payloads and substituted references are located failures.

`DensityDigest` wraps `CanonicalDigest[DensityView[U]]`. Its versioned canonical
map document records geometry first, including the complete saved plan
context, then provenance and row-major float64 little-endian cells. Storage
kind and chunk location are excluded. The same map has the same identity in
all three modes. The pinned `study-result-v2.json` fixture records a recomputable
archive; `study-result-v1.json` remains unchanged. The schema ladder reads both
and lifts the old inline representation without estimating maps.

Each `DensityView` exposes checked `geometry`: frame and bounds, axis direction,
grid dimensions, native-unit cell extents, admission-frame origin, and explicit
`RowMajorXFastest` order. Angular cell extents exist only when the saved study
supplied a linear angular scale; they are never inferred from pixels.
`view.levels(Vector(0.5, 0.9))` computes highest-density-region thresholds when
requested. Every exact tie at the selected threshold is included. Requested
order and duplicates are preserved, and a mass whose accepted total is below
the requested coverage returns an error instead of being renormalized.
Marching squares and drawing remain the renderer's responsibility.

This archive API covers the fixation-study family. Temporal result storage and
manifest integration are sequenced separately; direct archive construction
accepts an explicitly supplied payload provider.

Recomputation callbacks receive `(scale, inputRowIndex, key)`. Materialization preserves every row occurrence, including distinct maps sharing a full key. The built-in recomputer checks the saved input identity, row bounds and row key before estimating that one map. `density(scale, key)` still reports `AmbiguousKey` for repeated full keys.
