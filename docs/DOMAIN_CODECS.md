# Scientific domain codecs

`DomainCodecs` in `eyes4s-codec` supplies conditional `VersionedCodec` instances
for frames, grids, spatial unit witnesses, clocks, instants, spans, half-open
intervals/windows, observed coverage, physical perspectives, affine
synchronization and named synchronization marks. Supply an explicit
`DefinitionId` for each schema. Decoding uses the existing domain smart
constructors and returns `CodecError` with the offending field and operands.

Existing study, recording and temporal plan schemas keep their version-one wire
meanings. Their frame, time, perspective and mark helpers now use this foundation.
These codecs describe scientific values; they do not load files or implement an
application project format. Observed synchronization marks travel with the
recording input payload (see [saved studies](SAVED_STUDIES.md)); fitted
synchronization diagnostics belong to the completed-result archive work in S4.
`TimelineCodecs` supplies the conditional `Timeline[A]`, `PlannedTimeline[A]`
and `ObservedTimeline[A]` codecs given a codec for the mark values.

## Numeric policy

Every `Instant` and `Span` uses a decimal **string** containing signed 64-bit
microseconds. Numeric JSON time values, fractional strings, exponent strings and
out-of-range integers fail. This preserves values beyond JavaScript's exact
number range, including both `Long` extrema. Interval ordering is checked by
`Interval.of`; no implicit seconds-to-microseconds conversion occurs.

Geometry, physical dimensions and drift use finite JSON numbers. Nonfinite
values, including numbers that overflow a Double during decoding, fail.
The domain constructors retain responsibility for positive dimensions, grid
size overflow, interval ordering, coverage gaps and synchronization scale.
Floating-point values retain their numeric meaning; whitespace, alternate
numeric spellings and signed-zero spelling do not promise identical bytes.

## Document identity

Build an immutable `DocumentIdentities.empty` table with `addFrame`, `addGrid`
and `addClock`, or decode it with `DomainCodecs.identities(schema)`. Adding a grid
registers its frame. The wire table has `frames`, `grids` and `clocks` arrays;
grid frame fields reference a registered nominal frame ID.

Resolve values with `frame[Px](FrameId(...))`, `grid[Px](GridId(...))` and
`clock(ClockId(...))`. The table supports all four built-in spatial units in one
document without casts. Typed resolution checks the unit. A missing reference
fails; it never invents a frame or clock. Standalone value codecs carry full
values; containing payload codecs must use these lookups to resolve references.

Repeated declarations with the same nominal ID and specification coalesce.
Conflicting frame geometry/axis or grid specifications fail through `Agreement`;
a reused frame ID with another unit also fails. Distinct IDs remain distinct even
with identical geometry. Clock identity is its full `ClockId.name`; the domain
has no separate clock display-label or clock-specification field. Applications
must preserve distinct clock IDs when their display labels happen to match.
Sharing here means nominal equality, independent of JVM object reference identity.
There is no global interning, mutable registry or cross-document identity state.

## Four separate identities

| Identity | Meaning |
|---|---|
| Schema `DefinitionId` | The versioned wire shape and interpretation |
| Method `DefinitionId` | The scientific implementation and parameter meaning |
| `ContentHash` / `ArtifactRef` | Existing semantic input identity; 16-hex representation remains unchanged |
| Byte digest (`ByteDigest`) | SHA-256 of an artifact's exact bytes, carried with its length by each `eyes4s.manifest@1` entry |

Schema or method IDs are not content hashes. `ContentHash` is not a cryptographic
checksum, and JSON formatting changes do not redefine it. Byte verification
belongs to the manifest resolver, which reads through a source the application
injects; decoding never reads. See
[artifact manifests](SAVED_STUDIES.md#artifact-manifests-and-verified-resolution).

## Schema compatibility

Every stored document is an envelope, `{"schema": {"name", "version"}, "value"}`. The version is
one positive integer; there are no minor versions. The rules below are enforced on the JVM and
Scala.js by `SchemaCompatibilitySuite` over a pinned v1 document of every shipped document schema:
the sixteen JSON codecs (study plan, study input, admission ledger, study result, recording,
binocular recording, recording input, temporal input, timeline, manifest, the four score and
difference schemas, and the conventional `eyes4s.recording-plan@1` and `eyes4s.temporal-study@1`
plan schemas), with a test that fails if a pinned schema is missing, and the packed recording,
whose decoder also takes its payloads. The schema registry described under
[Evidence](#evidence) checks the same fixtures on the JVM.

**What a new version means.** A schema's version fixes the meaning of every document it admits,
so a change that could make an existing reader decode a document to a meaning its writer did not
intend is a new version: adding, removing, renaming or retyping an object member; changing how a
member is interpreted, its unit, or its numeric or time encoding; changing how a semantic identity
(`ContentHash`) is derived. A new version is a new `DefinitionId` with its own decoder beside the
old one. Two kinds of addition extend a version instead, because no existing reader can misread
them: a new value of an enumerated field (an artifact role, a relation kind, an estimator, a sample
state), which an older reader refuses with a `CodecError.Field` naming the member and carrying the
value it does not know (inside a `CodecError.Entry` giving its path wherever the codec locates
entries, as the manifest and sample columns do); and a new registration (a
method, a key layout, a score or difference schema), which is its own identity with its own
version and never changes the schema that carries it. A pinned fixture must decode on the current
code and re-encode to the same JSON value on both platforms (and the same bytes on the JVM), so a
change to a pinned v1 writer's output fails the build rather than silently becoming a second v1.

**Which decoders stay readable.** Every released version stays decodable, with its original
meaning, in every later release. Version 1 is the first version of every shipped schema, so every
current decoder reads exactly version 1; there is no version 0 or earlier variant to keep or
migrate from, and `DefinitionId` refuses a version below 1. Decoding never migrates. A migration
from one version to the next will be offered only where it is scientifically lossless, preserving
every identity-bearing value and semantic identity, and then as an explicit function the caller
applies, never inside a decoder; otherwise the older version is refused with a named
incompatibility.

**Unknown versions** are refused as values naming both identities:

| Where the version appears | Refusal |
|---|---|
| A document's envelope | `CodecError.Schema(expected, found)` |
| Version 0, a negative or a non-integer version | `CodecError.Definition(PlanError.InvalidDefinition(name, version))`, or `CodecError.Field("version", …)` |
| A nested identity a codec requires: layout, key, parameter, score or difference schema | `CodecError.Schema(expected, found)` |
| An identity a registry selects on: a plan's method, an input's key schema, a result's method | `CodecError.MissingMethod`, `MissingKeySchema` or `MissingResultCodec` |
| A standalone recording's envelope | `CodecError.UnsupportedSchema("recording", found, supported)` |
| A manifest | `ResolveError.ManifestDecode(address, CodecError.Schema(…))` |
| An artifact its manifest declares at that version | `ResolveError.Decode(entry, CodecError.Schema(…))` |
| An artifact whose bytes hold another schema than declared | `ResolveError.Schema(entry, declared, found)`, before its decoder runs |

**Unknown members** of an object are ignored at every level and are not preserved: re-encoding
writes the version's own members only. The first rule makes this safe. A member that carries
meaning cannot be added without a new version, so an unknown member in a v1 document is either
another tool's annotation, which eyes4s neither interprets nor round-trips (an application keeps
its own metadata in its own files), or the output of a writer that broke the rule. It cannot pass
verification unnoticed: an artifact with an added member is other bytes, which the manifest that
listed the original refuses by its length or, when the length happens to match, by its SHA-256
(both cases are tested); a manifest written over the extended bytes
admits it with the semantic identity re-derived from its version-1 values, and a manifest document
with an added member has another address. Three members are refused where they would be misread,
because they have a version-1 meaning elsewhere: any member of an `eyes4s.unit@1` payload, which
is exactly `{}` whether it is a parameterless method's parameters or a trial's metadata; a `timing`
member on a neutral `eyes4s.timeline@1`; and a declared dispersion value on a source-supported
fixation, whose value is derived from its samples.

## Evidence

The generated-value tests apply published `CodecLaws` to these codecs on JVM and
Scala.js. Negative tests cover invalid geometry, time, coverage, conflicting IDs
and unresolved references. Frame-ID, y-axis and microsecond-unit mutants must
fail the roundtrip laws. The frozen study-v1 and recording-v1 fixtures separately
pin decoded scientific meaning, preventing a mutually wrong encoder/decoder
from passing solely by agreeing with each other. The recording, binocular,
source-supported study and temporal input fixtures do the same for the input
payloads. Their published laws are shown to discriminate dropped-sample,
swapped-clock, dropped-mark and moved-anchor mutants of the decoded value, and
the payload-editing suites show that the decoders themselves refuse the same
changes through the declared digests.

Every built-in identity has a round-trip law and a pinned fixture, and the build checks it.
`SchemaRegistryJvmSuite` (in `eyes4s-laws`, JVM) lists each `DefinitionId` the library ships, what
it names (a document envelope, a nested schema, a method or layout, or a payload), the pinned v1
fixtures that carry it and the published laws that exercise it. Reflection over the `DefinitionId`
companion requires exactly one entry per built-in identity; instantiating each named law suite
requires the named round-trip law to be registered there; and every fixture must exist, carry the
identity and, for a document, decode and re-encode to itself through the shipped codec. Every file
under `codec/src/test/resources/eyes4s` must be claimed by an entry, so an unclaimed or undecodable
fixture fails as well. The shipped codecs whose schema the caller supplies are registered too,
under the conventional identities their pinned fixtures use: `eyes4s.recording-plan@1`
(recording-v1.json) and `eyes4s.temporal-study@1` (temporal-study-v1.json, the temporal fixture's
plan). A test shows each check failing: a missing or duplicated entry, a missing or misnamed law, a
missing fixture, a fixture that does not carry its identity, a fixture of another schema, a
fixture that decodes but does not round-trip, an unclaimed resource file and a registered fixture
that does not exist. A new built-in schema without a law and a fixture therefore fails
`lawsJVM/test`.

UI-S6 added the laws the registry found missing: `PlanCodecLawSuite` for the study plan
(`eyes4s.study@1`, with the `cosine@1`, layout and `unit@1` identities inside it) and for the
temporal and I-VT, I-DT and Engbert-Kliegl recording plan codecs; `ArtifactCodecLawSuite` for the
manifest codec, the packed recording and packed arrays through the published
`eyes4s.laws.PayloadLaws`, and the four score and difference schemas compared by exact IEEE bits.
Each suite kills deliberate mutants (a dropped scale or relation, swapped phases or identities, a
reset failure policy, a dropped window, mark or sample, a moved threshold, a lost signed zero,
single-precision rounding, a payload filled in from elsewhere) by a falsified property from a fixed
seed, and requires every property of the shipped codec to pass outright rather than merely not
fail. No generator in these suites or in `PayloadLaws` discards a value, so no property can pass
by exhaustion. It also pinned the built-in schemas no fixture had carried: `timeline-v1.json` (a timeline
of study keys, with equal instants beyond 2^53 kept in order), `score-codecs-v1.json` (one
envelope of each score and difference schema, the only fixture of `measure-distance@1` and
`scalar@1`), and the standalone, binocular and packed recordings with their payloads listed under
[artifact manifests](SAVED_STUDIES.md#artifact-manifests-and-verified-resolution).
