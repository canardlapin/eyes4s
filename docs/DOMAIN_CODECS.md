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
| Optional byte checksum | Integrity of specified bytes, to be carried by S5's manifest |

Schema or method IDs are not content hashes. `ContentHash` is not a cryptographic
checksum, and JSON formatting changes do not redefine it. Byte verification is
a separate effectful resolver concern, not an effect hidden inside decoding.

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
