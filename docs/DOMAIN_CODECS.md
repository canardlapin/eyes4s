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
`EpochCodecs.plan(schema, keyCodec)` similarly persists the selector, checked bin width
and final-bin policy of an [epoch plan](EPOCH_PLANS.md), under explicit caller-supplied identities.

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

## Separate identities

| Identity | Meaning | Scope |
|---|---|---|
| Schema `DefinitionId` | The versioned wire shape and interpretation | Wire |
| Method `DefinitionId` | The scientific implementation and parameter meaning | Wire |
| `CanonicalDigest[A]` | SHA-256 of a value's canonical document under its codec (`codec.digest(value)`), typed by what it identifies | Persisted and cited across files and runs |
| Byte digest (`ByteDigest`) | SHA-256 of an artifact's exact bytes, carried with its length by each `eyes4s.manifest@1` entry | Persisted: verifies stored bytes |
| `ContentHash` / `ArtifactRef` | 64-bit FNV-1a semantic identity; 16-hex representation unchanged | In memory, and the cross-check version-1 documents already carry |

Schema or method IDs are not content hashes. `ContentHash` is not a cryptographic
checksum, and JSON formatting changes do not redefine it. Byte verification
belongs to the manifest resolver, which reads through a source the application
injects; decoding never reads. See
[artifact manifests](SAVED_STUDIES.md#artifact-manifests-and-verified-resolution).

**Which identity to persist.** Anything persisted or cited across files and runs is identified by
SHA-256: stored bytes by their `ByteDigest`, which a manifest verifies, and values by their
`CanonicalDigest`. A plan revision, the input a run used and the result a report or figure is
bound to should be cited by `CanonicalDigest`. This is the contract for stale-run rejection:
compare the digest of the current plan (or input) with the one the run was stamped with. No
shipped code compares them yet; UI-D wires the run stamp to `CanonicalDigest`. Because the writer
emits one canonical document per value (under the earliest version that expresses it; see the
[version policy](#version-policy) and [canonical wire forms](#canonical-wire-forms)), equal
values have equal digests, and a changed value has a different digest except with SHA-256's
negligible probability. The digest is taken over a portable binary rendering of the document's JSON
value (a tag per node, lengths before contents, strings as UTF-16 code units, numbers as their
IEEE-754 bits or, beyond 2^53, their exact integer), so the JVM and Scala.js agree although they
print some doubles differently; `CanonicalDigestSuite` pins one on both platforms. A digested
number must be exactly a 64-bit integer or a finite double: any other (2^64, 1e400,
0.10000000000000001) is refused with `CodecError.Unsupported` rather than rounded onto another
value's digest. Numbers the codecs write always qualify; Scala.js parses JSON text into doubles, so
there a number in parsed text that no double represents has already been rounded and cannot be
detected. `-0.0` and `0.0` digest differently, deliberately: a digest may tell equal values apart,
but never identifies different ones.
`CanonicalDigest[A]` is typed by the value it identifies, so a plan digest does not compare with
an input digest (`sameAs`), and it neither converts to nor from a `ContentHash`. It persists as its
64 hexadecimal digits (`CanonicalDigest.parse`) and is shown as `sha256:` and those digits.

`ContentHash` stays what it was built for: a fast, portable change detector for in-memory keys
(provenance and cache keys, a preview's `checkCurrent`, a `StudyRunId` within one process). The
16-hex `ArtifactRef` that version-1 plan, input, ledger and result documents carry is kept as it is,
so no pinned wire form changes: it is an in-document cross-check that the decoder re-derives
(`CodecError.InputIdentity` on a mismatch) and the manifest records beside each entry's SHA-256,
never the identity a file or a run is cited by. Stamped study results use the existing `eyes4s.study-result@2` ladder to add separate
canonical plan and input digests; pinned unstamped `@1` bytes remain unchanged.
`DensityArchiveCodec.encodeStamped` accepts only the completion-bound
`StampedStudyResult`, while decoded `StudyResultArchive.stampClaim` remains an
unverified claim. Manifest resolution re-encodes the actual decoded plan and input
and checks their canonical identities before accepting a stamped result; custom
input decoders without a canonical encoder fail closed. Legacy archives expose
`None` for the stamp.

`eyes4s.study-progress@1` carries a typed `RunStamp`, execution quanta and a
committed progress snapshot. Counting and unknown totals are different wire cases;
running stages require their matching scientific meter. Every 64-bit counter is a
canonical decimal string, preserving values above 2^53 through JavaScript JSON
parsing. Invalid counters, mismatched units, missing running meters and inconsistent
work totals are refused. The codec owns the DTO; the io module maps fs2 progress to
it through `StampedStudyExecution.snapshot`.

## Schema compatibility

Every stored document is an envelope, `{"schema": {"name", "version"}, "value"}`. The version is
one positive integer; there are no minor versions. The rules below are enforced on the JVM and
Scala.js by `SchemaCompatibilitySuite` over a pinned v1 document of every shipped document schema:
the twenty-one JSON codecs (study plan, study input, admission ledger, study result, recording,
binocular recording, recording input, temporal input, timeline, manifest, the four score and
difference schemas, the recording and temporal result archives, the covariate schema, report
specification and report of [reducing study results](REDUCING_RESULTS.md), and the conventional
`eyes4s.recording-plan@1` and `eyes4s.temporal-study@1` plan schemas), plus a pinned v2 document
of the study plan and admission ledger and a pinned v3 document of each, with a test that fails if a pinned schema version is
missing, and the packed recording,
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
meaning, in every later release. The study plan and the admission ledger have a version 2
(`eyes4s.study@2`, `eyes4s.admission-ledger@2`, pinned by `study-v2.json` and
`admission-ledger-v2.json`), and both a version 3: `eyes4s.admission-ledger@3` (declared in
`InventoryDefinitions` and pinned by `admission-ledger-v3.json`) adds the trial inventory, and
`eyes4s.study@3` (declared in `StudyCodecDefinitions` and pinned by `study-v3.json`) adds the
initial-fixation policy. A version-1 document decodes with its version-1 meaning (the whole frame,
scales in frame units, every matched reference averaged, every fixation kept, and the admission
policy that quarantines off-screen records), a version-2 plan keeps every fixation, and a document
older than the version that introduced a cause may not name it. For every other schema, version 1 is the first version, so every current decoder reads exactly
version 1; there is no version 0 or earlier variant to keep or migrate from, and `DefinitionId`
refuses a version below 1.

### Version policy

A schema with more than one version is a `SchemaLadder`, and there is no other way to give a
schema a second version. A ladder lists consecutive versions of one schema name; each version has
its own writer and reader, and each version after the first has an **upcast**: a total function
from a payload of the previous version to a payload of the new version with the same meaning.
`ladder.next(previousExpresses, upcast)(write)(read)` is the only way to add a version, so a
version cannot exist without its upcast, and `ladder.codec` is the `VersionedCodec` the schema
ships. The upcasts state what an earlier version left implicit: the study plan's v1 to v2 upcast
turns `estimates` into native `scales` and writes the whole-frame geometry, the version-1 pairing
and no angular scale, and its v2 to v3 upcast writes `"initialFixations": {"kind": "keepAll"}`;
the ledger's v1 to v2 upcast writes the version-1 admission policy and no records outside the
frame, and its v2 to v3 upcast writes `"inventory": null`.

The versions' vocabularies are nested: every value a version expresses is expressed by each later
version. Writing is **earliest-version**: a value is written under the lowest version that
expresses it (`ladder.earliest(value)`), so a pinned version-1 document re-encodes to its own
bytes, an older release can read every document whose value it can express, and the writer emits
one canonical document per value. Decoding reads each version with that version's own reader and never migrates.
`ladder.lift(document)` is the explicit migration: it rewrites a stored document of any listed
version as the latest version with the same meaning, by applying the upcasts in order. Because the
vocabularies are nested, every later reader accepts later-version spellings of earlier values, and
the codec writes the decoded value back under its earliest version; a version-3 ledger with a
`null` inventory is read with version 2's vocabulary, so it may not name an inventory cause.
`lift` first reads the document with its own version's reader and fails as that reader does, so
it never turns a document its version refuses into a valid later one. Canonical-form refusals (see
[canonical wire forms](#canonical-wire-forms)) apply within a version's vocabulary.

The published `SchemaLadderLaws` state the policy over generated values of every version. Writing
`write_N`, `read_N` and `upcast_N` for version N's writer, reader and upcast, and `E` for the version
the codec writes a value `x` under:

| Law | Statement |
|---|---|
| Earliest | `read_N(write_N(x))` is `x` exactly when `N >= E`: `E` is the lowest version whose vocabulary expresses `x`, and the vocabularies are nested |
| Old readers | the ladder cut at `E` (`ladder.upTo(E)`, what the release that introduced `E` read) decodes the written document to `x` |
| Upcast | for every later version `M`, `upcast^(M-E)(write_E(x))` is exactly `write_M(x)`, so decoding the vN document and decoding its lift give the same value |
| Canonical | the document lifted to the latest version decodes to `x` and re-encodes to the earliest document, so writing is canonical and idempotent |
| Refusal | a document the codec refuses is still refused after `lift`; the invalid documents come from edits of written ones (generic payload damage by default, plus edits that reach a later version's vocabulary) |

`PlanCodecLawSuite` and `StudyInputCodecLawSuite` apply them to the study-plan ladders (both key
layouts, with plans under every initial-fixation policy) and the admission-ledger ladders (the study key layout for versions 1 and 2, the trial
layout at all three versions), with ledger edits that write a version-2 or version-3 quarantine
cause into a ledger of any version, and kill, by a falsified property from a fixed seed, a
dropped upcast step (each step of the ledger in turn), an upcast that states another pairing or
today's default admission policy instead of version 1's, a v2 to v3 plan upcast that states
dropping the first fixation, a codec that writes the latest version instead of the earliest, and
vocabularies that claim a policy ledger is a version-1 ledger or an initial-fixation plan a
version-2 plan.
`TemplateRecipeLawSuite` applies them to the template recipe ladder (fixed-feature and map
inputs) under the conventional `eyes4s.template-recipe` schema: version 1 is the form the three
earlier recipe codecs wrote, told apart by `method`, and version 2 adds match groups to
fixed-feature rows; pinned recipes written by the earlier codecs decode and re-encode unchanged.
`SchemaRegistryJvmSuite` finds the ladders from the shipped codecs that expose one (through the
decoder of every registered document fixture), not from a list, and requires every schema name
registered at more than one version to have one, each ladder's versions to be exactly the
registered ones, a pinned fixture of every
version, the ladder laws, and every pinned document of a version below the latest to lift, decode
to the value it decodes to as written, and re-encode to itself (see [Evidence](#evidence)).

### Unknown versions and members

**Unknown versions** are refused as values naming both identities:

| Where the version appears | Refusal |
|---|---|
| A document's envelope | `CodecError.Schema(expected, found)` |
| The envelope of a schema with a `SchemaLadder` (the study plan, the admission ledger, a template recipe) | `CodecError.UnsupportedSchema(role, found, versions)` |
| Version 0, a negative or a non-integer version | `CodecError.Definition(PlanError.InvalidDefinition(name, version))`, or `CodecError.Field("version", …)` |
| A nested identity a codec requires: layout, key, parameter, score or difference schema | `CodecError.Schema(expected, found)` |
| An identity a registry selects on: a plan's method (a temporal plan's base method), an input's key schema, a result's method (a recording or temporal result's too) | `CodecError.MissingMethod`, `MissingKeySchema` or `MissingResultCodec` |
| The plan embedded in a recording or temporal result archive | `CodecError.Entry("plan", CodecError.Schema(expected, found))` |
| A standalone recording's envelope | `CodecError.UnsupportedSchema("recording", found, supported)` |
| A manifest | `ResolveError.ManifestDecode(address, CodecError.Schema(…))` |
| An artifact its manifest declares at that version | `ResolveError.Decode(entry, CodecError.Schema(…))` |
| An artifact whose bytes hold another schema than declared | `ResolveError.Schema(entry, declared, found)`, before its decoder runs |
| A recording or temporal plan or result no registration was added for | `ResolveError.Decode(entry, CodecError.UnsupportedSchema(role, found, Vector()))` |

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
fixation, whose value is derived from its samples, in a study input's scanpath and in a recording
result's detected events alike. The members a recording result archive re-derives
(its synchronization, detection and assignment) are compared with the derivation member by member
over the members version 1 writes, so an unknown member among them is ignored like any other and a
known one that differs is refused with `CodecError.Derived(path, declared, derived)`.

### Canonical wire forms

The writer emits one canonical document per value, and within a version's vocabulary a decoder
refuses a second spelling that would decode to the same value. Later readers still accept
later-version spellings of earlier values (the [version policy](#version-policy)), and unknown
members and numeric spellings are treated as described above. A 64-bit integer member written as a
JSON number must lie within ±2^53 on every platform, since Scala.js cannot tell larger integers
from their neighbours; larger integers are written as decimal strings. A refusal is `CodecError.NonCanonical(path, found, canonical, rule)`,
naming the member, what it found, what the writer writes and the rule, or `CodecError.Field` where
the member has the wrong JSON type:

| Rule | Where |
|---|---|
| A set is written in ascending order, once per member | a template recipe's `heldOutFolds`, a learned template recipe's `heldOutGroups`, the entries of `VersionedCodec.entries`, a temporal input's `epochs` (by key), observed coverage `intervals` (by onset, standalone and in an epoch), an evaluation specification's `parameters` (by name) |
| An identity is declared once | the `frames`, `grids` and `clocks` of a document identity table |
| Microseconds are a plain decimal string | every `*Micros` member: `"5"`, never `"+5"` or `"05"`, and `"0"`, never `"-0"` |
| A number is a JSON number | every numeric member: never a numeric string, and never `null` for a number |
| An integer is spelled as an integer | every integer member: `3`, never `3.0` or `3e0` (on the JVM; the Scala.js parser does not keep a number's spelling) |
| An absent value has one spelling | a member written as `null` when absent must be present; a member omitted when absent (a scanpath's `source`, a neutral timeline's `timing`) may not be `null` |

A document identity table's order and its unreferenced entries are not yet checked against the
table the writer derives from the value; a table that lists the same identities in another order,
or adds one nothing references, still decodes.

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
companion and every `*Definitions` object on the classpath requires exactly one valid, singly
declared entry per built-in identity; instantiating each named law suite
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

**Where a new identity is declared.** The identities in the `DefinitionId` companion
(`plan/StudyPlan.scala`) stay where they are, but new ones are not appended there. Declare each in
the file that introduces it, as a `val` of an object whose name ends in `Definitions`, built with
the package-private `DefinitionId.builtIn`:

Illustrative declaration pattern (not an executable example):

```text
// Illustration: declared in the file that introduces the KDE method.
object KdeDefinitions:
  val gaussianKde: DefinitionId = DefinitionId.builtIn("eyes4s.gaussian-kde", 1)
```

The registry finds every such object on the classpath, so parallel changes never edit one shared
list of identities. Each identity still needs its registry entry, law and pinned fixture.

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
by exhaustion. The recording and temporal result archives (`eyes4s.recording-result@1`,
`eyes4s.temporal-result@1`) have `RecordingResultCodecLawSuite` (I-VT and I-DT analyses over
generated recordings with fixations, saccades, blinks, signal loss and off-screen samples,
synchronized by generated marks with rejected outliers, optionally interpolated and assigned to
generated areas) and `TemporalResultCodecLawSuite` (generated temporal studies with clipped and
fully contained fixations, coverage gaps, anchors at zero, beyond 2^53 and near the Long maximum,
where a late window overflows, missing epochs, one or two repetitions and binned and Gaussian
scales), compared by the published `RecordingResultEquivalence` and `TemporalResultEquivalence`.
Each kills its mutants by a falsified property only (`Test.Failed`, never an exception or
exhaustion): a dropped event, a moved angular sample, an emptied area and an encoder that declares
a fixation's derived dispersion value; reordered cells, a forged
missing-epoch digest, a ledger re-anchored by a microsecond and a changed contrast difference. The
pinned recording-result-v1 and temporal-result-v1 fixtures are described under
[result archives](SAVED_STUDIES.md#recording-and-temporal-result-archives).

UI-S6 also pinned the built-in schemas no fixture had carried: `timeline-v1.json` (a timeline
of study keys, with equal instants beyond 2^53 kept in order), `score-codecs-v1.json` (one
envelope of each score and difference schema, the only fixture of `measure-distance@1` and
`scalar@1`), and the standalone, binocular and packed recordings with their payloads listed under
[artifact manifests](SAVED_STUDIES.md#artifact-manifests-and-verified-resolution).
