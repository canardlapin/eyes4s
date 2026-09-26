# Checked Session membership

`eyes4s.design.Session[U]` is an immutable container with a declared `Frame[U]`.
`Session.empty(frame)` starts with no members. Its argument is already a domain
value, so empty construction cannot fail. No coordinate transformation,
synchronization or resampling occurs during admission.

Use `SessionKey.of` to parse a nonblank key. Nonblank whitespace is retained;
`"trial"` and `" trial "` are distinct keys. Recordings, AOI sets and grids have
separate key namespaces. `addRecording`, `addAoiSet` and `addGrid` return
`Either[SessionError, Session[U]]`, rejecting an existing key in that namespace.
All values must share the complete frame identity and specification, checked with
`kernel.Agreement`. A Session declares **no timeline**: each recording keeps its own
`ClockId`. Independently acquired recordings run on independent clocks, and a shared
display does not make them one timeline; forcing one `ClockId` would assert a
synchronisation nobody established. An analysis that needs two recordings on one
timeline checks `Agreement.clocks` on those recordings itself.

`replaceRecording`, `replaceAoiSet` and `replaceGrid` require an existing key and
recheck the replacement. Replacement preserves insertion position. The matching
`remove` methods return a typed missing-key error for an absent key and preserve
the remaining entries' order. Every operation returns a new container; refusal
leaves the original value usable and unchanged.

Different grids in one frame may have different resolutions and identities.
Multiple keys can refer to the same grid identity only when its specifications
agree. Replacing the last occurrence of an identity may replace its specification;
there is no remaining conflicting declaration in that Session. A Session may be
empty, but an admitted `AoiSet` remains nonempty under its own constructor contract.
An absent AOI set is represented by absence of its key, not an invented frame.

`recording`, `aoiSet` and `grid` return `Option` and the stored object directly.
`recordings`, `aoiSets` and `grids` expose immutable vectors in insertion order.
These accessors do not manufacture a capability to validate unrelated values or
skip the library's checked binary operations. A key can be reused with another
Session, where it simply performs that Session's independent lookup.

The external-package `eyes4s.sessionconsumer.SessionSuite` exercises construction,
every mutation operation, total access, refusal stability, frame specification
conflicts, coexisting recordings with distinct clocks and grid aliases on JVM and JS. Compile-time tests
reject constructor access, raw-string keys, mixed spatial units and mutable
access. The array probe uses safe `IArray.from` conversion and proves that later
changes to the original mutable array cannot change the recording. Deliberate
unsafe casts or `IArray.unsafeFromArray` are outside that immutable input contract.

Session persistence and the adapter to plan preflight/epoch availability are
separate concerns. The latter belongs to `app-prereq`; design has no dependency
on plan, codecs or effects.
