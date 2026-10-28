# Compact native archives

Status: document layer qualified and landed; native integration implemented in
an unqualified candidate. Owner selected compact storage under the existing
limits on 28 October 2026. Full-cohort application qualification is pending.

The current defaults remain 20,000,000 density cells, 1,000,000 structural rows,
512 entries and 268,435,456 stored bytes. Existing JSON archives retain their
existing checks and refusals. Raising a limit or discarding source rows is not
a solution to this ticket.

## Representation

The first slice is a private, versioned columnar document codec. It interns exact
scalar tokens and repeated subtrees, groups objects by their ordered member keys,
and stores each object's member values in reference columns. Arrays retain their
length and ordered references, including repetitions. Integer spellings, decimal
spellings, signed zero, UTF-16 code units and object member order survive exactly.
It accepts a lazy canonical document and exposes a lazy canonical document; it
must not reconstruct the entire JSON tree in order to restore typed rows.

The binary reader checks byte length before parsing and counts, references, text,
acyclic postorder, nesting depth and expanded traversal cost before exposing a
usable document. A small shared graph must not permit an exponentially large
restoration. Malformed input is a codec error, not an exception. Reader limits
are explicit; no new capacity defaults are established by this private codec.

## Native integration gate

The physical format needs an explicit version/media type; legacy logical schemas
and scientific identities stay unchanged. The native integration must state
exactly which physical structures the structural-row budget bounds, how primitive
columns and restoration work are bounded, and why the change is actual columnar
storage rather than an exemption around the old guard. No new representation is
accepted by default until that resource contract is implemented and tested.

Restoration must decode the large trial, fixation, ledger, inventory and comparison
tables one row at a time. Existing JSON codec entry points remain supported.
The complete JSON AST is not an acceptable intermediate for the full cohort.

Qualification requires: malformed and expansion-bomb refusals; exact canonical
identity on JVM and Scala.js; legacy project reopening; all retained source rows,
flags, failures and comparison rows; and an actual single-cohort API, application,
save, fresh-process reopen and export journey within the unchanged default limits.
Private sizing estimates and compression of partitioned archives are not this proof.

## Private document format, version 1

This is an internal codec format. The unqualified native candidate admits it
through the explicit media type described below. Its byte sequence is:

1. ASCII `E4CD` and a version byte `1`.
2. Node and shape counts as minimal unsigned LEB128 integers, at most 31 bits.
3. Ordered key shapes: field count, then each key string.
4. One unsigned tag per postorder node: null, false, true, number, string,
   array, or object shape plus six. The final node is the root. Every node and
   shape must be reachable from that root.
5. Scalar and array payloads in node order. Numbers retain their token and an
   explicit negative-zero flag because a platform can print negative zero as
   zero. Strings have a mode byte, a code-unit count and either checked ASCII
   bytes or big-endian UTF-16 code units. Array payloads are their length and
   ordered unsigned node references.
6. Object values by shape, member column, then row in node order. Each column
   stores an unsigned base, a bit width from 0 through 31 and unsigned deltas
   packed least-significant-bit first. Unused padding bits must be zero.

Counts are checked against explicit limits and remaining bytes before allocation.
All child references must precede their owning node. Per-node expanded traversal
cost is added with an overflow-safe bound; depth and aggregate references/text are
bounded separately. There are no trailing bytes. Empty, forward/cyclic, unreachable,
truncated and malformed dictionaries are refused. Row and object-member views stay
lazy; making the view cannot expand an object-only shared graph into a full tree.

The constructor limits used in private document experiments are declared with each
experiment. They are not a change to the native application's default budget.

## Initial document experiment

A private five-participant archive's three existing JSON documents were encoded,
decoded and re-encoded using the reviewed codec. All canonical document identities
matched, every binary re-encoding was identical, and compiled inputs were hashed
before and after the probe without changes.

| Document | Existing JSON bytes | Compact bytes |
| --- | ---: | ---: |
| Input | 8,669,927 | 1,292,724 |
| Ledger | 90,298,190 | 3,818,249 |
| Result metadata | 50,140,591 | 1,699,753 |
| Total | 149,108,708 | 6,810,726 |

These measurements exclude density payloads and other native package files.
The probe started from existing JSON and used explicit experimental constructor
limits. It is document-level evidence, not a native-memory, typed restoration,
application, legacy-compatibility or single-cohort capacity qualification.

## Native candidate resource contract

Ordinary native saves retain the existing JSON format. The candidate retries only
structural-row and metadata/total-byte capacity refusals using columnar input,
ledger and packed-result metadata. Density-cell and entry refusals remain refusals.
Other failures never trigger a retry. All-JSON files retain their existing row
accounting and refusals, including when opening them in the new reader.

A compact entry is explicitly `application/vnd.eyes4s.columnar-json;version=1`.
It carries the same logical input, ledger or result schema; it requires
`eyes4s.manifest@2`. The manifest ladder writes ordinary graphs as v1, and v1
refuses compact media. Only input, ledger and result roles admit this media.
Old project files remain readable; an older reader cannot read a compact entry.

For compact entries the whole archive's existing structural-row cap counts the
physical variable-length array descriptors, object-key shape descriptors and
shape-member descriptors. Object member values and node tags are primitive
columns; they are not expanded into JSON object rows. This is format-specific
accounting, not a claim that the study's logical row counts add up to less than
one million. Each described array also retains an explicit item cap equal to
`maxRows`, so a tiny shared dictionary cannot conceal an arbitrarily long table.
Logical trial, fixation, record, attribute, failure and pair counts must be
reported and compared independently during qualification.

Additional decoded-work bounds are derived from that same host cap: at most
`8 * maxRows` unique nodes, `64 * maxRows` references and
`128 * maxRows` expanded traversal nodes per document, with depth at most 128.
Counts are saturated only at the collection representation's integer bound.
Stored bytes and text units are bounded by the host byte cap. A row/member JSON
projection is limited to 65,536 nodes and a small object header to 524,288 nodes;
large tables use row readers. Original JSON inputs already materialized by the
legacy decoder retain their existing behavior. A compact representation that
needs a larger projection refuses explicitly; the compact profile does not
qualify arbitrary large embedded raw recordings.

The whole native package still checks all stored bytes, density cells, entries
and aggregate structural descriptors against the unchanged host limits before
accepting it. Schema, physical SHA-256, source identity, plan/input/result
bindings, packed-payload ownership, density digests and completed-result
reconstruction remain required. Reopening uses retained results, with no
analysis re-execution. Tests cover the actual fallback, copied-byte verification,
unchanged facts and a legacy package that still refuses under the same tighter
byte budget on both JVM and Scala.js.

## Complete-cohort integration pilot

The first frozen native integration candidate (`3af0f47a`) saved the complete
cohort within the unchanged defaults: 243,893,043 stored bytes, 753,249 physical
structural descriptors, 109 entries and 11,696,640 density cells. Its five-person
control retained the previous archive, map/failure, query and export bytes exactly.
The full-cohort API scientific identity matched the frozen reference.

That candidate's application journey subsequently failed its existing 60-second
participant/scale-profile plot-readiness wait. Its source, failure receipt and
saved project are preserved. These capacity measurements are not an application,
reopen or export qualification. A successor indexes repeated inventory-label and
nominal clock-identity scans while retaining first-error selection, canonical
identity order and all capacity and readiness limits. A fresh complete journey
and the full release gate remain required.

The indexed successor (`4fc14f9b`) passed the complete-cohort initial application
and export journey, but fresh-process reopening still reached the unchanged
60-second plot-readiness limit. A live reporting stack identified canonical
covariate-source hashing through the complete legacy ledger JSON. The next
candidate gives the ledger codec the same lazy earliest-schema envelope already
used for compact storage. The logical JSON writer, schema ladder and every
canonical digest remain unchanged. Tests pin all four versions and require lazy
record descriptions; deliberately restoring the old route fails three regressions.
Both failed generations remain preserved. Full qualification is still pending.
