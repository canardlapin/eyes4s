# Compact native archives

Status: implementation in progress; not enabled in the native writer. Owner
selected compact storage under the existing limits on 28 October 2026.

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

This is an internal codec format, not an accepted native project media type yet.
Its byte sequence is:

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
