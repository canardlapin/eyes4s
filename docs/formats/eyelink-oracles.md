# EyeLink independent oracles

eyes4s does not reverse-engineer or redistribute EyeLink EDF. EDF is a
proprietary binary container, so the trusted import boundary has two separately
audited paths:

1. a licensed SR Research tool converts EDF to ASC, and eyes4s records the EDF,
   ASC, executable, version, platform, recovery mode, and exact option vector;
2. a licensed SR Research EDF Access API installation reads the original EDF
   into an independent canonical fact manifest.

The eyes4s ASC parser is then compared with that EDF-derived manifest and, where
possible, a second independently maintained ASC reader. Agreement among code
paths with different implementations is evidence against a shared parser bug.
It is not a substitute for real-device coverage or manual review.

The official EDF Access API exposes `FSAMPLE`, `FEVENT`, and `RECORDINGS`
structures and advances through a file with `edf_get_next_data` followed by
`edf_get_float_data`. SR Research distributes the headers and library with its
developer kit but does not authorize eyes4s to redistribute them. The repository
therefore contains only
[`edfapi_reference.c`](../../tools/eyelink-oracles/edfapi_reference.c), which is
compiled locally against a licensed SDK.

Primary vendor references:

- [SR Research EDF Access API installation and file locations](https://www.sr-research.com/support/docs.php?topic=linuxsoftware)
- [EyeLink data structures and missing-value semantics](https://www.sr-research.com/download/dispdoc/page9.html)
- [EyeLink ASC analysis format](https://www.sr-research.com/download/dispdoc/page25.html)
- [EyeLink integration-message timing](https://www.sr-research.com/support/thread-83-post-83.html)

## Canonical fact format

An oracle is a versioned TSV document. Its preamble identifies:

- oracle and corpus fixture IDs;
- independent implementation and input kind;
- ordering guarantee;
- tool name, reported version, and source or executable SHA-256;
- adapter SHA-256;
- input SHA-256;
- exact invocation; and
- conversion-receipt digest when an ASC oracle derives from a real conversion.

Each following row is one atomic field with record and field ordinals, record
kind, recording block, source order, field path, presence, value, and detail.
Presence has three meanings:

- `value`: the independent implementation exposed the exact value;
- `missing`: it exposed a native missing value; or
- `omitted`: it cannot expose the semantic, with a required reason.

This distinction is essential. A reader that discarded cross-table source order
or did not interpret a message offset must not certify the corresponding eyes4s
behavior.

Message records have separate paths for raw logged time, integration-message
offset, effective time, and payload. The EDF adapter also writes payload bytes as
hex so a control or NUL byte is never silently converted to text.

## Independent ASC reader

The committed public oracle was generated with CRAN `eyelinker` 0.2.2, whose
source tarball is pinned by SHA-256 in `oracles/registry.tsv`. The adapter invokes
exactly:

```text
eyelinker::read_asc(samples=TRUE,events=TRUE,parse_all=FALSE)
```

`eyelinker` independently recovers the synthetic fixture's sample, native
saccade, fixations, blink, messages, and recording metadata. It does not retain
cross-table source ordering and does not interpret Data Viewer integration-
message offsets. Those cells are committed as `omitted`, not inferred by the
adapter.

To regenerate after installing exactly `eyelinker` 0.2.2 into an isolated R
library:

```sh
tools/eyelink-oracles/regenerate.sh eyelinker \
  io/src/test/resources/eyes4s/io/eyelink/corpus/synthetic-events-messages.asc \
  io/src/test/resources/eyes4s/io/eyelink/oracles/synthetic-events-messages-eyelinker.tsv \
  synthetic-events-messages \
  synthetic-events-messages-eyelinker \
  /path/to/eyelinker_0.2.2.tar.gz \
  /path/to/isolated-r-library
```

The script rejects a source tarball whose digest differs from the pinned CRAN
artifact, and the R adapter rejects every installed version except 0.2.2.

## Licensed EDF oracle

Set the installed SDK paths and run:

```sh
EDF_SDK_INCLUDE=/path/to/edfapi/include \
EDF_SDK_LIB=/path/to/edfapi/lib \
EDFAPI_LIBRARY=/path/to/edfapi/lib/libedfapi.dylib \
tools/eyelink-oracles/regenerate.sh edf \
  /private/path/recording.edf \
  /private/path/recording-edfapi.tsv \
  private-fixture-id \
  private-fixture-edfapi \
  1
```

The generator compiles with strict C warnings, hashes the exact vendor library,
adapter, and EDF, and records `consistency=1`, `load_events=1`, and
`load_samples=1` in the invocation. The resulting private manifest can be used
by a private conformance run; neither the EDF nor proprietary SDK enters the
repository. Only a legally reviewed, deidentified aggregate artifact may be
published.

The current public registry deliberately marks this row
`awaiting-licensed-real-input`. There is no committed EDF manifest and therefore
no current claim that an EyeLink hardware family or EDF converter has been
validated.

## Stale and mutation courts

The JVM court recomputes the SHA-256 of every committed adapter, source fixture,
and generated manifest. The cross-platform court reparses and canonically
rerenders the manifest. It also proves that changing the exact invocation or
swapping an eye-channel value changes the scientific digest.

Before a release may claim EyeLink reliability, the differential court must
additionally show that deliberate mutations to converter flags, eye channels,
message timing, optional columns, and block boundaries fail against a real EDF
oracle. Synthetic agreement remains parser evidence only.
