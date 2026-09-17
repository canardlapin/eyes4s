# eyes4s-io

Ingest and export: portable EyeLink ASC, sample/fixation CSV admission and typed
AOI, contrast and temporal CSV exports. Arrow and general Mirror-derived metadata
decoders are requirements, not delivered features.

Binary .edf is out of scope -- it requires SR Research's proprietary edfapi.
Run edf2asc first.

The portable EyeLink ASC implementation includes byte-preserving lexical
parsing, block-specific layouts, monocular and binocular sample materialization,
native events, observed messages, conversion provenance, streaming FS2 input,
line accounting, JVM/Scala.js parity fixtures, mutation tests, and bounded-memory
performance courts. It is not yet a broad vendor-support claim: licensed EDF API,
EDF2ASC, and real-device validation remain external release gates.

Start with the [EyeLink import guide](../docs/formats/eyelink-asc.md). The corpus
and independent-oracle policies are documented beside it. See `../PRD.md` for
the module requirements and `../.mote/` for the live work items.

For existing fixation summaries, use [fixation studies](../docs/FIXATION_STUDIES.md)
or the compiled [repetition guide](../docs/REPETITION_STUDIES.md). Both preserve
admission diagnostics rather than silently dropping invalid trials; `FixationEvidence`
turns a fixation import into the pure admission ledger that `eyes4s-codec` serializes
beside the study input. The [task index](../docs/START_HERE.md) connects the other
public workflows.
