# eyes4s-io

Ingest and export: EyeLink ASC, CSV, Mirror-derived metadata decoders, and
result export to CSV and Arrow.

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
