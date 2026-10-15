# Public EDF-derived ASC source

`C023.asc` is the complete, unmodified archive entry
`EyeMovementsGlaucomaDataset/Data/Gaze/Raw/Controls/C023.asc` from:

Daniel S. Asfaw, Pete R. Jones, Nicholas D. Smith and David P. Crabb (2018),
*Data on eye movements in people with glaucoma and peers with normal vision*,
[Zenodo record 1156863](https://zenodo.org/records/1156863),
[DOI 10.5281/zenodo.1156863](https://doi.org/10.5281/zenodo.1156863).

The dataset metadata assigns
[Creative Commons Attribution-NoDerivatives 4.0 International](https://creativecommons.org/licenses/by-nd/4.0/).
The ASC file is redistributed unchanged, including its header and CRLF bytes;
no rows are selected, rewritten or anonymized by eyes4s. The adjacent manifest,
accounting and workflow tests are eyes4s-authored material. The dataset's license
is distinct from the paper's license and the library's Apache license.

The file is 8,794,652 bytes with SHA-256
`c23a7fb86d2fa4d7dcd8578a06f82911214073f1b6d36536bc9bc39316d70b19`.
The downloaded entry matched its ZIP-directory size and CRC-32 `73c74f12`.
Git attributes disable newline conversion for this file. The full archive's
published checksum is retained as published metadata, not claimed as an
observed checksum of a complete archive download.

The source header reports conversion from `C30V.EDF` using edfapi 3.1 Win32
(build November 24, 2009) on October 10, 2011. The public archive entry has a
different basename. The converter executable digest, arguments, recovery mode
and original EDF digest are unavailable and remain explicit nulls in the
manifest. This header evidence does not manufacture a complete
`Edf2AscReceipt` or satisfy release-converter certification.

The source declares three left-eye GAZE recording blocks, pupil AREA, rate
250 Hz, CR tracking and filter 2. Its 237,664 samples include a trailing
three-character CR warning token. All observed tokens are `...`; that does not
erase the missing-coordinate or native blink records. DISPLAY_COORDS and
GAZE_COORDS supply the 1600 by 1200 pixel display. Physical viewing geometry is
unknown, so the workflow must not invent a pixel-to-degree conversion.

`manifest.json` records independent lexical counts and observed conversion
information. `PublicAscEndToEndSuite` imports the complete file on JVM and
Scala.js and pins all three source-supported pixel IDT Scanpath fingerprints.
Both runtimes retain all 237,664 samples, reconcile 247,992 physical lines and
10,295 native records, and retain 42 observed messages. The detector uses a 50 px
square extent and 100 ms minimum duration with `GapPolicy.Break`; these are stated
workflow parameters, not a reconstruction of EyeLink's online detector settings.

Native event durations include the final 4 ms sample at the declared 250 Hz rate.
The parser retains the exported end and duration under
`AscNativeIntervalConvention.InclusiveFinalSample` and derives the half-open
offset for native blink reconciliation. The manufacturer's
[ASC guide](https://www.sr-research.com/download/dispdoc/page25.html) describes
the exported end as the last sample within an event.

The file contains a second SFIX at line 103 while the SFIX at line 32 has no
matching end. The test explicitly chooses
`AscNativePairingPolicy.PreserveUnconfirmedEvents`; the source diagnostic and
all events remain in the ledger, and only exactly paired ends enter
`confirmedEnds`. The default `RequireConsistentPairs` still rejects that
inconsistency. The fixture path uses exploratory conversion evidence because
the complete converter receipt remains unavailable.

The qualification receipt is `docs/evidence/public-asc-20261014.json`. Portable
acceptance and warning-as-error compilation passed; the complete branch landing
gate remains separate. This does not certify every EyeLink exporter/device or
claim that pixel IDT reproduces the native online fixation annotations.
