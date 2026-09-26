# Importing EyeLink ASC

Status: the portable ASC importer is implemented and tested, but broad EyeLink support is not yet
release-certified. The remaining gate is an independent run with a licensed EDF Access API,
vendor EDF2ASC, and permitted real EDF data. The executable support matrix is `EyeLinkSupport` in
`eyes4s-io`; planned rows are obligations, not advertised capabilities.

## Portable boundary

eyes4s does not decode or reverse-engineer EyeLink EDF. EDF is a vendor-controlled binary format;
SR Research exposes it through the EDF Access API and provides EDF2ASC for portable text export.
The supported workflow is therefore:

```text
immutable EDF backup
  -> explicitly identified EDF2ASC conversion
  -> SHA-256-identified ASC
  -> eyes4s lossless streaming parser
  -> validated recording blocks, native events, and observed messages
```

The converter is part of scientific provenance. A release-grade import records the EDF digest when
available, ASC digest, converter executable digest, reported version when available, complete
options, platform, exit status, and whether failsafe recovery was requested. Supplying an ASC
without that information remains possible, but strict workflows must report the missing evidence.

## Import one recording

1. Preserve an immutable copy of the original EDF. Never overwrite it with a converted file.
2. Hash the EDF, the `edf2asc` executable, and the resulting ASC. On macOS or Linux,
   `shasum -a 256 <file>` provides the required digest.
3. Run the vendor converter yourself and retain its reported version, platform, exact argument
   vector, exit status, and whether failsafe recovery was used. eyes4s deliberately does not invoke
   proprietary tooling silently.
4. Construct the conversion receipt and import the ASC:

```scala
import cats.effect.IO
import fs2.io.file.Path
import eyes4s.io.*
import eyes4s.kernel.*

val setup =
  for
    edfDigest <- Sha256.fromHex(
      "recording.edf",
      "0000000000000000000000000000000000000000000000000000000000000000"
    )
    ascDigest <- Sha256.fromHex(
      "recording.asc",
      "1111111111111111111111111111111111111111111111111111111111111111"
    )
    executableDigest <- Sha256.fromHex(
      "edf2asc executable",
      "2222222222222222222222222222222222222222222222222222222222222222"
    )
    converter <- Edf2AscConverter.of("edf2asc", executableDigest, Some("recorded-version"))
    arguments <- Edf2AscArguments.of(Vector("replace", "with", "the", "exact", "options"))
    receipt <- Edf2AscReceipt.declared(
      Some(edfDigest),
      ascDigest,
      converter,
      arguments,
      "recorded-platform",
      Edf2AscRecovery.Normal
    )
    frame <- Frame.screen("experiment-display", 1920, 1080)
    config <- EyeLinkAscSessionConfig.of(
      frame,
      ClockId("eyelink-tracker"),
      ConversionEvidencePolicy.Release,
      AscBlinkReconciliationPolicy.ConfirmedNativeIntervals,
      AscUnspecifiedPupilPolicy.RejectMeasuredValues
    )
    stream <- AscStreamSettings.of("recording.asc", 1024 * 1024, 64 * 1024)
  yield (EyeLinkAscOrigin.Converted(receipt), stream, config)

def importRecording(path: Path) = setup.map { case (origin, stream, config) =>
  EyeLinkAscImport.readPath[IO](path, origin, stream, config)
}
```

Replace every placeholder with evidence from the actual conversion. `setup` retains constructor
failures as typed values; `importRecording(Path("recording.asc"))` returns an `Either` containing the resource-safe
`IO` import. Execute that effect within your application; constructing it does not read the file.

For exploratory byte input without a conversion receipt:

```scala
val materialized = EyeLinkAscImport.fromBytes(ascBytes, streamSettings, sessionConfig)
```

This computes the ASC digest exactly but marks the origin `Unidentified`. A configuration using
`ConversionEvidencePolicy.Release` will therefore reject trusted assembly.

Always inspect the result before requesting its trusted session:

```scala
materialized.report.isReconciled
materialized.warnings.map(_.message)
materialized.errors.map(_.message)
materialized.trusted
```

`trusted` is an `Either`: malformed source, unsupported layouts, digest mismatch, incomplete
conversion evidence, inconsistent sampling, or any other error cannot masquerade as an empty
recording. Monocular and binocular blocks remain distinct cases of `EyeLinkAscRecordingArtifact`.

Experimenter messages are available through `session.observedMessages`. They are tracker-clock
observations, not planned stimulus timestamps. Use explicit common marks and
`SyncEvidence.fromCommonEvents` before relating them to the experiment clock.

EyeLink `EFIX`, `ESACC`, and `EBLINK` records remain in `session.nativeEvents` as vendor-native
evidence. They are not an eyes4s `DetectionResult`. To produce a scanpath, choose a monocular
recording, state the viewing geometry, warp pixels to degrees, run a named eyes4s detector, and call
`Scanpath.fromEvents` on its source-supported event series. The executable end-to-end example is
`EyeLinkAscImportSuite`.

## Which EyeLink-like file do I have?

| Input | Route |
|---|---|
| Native binary `.edf` | Convert with licensed vendor EDF2ASC; eyes4s does not decode EDF |
| EDF-derived `.asc` | `EyeLinkAscImport` after recording conversion provenance |
| Data Viewer sample report | Generic delimited importer; it is not lossless ASC |
| Generic CSV/TSV export | Declarative delimited schema |
| BIDS eye tracking TSV/JSON | Planned v1.1 codec; not the EyeLink ASC parser |

Unknown ASC record tokens and exact raw lines remain inspectable. Known records with malformed or
unsupported layouts return line-numbered diagnostics. Neither case is silently dropped.

Primary format references:

- [SR Research ASC File Analysis](https://www.sr-research.com/download/dispdoc/page25.html)
- [SR Research EyeLink Data Types](https://www.sr-research.com/download/dispdoc/page9.html)
- [SR Research File Data Control](https://www.sr-research.com/download/dispdoc/cmds7.html)
- [SR Research EDF2ASC guidance](https://www.sr-research.com/support/thread-7674-post-30938.html)

## Support contract and semantic rules

1. Every nonblank physical line is represented exactly once as a typed record, a preserved unknown
   record, or a rejected line with diagnostics. Nothing is silently skipped.
2. `START` and its configuration belong to one recording block. A later block may change eye,
   content, coordinate, optional-field, tracking, or rate configuration.
3. Monocular left, monocular right, and binocular samples are different layouts. Binocular input
   preserves both eyes and their shared index.
4. Only configured `GAZE` values may become screen-pixel points. `HREF`, `RAW`, and `PUPIL` camera
   coordinates are retained natively and rejected by pixel materialization.
5. Pupil area, diameter, and arbitrary or undocumented values remain distinct. No conversion is
   inferred from magnitude.
6. A dot gaze value or zero pupil is missing native data; it does not alone prove a physiological
   blink. Native blink intervals may be reconciled through a separately named policy.
7. EyeLink fixation, saccade, blink, and velocity values remain vendor-native evidence. They never
   acquire an eyes4s detector identity.
8. `MSG` retains logged tracker time, any reference-backed offset interpretation, effective
   observed time, exact payload bytes, and source location. It materializes into an observed—not
   planned—timeline.
9. Unknown well-formed tokens are preserved for forward compatibility. Unsupported known layouts
   fail explicitly instead of being guessed.
10. Failsafe-recovered converter output is marked recovered and cannot be presented as an ordinary
    clean source.

## Target capability matrix

`required` means that the capability must have a conformance fixture before v0.6 can claim EyeLink
support. `preserve` means that eyes4s retains the native value but does not interpret or materialize
it into a stronger domain type. `reject` means that the portable API must fail explicitly.

| Dimension | Values | v0.6 disposition |
|---|---|---|
| Source | EDF2ASC text | required |
| Source | binary EDF | reject; vendor conversion required |
| Conversion content | samples; events; samples + events | required |
| Eye layout | left; right; binocular | required |
| Coordinate mode | GAZE | required |
| Coordinate mode | HREF; RAW/PUPIL camera coordinates | preserve, never relabel as pixels |
| Tracking | head fixed; remote with head target | required |
| Nominal rates | 250; 500; 1000; 2000 Hz | required with timestamp evidence |
| Pupil | area; diameter; arbitrary/unknown | required without inferred conversion |
| Optional fields | GAZERES; velocity; input; button; status; head target | required/preserved as declared |
| Native events | fixation; saccade; blink | required as vendor evidence |
| Messages | ordinary; numeric-leading; offset; Data Viewer integration | required |
| Structure | multiple blocks and block-specific configuration | required |
| Forward compatibility | unknown record tokens | preserve |
| Recovery | EDF2ASC failsafe output | preserve with mandatory warning |
| Hardware evidence | 1000 family; Portable Duo; EyeLink 3 | required before broad current support |
| Legacy hardware | EyeLink I/II | preserve; unvalidated until licensed fixtures exist |

The executable matrix names a planned fixture for every required row. A generated validation
artifact may change a row from planned to validated only by binding it to a fixture/oracle digest.

## Equality and tolerance contract

- Source ordering, line numbers, record kinds, eye identity, timestamps, message payloads, block
  boundaries, counts, configuration tokens, and integer/status fields compare exactly.
- Decimal tokens in ASC compare by their parsed decimal value; tests do not introduce an epsilon to
  excuse lexical mistakes.
- EDF Access API values compared with EDF2ASC decimal output use a named absolute-plus-relative
  tolerance derived from the converter's printed precision and recorded with the fixture.
- JVM and Scala.js canonical manifests and SHA-256 digests compare exactly.
- Unsupported cases must produce the expected typed requirement or diagnostic; an empty output is
  never evidence of success.

## Required validation evidence

The release validation artifact must bind the source revision, dirty state, support matrix, corpus
and oracle digests, converter identities and options, exact/differential comparisons, JVM/Scala.js
parity, required mutation receipts, malformed-input results, throughput and peak-memory results,
warnings, and unsupported cells. Private gaze data may remain private, but its digest, capability
coverage, oracle identity, and aggregate result remain public.

## Current portable conformance evidence

`META-INF/eyes4s/eyelink/portable-conformance.tsv` is the machine-readable summary of the
platform-independent court. Six generated fixture families have pinned semantic digests and run on
both the JVM and Scala.js. The metamorphic court covers LF versus CRLF, legal structural whitespace,
arbitrary byte repartitioning, separate sample/event conversion subsets, frame-matched translation
of GAZE coordinates, and concatenation of independent recording blocks. Exact-comparison and
semantic-projection mutants have been shown to fail on both runtimes.

One fixture is also compared against a digest-pinned manifest generated by the independently
maintained CRAN `eyelinker` 0.2.2 reader. The typed comparator checks 57 common sample, native-event,
and message fields exactly. Fields the reference reader does not expose remain explicit omissions;
they are not counted as agreements. The other synthetic fixtures record `missing-external` for the
ASC oracle.

Every EDF cell currently records `missing-external`. The portable court therefore establishes
parser consistency, cross-runtime equality, and limited independent ASC agreement. It does not
certify EDF2ASC, the EDF Access API, EyeLink hardware, or a real EDF-derived workflow. Those claims
remain blocked until a licensed vendor installation and a redistributable or otherwise permitted
real EDF fixture are run through the registered oracle harness.

## Streaming memory and performance evidence

`AscStreamSettings` contains two byte budgets: the maximum retained physical line and the maximum
read chunk. `EyeLinkAscStreaming.pipe` rechunks every upstream byte stream to the configured read
budget, even when the caller supplies a much larger chunk. The logical source content presented to
one parser step is therefore bounded by:

```text
maximum line bytes + maximum read-chunk bytes
```

`AscStreamingMemoryEnvelope` exposes that contract. It is not described as a JVM heap estimate:
objects, tokens, and transient copies have runtime overhead. The performance court measures sampled
heap usage and HotSpot all-thread allocated bytes separately inside a forked JVM with `-Xmx256m`.
A streaming result discards each emission after the downstream fold consumes it. A materialized result is explicitly
`AscWorkflowMemoryEnvelope.Materialized` and reports its input size; it does not inherit the bounded
retention claim.

The pull-request smoke court covers 250, 500, 1000, and 2000 Hz; left, right, and binocular layouts;
message-rich input; a small materialized run; and 64 MiB of deliberately overlong lines. Every row
stores its input and emitted counts, line and chunk budgets, elapsed time, throughput, sampled peak
heap delta, allocated bytes and input-normalized allocation, JVM maximum heap, thresholds, and
derived pass status. A JVM without supported allocated-byte counters fails explicitly. Repeated path cancellation is
also tested 256 times with a file-descriptor leak court where the JVM exposes that counter.

The generated `performance.yml` workflow runs weekly and on manual dispatch. Its scheduled profile
adds a two-hour 2000 Hz binocular stream (14,400,004 physical lines), records the Git revision and a
SHA-256 digest of `git archive HEAD`, and uploads the measured TSV rather than turning a green test
into an undocumented performance claim.

The checked-in first baseline is
`META-INF/eyes4s/eyelink/performance-baseline.tsv`. It was measured on 2026-08-15 using an Apple M3
Max with 36 GiB RAM, macOS 14.3 arm64, and OpenJDK 25.0.1. The child JVM was capped at 256 MiB. The
two-hour 2000 Hz case processed 489,600,081 bytes and 14,400,004 lines in 46.88 seconds, at 10.44
MB/s, with a sampled peak heap delta of 160,511,552 bytes. It allocated 101,433,508,744 bytes across
the JVM threads, or 207.176 allocated bytes per input byte. The 64 MiB hostile-line case completed in
2.77 seconds with every line represented once as a bounded rejection. All seven scheduled rows
passed the stored 1 MiB/s, 256 MiB heap, and 512 allocated-bytes-per-input-byte gross-regression
thresholds.

This first baseline deliberately records `source_dirty=true` and the exact working-source digest.
It proves the current implementation under the named local environment; it is not hosted CI proof
and does not upgrade any EyeLink hardware or licensed EDF cell to validated. Release evidence must
replace or supplement it with a clean-revision scheduled artifact.

The local working-source digest is the SHA-256 of the sorted SHA-256 manifest for `build.sbt` and
the Scala files under `io/src/main/scala`, `io/src/test/scala`, `io/.jvm/src/test/scala`, and
`io/.js/src/test/scala`. The performance TSV itself is excluded, avoiding a self-referential digest.

The local commands are:

```sh
sbt "ioJVM/Test/runMain eyes4s.io.EyeLinkAscPerformanceMain --profile smoke"
sbt "ioJVM/Test/runMain eyes4s.io.EyeLinkAscPerformanceMain --profile scheduled --output target/eyelink-performance.tsv"
```
