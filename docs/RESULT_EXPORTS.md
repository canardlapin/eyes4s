# Reusable result tables

`ResultExports` produces checked `ResultTable` values for the finite schema matrix below.
Each table has ordered typed columns, immutable rows, and a versioned JSON metadata sidecar.
Call `table.csv.encode` on JVM or Scala.js. On JVM, `ArrowResultExport.write[IO](table, path)`
writes an IPC stream from those same rows. Neither route needs R.

There is one result-table layer. `ResultTable`, its columns, cells and families live in the pure
`eyes4s-results` module (JVM and Scala.js, no JSON library), with the table's context and JSON
cells as `eyes4s.results.TableJson`; `eyes4s-io` keeps the circe entry point
`eyes4s.io.ResultTable.of(family, columns, rows, context: Json)`, the `csv` rendering and the
Arrow writer, and the same names as aliases. Reports render into the same layer as the
`ReportCells`, `ReportParticipants` and `ReportContrasts` families
(see [reducing study results](REDUCING_RESULTS.md)). `BaselineExports` is a deprecated facade
that exports `ResultExports`: its tables are the same tables, with the same identities. The move
kept every pinned artifact byte-identical: `python3 tools/result-export/generate_exports.py
--bytes-only` rewrites the export example and compares every CSV, metadata and Arrow file with
`receipt-v1.json`, without the R and PyArrow readers.

The complete [public example](../io/src/test/scala/eyes4s/examples/BaselineExportGuide.scala)
constructs analyses, calls every adapter, and is executed by `BaselineExportsSuite` on both
runtimes. The [artifact command](../tools/study-cli/BaselineExportMain.scala) emits CSV,
metadata and Arrow files from that example. It uses public APIs throughout.

## Frozen result matrix

The machine-readable [schema contract](../tools/result-export/schema-v1.json) pins column order,
type, nullability, units, meanings and label domains for every example variant. Shared tests
compare the emitted schemas to the portable copy of that contract.

| Adapter | Tables and scientific rows | Interpretation retained in metadata |
| --- | --- | --- |
| `pairs` | One row per selected edge and named score component, including failed edges | Versioned evaluation specification, typed key schema, pairing orientation/relation, selection seed/cap/sample identity, eligible/selected counts, unmatched/ambiguous keys and provenance |
| `reductions` | One row per focal key/component with selected, successful and contributing counts | Exact retained source analysis is checked; source pairing exclusions, reduction policy/orientation, method and provenance survive |
| `contrasts` | Matched, control and matched-minus-control roles with separate failures | Both complete operand contexts; difference rows have absent counts rather than invented denominators |
| `repetition` | Separate matched/control edge tables over arbitrary registered occasions | Complete existing versioned recipe, role, plan hash and each edge table's context |
| `points` | Source admission, every selected control/time, every observed/control query, and every bin/role | Complete existing replay-verified point archive, exact clock/time, normalization, lookup, endpoint, selection and both aggregation policies |
| `fixedTemplate` | Coefficients and every held-out prediction, including failures | Existing native fixed-feature recipe, method/backend, training/held-out hashes, response unit and residual sign |
| `learnedTemplate` | Coefficient, held-out predictions and excluded training rows | Existing learned recipe, split/match groups, training/held-out/excluded counts and identities |
| `ols` | Predictor/intercept coefficients, diagnostics, and every fitted/residual cell | Grid/frame/units, intercept and rank-tolerance provenance, centered/uncentered R-squared meaning and residual sign |
| `study`, `temporal` | Existing study contrasts and duration-window contrasts/coverage | Reuse `ContrastCsv` and `TemporalContrastCsv`; saved plan remains in the sidecar even for empty output |

Scalar and structured scores use checked `ScoreColumns`. Their names must agree with the
versioned `EvaluationSpec`; component order is preserved. The caller supplies the scientific
score unit. MultiMatch components are unitless. The generic table constructor validates transport
shape and finite values; the adapters establish the association with scientific result types.
This matrix does not promise an adapter for every future result type.

The `study` and `temporal` sidecars record `source_schema` (`ContrastCsv.schemaVersion` or
`TemporalContrastCsv.schemaVersion`) and the typed `key_schema`. Temporal coverage reports
`excluded_fixation_count` (Int64) and, separately, the excluded fixation indices as a JSON array
in `excluded_fixations_json`. The sidecar's `row_count` is a JSON number.

`reductions` and `contrasts` require their original pair analyses so a typed key codec can retain
all excluded source keys. A mismatched source is refused. Template adapters execute the existing
checked split/fit/evaluation workflow. A failed whole-model fit returns an export error; it is not
a fabricated coefficient table. Successful fits retain any per-prediction failures. OLS undefined
R-squared has a missing number and the explicit zero-total-variation reason.

## CSV and Arrow semantics

`ResultTable.schema` is `eyes4s.result-table/1`. Each column declares UTF-8 text, JSON encoded as
UTF-8, signed Int64, finite Float64, or Boolean. JSON cells are parsed and canonically rendered;
ordinary text is preserved verbatim. Int64 times and counts never pass through a Double.

CSV uses RFC-4180 quoting and exact round-trip numeric spelling. Each nullable column has an
adjacent `__valid` Boolean column. A false flag means missing; a true flag with empty text means
an actual empty string. Typed scientific failures have status and structured error evidence,
not NaN sentinels. Commas, quotes, embedded newlines, lambda and supplementary Unicode characters
are exercised. Empty tables retain their full schema and metadata.

Arrow uses UTF-8, signed 64-bit integers, doubles, booleans and native validity bits. Finite label
domains are stored as UTF-8 with the permitted labels in field metadata; no dictionary codes are
inferred or emitted. Field metadata includes units and meanings. Schema metadata embeds the same
JSON sidecar. `table_sha256` in every row links it to that metadata; empty tables retain the link
in the Arrow schema and sidecar.

The table SHA-256 binds schema, family, scientific context and canonical cells. JSON object keys
are sorted and decimal numbers normalized; length framing counts UTF-16 code units, matching
Scala strings. Missing and present cells have distinct tags. The independent Python reader
recomputes this hash, including supplementary Unicode. This export identity is separate from
the library's noncryptographic scientific `ContentHash`.

Hash equality requires equal values. The demonstrated JVM and JavaScript learned coefficients
differ by one floating-point rounding step and pass the named `1e-12` numerical tolerance; their
coefficient/prediction table hashes correctly differ. Their input identities agree. Other example
table identities agree across the two runtimes. No rounding is introduced to force equality.

## JVM Arrow setup and ownership

Arrow Java 19.0.0 is an optional dependency of `eyes4s-io` on JVM only. Applications using the Arrow
writer need `arrow-vector` and one memory implementation (`arrow-memory-unsafe` is the tested
choice), plus `--add-opens=java.base/java.nio=ALL-UNNAMED`. Shared/Scala.js and pure modules have
no Arrow dependency. See the [Arrow IPC cookbook](https://arrow.apache.org/cookbook/java/io.html)
and [memory ownership documentation](https://arrow.apache.org/java/main/memory.html).

Arrow 19.0.0 requests Jackson 2.21.0. The JVM build and published optional declarations use
Jackson core, databind and datatype-jsr310 **2.21.7**, with annotations **2.21**, following the
[Jackson 2.21.7 BOM](https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.21.7/jackson-bom-2.21.7.pom).
Applications opting into Arrow must also select these patched Jackson dependencies: optional
dependencies do not propagate through the published `eyes4s-io` artifact. For sbt:

```scala
libraryDependencies ++= Seq(
  "org.apache.arrow" % "arrow-vector" % "19.0.0",
  "org.apache.arrow" % "arrow-memory-unsafe" % "19.0.0",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.21.7",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.21.7",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.21.7"
)
```

The isolated artifact consumer exercises this opt-in and reads the emitted IPC stream back.

`write` returns `F[Either[ArrowExportError, Unit]]`, owns the file and its allocators/vectors/writer,
and writes bounded batches. The defaults are 1,024 rows and a 64-MiB allocator budget. Invalid
limits are rejected before opening a file. Write failure can leave a partial destination; callers
should publish an output only after success. All resources close after success or failure.
An explicit stream finalizer is necessary because Arrow writer shutdown itself can fail while
writing its end marker. The forced-failure test proves the stream closes and allocated bytes
return to zero; it also proves a borrowed test allocator remains usable.

## Reproduce and independently consume artifacts

```sh
sbt 'ioJVM/Test/runMain eyes4s.examples.BaselineExportMain /tmp/eyes4s-baseline-exports'
LC_ALL=C Rscript tools/result-export/check_exports.R /tmp/eyes4s-baseline-exports
python tools/result-export/check_arrow.py /tmp/eyes4s-baseline-exports
```

The Python reader requires PyArrow; the evidence run used CPython 3.12.11 and PyArrow 19.0.1.
Base R uses no additional packages. It reads every integer column as text, verifies exact
`9007199254740993` and `9223372036854775807`, and independently checks keys, component order,
counts, missingness and analytic predictions/coefficients. PyArrow checks every IPC cell against
CSV, field types/nullability/units, UTF-8 label domains, metadata and content identity.

The [receipt](../tools/result-export/receipt-v1.json) records actual emitted file digests;
[evidence](evidence/baseline-export-2026-09-19.md) records validation and its limits. JSON scientific
archives continue to use their existing codecs. The export sidecar is a descriptor and evidence
link, not a second result-reconstruction format.
