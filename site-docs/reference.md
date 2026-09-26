# Methods and support

## Choose a method by its contract

| Family | Choice and units | Result or common failure |
|---|---|---|
| I-VT | Degrees/second threshold and minimum duration | Fixations/saccades with sample support; needs viewing geometry before pixel input can be used. |
| I-DT | Bounding-box extent in frame units and minimum duration | Fixations; dispersion extent is not a Gaussian bandwidth. |
| Engbert–Kliegl | Typed thresholds and minimum samples | Microsaccades; consult the detector's `AlgorithmCard` for deviations. |
| Gaussian maps | `Sigma[U]` SD and `EdgePolicy` | Intensity or normalized Mass; empty/degenerate occupancy fails explicitly. |
| Pearson / Fisher z | Compatible Mass grids | Correlation / unbounded z; constants or singular transforms fail. |
| Cosine / extended Jaccard | Compatible Mass grids | Finite similarity; not a metric. |
| Spearman / distance correlation | Compatible Mass grids | Average rank ties / biased energy convention; constants fail explicitly. |
| Point density sampling | Fixed template and focal trajectory | Explicit normalization, lookup, query order and bin endpoints. |
| Fixation overlap | Shared queries and position threshold | Strict threshold; missing support retains its denominator policy. |
| Native template regression | Training rows and fixed features | Through-origin QR; rank failure is a value. |
| Total variation / Hellinger | Compatible Mass grids | Metric distance. |
| Jensen–Shannon | Compatible Mass grids | Semimetric, not a triangle-inequality claim for unsquared divergence. |
| Sinkhorn | Regularization, iteration and allocation limits | Entropic transport cost; self-cost need not be zero. |
| MultiMatch | Compatible ordered scanpaths | Five named bounded similarities; signed contrast has a distinct result type. |

`MeasureInfo`, `AlgorithmCard`, `SmootherCard` and typed method descriptors expose metadata.
Always inspect a method's current parameters and documented deviations before interpreting it.
Only implemented method families are listed here; this table is not the full eyesim baseline.

## Errors and persistence

Recoverable errors are sealed values with `message`, naming relevant operands. `preflight` checks
prerequisites without calculating scores. JSON schemas, method versions, content identity and
payload checksums are separate concepts. Unknown versions and missing registrations fail; decoding
does not silently replace a method or read files.

## API documentation

Generate per-module Scaladoc locally with `sbt kernelJVM/doc coreJVM/doc designJVM/doc planJVM/doc
codecJVM/doc ioJVM/doc`. Each module writes under its `target/scala-3.7.4/api` directory. No remote
API URL is advertised until an actual artifact/site is published. The guide covers workflows;
Scaladoc supplies symbol-level contracts. API docs may currently report unresolved-link warnings.

## Runtime and evidence boundary

Core modules are pure and cross-compile to JVM and Scala.js. Effects belong to `io` and `fs2`.
The guide's mdoc examples run on JVM; portable repository suites separately exercise JVM and JS.
Local documentation generation does not certify hosted CI, release signing or vendor conversion.
