# Exact sorted distance correlation

`Distribution.distanceCorrelation` compares the paired cell values of two aligned
mass maps. It computes the biased sample estimator used by `energy::dcor`; the
cells are observations, and their spatial distances are not part of this measure.
Constant operands remain named errors under the existing relative-constancy
convention shared with Pearson and Spearman.

The implementation uses sorted row sums and a Fenwick tree of strict prefixes.
For paired observations `(x_i, y_i)`, let
`P = sum_{x_j < x_i, y_j < y_i} (x_i - x_j)(y_i - y_j)`. The cross-distance product
is `4P - 2(n sum(x_i y_i) - sum(x_i) sum(y_i))`. Sorted prefix sums provide each
distance-matrix row sum. Subtracting the two row-product terms and adding the
grand-product term gives the same double-centered covariance as the full matrices.
This is an exact algebraic replacement, with no subsampling, truncation or
approximate neighbor search. It takes O(n log n) time and O(n) storage. Each of
the two stable index mergesorts has at most `ceil(log2(n))` passes over n
indices. Each observation makes one Fenwick query and one update, each visiting
at most `floor(log2(n)) + 1` nodes. The remaining reductions are linear. Sorting
uses primitive index buffers; the Fenwick tree and row sums retain the two
parts of their arithmetic values in primitive arrays.

Equal x values are queried as a group before inserting that group; y queries use
strict ranks. Min/max scaling preserves correlation while avoiding underflow in
tiny map values. Double-double sums and products protect the subtraction of large
moments on sparse maps. Arithmetic order differs from the quadratic implementation,
so agreement with it is specified at absolute tolerance `1e-12`. Canonical operand
order makes reversed calls follow identical reductions within a runtime. This
does not assert a new bit-for-bit contract against another library.

The default `DistanceCorrelationLimit.maximumPairs` is now `Long.MaxValue`.
Explicit `distanceCorrelationWithin`, `MapSimilarityMethod.similarityWithin` and
`MapComparison.scales` limits remain admission policies on `n(n-1)/2`, even though
the algorithm does not enumerate these pairs. Grid disagreement precedes that
check, which precedes constancy. The estimator and saved method identity remain
unchanged. The decision is recorded in bead `q-sorted-dcor-budget`.

Portable conformance includes exhaustive nonconstant ternary vectors of lengths
2–4; dense, near-uniform, sparse and wide-range maps; paired permutations; and
65,536-cell binary maps with analytic absolute-Pearson answers. The existing
`MapComparisonSuite` retains the pinned independent and eyesim method-dispatch
fixtures. Historical quadratic timings remain in
[Execution responsiveness](EXECUTION_RESPONSIVENESS.md).

Run affected conformance and opt-in timing separately:

```sh
sbt 'compareJVM/testOnly *FastDistanceCorrelationSuite *DistanceCorrelationLimitSuite *MapComparisonSuite' \
    'compareJS/testOnly *FastDistanceCorrelationSuite *DistanceCorrelationLimitSuite *MapComparisonSuite'
sbt 'compareJVM/Test/runMain eyes4s.compareconsumer.DistanceCorrelationBenchmark --full'
sbt 'project compareJS' \
    'set Test / mainClass := Some("eyes4s.compareconsumer.DistanceCorrelationBenchmark")' \
    'set Test / scalaJSUseTestModuleInitializer := false' \
    'set Test / scalaJSUseMainModuleInitializer := true' \
    'Test/run'
```

The timing tool warms each size and emits three measurements with scores and bit
patterns. Wall-clock measurements are evidence for their recorded machine and
runtime, not portable deadlines or cancellation guarantees. The call remains
synchronous.

The sorted identities and tie treatment follow
[Chaudhuri and Hu, *A fast algorithm for computing distance correlation*](https://arxiv.org/abs/1810.11332)
and [Huo and Székely, *Fast Computing for Distance Covariance*](https://arxiv.org/abs/1410.1503).

The [2026-10-14 receipt](evidence/distance-correlation-20261014.json) records
47 affected tests per runtime, matching JVM/Node score bits at all four timing
sizes, and the complete measured ranges. On arm64 macOS with Temurin 25.0.4.1 and
Node 24.21.0, median milliseconds from three calls were:

| Cells | JVM ms | Node ms |
|---:|---:|---:|
| 16,384 | 51.90 | 123.09 |
| 65,536 | 87.38 | 584.22 |
| 262,144 | 569.37 | 2,332.63 |
| 1,048,576 | 2,869.77 | 11,058.01 |

These are synchronous calls. Large grids still take seconds, particularly on
Node; removing the former default cap does not make execution cancellable.
The receipt retains startup and garbage-collection variation rather than
presenting the fastest call as a deadline.
