# Numerical reproducibility

The native Gaussian kernel evaluates its exponential with shared fdlibm 5.3
arithmetic. `Bandwidth.silverman`, `Bandwidth.scott` and `IqrBandwidth.suggest`
also share the fdlibm evaluation of `pow(n, -1.0 / 6.0)` for positive Int-sized
observation counts. Neither seam delegates to the runtime's native `exp` or
`pow`. The bandwidth exponent is the binary64 value of `-1.0 / 6.0`, preserving
the existing sample-size convention rather than substituting another estimator.

These primitives use the same elementary operations and explicit bit-word
manipulation on JVM and Scala.js. Finite results have an exact bit contract
against `StrictMath`'s fdlibm 5.3 definition. Nonfinite exponential witnesses use
canonical NaN bits; the public smoother only supplies finite kernel arguments.
The power implementation is restricted to the positive observation counts its
bandwidth callers admit. Its logarithm and scaling stay in the normal finite
range over that domain.

The original Sun notice is preserved in `KernelMath.scala`. The independent
oracle generator compiles a standalone Java program which calls `StrictMath`
and never loads eyes4s classes:

```sh
python3 tools/numerical-reference/generate.py --check
sbt 'surfaceJVM/testOnly *KernelMathSuite *KernelMathJvmSuite' \
    'surfaceJS/testOnly *KernelMathSuite'
```

The portable fixtures include argument-reduction, overflow and underflow
boundaries; positive-count interval and exponent boundaries through
`Int.MaxValue`; and exact digests of 634,949 Gaussian taps and powers for counts
1–100,000. JVM tests additionally compare those primitives directly with
`StrictMath`. On the qualified arm64 host, native JVM math matched these probes;
Node 24.21.0 native `exp` differed on 276 taps and native `pow` on 11,947 counts.
An equivalent native-math mutant on one architecture cannot prove the absence
of the defect on another. A Temurin21 x86_64 JVM executed under macOS translation
passed 42 focused surface tests, including the same exact oracle comparisons.
Its native `exp` differed on 57,842 taps and native `pow` on 11,925 counts. This is
an Intel JVM execution witness on the arm64 host, not a native Intel hardware run.

The designated A-9 Gaussian pipeline uses the existing isolated artifact
consumer. Its fixation CSV becomes typed scanpaths and a saved plan; the real
Gaussian maps produce pairs, reductions and contrasts. The cosine and scaled
extension journeys retain plan/input codec, archive, fresh-reader, progress and
cancellation checks. Cross-runtime evidence now includes every density, pair,
reduction and contrast bit in those journeys. A separate saved/reloaded Gaussian
plan uses pooled fixation occupancy to choose Silverman and Scott bandwidths
before estimation. Its bandwidth and complete result bits are also compared
exactly. A missing or failed outcome cannot stand in for a numerical result.

The artifact consumer keeps two distinct contracts:

| Comparison | Contract |
|---|---|
| Shared exponential and observation-count power versus the independent fdlibm oracle | Exact bits |
| Designated saved Gaussian pipelines on JVM versus Scala.js | Exact plans, input identities and result bits |
| Gaussian results versus independently pinned reference-library fixtures | Existing absolute tolerance `1e-12` |
| Artifact content hashes and addresses | Exact encoded bytes |

This does not claim exact equality for every numerical operation in the library.
For example, the IQR rule still uses a runtime `hypot` in its spread calculation;
its power term is exact under this contract, while a complete IQR recommendation
is not thereby designated an exact cross-runtime pipeline. Other measures retain
their declared tolerances. Export digests certify their bytes, and tolerances
never weaken stored-content verification.

Run the complete isolated consumer with
`python3 tools/study-consumer/verify.py`. Its receipt records the actual packaged
source revision, artifacts, runtime outcomes and exact comparisons. Primitive
conformance or consumer compilation alone does not qualify the complete A-9
pipeline; successful packaged execution on both runtimes is required.

The 2026-10-14 packaged run at `deec89459f094e57c171fc9186bfc3885f6caf75`
passed both consumer suites with exact designated pipeline comparisons. Its
[summary](evidence/numeric-pipeline-20261014.json) binds the
[complete receipt](evidence/numeric-pipeline-20261014-receipt.json), including
raw bits, package digests and executed source revision. Architecture conformance
and the complete landing gate remain separate qualification steps.

The same published jars and shared Gaussian consumer sources also passed 53
focused consumer tests on the translated x86_64 JVM. The
[Intel receipt](evidence/numeric-x64-20261014.json) retains the actual result
bits, which matched both the arm64 and Scala.js receipts exactly for the
designated saved plans and complete cosine/scaled pipelines. Its scope excludes
rerunning the fresh-process archive readers on Intel.

## Native template and decomposition norms

Native QR column scaling, Householder norms and active-set admission use the
shared `LeastSquaresNormConvention.FdlibmHypotV1`. The implementation adapts
[fdlibm e_hypot.c](https://www.netlib.org/fdlibm/e_hypot.c), preserving its Sun
notice, arithmetic order, scaling and word splitting. It uses no fused
multiply-add or runtime `hypot` call. Existing training-recipe digests retain
their scientific-data meaning.

The complete template/decomposition family plans bind the norm convention in
their saved data and plan identities and refuse an unsupported convention.
Imported fixed-feature receipts retain their declared external backend;
checking a receipt does not perform a native refit. Native saved results keep
exact replay verification, including learned-template cells and provenance.

The JVM conformance suite compares 100,000 deterministic raw binary pairs and
20,000 close-exponent pairs with both `StrictMath.hypot` and `Math.hypot`.
The portable corpus contains 1,024 independent JVM oracle cases covering
signed zero, subnormals, overflow/scaling boundaries, nonfinite semantics and
varied finite values. Its binary rows have SHA-256
`3e7f6f86895e3c33397f9cca5844ac4630ce6781c3d1ed3f33a028b5b2f63ab9`.
NaN results are compared through canonical NaN bits; public solvers refuse
nonfinite inputs before computing norms.

```sh
python3 tools/numerical/hypot.py
sbt 'designJVM/testOnly *ReproducibleHypotSuite *ReproducibleHypotJvmSuite' \
    'designJS/testOnly *ReproducibleHypotSuite' \
    'codecJVM/testOnly *TemplateFitCodecSuite *DecompositionCodecSuite' \
    'codecJS/testOnly *TemplateFitCodecSuite *DecompositionCodecSuite'
```

These checks accompany the existing rational and pinned R decomposition and
template oracles under their named tolerances. They do not designate native
`hypot` calls elsewhere in the library as exact across runtimes.


## Shared Euclidean overlap norm

Euclidean fixation overlap now uses the neutral kernel's shared fdlibm `hypot` operation
order, named `FixationNormConvention.FdlibmHypotV1`; Manhattan distance names
`AbsoluteAxisSumV1`. The norm convention is encoded and checked in saved method parameters.
The observed native calculation `hypot(24,4)` returned 24.331050121192877 on JVM and
24.331050121192874 on Scala.js. The shared kernel and a public overlap regression require the
JVM reference value exactly on both runtimes. Its 1,024 independently generated portable cases
retain SHA256 `3e7f6f86895e3c33397f9cca5844ac4630ce6781c3d1ed3f33a028b5b2f63ab9`;
120,000 JVM raw/close-exponent pairs also agree bit for bit with StrictMath and Math.

Saved scanpath results use exact replay or typed refusal. The complete fixtures replay on
both runtimes, including the original overlap query distances and transport residuals.
They do not establish a universal exactness claim for every input to native elementary
operations elsewhere in MultiMatch, transport or bandwidth estimation. Reference tolerances
have not been widened to accommodate archive differences.
