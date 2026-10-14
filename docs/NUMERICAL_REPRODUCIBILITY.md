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
of the defect on another.

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
