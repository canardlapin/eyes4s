# Add a comparison without changing the interpreter

The [isolated consumer](../tools/study-consumer) adds a scaled cosine comparison with its own typed
multiplier, score and trial key. It imports packaged eyes4s artifacts; it has no project dependency
on the library's source tree and uses the public `example` package.

A method author supplies:

1. `StudyMethod[P, U, S, D]`: a stable method identity/version, typed parameter description and a
   comparison constructed from `P`, with `ScoreMean[S]` and `Contrastable[S, D]` instances.
2. A `StudyLayout[K]` defining participant, stimulus and phase projections, with a stable layout
   identity, `KeyDigest[K]`, and a lawful key ordering.
3. Conditional `VersionedCodec[K]` and `VersionedCodec[P]` instances. `StudyCodec` captures these
   with the method before registration; runtime lookup never reconstructs their types by casting.
4. `ScoreColumns[S, D]` if tabular output is needed. Export rejects the wrong component count or
   non-finite projected values instead of producing malformed or misleading rows.

The example runs the published `CodecLaws` against custom parameters, custom keys and complete plans,
and `ContrastLaws` against its custom score. It then registers, saves, reloads, executes and exports a
three-scale study. Missing methods, duplicate registration, invalid parameters and missing schemas
are explicit failures. The method can be removed from the registry without modifying its interpreter.

## Spatial scales

`StudyEstimate.Gaussian(sigma, edges)` carries a standard deviation in frame units and an explicit
edge policy. Each scale retains its own estimates, failures and contrasts. Scales are not averaged
or silently dropped. Plans preserve scale order and reject duplicate scale declarations.

The [independent oracle](../tools/r-parity/generate_multiscale.py) evaluates the two-by-two fixture in
60-digit decimal arithmetic. With `t = exp(-1 / (2 sigma^2))`, a cell with mass count `a`, its two
neighbors `b,c`, and diagonal `d` has normalized mass
`(a + t(b+c) + t^2 d) / (total (1+t)^2)`. This closed form needs no convolution implementation.
It supplies 36 maps and 18 contrasts at sigma 0.5, 1 and 2 pixels.

All four cells in this fixture are corners. Source edge correction is consequently constant and
cancels on normalization, so both edge policies share these particular normalized targets. This
fixture does not establish general equivalence of the edge policies or parity with an R KDE.

The consumer's positive multiplier has an independent prediction: every contrast is multiplied by
that value at every scale. This checks the extension's parameter persistence and execution together.
The application-facing result remains typed; adding a GUI belongs in its separate repository.

Run `python3 tools/study-consumer/verify.py` from the repository root to publish the development
artifacts locally and execute the consumer in a fresh directory. The verifier checks both
classpaths and compares emitted JVM/Scala.js numerical results, keeping logs and a receipt in the
printed directory. Input digests, decoded plan values and binned result bits must match exactly;
Gaussian results use the named `1e-12` absolute tolerance. The fixture generator's `--check` also
checks the consumer's copied input and target data.
