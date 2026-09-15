# Save and rerun a study

A saved study records which fixation artifact to analyze, which phases to compare, the spatial
frame and grid, duration or uniform weighting, the estimation scales, the comparison method and
its parameters, and the reduction failure policy. Loading JSON does not run the analysis or read
files. Supply the referenced input explicitly when you run it.

Start with the [fixation study guide](FIXATION_STUDIES.md). Its exact
[Scala source](examples/StudyGuide.scala) compiles and runs on JVM and Scala.js. The ordinary
`StudyPlan.cosine` and `StudyCodecs.cosine` constructors supply the built-in method and key schemas;
the scientific choices remain arguments. These constructors use the same `StudyPlan.of` interpreter
and `StudyCodec` machinery available to method authors.

## Inspect, compare, execute

`plan.description` exposes named fields with typed parameter values. `plan.diff(other)` reports
changed fields with their before/after values. Plans have structural equality under those declared
fields. A change of weighting, bandwidth, phase, failure policy, input digest or registered method
version is visible without executing either plan.

`plan.prerequisites(None)` reports the missing input artifact. Supplying the wrong input reports
both the expected and actual digest. `plan.run(input)` executes each declared scale independently.
The result retains estimation failures, keys excluded by phase selection, and the full matched and
control analyses, including pair failures and per-key denominators. Input identity uses the existing
portable `ContentHash`; it is not a cryptographic authentication guarantee.

Input artifacts contain the fixation data, typed keys, frames and trial clocks. Their JSON reference
contains only the content digest. The importer or application resolves the artifact; pure plan and
codec modules never load files or access a network. The first workflow supports existing fixation
summaries. Detection plans and a general artifact storage service remain separate work.

## Versions and extensions

The JSON envelope has a schema identifier and version. Its payload separately records the method
identifier/version, key schema, key layout, and parameter schema. Missing or unsupported versions
are explicit failures. The pinned [version-one project](../codec/src/test/resources/eyes4s/study-v1.json)
is exercised by the portable codec suite, so changing defaults cannot silently reinterpret it.
This first schema has no historical migration; unsupported schemas are rejected precisely.

`VersionedCodec[A]` encodes and decodes with `Either`, including checked encoding for values belonging
to a different registration. Generic maps use arrays of typed key/value entries and reject duplicate
keys, retaining all duplicate occurrence indices. They never convert user keys to JSON field names.

`StudyMethod[P, U, S, D]` belongs to `plan`; `StudyCodec` pairs it with versioned key and parameter
codecs in `codec`. This keeps the dependency direction acyclic. Registry lookup selects a closure
that already contains those types. A loaded method retains abstract score and difference types;
there is no cast or universal untyped parameter object. Use the typed `StudyCodec` directly when your
program needs the concrete custom score type. See [extending studies](EXTENDING_STUDIES.md).

Method and layout authors must change their registered versions when behavior changes. Every
score-affecting parameter must appear in the typed description. The library cannot infer the behavior
of arbitrary user-supplied comparison functions or key projections.

## Reproducibility contract for this workflow

The isolated consumer compares actual JVM and Scala.js output. Input digests, decoded JSON values
and binned contrast bit patterns must match exactly. Gaussian contrasts are checked against the
independent oracle and across runtimes with absolute tolerance `1e-12`. JSON text can differ in
numeric spelling (`1.0` versus `1`); byte-identical JSON serialization is not the contract.
Within either runtime, saving/reloading the same plan preserves its numerical results exactly.
