# eyes4s-laws

Published Discipline rule sets, ScalaCheck generators, detector validation
courts, and versioned scientific-validation artifacts.

munit, scalacheck and discipline are MAIN-scope dependencies of this module,
not Test: these suites are library code that downstream authors run against
their own instances.

Generate a typed report and canonical machine-readable document without parsing
test output:

```scala
val build = ValidationBuildIdentity.release(
  libraryVersion = "0.1.0",
  sourceRevision = "0123456789abcdef0123456789abcdef01234567"
)

val oracles = ValidationOracleInputs.fromCanonicalFiles(
  detectorConformanceJson = detectorReferenceContents,
  engbertKernelsJson = engbertReferenceContents
)

val json = for
  identity <- build
  inputs   <- oracles
  report   <- ScientificValidationArtifact.generate(
    identity,
    ValidationPlatform.Jvm,
    inputs
  )
yield report.canonicalJson
```

The report retains algorithm cards and pinned source references, exact
configurations, quantitative conformance fixtures, deterministic synthetic
self-check metrics, metamorphic-law identities, warnings, and SHA-256 input
digests. `ValidationBuildIdentity.workingTree` records a base revision plus an
explicit dirty state; `release` records the clean source commit and avoids
claiming that an artifact contains its own commit identity.

The checked example is `../validation/eyes4s-validation-0.1.0.json`. Its JVM
drift suite hashes `../tools/detector-conformance/reference.json` and
`../tools/engbert-kernels/reference.json` independently, reconstructs the
typed report, and requires exact canonical JSON equality. Before publication,
the example moves from `working-tree` to `release` in an artifact-only commit
whose `source_revision` names the already committed source and oracle files.

After the source commit is published, generate that release document from a
clean checkout at the source commit with:

```sh
sbt 'lawsJVM/Test/runMain eyes4s.laws.GenerateScientificValidationArtifact 0.1.0 <source-sha> validation/eyes4s-validation-0.1.0.json'
```

The JVM-only writer refuses to run unless `<source-sha>` is the current clean
`HEAD`, verifies the two oracle files byte-for-byte against that commit, and
writes the canonical JSON atomically. Commit the generated JSON and
`ScientificValidationFilesSuite.scala` together afterwards; the source commit
cannot contain a drift test for an artifact whose source identity does not yet
exist.
