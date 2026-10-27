# Binary and schema compatibility

Eyes4s is currently pre-release. No Maven Central baseline is asserted by local
`publishLocal` or by a green MiMa task with an empty previous-artifact set.
`rel-publish` must retain a Central download/consumer receipt, after which
`rel-mima` must configure and exercise the first real baseline.

For the 1.x release line, patch and minor releases preserve the published JVM and
Scala.js binary API for each supported Scala binary version. Breaking API changes
require a major release. This applies to the library modules, including published
law suites; Studio is an unpublished application and is not a library API.
Pre-1.0 minor releases may break APIs, but must document migrations. Patch
releases must not silently remove or change public contracts.

MiMa is a binary compatibility check, not a proof of source compatibility,
scientific equivalence or document compatibility. CI also compiles documentation,
negative type tests and isolated downstream consumers on the supported platforms.
The library supports JVM 17 and 21 and Scala.js; Studio uses JDK 25. Dependency and
module boundaries are enforced by the build, as documented in `AGENTS.md`.

When the first artifacts are published:

1. Record exact coordinates, versions and downloaded hashes for every published
   module/platform. Resolve them from Maven Central in a clean consumer.
2. Set the previous-version baseline through the sbt-typelevel/MiMa build settings
   and inspect the resolved `mimaPreviousArtifacts` for every published project.
3. Run `mimaReportBinaryIssues` on JVM and Scala.js with those actual artifacts.
   Prove the check is active with a temporary incompatible API mutation; preserve
   its failure, restore the source and retain the green comparison.
4. Any proposed exclusion must name the symbol, affected consumers and migration
   rationale. A broad filter or empty baseline is not acceptance evidence.

Persistence is independently versioned. Follow [domain codec compatibility](DOMAIN_CODECS.md#schema-compatibility)
and [saved-study contracts](SAVED_STUDIES.md): explicit schema identities, pinned
previous-version fixtures, exact artifact digests, and named migration or refusal.
Unknown versions, identities or nodes are never guessed. Package version numbers
must not be substituted for schema versions.

Numerical changes need their own evidence even when the binary API is unchanged.
Retain declared units, conventions and tolerances; record changed reference
results and independently justify differences. Do not widen tolerances or change
missing-data/denominator behavior to preserve an apparent compatibility result.
