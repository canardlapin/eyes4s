# 1.0 pre-publication readiness

The [completed pre-publication review](evidence/release-20261027/review.json) maps PRD A-1–A-27 to the [qualified candidate](evidence/release-20261027/qualification/qualification.json). A-16 publication and first-baseline acceptance remain pending.

The release audit is a requirement-to-evidence review for PRD A-1–A-27. Its current
candidate receipt belongs in `docs/evidence/release-20261027/`; a list of source
files alone is not a passing audit. The full candidate gate must record SHA/tree,
API inventory/probes, JVM/JS tests and boundaries, executable docs, and the isolated
artifact consumer. Supported JDK 17/21 hosted checks supplement the local JDK 25
court. Generated API evidence is re-recorded, never hand-merged.

The previous qualified implementation is `a74f86b758653de67235ea0f0d9e4ca0652910fd`,
landed with evidence at `6b02b64256db6f36037bb5b714d09eeae705b947` and tracker closure
at `01628cbd441ad5de1599adf3c43a0d129ace2a6f`. The packaging tranche changes Studio
and build/CI tooling; it introduces no scientific-library implementation change.
The combined candidate was requalified after the example-loader fix and build/CI changes; its receipts do not rely on earlier green runs.

| PRD | Evidence route | Acceptance boundary |
| --- | --- | --- |
| A-1: Supported JVM and Scala.js builds/tests | [build.sbt](../build.sbt) | Candidate gate plus the mapped contract/evidence review. |
| A-2: Library effect and dependency boundaries | [StudioLint.scala](../project/StudioLint.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-3: Neutral kernel vocabulary | [build.sbt](../build.sbt) | Candidate gate plus the mapped contract/evidence review. |
| A-4: Published numerical laws with named tolerances | [scala](../laws/src/main/scala) | Candidate gate plus the mapped contract/evidence review. |
| A-5: Negative compile-time claims | [check-docs.py](../tools/check-docs.py) | Candidate gate plus the mapped contract/evidence review. |
| A-6: MultiMatch no-grouping reference conformance | [MultiMatchConformanceSuite.scala](../compare/src/test/scala/eyes4s/compare/MultiMatchConformanceSuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-7: Finite streaming and pure runner agreement | [StreamingSuite.scala](../fs2/src/test/scala/eyes4s/fs2/StreamingSuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-8: Portable deterministic RNG | [CrossPlatformDeterminismSuite.scala](../design/src/test/scala/eyes4s/design/CrossPlatformDeterminismSuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-9: Portable end-to-end results | [FixationJourneySuite.scala](../tools/study-consumer/src/test/scala/example/FixationJourneySuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-10: Required offline reference baseline and justified divergences | [baseline.json](../tools/r-parity/baseline.json) | Candidate gate plus the mapped contract/evidence review. |
| A-11: Complete public EDF-derived ASC to Scanpath | [PublicAscEndToEndSuite.scala](../io/src/test/scala/eyes4s/io/PublicAscEndToEndSuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-12: Executable public documentation | [site-docs](../site-docs) | Candidate gate plus the mapped contract/evidence review. |
| A-13: Public invariants, scale and units documentation | [inventory.json](../tools/api-audit/inventory.json) | Candidate gate plus the mapped contract/evidence review. |
| A-14: Pure failure values, cardinality and contract review | [AGENTS.md](../AGENTS.md) | Candidate gate plus the mapped contract/evidence review. |
| A-15: Abstractions have instances and laws/conformance | [evidence.json](../tools/api-audit/evidence.json) | Candidate gate plus the mapped contract/evidence review. |
| A-16: Compatibility policy and real Central/baseline receipts | [BINARY_COMPATIBILITY.md](../docs/BINARY_COMPATIBILITY.md) | Policy prepared; Central publication and first real MiMa baseline remain pending. |
| A-17: Repository and migration documentation | [README.md](../README.md) | Candidate gate plus the mapped contract/evidence review. |
| A-18: Versioned codec round trips | [SchemaCompatibilitySuite.scala](../codec/src/test/scala/eyes4s/codec/SchemaCompatibilitySuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-19: Saved plan reload/execution identity | [FixationJourneySuite.scala](../tools/study-consumer/src/test/scala/example/FixationJourneySuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-20: Named missing viewing prerequisite | [ProjectPrerequisitesSuite.scala](../plan/src/test/scala/eyes4s/plan/ProjectPrerequisitesSuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-21: Method/detector/smoother metadata | [MethodDescriptorSuite.scala](../plan/src/test/scala/eyes4s/plan/MethodDescriptorSuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-22: Observed ASC messages, not planned timing | [EyeLinkAscNativeTimelineSuite.scala](../io/src/test/scala/eyes4s/io/EyeLinkAscNativeTimelineSuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-23: Generic-key metadata/timeline codec contracts | [scala](../codec/src/test/scala) | Candidate gate plus the mapped contract/evidence review. |
| A-24: Complete required capability map and template route | [EYESIM_CAPABILITIES.md](../docs/EYESIM_CAPABILITIES.md) | Candidate gate plus the mapped contract/evidence review. |
| A-25: Three isolated public-API journeys | [verify.py](../tools/study-consumer/verify.py) | Candidate gate plus the mapped contract/evidence review. |
| A-26: Downstream extensions, laws and headless application consumption | [ConsumerLawsSuite.scala](../tools/study-consumer/src/test/scala/example/ConsumerLawsSuite.scala) | Candidate gate plus the mapped contract/evidence review. |
| A-27: Persistence identity, invalid schemas, migration/refusal and exports | [SchemaCompatibilitySuite.scala](../codec/src/test/scala/eyes4s/codec/SchemaCompatibilitySuite.scala) | Candidate gate plus the mapped contract/evidence review. |

The machine-readable [requirement map](../tools/release-audit/requirements.json)
includes supporting references beyond the first link in each row. The [last full
qualification](studio/evidence/20261026-hardening/qualification/qualification.json)
retains local and hosted logs, failures, skips and scope limits.

## Operational evidence and limits

- The [recording importer court](formats/eyelink-asc.md#streaming-memory-and-performance-evidence) records a
  two-hour 2000 Hz binocular stream, physical lines/bytes, elapsed time, heap and
  allocation limits on its named machine. The clean scheduled
  artifact at `1872c479` supplements the retained historical baseline; its source
  archive identity and all original thresholds were independently verified.
- [Archive memory](ARCHIVE_MEMORY.md) qualifies a bounded density workload and
  independent writer/reader processes under a 1 GiB heap. GC live-heap samples
  are not peak process memory, and neither establishes a maximum supported study.
- [Studio performance](studio/PERFORMANCE.md) qualifies normal and tenfold
  interaction workloads with exact inputs, timings and memory samples. The tenfold
  completed live result is usable, but its oversized cold archive is explicitly
  refused by the existing budget. Preserve that support limit in release notes.
- [Scaladoc runtime](SCALADOC_RUNTIME.md) separates the current observed JDK 25
  pass from the older failure and the supported JDK 17/21 CI route.

## Decisions that remain outside this audit

`rel-audit` is satisfied by the linked candidate checks and evidence review.
[A-16 compatibility policy](BINARY_COMPATIBILITY.md) is preparatory: `rel-publish`
and `rel-mima` remain open until real Central artifacts and an active baseline
comparison are proved. A locally packaged artifact is not a Central release.

Studio G2 still needs a provenance-declared real-study production journey. Its
private source audit cannot invent acquisition geometry or event origins.
S10.5 needs actual VoiceOver/physical Retina observations; S10.7 needs the specified
clean-machine installation (unsigned internal distribution was selected by the owner); S10.8 needs five real researchers.
The [prepared manual packet](studio/evidence/20261027-release/manual-checks.json)
starts unobserved. Their implementation and test infrastructure do not close these
gates. Meridian integration remains deferred.
