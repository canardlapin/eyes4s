# Jackson dependency remediation, 2026-10-05

Tracker: `bd-01M473JQX6Q232YKCJTG478R68`.

The current submitted GitHub SBOM contains Jackson core/databind/datatype-jsr310 2.21.0 and
annotations 2.21. Local `ioJVM/update` resolution identifies the runtime path as
`eyes4s-io` → optional `arrow-vector` 19.0.0 → Jackson. Arrow's
[19.0.0 parent POM](https://repo.maven.apache.org/maven2/org/apache/arrow/arrow-java-root/19.0.0/arrow-java-root-19.0.0.pom)
imports Jackson BOM 2.21.0. The resolved update report confirms those versions in `optional`,
`compile-internal`, `runtime-internal` and `test-internal`; the plain Compile dependency tree
omits optional dependencies, so it alone cannot establish the transport's exposure.

The same report contains a separate build-tool path: ScalaDoc 3.7.4 → Jackson YAML/core/databind
2.15.1, and ScalaDoc → liqp 0.8.2 → datatype-jsr310 2.12.1. Those versions are not in the submitted
SBOM, but several recorded advisories also cover them. Build-wide dependency overrides align
this tool configuration and the Arrow runtime configuration without introducing a Jackson
dependency into pure or Scala.js module graphs.

`ArrowResultExport` constructs Arrow schemas and writes FlatBuffer IPC; it does not accept
external JSON through Jackson. Its metadata and JSON cells use Circe and the checked result-table
layer. Arrow's `Schema` initializes a Jackson mapper and exposes JSON operations in its own API,
so Jackson remains a runtime dependency whenever the optional transport is installed. The
analysis does not justify dismissing any alert. The pinned core advisory
[GHSA-7hhh-6rmp-j9qf](https://github.com/FasterXML/jackson-core/security/advisories/GHSA-7hhh-6rmp-j9qf)
concerns malformed input through a DataInput parser; eyes4s' writer does not call that parser.

The selected versions follow the upstream
[2.21.7 BOM](https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.21.7/jackson-bom-2.21.7.pom):
core, databind, datatype-jsr310 and the build-tool YAML module are 2.21.7; annotations remains
2.21. The [2.21 release notes](https://github.com/FasterXML/jackson/wiki/Jackson-Release-2.21)
identify the branch as LTS and retain the previous Java baseline. Arrow remains 19.0.0.

All twenty recorded public advisory records were read on 2026-10-05. The applicable 2.21 patch
floors are below; 2.21.7 satisfies every recorded range.

| Advisory | Artifact | First patched 2.21 version |
| --- | --- | --- |
| [GHSA-7hhh-6rmp-j9qf](https://github.com/advisories/GHSA-7hhh-6rmp-j9qf) | core | 2.21.7 |
| [GHSA-p6pp-m3f8-5c89](https://github.com/advisories/GHSA-p6pp-m3f8-5c89) | core | 2.21.7 |
| [GHSA-cxp5-3px4-pw24](https://github.com/advisories/GHSA-cxp5-3px4-pw24) | databind | 2.21.7 |
| [GHSA-wv8q-qhhj-9h54](https://github.com/advisories/GHSA-wv8q-qhhj-9h54) | databind | 2.21.7 |
| [GHSA-gx83-3vf8-gh7j](https://github.com/advisories/GHSA-gx83-3vf8-gh7j) | databind | 2.21.6 |
| [GHSA-q4xh-88c3-wmh7](https://github.com/advisories/GHSA-q4xh-88c3-wmh7) | databind | 2.21.6 |
| [GHSA-wjgm-6hv5-3cvf](https://github.com/advisories/GHSA-wjgm-6hv5-3cvf) | databind | 2.21.6 |
| [GHSA-vvgp-rfg2-7rr6](https://github.com/advisories/GHSA-vvgp-rfg2-7rr6) | databind | 2.21.5 |
| [GHSA-5gvw-p9qm-jgwh](https://github.com/advisories/GHSA-5gvw-p9qm-jgwh) | databind | 2.21.5 |
| [GHSA-r7wm-3cxj-wff9](https://github.com/advisories/GHSA-r7wm-3cxj-wff9) | core | 2.21.4 |
| [GHSA-mhm7-754m-9p8w](https://github.com/advisories/GHSA-mhm7-754m-9p8w) | databind | 2.21.5 |
| [GHSA-3pjw-73gf-8qr5](https://github.com/advisories/GHSA-3pjw-73gf-8qr5) | databind | 2.21.4 |
| [GHSA-5hh8-q8hv-fr38](https://github.com/advisories/GHSA-5hh8-q8hv-fr38) | databind | 2.21.4 |
| [GHSA-9fxm-vc8v-hj55](https://github.com/advisories/GHSA-9fxm-vc8v-hj55) | databind | 2.21.4 |
| [GHSA-5jmj-h7xm-6q6v](https://github.com/advisories/GHSA-5jmj-h7xm-6q6v) | databind | 2.21.5 |
| [GHSA-hgj6-7826-r7m5](https://github.com/advisories/GHSA-hgj6-7826-r7m5) | databind | 2.21.4 |
| [GHSA-rmj7-2vxq-3g9f](https://github.com/advisories/GHSA-rmj7-2vxq-3g9f) | databind | 2.21.4 |
| [GHSA-j3rv-43j4-c7qm](https://github.com/advisories/GHSA-j3rv-43j4-c7qm) | databind | 2.21.4 |
| [GHSA-rcqc-6cw3-h962](https://github.com/advisories/GHSA-rcqc-6cw3-h962) | databind | 2.21.4 |
| [GHSA-72hv-8253-57qq](https://github.com/advisories/GHSA-72hv-8253-57qq) | core | 2.21.1 |

The build also declares patched core/databind/datatype-jsr310 as optional JVM dependencies in the
published artifact, because sbt overrides are not published into its Maven POM. Applications
opting into Arrow explicitly select these versions, as shown in
[the transport setup](../RESULT_EXPORTS.md#jvm-arrow-setup-and-ownership). The isolated consumer
adds Arrow and patched Jackson only to JVM test scope and checks public artifact export/readback,
exact large integers, nulls, metadata and Arrow's Jackson schema JSON round trip.

Local validation used JDK 21.0.12.1 and sbt 1.11.7 on the integrated working tree:

- `ioJVM/update` resolved patched Jackson in optional/runtime/test and ScalaDoc configurations.
- `ioJVM/makePom` declared patched core/databind/datatype-jsr310 with `optional=true`.
- `ArrowResultExportJvmSuite` passed all four tests, including resource-failure paths.
- `ioJVM/doc` succeeded with the patched ScalaDoc tool graph; it reported unresolved Scaladoc
  links and a repeated classpath flag, with no Jackson linkage failure.
- `BaselineExportMain` regenerated exports; all 67 files agreed byte for byte with
  `tools/result-export/receipt-v1.json`.

The new isolated consumer Arrow suite, the full library/CI gates and the committed SHA/tree
receipt remain integration checks; this evidence does not claim they have run.

Remote completion requires dependency submission from the eventual committed SHA and a fresh
alert-state read for that SHA. The local GitHub credential can read the SBOM, but listing
repository Dependabot alerts returned HTTP 403 for insufficient scope. No alert was dismissed.
