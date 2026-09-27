# PM3.3 CSV column-resolution candidate

The first PM3.3 candidate is source commit
`ee3cbaf62b8cbe27555fe7a4f59773f6e3f04598`, based on PM3.2 commit
`2ad14421d1afbf9895f1bfe9b9a70c7599657e2e`. The PM3.2 large-workload
JFR recording sampled `SeqOps.indexOf` five times among 125 hot-method samples.
That profile was collected under competing load, so it is a bottleneck lead,
not a measured attribution of elapsed time.

`Delimited.validate` now resolves the required time, position, validity,
pupil, and marker column positions once after header validation. Row decoding
uses those integer positions while preserving the column names in diagnostics,
native row fields, missing-token policy, and the accepted/rejected partition.
Header errors still reject all rows before any column access. The reordered
header regression includes unrelated native columns, a tracked row, a lost
row, pupil missingness, and marker projection.

The focused JVM and Scala.js `DelimitedSuite` each passed 12 tests. On the candidate revision,
`headerCheckAll`, `scalafmtCheckAll`, `scalafmtSbtCheck`,
`githubWorkflowCheck`, JVM and JS `Test/compile` with `GITHUB_ACTIONS=true`,
and `checkBoundaries` passed.
The one-round frozen `pipeline-csv-ivt` smoke matched pymovements' canonical
fixation output in all six scale/mode pairs, including the 1,000,000-row
case. All twelve child samples completed and retained their raw outputs.

The candidate smoke is
`/private/tmp/eyes4s-pm3-3-20260927/smoke-001/exploratory.json`; the earlier
baseline smoke is
`/private/tmp/eyes4s-pm3-2-20260927/smoke-002/exploratory.json`.
Their SHA-256 digests are `f5f55a58de253b8f02ea29317a10f5ecefafef71852b6ac5cfd2359b4dd2baa6`
and `ae4e93f05c102aa8911bb42fb3e77065d1a69659cbbd3203355e01e818abdecd`,
respectively.
Each binds a clean source revision, compiled artifact, adapter, protocol, and
input digests. **They are not comparable performance evidence:** both report
competing compute and no thermal-settling attestation, and the candidate
smoke began on battery power while the baseline smoke began on AC. Each has
one round rather than the frozen twenty. No before/after speedup, allocation,
or RSS improvement is claimed. The candidate's exploratory large warm cell
still used about 6.0 GiB peak RSS and 2.93 s, versus pymovements' 0.70 GiB
and 0.28 s in that same contaminated run; this is a reason to continue
profiling, not a protocol score.

PM3.3 remains open. First complete PM3.2's twenty-round baseline on the
idle, AC-powered, thermally settled pinned machine. Then measure this exact
candidate with the same frozen workload and conditions, check the
predeclared effect and no-regression budgets, and add a reliable scaling or
allocation regression gate plus scheduled timing comparison. If the candidate
does not make a material improvement, profile the remaining CSV admission
and output-allocation costs before choosing another bounded optimization.
The other required PM3 workloads remain separate epic children.
