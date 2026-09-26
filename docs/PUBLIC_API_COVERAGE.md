# Public API execution audit

The audit inventories compiled APIs and connects them to successful test execution.
It covers all twelve published modules on JVM and Scala.js. Numerical correctness
still requires the independent fixtures, analytic oracles, laws and mutation tests
linked from the method guides; an invocation alone does not establish correctness.

Run the same gate used by the scheduled `evidence.yml` workflow (weekly and on demand; it is
too slow for every push):

```sh
python3 tools/api-audit/run.py
```

The isolated tooling project uses the repository's Scala version to inspect every
main-scope TASTy file on both platforms. `tools/api-audit/inventory.json` freezes
intentional public declarations, including source-level parameter/result/alias types, overload signatures, extensions,
instances, public constructors, fields, inherited library methods and deprecation.
`target/api-audit/inventory-{jvm,js}.json` retain the full compiler inventory and
source positions, including internal and generated declarations. The only
platform-specific declarations are in JVM IO sources; a new exception needs review.

The runner forks each JVM suite with its own JaCoCo data file. Bytecode owners,
method descriptors and source positions resolve compiler declarations to method
probes. Overload ambiguity fails rather than guessing. Abstract members require an
executed concrete implementation; compiler bridges are followed to their actual
bytecode targets. An abstraction also names a concrete implementation class and a
successful law or conformance suite in `tools/api-audit/evidence.json`.

The evidence catalog retains exact test names from successful XML reports on both
platforms. The gate rejects missing, failed, skipped, renamed or stale reports,
uninvoked mapped methods, disappeared dispatch witnesses, changed public APIs and
missing abstraction instances. Reports and probes are regenerated from scratch;
source identity is checked before and after execution. A candidate receipt names
the base Git SHA and a content fingerprint because this workspace can contain
uncommitted changes. It does not describe a clean commit or a hosted CI result.

## Classifications

Every declaration remains visible in the generated report. Exclusions from direct
runtime method accounting have specific reasons:

| Classification | Reason and evidence |
| --- | --- |
| Internal | Compiler visibility includes private or package-restricted ownership. |
| Compiler generated | TASTy marks a symbol synthetic or artifact; wrappers are represented by their source declaration. |
| Structural accessor or constructor | Case/product fields and generated construction are checked through the containing value's constructor, equality, codec and conformance tests. |
| Module reference or initializer | Scala object initialization scaffolding; its public operations are counted individually. |
| Enum case value | Data alternative, exercised through the containing sum's conformance and diagnostic tests. |
| Default argument getter | Compiler helper for a declared default; the source method is counted. |
| Type-level or equality witness | No meaningful runtime invocation; compile-time consumer probes test admission and rejection. |
| Abstract constructor | Cannot be invoked independently; its concrete inhabitants are inventoried. |
| Abstract member | Requires invocation through a concrete implementation, including erased bridges. |
| Abstraction or container | Concrete implementation plus named law/conformance evidence for abstract types. |

The five `Unit2D` marker types intentionally have no runtime inhabitants. They are
phantom indices: geometry's `typeCheckErrors` probes and the external session
consumer reject unit mixing. This is an explicit exception, not an unimplemented
runtime interface. `Kernel` has the cosine instance and a Gram-matrix conformance
suite using independent normalized feature vectors and signed coefficients.

## Contract probes and limits

The complete named-test catalog preserves the existing constructor failure tests,
not just the new convenience-method probes. Geometry tests reject nonfinite,
degenerate and overflowing bounds. Recording tests reject empty samples,
nonmonotone timestamps, invalid gaze/pupil states and false fixed-rate claims.
Configuration, schema, codec, plan and import suites exercise invalid operands and
report their identities. The external `eyes4s.sessionconsumer.SessionSuite` tests
private construction, forged keys, units, mutable access, array isolation and
nominal frame/clock disagreements. Removing or renaming those tests breaks the
catalog gate. Published laws and their existing mutation tests remain separate
scientific contracts.

Method attribution is **per forked JVM suite**, including fixture setup; it does
not identify the individual test body responsible for a call. Named test cases
must all pass, but the tool cannot prove that every assertion is a good oracle.
Scala.js runs the shared tests and has an independently compared compiler
inventory; JaCoCo does not provide JavaScript method probes. Thus the audit makes
no claim of independently measured per-method JavaScript coverage.

Aggregate branch counters are retained in `target/api-audit/execution.json` for
review. They are not used as a substitute for constructor-contract tests: compiler
branches, pattern-match scaffolding and delegated validation do not correspond
one-to-one with public failure contracts. This is entry-point and evidence-drift
enforcement, not a claim of exhaustive path coverage.

## Updating a reviewed API

After implementing an intentional API change and its tests, run:

```sh
python3 tools/api-audit/run.py --record
```

This produces a candidate inventory and evidence mapping only after successful
execution and zero unexplained entry-point gaps. Review both diffs, including the
selected law/conformance witnesses, then rerun the ordinary command. CI never
records a baseline automatically. The finite uncovered report is
`target/api-audit/uncovered.tsv`; source paths identify the owning module.
