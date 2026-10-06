# Compiler inventory tooling

The root audit entrypoint is `python3 tools/api-audit/run.py`. It compiles both
library platforms, generates their inventories, then checks execution evidence.
A full audit runs this isolated project's compiler regressions before collecting
library evidence. `--prepare` retains its compile-only behavior.

Run the tooling checks independently:

```sh
python3 -m unittest discover -s tools/api-audit/tests -v
(cd tools/api-audit && sbt test)
```

The Scala tests use a forked JVM so TASTy inspection sees the tooling's Scala
classpath instead of the sbt launcher's Scala version.

## Declaration origins

Declaration source paths belong to the owning TASTy compilation unit. A lazily
loaded member tree can retain the source context of a referring file, and the
compiler's shared `SourceFile` line table can be initialized with a different
unit's line sizes. Neither is an authoritative declaration origin.

The collector takes the owning TASTy's root source path, resolves it against the
supplied repository root, and reads that source to map declaration spans to
lines. Spans use UTF-16 offsets; line lookup follows Scala's line terminators,
including treating CRLF as one terminator. Missing source files and out-of-range
spans fail inventory generation. The audit runner's source fingerprint checks
still bind compilation, inventory and execution to the same candidate.

The enum and ordinary-class fixtures assert independently named declaration
paths and lines, compare full inventories in opposite TASTy input orders, and
simulate the compiler boundary defect by attaching a referring file to a member
tree while preserving its declaration span. Restoring reflected tree source
attribution is rejected by that regression. The simulated context check is not a
claim that the small fixture naturally reproduces the hosted source-path drift.

The reviewed inventory wire format, source columns and API drift checks remain
in place. Canonicalization changes require an official full audit to re-record
inventory, evidence and provenance together; a tooling test run alone does not
qualify a repository tree or a hosted build.
