# Scala.js bundle-size evidence

Run from the repository root:

```sh
python3 tools/js-bundle-sizes/report.py
```

The root build compiles and exports codec, plan and fs2 Scala.js classpaths.
A standalone build then links three small executable programs against those
current-checkout outputs. It uses its own target directories and never publishes
artifacts to the shared Ivy repository. Run it with no other build in this checkout.

- **codec** encodes a checked pixel frame with a versioned domain codec.
- **plan** builds and inspects a binned cosine study of two one-fixation trials.
- **fs2** executes that study through `StudyExecution[IO]` and awaits completion.

Both `fastLinkJS` and `fullLinkJS` outputs are measured and executed with Node.
Sizes sum every emitted `.js`/`.mjs` file; source maps are disabled. Gzip sizes sum
individually compressed files with zero gzip timestamps. These numbers describe
what dead-code elimination retains for each documented workload, not the complete
module or a representative Studio application. No threshold is enforced.

`target/js-bundle-sizes/` contains JSON and Markdown reports and the full link log.
The JSON records commit/tree identity, dirty state, a hash of actual source/build/probe
inputs, compiler/linker/Node versions, and each output file's byte count and digest.
Measurements fail if inputs change or a probe does not run successfully. The weekly
Scheduled Evidence workflow retains the reports; the per-push matrix is unchanged.
