# Archive memory and workload qualification

Packed density archives construct storage metadata without first building inline JSON cells. Packing traverses primitive values through iterators instead of retaining a boxed vector of every cell. Restoration reconstructs results from verified typed masses and retains the ordinary completed-result, geometry, provenance and cardinality checks. The public inline codec and canonical document order remain unchanged.

Artifact resolution retains verified document bytes and parses each structural document when it is needed. Native packages use owned primitive byte storage behind an immutable sequence interface. They check entry names and byte budgets before copying bodies, inspect structural documents one at a time, and reuse resolver-verified density masses when reconstructing the native result. Native restoration still checks the manifest closure, recipe, source identity and canonical plan/input/result identities.

## Density workload

`DensityMemoryCourt` is an opt-in JVM test main. Its large workload has five participants, ten items and three phases: 150 trials, 32 fixations per trial, four bandwidths and a 256 × 128 grid. The 600 resulting maps contain 19,660,800 binary64 cells, stored as five participant chunks totalling 157,286,400 bytes (150 MiB). The structural result document is 1,273,448 bytes.

Each write and read runs in a separate JVM with `-Xmx1g`. Restoration receives only the saved result document and packed chunks. It receives no source fixations and checks the full canonical result digest:

```
f0e6663da0180435426d3a0f7a62b93e1fdddc35880047afd0d105f762b3da75
```

| Implementation | Same workload under a 1 GiB heap |
|---|---|
| Baseline `d3c64bec02a0d276e5e1fa5353badb0c6aa70cc9` | Input, analysis and canonical identity succeed; packing fails with `OutOfMemoryError: Java heap space`. |
| Updated packed writer | Save succeeds; 600 maps and all packed bytes retained. |
| Updated source-free reader, fresh JVM | Verification, restoration and canonical identity succeed. |

After an explicit GC, the updated writer retains about 322 MiB at save, and the reader about 326 MiB after restoration. These are live-heap checkpoints, **not peak-memory measurements**. The successful bounded-heap processes provide the stronger operational result: this complete workload fits a 1 GiB Java heap. They do not establish the largest supported workload, total process memory, or performance on other machines.

Single-run stage times and exact heap samples are retained in the evidence receipt. Other isolated builds ran concurrently, so these times are descriptive and do not support a throughput speedup claim. An initial read-summary byte counter incorrectly deduplicated equal chunk lengths; the final fresh read uses an iterator and reports all 157,286,400 bytes. Archive bytes and identity checks were unaffected.

## Complete native package workload

`NativeMemoryCourt` admits actual CSV text through `RealAdmission`, prepares and runs the real study, builds and verifies a native package, and saves a bound Studio document with the archive files. Its reader starts in a fresh JVM with only that directory, verifies the closure and restores the retained result through `NativeArtifactRestore`. It does not receive the original CSV text or call the study runner.

The native court uses the same 150-trial, 32-fixation, four-scale, 256 × 128 dimensions, with the Studio image window and four angular bandwidths of 1, 1.5, 2 and 2.5 degrees. The complete archive contains 164,945,432 bytes (about 157.3 MiB). Both processes complete under `-Xmx1g`, without widening the default native budget of 20,000,000 density cells and 256 MiB. The restored 600 maps have the verified canonical result identity:

```
8163316fa28e7eac4760aa8076a5a55346579f7347e7aee8e75996b686677d2f
```

After explicit GC, the writer retains about 482 MiB after package creation and the reader about 328 MiB after cold restoration. The largest sampled heap-with-garbage value is about 941 MiB during package creation; it is not a measured peak. This native court qualifies the complete package and document-binding path, separately from the lower-level codec court. It does not measure desktop ZIP compression, host filesystem throughput, stimulus-image retention or UI rendering.

The saved recipe records the computed semantic input binding and preserves the exact admitted dataset definition. Setup probes deliberately retained in the receipt show that cold restoration refuses an unbound recipe or a changed dataset definition. The final workload passes those existing checks; no production refusal was weakened.

## Reproduction

Use JDK 25 and a fresh directory. The test mains require a forked JVM whose maximum heap is at most 1 GiB:

```sh
sbt 'project codecJVM' \
  'set Test / run / fork := true' \
  'set Test / run / javaOptions += "-Xmx1g"' \
  'Test/runMain eyes4s.codec.DensityMemoryCourt write /tmp/density-court 5 10 256 128' \
  'Test/runMain eyes4s.codec.DensityMemoryCourt read /tmp/density-court'
```

The workload is synthetic and deterministic. Its size represents a moderate multi-participant analysis with four full-resolution map stacks; it is not a real-participant scientific validation dataset. The native package budget remains a separate bound. Passing the codec court alone does not qualify Studio's complete save/cold-open path.

Run the native court independently. This JVM-only filesystem harness lives in `studio-desktop` test sources; the portable Studio modules contain no filesystem imports:

```sh
sbt 'project studioDesktop' \
  'set Test / run / fork := true' \
  'set Test / run / javaOptions += "-Xmx1g"' \
  'Test/runMain eyes4s.studio.core.real.NativeMemoryCourt write /tmp/native-court 5 10 256 128' \
  'Test/runMain eyes4s.studio.core.real.NativeMemoryCourt read /tmp/native-court'
```
