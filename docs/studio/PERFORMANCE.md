# Studio interaction performance

S10.6 qualification protocol, frozen before measurement on 2026-10-26.
Results are pending. Passing scrolling or numerical benchmarks does not qualify these interactions.

## Workloads

Use the committed public golden fixture and a deterministic 10-fold expansion. Expansion
copies all fixation and inventory rows, gives each copy distinct participant identities,
and preserves trial/occurrence identity within participants, coordinates, timing, responses,
exclusions and stimulus identities. Images are shared immutable inputs. No rows are filtered.
Each workload imports into a real file-backed project and uses the production native backend,
admission, Encoding -> Retrieval preset, four scales, completed run, reporting and figure.
No fake scores or fake backend are admitted to this court.

## Measurements

Run alone on the named local machine, with the registered JDK25, repository default heap,
Monocle software rendering and the 1440x900 logical board viewport. No physical window opens.
Record machine model, CPU, RAM, macOS, JVM, source fingerprint, source/input hashes, screen
and scale. This measures application work through a completed rendered frame; it does not
measure OS pointer delivery, compositor presentation or physical display latency.

For each workload, warm each interaction with ten unmeasured operations, then retain 100
observations for selection and cached trial switching, and 50 per destination perspective.
The timer starts before submitting the action to the FX thread, waits for the expected live
view state and a layout pulse, then waits for a fence on the real QuantumRenderer queue after that pulse has submitted
its paint. The fence finishes after the preceding render work. Snapshot export is outside
timing. Do not subtract harness overhead. Record all timings, nearest-rank p95, maximum and counts.
A timeout or refused/unready view is a failure, not a discarded observation.

Acceptance on each workload, and individually for every destination perspective:

- Linked selection feedback p95 <100 ms.
- Cached trial switching p95 <250 ms (two encoding trials warmed before measurement).
- Perspective switching p95 <150 ms.
- Record heap/non-heap and process RSS before/after each workload and after collection;
  the operational limits are 4 GiB retained heap and 8 GiB observed process RSS.

Keep screenshots outside timed operations. Preserve failed measurements with their source
identity. Optimize only measured bottlenecks; rerun affected measurements after changes and
qualify the final frozen candidate. A failed interaction budget keeps S10.6 and G5 open.
