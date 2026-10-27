# Studio interaction performance

S10.6 qualification protocol, frozen before measurement on 2026-10-26.
Both frozen workloads passed on 2026-10-26. The full results and their limits follow the protocol.

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
Admission setup has a separate 300-second allowance; it is outside the interaction timer.
All retrieval design rows must be loaded and retained before timing starts.

Acceptance on each workload, and individually for every destination perspective:

- Linked selection feedback p95 <100 ms.
- Cached trial switching p95 <250 ms (two encoding trials warmed before measurement).
- Perspective switching p95 <150 ms.
- Record heap/non-heap and process RSS before/after each workload and after collection;
  the operational limits are 4 GiB retained heap and 8 GiB observed process RSS.

Keep screenshots outside timed operations. Preserve failed measurements with their source
identity. Optimize only measured bottlenecks; rerun affected measurements after changes and
qualify the final frozen candidate. A failed interaction budget keeps S10.6 and G5 open.

## Archive boundary discovered during qualification

The first 10-fold run completed its analysis but could not persist its full density archive:
the default native package budget is 20,000,000 density cells and 256 MiB. These limits stay
unchanged. The 1-fold workload must store a verified archive. The 10-fold workload may
continue only after an explicit `native-artifacts.budget` refusal naming density cells;
any other storage failure fails setup. Its completed in-memory results, reports and figures
are used for the interaction measurements, and the refusal is retained in its receipt.
This court does not qualify cold restoration of an oversized archive. The original failed
setup and timings are preserved under `evidence/20261026-hardening/corrected-1`.

## Qualified interaction measurements (2026-10-26)

Measured source commit `e34becdf025277bfa5ec0b43d6108f74f040d27b`, tree
`44d6edc0e82c2f0825c23432a0a053b8db7f86bf`; clean committed input remained unchanged.
Machine: Mac14,12, Apple M2 Pro, 32 GiB RAM, arm64, macOS 15.1.1 (24B91).
Temurin 25.0.4.1+1-LTS, default 8 GiB FX heap, Monocle software pipeline,
1440x900 logical viewport at native scale 1. Only this court's build was active;
the ordinary background virtualization process remained running and is recorded.
No JVM workaround, visible window, timing subtraction, filtered rows or reduced sample count.

| Interaction p95 (ms) | Fixture | 10x fixture | Strict limit |
| --- | ---: | ---: | ---: |
| linked-selection | 55.17 | 74.15 | <100 |
| cached-trial-switch | 65.84 | 73.59 | <250 |
| perspective-Data | 84.95 | 95.40 | <150 |
| perspective-Explore | 80.62 | 92.69 | <150 |
| perspective-Analysis | 86.80 | 93.24 | <150 |
| perspective-Compare | 55.87 | 66.48 | <150 |
| perspective-Figures | 50.56 | 47.34 | <150 |

Each workload has 450 measured observations, in addition to its unchanged warmups.
The fixture retains 11,520 fixation rows and 960 trial-inventory rows; the tenfold
workload retains 115,200 and 9,600, respectively. All 480/4,800 retrieval design
rows are present before measurement.

| Memory (GiB) | Fixture | 10x fixture | Limit |
| --- | ---: | ---: | ---: |
| Retained heap after collection | 0.383 | 1.640 | 4 |
| Largest observed process RSS | 2.109 | 5.860 | 8 |

RSS is sampled at the named checkpoints; this is not a continuous peak-RSS claim.
Transient pre-collection heap can exceed the retained-heap limit. The 1x archive is
verified and stored. The 10x density archive is explicitly refused at the unchanged
20-million-cell budget, so its successful interactions cover live completed results,
not reopening an oversized archive. That storage work remains tracked separately.

[Machine, source and input receipts](evidence/20261026-hardening/performance/run.json),
[fixture timings](evidence/20261026-hardening/performance/fixture-1x.json),
[tenfold timings](evidence/20261026-hardening/performance/fixture-10x.json),
[CPU observations](evidence/20261026-hardening/performance/activity.json), and
[screenshot review](evidence/20261026-hardening/performance/visual-review.json)
retain the complete measurements. The exact pre-measurement protocol is preserved
in `performance/protocol-at-measurement.md` beside those receipts. All earlier
failed courts and diagnostic profiles remain under `evidence/20261026-hardening/`.

Measured bottlenecks were duplicate-key scans, repeated unchanged view projections,
nonvirtual rows, blocking report/clock work, preview invalidation during draft promotion,
and canvas repainting after temporary dock detachment. Tenfold Compare p95 was
186.48 ms before the rendering fixes, 169.35 ms after reducing frame clipping,
and 66.48 ms after preserving the painted frame across dock detachment. No scientific rows,
render completion requirement, sample counts or acceptance limits were relaxed.
These measurements qualify S10.6 on this machine; they do not qualify G2, physical
Retina/VoiceOver, installation, publication or researcher usability.

The final [release qualification](evidence/20261026-hardening/qualification/qualification.json)
uses the same measured production code, fixture and performance harness. A later
[test-only native-scale correction](evidence/20261026-hardening/native-scale-test-correction/review.json)
keeps the reattachment regression within supported 1×/2× texture bounds; the
[focus-audit correction](evidence/20261026-hardening/focus-audit-readiness/review.json)
realizes virtual rows before their focus is inspected, and the
[Figures theme correction](evidence/20261026-hardening/theme-figures-readiness/review.json)
waits for served data before comparing themes. The
[Compare theme correction](evidence/20261026-hardening/theme-compare-readiness/review.json)
also waits for reports, pair scores, maps and ledger reads while accepting the
fixture's intentional unavailable trial content. These changes affect
no measured implementation or workload. The qualification records this explicit
test-only difference rather than claiming identical whole-test-tree fingerprints.
