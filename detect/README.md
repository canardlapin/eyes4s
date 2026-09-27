# eyes4s-detect

Event detection: Detector instances, filters, and the pure Machine runner.

Detectors are pure state machines, so the same definition runs offline over a
Vector here and online over an fs2.Stream in `eyes4s-fs2`.

The module provides `Detectors.ivt`, `Detectors.idt` and the
`Detectors.engbertKliegl` extension, plus `Kinematics.velocities`,
`EkThresholds.estimate` and `Merge.adjacentFixations`. `Filter` supplies missing
padding, bounded gap interpolation, median and Savitzky-Golay filtering.

Detector conformance fixtures are documented in
[`tools/detector-conformance`](../tools/detector-conformance/README.md), with
Engbert-Kliegl kernel evidence in `tools/engbert-kernels`. These are distinct
from human-data qualification. Pupil-based blink detection, I-HMM and general
resampling remain gaps in the
[pymovements comparison](../docs/plans/PYMOVEMENTS_COMPARISON.md).
See [`PRD.md`](../PRD.md) for requirements and `.mote/` for current work ownership.
