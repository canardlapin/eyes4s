# The model in five ideas

**A recording is not a fixation table.** A recording contains timed raw gaze samples, including
blink, lost and off-screen states. A scanpath contains ordered fixation summaries. Importing
summaries does not invent raw samples, dispersion or detector provenance.

**Positions have units; collections have identities.** A `Pt[Px]` cannot enter an operation that
requires degrees. Two pixel frames may still describe different screens: `Agreement` checks their
runtime identities. Visual-angle conversion needs measured viewing geometry. Similarly, clock
alignment needs a supplied `Sync` or fitted synchronization evidence.

**Order matters until you discard it.** A `Scanpath` retains fixation order. Its duration-weighted
occupancy is a `PointMeasure`, which forgets order. Smoothing it yields an `Intensity`; explicit
normalization produces a `Mass`. A difference between maps is `Signed`, not probability mass.

**A study design determines the question.** A `Trial` keeps identity in its key and annotations in
metadata. A `Relation` names matching dimensions. `PairDesign` declares orientation, self edges
and selection. Removing a participant clause asks a different scientific question.

**Failures and denominators are results.** Pair scores retain both keys and either a score or a
failure. Reductions choose `RequireAll` or an explicit successful-only policy and report counts.
Contrasts retain matched and control results, not just their subtraction. A saved plan identifies
its input and method; loading JSON does not load data or execute an analysis.

Continue with [fixation studies](fixation-studies.md) or [raw recordings](recordings.md).
