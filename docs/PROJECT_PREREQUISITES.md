# Project prerequisites

`ProjectPrerequisites.query(session, evidence, catalogue)` evaluates a complete application catalogue in one pure call. Every entry carries a `DefinitionId` and an ordered list of typed requirements. The report preserves all entries and exposes both available analyses and unavailable entries with their exact findings. Availability means that the declared prerequisites are satisfied; numerical execution may still return data-dependent failures.

The checked `Session` supplies recordings, AOI sets and grids in its spatial frame. `ProjectEvidence` supplies a physical `Viewing`, observed timelines keyed by trial, directed synchronization evidence and the exact installed schema identities. No planned timeline is accepted as observed evidence. A request for schema version 3 remains unavailable when only versions 1 and 2 are installed; the finding names all three versions.

An observed-anchor requirement names the trial, recording, selector, expected observed clock and maximum accepted synchronization uncertainty. `MarkSelector.resolve` performs the same stable occurrence selection for this query and for `EpochPlan.resolve`: unique, first, last or zero-based Nth. Missing and ambiguous selections preserve the selector and observed match count. A timeline on a foreign nominal clock is refused through `Agreement.clocks`.

If the observed and recording clocks agree, no fitted conversion is needed. Otherwise, only evidence directed from the observed clock to that recording clock is eligible. Any eligible fit within the requested inclusive uncertainty limit satisfies the requirement. A reversed fit is not implicitly inverted. Missing evidence reports the required clock operands and available directed pairs; imprecise evidence reports the best eligible uncertainty and requested limit.

Requirements and catalogue entries are deliberately not deduplicated. The same analysis definition can appear with different configured requirements, and every requested entry receives a result. Session insertion still owns frame validation. This query does not grant authority to use arbitrary objects outside that checked Session.

`ProjectCatalogue.query(session, evidence, entries)` is the typed built-in entry point. Each
entry supplies its definition, a `ProjectWorkflow` and its exact registered schema requirements.
The workflow derives the resource requirements, rather than asking the application to assemble
an untyped list. The conformance witness covers every `AnalysisKind` and `AnalysisComponent`:
fixation study, event recording, temporal study, repetition, point sampling, template fitting,
decomposition, scanpath comparison and epochs.

`ProjectInputOrigin.Recordings` requires a nonempty collection of recording keys;
`ProjectInputOrigin.Supplied` represents checked scanpaths or maps already supplied to the
analysis. Supplied inputs do not require their original recordings to remain in the project.
`ProjectCoordinates.DegreesFromPixels` requires physical viewing geometry, while native measured
coordinates require no new calibration. Fixed-feature template fitting needs neither viewing
geometry nor a map grid; mean-map fitting and map decomposition name their required grid.
Event-recording analysis names its recording, AOI set and viewing geometry.

Temporal studies distinguish supplied measured windows from a newly resolved observed-anchor
binding. `ProjectTemporalWindows.Observed` and the epoch component require a nonempty set of
`ProjectAnchor` values, preserving the trial, recording, selector, observed clock and uncertainty
limit through the shared query. Planned timing cannot substitute for any of those observations.

This query reports resource prerequisites. Supplied data still go through their checked domain
constructors and the corresponding plan's artifact-identity, configuration, budget and numerical
checks. The low-level `ProjectPrerequisites.query` remains available for downstream catalogue
extensions; arbitrary custom behavior must declare its own requirements.
