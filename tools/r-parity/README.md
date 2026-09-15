# eyesim baseline and reference fixtures

[baseline.json](baseline.json) is the executable inventory for the 13-row eyesim baseline. It pins
the eyesim revision, source inventory, entry points, methods, estimands, conventions, backend
dependencies, fixture identities, evidence status and exact Mote owner for every gap.
[baseline-cases.json](fixtures/baseline-cases.json) fixes small falsification inputs for work that is
still open; it contains no manufactured reference results.

Run the contract check from the eyes4s repository root:

```sh
python3 tools/r-parity/check_baseline.py \
  --eyesim /path/to/eyesim \
  --mote
```

The checkout only needs to contain the pinned Git object. The checker reads inventory files from
that object, validates every fixture digest, ensures all required entry points are classified, and
optionally confirms that every implementation gap remains a live task blocking the downstream
workflow epic.

## Implemented regeneration

The matched/control generator installs eyesim from a Git archive of the revision pinned in the
manifest into a temporary library. It invokes public exported R functions, checks their results
against an independent rational oracle, and writes reviewable JSON plus portable Scala values. It
never modifies the eyesim checkout or uses an arbitrary installed eyesim package.

Requirements: Python 3.12 or later, Git, R with a working package compiler, the eyesim Imports and
LinkingTo dependencies, and jsonlite/future for the harness. The exact R and package versions from
the reference run are in `fixtures/reference-lock.json`. Provision those versions before checking
the reference; the generator does not modify the global R library.

```sh
python3 tools/r-parity/generate_reference.py --eyesim /path/to/eyesim
python3 tools/r-parity/generate_reference.py --eyesim /path/to/eyesim --check
python3 tools/r-parity/generate_multiscale.py --check
python3 tools/r-parity/generate_temporal.py --check
```

`generate_reference.py --check` compares regenerated results, runtime versions, the input digest
and generated Scala bytes without overwriting artifacts. Regeneration without `--check`
deliberately updates those artifacts. Review changes to the environment lock as well as numerical
output. The lock records consumed evidence; it is not a dependency installer.

The independent multiscale and temporal generators do not call eyesim. They establish eyes4s
scientific cases while the corresponding eyesim KDE and temporal point-sampling cases remain open.
Run all implemented checks through the manifest with:

```sh
python3 tools/r-parity/check_baseline.py \
  --eyesim /path/to/eyesim \
  --mote \
  --run-regeneration
```

## Files

- `baseline.json`: normative capability and evidence inventory.
- `fixtures/baseline-cases.json`: selected adversarial and analytic inputs for all open rows.
- `fixtures/matched-control.csv`: 48 authored fixation summaries in 12 trials.
- `fixtures/exact.json`: rational pair scores, reductions, signed contrasts and a separate analytic
  five-component score-algebra case.
- `fixtures/eyesim.json`: public R output for the correct workflow, image-only matching, missing
  source matches, duplicate references and constant Pearson correlation.
- `fixtures/reference-lock.json`: input identity, conventions, source revision and R environment.
- `fixtures/multiscale.json`: 60-digit closed-form eyes4s Gaussian oracle at three scales.
- `fixtures/temporal-study.csv` and `fixtures/temporal.json`: 18-trial temporal input plus exact
  interval ledgers, pair lists and 96 contrast targets.
- `laws/src/test/scala/eyes4s/examples/MatchedControlFixtures.scala`,
  `codec/src/test/scala/eyes4s/codec/MultiscaleFixtures.scala`, and
  `codec/src/test/scala/eyes4s/codec/TemporalFixtures.scala`: generated values used by JVM and
  Scala.js tests without starting R or reading runtime fixtures.

Run the matched/control conformance example with:

```sh
sbt 'lawsJVM/testOnly eyes4s.examples.MatchedControlSuite' \
    'lawsJS/testOnly eyes4s.examples.MatchedControlSuite'
```

See [the capability baseline](../../docs/EYESIM_CAPABILITIES.md),
[the contrast contract](../../docs/CONTRAST_CONTRACT.md), and [PARITY.md](../../PARITY.md) for the
scientific meaning and limits. The matched case is fixed-grid occupancy rather than KDE; exhaustive
controls do not establish finite-sampler equivalence. The multiscale oracle is not eyesim KDE
evidence, and the temporal duration-mass analysis is a different estimand from
`sample_density_time`.
