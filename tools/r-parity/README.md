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
python3 tools/r-parity/generate_transforms.py --eyesim /path/to/eyesim --check
python3 tools/r-parity/generate_entropy.py --eyesim /path/to/eyesim --check
python3 tools/r-parity/generate_multiscale.py --check
python3 tools/r-parity/generate_temporal.py --check
```

`generate_reference.py --check` compares regenerated results, runtime versions, the input digest
and generated Scala bytes without overwriting artifacts. Regeneration without `--check`
deliberately updates those artifacts. Review changes to the environment lock as well as numerical
output. The lock records consumed evidence; it is not a dependency installer.

The transform generator installs the same pinned archive, calls the public `center`, `rescale` and
`normalize` fixation-group methods on the non-square transform case, compares them with an exact
rational oracle, and records how `affine_transform` and `contract_transform` (fitted density-space
maps, not coordinate maps) respond to coordinate tables. The homogeneous affine, its training-only
solve from three pairs and the held-out target are oracle values only: eyesim has no coordinate
affine entry point.

The entropy generator installs the same pinned archive, builds `eye_density` objects over the
two-by-two lattice in `baseline-cases.json`, and calls the public `fixation_entropy` methods and
`Ops.eye_density` on them. Entropy on positive maps, the two-map mean, the difference and the log
ratio away from zero cells are compared with an exact rational or 60-digit decimal oracle. Every
`NA`, `Inf`, `-Inf`, `NaN` cell and every error is pinned as a literal, and the reading of eyesim's
positive-cell formula on signed maps is labelled as a reading, not an estimand.

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
- `fixtures/transforms.json`: pinned public eyesim center/rescale/normalize output, observed fitted
  transform behaviour on coordinate tables, R environment, and the exact affine oracle with a
  training-only solve, held-out target and falsification mutants.
- `fixtures/entropy.json`: pinned public eyesim `fixation_entropy` and `Ops.eye_density` output on
  the entropy lattice, including non-finite cells, `NA` entropies and errors, R environment, and
  the exact and decimal oracle.
- `fixtures/multiscale.json`: 60-digit closed-form eyes4s Gaussian oracle at three scales.
- `fixtures/temporal-study.csv` and `fixtures/temporal.json`: 18-trial temporal input plus exact
  interval ledgers, pair lists and 96 contrast targets.
- `laws/src/test/scala/eyes4s/examples/MatchedControlFixtures.scala`,
  `core/src/test/scala/eyes4s/core/TransformFixtures.scala`,
  `kernel/src/test/scala/eyes4s/kernel/EntropyFixtures.scala`,
  `codec/src/test/scala/eyes4s/codec/MultiscaleFixtures.scala`, and
  `codec/src/test/scala/eyes4s/codec/TemporalFixtures.scala`: generated values used by JVM and
  Scala.js tests without starting R or reading runtime fixtures.

Run the matched/control conformance example, the coordinate-transform conformance suite and the
entropy and map-arithmetic conformance suite with:

```sh
sbt 'lawsJVM/testOnly eyes4s.examples.MatchedControlSuite' \
    'lawsJS/testOnly eyes4s.examples.MatchedControlSuite' \
    'coreJVM/testOnly eyes4s.core.TransformConformanceSuite' \
    'coreJS/testOnly eyes4s.core.TransformConformanceSuite' \
    'kernelJVM/testOnly eyes4s.kernel.EntropyConformanceSuite' \
    'kernelJS/testOnly eyes4s.kernel.EntropyConformanceSuite'
```

See [the capability baseline](../../docs/EYESIM_CAPABILITIES.md),
[the contrast contract](../../docs/CONTRAST_CONTRACT.md), and [PARITY.md](../../PARITY.md) for the
scientific meaning and limits. The matched case is fixed-grid occupancy rather than KDE; exhaustive
controls do not establish finite-sampler equivalence. The multiscale oracle is not eyesim KDE
evidence, and the temporal duration-mass analysis is a different estimand from
`sample_density_time`.
