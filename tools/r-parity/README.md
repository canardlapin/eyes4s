# eyesim baseline and reference fixtures

[baseline.json](baseline.json) is the executable inventory for the 13-row eyesim baseline. It pins
the eyesim revision, source inventory, entry points, methods, estimands, conventions, backend
dependencies, fixture identities, evidence status and exact Mote owner for every gap. Per-ticket
fragments in `manifest.d/` add fixtures, regeneration entries and cases to it without editing its
shared arrays. [baseline-cases.json](fixtures/baseline-cases.json) holds the first small
falsification inputs and is frozen; later inputs live in `fixtures/cases/<ticket>.json`. Neither
contains manufactured reference results.

R generates the checked-in fixtures offline. No eyes4s build, test or CI workflow runs R: JVM and
Scala.js tests consume the generated values only.

## Checking the contract

Run from the eyes4s repository root:

```sh
python3 tools/r-parity/check_baseline.py --eyesim /path/to/eyesim --mote
```

The checkout only needs to contain the pinned Git object. The checker merges `baseline.json` with
`manifest.d/*.json` and validates every fixture digest; that each required entry point is
classified; that `fixtures/baseline-cases.json` is unchanged and every file in `fixtures/cases/`
is registered by its own ticket's fragment; that no generator names a revision or starts R, an
archive or an install itself; that every fixture and generated Scala file recording an eyesim
revision records the pinned one, every eyesim generator has a JSON output that records it, and
nothing else in the repository names it; and that every R runtime a fixture records matches
[r-lock.json](r-lock.json). With `--eyesim` it reads the inventory sources from the pinned object
and checks the DESCRIPTION version and the dependency entries, with their version constraints,
against the lock; with `--mote` it confirms that every implementation gap is a live task blocking
the downstream workflow epic and that every fragment names a Mote task.

The checker's summary is the only source of case counts; documents point to it instead of
repeating numbers that change as cases close. `--list-cases` names the cases in each status.
Out-of-baseline records are reported on their own line and are not counted.

`--run-regeneration` runs every registered generator with `--check`, which compares regenerated
bytes without writing. `--dry-run` prints the commands instead. When the pin in `baseline.json`
differs from the revision the fixtures record, or with `--write`, it first regenerates every
registered fixture, refreshes `eyesim.version` and the digests of the outputs the write pass
actually rewrote (any other drift fails), validates again and then runs the checks. After a
regeneration it started because the pin moved, it exits non-zero even when every check passes, so
that no gate passes on a tree it has just rewritten: review and commit the changes, then rerun.

## The shared R harness

Every generator that runs R goes through [parity.py](parity.py):

```python
with parity.r_session("eyes4s-r-topic-", args.eyesim) as r:
    r.rscript(HERE / "topic.R", r.eyesim_library, INPUT, output)
```

`r_session` reads the revision from `baseline.json`, runs `git archive` on exactly that object in
the given checkout, and installs it with `R CMD INSTALL` into a temporary library. It never uses
the checkout's working tree or an installed eyesim. Each script then runs with
[session.R](session.R) as its profile, which fixes what the session can contribute to a fixture:

- the package library: the project library, followed only by R's own `.Library`, which R always
  keeps; the user and site libraries are removed;
- the options `digits` (R's default 7), `scipen`, `OutDec`, `width`, `warn`, `warning.length`,
  `nwarnings`, `useFancyQuotes`, `keep.source` and `encoding`;
- the RNG kinds (Mersenne-Twister, Inversion, Rejection); scripts still set their own seeds;
- the C locale, `LANGUAGE=en`, `TZ=UTC` and `OMP_NUM_THREADS=1`, with no user or site profile,
  no site or user environment file (also while installing eyesim), and no `R_DEFAULT_PACKAGES` or
  `R_FUTURE_PLAN`.

A script that needs another option value sets it explicitly. The current fixtures are byte-identical
with `digits` at 7 and at 17, so the scripts no longer set it. After a script succeeds, the harness
checks every namespace it loaded: eyesim must come from the archive of the pin, base packages from
the locked R, and everything else from the project library at its locked version. The audit sees
loaded namespaces only; it does not see a script that merely probes `.Library` with
`system.file` or `installed.packages`.

The harness does not pin the C and C++ toolchain that compiles eyesim (R's `Makeconf` and any
`~/.R/Makevars`) or the BLAS and LAPACK that R links. The current fixtures were built with the
maintainer's toolchain, Apple's Accelerate (vecLib) BLAS and R's own LAPACK; a different
compiler or linear-algebra library could change the last bits of numerical results.

### Provisioning the R library

[r-lock.json](r-lock.json) records the R version and the name, version and source of every package
a generator may load: the `Depends`/`Imports`/`LinkingTo` closure of the pinned eyesim, the harness
roots (`jsonlite`, `future`) and the packages later baseline cases need (`energy` for distance
correlation, `T4transport` for exact transport). Build the git-ignored `tools/r-parity/library/`
from it with:

```sh
python3 tools/r-parity/provision.py
```

Each locked package comes, in order of preference, from an installed copy of exactly that version
in one of the machine's R libraries, the current CRAN binary of that version, or the CRAN source
archive of that version. Base packages come with R and are only recorded. To add a root or follow a
pin that changes eyesim's dependencies, add the package to `roots` if needed and run
`python3 tools/r-parity/provision.py --update --eyesim /path/to/eyesim`; review and commit the lock.
Requirements: Python 3.12 or later, Git, the locked R version with a working package compiler, and
network access only for packages the machine does not already have.

## Implemented regeneration

Each generator also runs on its own; for example:

```sh
python3 tools/r-parity/generate_reference.py --eyesim /path/to/eyesim --check
python3 tools/r-parity/generate_multiscale.py --check
```

Without `--check` a generator deliberately rewrites its artifacts. Review changes to runtime records
as well as numerical output.

The matched/control generator invokes public exported R functions, checks their results against an
independent rational oracle, and writes reviewable JSON plus portable Scala values.

The transform generator calls the public `center`, `rescale` and `normalize` fixation-group methods
on the non-square transform case, compares them with an exact rational oracle, and records how
`affine_transform` and `contract_transform` (fitted density-space maps, not coordinate maps) respond
to coordinate tables. The homogeneous affine, its training-only solve from three pairs and the
held-out target are oracle values only: eyesim has no coordinate affine entry point. The fitted
transforms themselves are out of the baseline (owner decision 2026-09-18); their record and this
evidence stay in `baseline.json`.

The entropy generator builds `eye_density` objects over the two-by-two lattice in
`baseline-cases.json`, and calls the public `fixation_entropy` methods and `Ops.eye_density` on
them. Entropy on positive maps, the two-map mean, the difference and the log ratio away from zero
cells are compared with an exact rational or 60-digit decimal oracle. Every `NA`, `Inf`, `-Inf`,
`NaN` cell and every error is pinned as a literal, and the reading of eyesim's positive-cell formula
on signed maps is labelled as a reading, not an estimand.

The repetition generator measures public `repetitive_similarity` cosine pairwise/reduced calls
on the supplied duration maps, including duplicated rows, singleton and empty input. Exact
rational dot products check its phase-only grouping and the different within-participant
reinstatement estimand. See [repetition studies](../../docs/REPETITION_STUDIES.md) for the compiled
direct and saved-plan workflows; other methods and multiscale reductions remain gaps.

The admission generator measures public `eye_table`, `fixation_group`, `coords` and
`as_eye_table` calls. It records clipping, row ordering, duplicate onsets, zero/negative
durations, missing coordinates, upper-bound closure, shifted/flipped bounds and empty inputs. An
independent clipping oracle checks the retained reference rows; portable tests check eyes4s'
explicit whole-trial rejection ledger and half-open bounds. Valid coordinate and timing values
agree, but admission policies intentionally differ.

The independent multiscale and temporal generators do not call eyesim. They establish eyes4s
scientific cases while the corresponding eyesim KDE and temporal point-sampling cases remain open.
The template generator consumes the committed actual Scala training export, executes the optional
R QR adapter in the same locked session and checks exact rational coefficients/predictions, a
held-out contamination mutant, an offset-response intercept control and rejected invalid designs.
It also measures `template_multireg` on normalized maps, a different statistical unit and
coefficient basis. The [template guide](../../docs/TEMPLATE_FITTING.md) reproduces the full export,
saved recipe, external fit and reimport path.

## Adding a case

A baseline ticket adds its evidence in files it owns, so parallel tickets do not edit the same
lines:

1. Put the ticket's inputs in `fixtures/cases/<ticket>.json`, or in several files under
   `fixtures/cases/<ticket>/`, where `<ticket>` is the Mote id. Never edit
   `fixtures/baseline-cases.json`: four generators hash the whole file, so any edit forces all of
   them to regenerate, and the checker refuses it. Copy a section you reuse into your own file, and
   hash only your own inputs in the artifacts you generate.
2. Write `generate_<topic>.py` and, if it calls R, `<topic>.R`. Set `sys.dont_write_bytecode = True`
   before `import parity`; run R only through `parity.r_session`; write
   `eyesim_revision=parity.pinned_revision()` at the top level of the JSON output and a runtime
   record `{"R": ..., "packages": {...}}` of the versions consumed; support `--check`, which
   compares bytes and never writes. Never write a revision into a generator. The R script prepends
   `args[[1L]]` to `.libPaths()` and loads eyesim with `library(eyesim)`. If it needs a package
   that `r-lock.json` lacks, add the package to `roots.baseline` and run
   `provision.py --update --eyesim /path/to/eyesim` in the same change.
3. Register everything in `manifest.d/<ticket>.json`; the file name must be the `ticket` value:

   ```json
   {
     "schema": 1,
     "ticket": "eyesim-kde",
     "fixtures": {
       "kde-cases": {"path": "tools/r-parity/fixtures/cases/eyesim-kde.json", "sha256": "<sha256>", "license": "Apache-2.0; synthetic data authored for eyes4s", "role": "KDE falsification inputs"},
       "kde-reference": {"path": "tools/r-parity/fixtures/kde.json", "sha256": "<sha256>", "license": "Apache-2.0", "role": "Pinned public eye_density output and its independent oracle"}
     },
     "regeneration": {
       "kde-reference": {"command": "python3 tools/r-parity/generate_kde.py --eyesim {eyesim} --check", "requires_eyesim": true, "outputs": ["kde-reference"]}
     },
     "cases": []
   }
   ```

   Fixture and regeneration ids must be new across `baseline.json` and every fragment, and each
   output has one regenerator. Every regeneration command must be a `--check` command, and an
   eyesim generator must have a JSON output that records `eyesim_revision`. Fill each `sha256`
   with `shasum -a 256 <file>` after the generator's first run without `--check`; from then on
   the checker and `--write` keep the digests.
4. Classify. Change the status, evidence, `regeneration_ids` and `fixture_inputs` of your own
   existing cases in place in `baseline.json`; put wholly new cases in the fragment's `cases` list.
   Do not edit counts anywhere.
5. Record the measured findings, not counts, in [PARITY.md](../../PARITY.md). Declare any new
   `DefinitionId` in the file that introduces it, as described in [AGENTS.md](../../AGENTS.md).
6. Run the generator's `--check`, `check_baseline.py --eyesim /path/to/eyesim --mote
   --run-regeneration` and the consuming Scala suites on JVM and Scala.js.

## Moving the eyesim pin

1. Change `eyesim.revision` in `baseline.json`. It is the only line that names the pin; generators
   read it through `parity.pinned_revision()`, and the checker refuses the pin anywhere else except
   in fixtures and generated files.
2. Run `python3 tools/r-parity/check_baseline.py --eyesim /path/to/eyesim --mote
   --run-regeneration`. It sees that the fixtures record the old revision, regenerates every
   registered fixture at the new one, refreshes the fixture digests and `eyesim.version` in
   `baseline.json` and the fragments, validates the whole contract again, and reruns every
   generator with `--check` to show the new fixtures reproduce. It then exits non-zero on purpose,
   because it has rewritten the tree. If the new revision changes eyesim's `Depends`, `Imports` or
   `LinkingTo` entries or their version constraints, the checker stops before regenerating and
   asks for `provision.py --update`.
3. Review the diff: numerical changes are findings to record in PARITY.md, and generator assertions
   that fail are behavior changes to investigate. Run the Scala suites that consume the generated
   values, commit, and run the same command again; it must now pass.

## Out-of-baseline records

An owner decision can remove a case from the baseline. Its record moves from `cases` to
`out_of_baseline` in `baseline.json`, without `status` or `gap_issue`, and gains `reason` and
`owner_decision` (an ISO date). Its fixtures and evidence stay and are still validated, and its
entry points still count as classified for their row. The checker reports it on a separate line; it
is neither a gap nor progress towards closing the baseline.

## Files

- `baseline.json`: normative capability and evidence inventory, and the only place the pin is
  written.
- `manifest.d/<ticket>.json`: per-ticket fixtures, regeneration entries and new cases.
- `parity.py`, `session.R`: the shared archive-install helper and pinned R session.
- `provision.py`, `r-lock.json`: the project R library and its lock; `library/` is git-ignored.
- `fixtures/baseline-cases.json`: frozen adversarial and analytic inputs selected before
  implementation.
- `fixtures/cases/<ticket>.json`: per-ticket inputs.
- `fixtures/matched-control.csv`: 48 authored fixation summaries in 12 trials.
- `fixtures/exact.json`: rational pair scores, reductions, signed contrasts and a separate analytic
  five-component score-algebra case.
- `fixtures/eyesim.json`: public R output for the correct workflow, image-only matching, missing
  source matches, duplicate references and constant Pearson correlation.
- `fixtures/reference-lock.json`: input identity, conventions, source revision and R environment of
  the matched/control reference.
- `fixtures/transforms.json`: pinned public eyesim center/rescale/normalize output, observed fitted
  transform behaviour on coordinate tables, R environment, and the exact affine oracle with a
  training-only solve, held-out target and falsification mutants.
- `fixtures/entropy.json`: pinned public eyesim `fixation_entropy` and `Ops.eye_density` output on
  the entropy lattice, including non-finite cells, `NA` entropies and errors, R environment, and
  the exact and decimal oracle.
- `fixtures/admission.json`: public constructor/accessor results and errors, independent row
  retention and coordinate checks, source identity and runtime versions. Generated
  `io/.../AdmissionReference.scala` is consumed by the portable admission conformance suite.
- `fixtures/multiscale.json`: 60-digit closed-form eyes4s Gaussian oracle at three scales.
- `fixtures/repetition.json`: phase-only cosine R output, runtime versions and independent
  within-participant pair/mean expectations, consumed by `design/.../RepetitionReference.scala`.
- `fixtures/template-training.csv`: actual JVM export of the baseline's four training rows.
- `fixtures/template.json`: R coefficient receipt, exact held-out oracle, contaminated-fit mutant
  and pinned eyesim normalized-map fit. `io/.../TemplateFitReference.scala` carries the transport
  strings for portable tests. The recipe is deliberately not the training request.
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
