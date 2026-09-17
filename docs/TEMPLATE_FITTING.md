# Fit on training trials, evaluate on held-out trials

This workflow fits an **intercept-free linear predictor from fixed features** using R's QR solver,
then imports its coefficients into eyes4s and evaluates held-out trials. Split membership, typed
trial keys, feature definitions, training identity and prediction failures remain inspectable.
There is no R dependency in the pure Scala modules; exporting and importing text work on JVM
and Scala.js. Running R is an explicit external step.

The basic model is `prediction = beta(0) * feature(0) + ...`. It does not fit a distribution or
turn regression coefficients into mixture weights. Surface decomposition, learned template/PCA
construction and general cross-validation are separate, unfinished workflows.

## Run the complete example

Requirements: this checkout's Scala build and R with its standard `stats` package. The exact
[TemplateFitGuide.scala](../io/src/test/scala/eyes4s/examples/TemplateFitGuide.scala) is compiled
on JVM and Scala.js; the [CLI](../tools/study-cli/TemplateFitCli.scala) supplies only filesystem
effects. No eyesim installation is needed to run this fitting route.

From the repository root:

```sh
fit_dir=$(mktemp -d /tmp/eyes4s-template.XXXXXX)
sbt "ioJVM/Test/runMain eyes4s.examples.TemplateFitCli export $fit_dir"
Rscript --vanilla tools/template-fit/fit.R "$fit_dir/training.csv" "$fit_dir/coefficients.csv"
sbt "ioJVM/Test/runMain eyes4s.examples.TemplateFitCli evaluate $fit_dir"
```

The export writes a versioned `recipe.json` and a **training-only** `training.csv`. The R script
writes `coefficients.csv`; it refuses an existing output file. Evaluation reloads the saved
recipe, imports that receipt and reports key `e`, fold `test`, prediction approximately `7`,
residual approximately zero and mean squared error approximately zero. The measured fixture gave
`7.000000000000002` and MSE `3.1554436208840472e-30`, within the named `1e-12` tolerance.
Temporary example files stay in the printed directory so you can inspect them.

The four training rows are `(1,0) -> 1`, `(0,1) -> 2`, `(1,1) -> 3`, `(2,1) -> 4`.
They identify coefficients `(1,2)`. The held-out row `(3,2) -> 7` is never sent to the fitter.
These are synthetic fixed-feature trial observations, not a claim about an acquired eye-tracking
dataset. They deliberately match the baseline's small linear falsification case.

## Use your own data

1. Construct `TemplateBasis.of(id, orderedFeatureNames, responseUnit)`. The ID names the fixed
   scientific feature definition; change it when that definition changes. All rows use the same
   ordering and units. No intercept, normalization, centering or learned transformation is added.
2. Construct each `TemplateObservation.of(key, fold, features, response)`, handling its `Either`.
   Features and responses must be finite. A key can be your product type with a `KeyDigest`
   instance; it need not be a string or a row number.
3. Call `TemplateSplit.of(basis, rows, heldOutFolds)`. Every requested fold must exist, keys must
   be unique across the entire split, and both partitions must be nonempty. Every row is assigned
   to exactly one partition. Width and admission failures are errors, not omitted observations.
4. Save with `TemplateRecipeCodec.of(schemaId, keyCodec)` and export
   `TemplateFitCsv.training(split.training)`. The fitting export does not accept
   `TemplateHeldOut`. Use the same R command as above, then call
   `TemplateFitCsv.importFit(split.training, receipt)` and `model.evaluate(split.heldOut)`.

Choose folds at the appropriate independence unit (for example participant or stimulus), not
merely by row. The library cannot infer that unit from a label. **Features must be predeclared or
computed without held-out information.** This route does not certify externally prepared features
as leakage-free, nor does it implement nested tuning, learned templates or per-fold preprocessing.
Those require a separate training-only feature-construction contract.

## What is checked and retained

The training digest includes the basis, ordered feature names, response unit, typed key digests,
folds, feature values and responses of training rows only. Editing held-out responses or features
does not change it. Training row order is part of this identity. The evaluation digest includes
held-out rows, so changing the held-out response changes the evaluation identity.

The saved recipe includes **both partitions**, their data and the explicit held-out fold set;
it is not the fitting request and should not be sent to the backend. Restore rejects unsupported
schema/method versions, duplicate keys/folds, invalid data and mismatched training identity.

The receipt must name the expected method, training digest, ordered feature names, complete
finite coefficients, full rank, training row count and rank tolerance. The adapter fixes QR rank
tolerance at `1e-7` and rejects rank-deficient/underdetermined designs. It records the R version.
There is no silent row deletion or rank-deficient coefficient substitution. The tolerance and
method are versioned conventions, not caller-adjustable ways to turn a failed fit green.

A receipt is **trusted-backend provenance, not proof against a modified backend or forged CSV**.
The digest is the library's non-cryptographic content identity, not a signature. Keep the exported
request with the recipe and run the provided adapter unchanged. The numerical conformance tests
establish this adapter's behavior; an arbitrary receipt carrying the same label does not.

Every held-out row retains its key, fold, observed response and either `(prediction, residual)` or
an operand-bearing numerical error. The descriptive `meanSquaredError` refuses failed predictions;
it never quietly changes its denominator. No per-cell coefficient p-values or standard errors are
imported, and no population inference is claimed.

## Evidence and limits

[template.json](../tools/r-parity/fixtures/template.json) records the actual R receipt and an exact
rational normal-equation oracle independent of R's floating-point QR solver. The fixture input is
the actual JVM export consumed by R. Portable tests check exact metadata and parsed Double values;
JVM and JavaScript may render integral doubles as `1.0` and `1`, respectively. Changing the held-out response from
7 to 700 leaves coefficients and prediction unchanged; the residual becomes 693. A deliberately
contaminated backend input instead produces coefficients `701/8` and `709/8`, so this case detects
the leakage it is meant to exclude. Rank-deficient, underdetermined and non-finite inputs are
rejected, and a one-row/one-feature fit is also checked.
A deliberate source mutant that included all rows in `TemplateTraining` failed four of the six
domain tests; the production source was then restored. The offset-response backend control also
detects an accidental intercept, which the exact through-origin example alone would not reveal.

Pinned eyesim `template_multireg(method = "lm", intercept = FALSE)` is also measured on the same
vectors as **normalized maps**. It fits across cells separately for each source map, not across
training trials, and returns `(0.4,0.6)` because its two predictor sums are 4 and 3 and its response
sum is 10. These coefficients are correct for that different basis. This is not parity with
`template_similarity_cv` or `template_regression`, and it does not establish learned-transform
leakage safety. Robust regression, NNLS, simplex surface fitting and partial association remain
unimplemented in this route; binomial regression on continuous mass is excluded.

```sh
python3 tools/r-parity/generate_template.py --eyesim /path/to/eyesim --check
sbt 'designJVM/testOnly eyes4s.design.TemplateFitSuite' 'designJS/testOnly eyes4s.design.TemplateFitSuite' \
  'ioJVM/testOnly eyes4s.io.TemplateFitSuite' 'ioJS/testOnly eyes4s.io.TemplateFitSuite'
```

For matched/control scores use [fixation studies](FIXATION_STUDIES.md); for explicit repetition
comparisons use [repetition studies](REPETITION_STUDIES.md). The [capability map](EYESIM_CAPABILITIES.md)
keeps the remaining template-method and surface-decomposition requirements visible.
