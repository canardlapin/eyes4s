# Fit on training trials, evaluate on held-out trials

This workflow fits an **intercept-free linear predictor from fixed features** using the
native scaled Householder QR solver. Split membership, typed trial keys, feature definitions,
training identity and prediction failures remain inspectable. Fitting, saving, reopening and
evaluation run on JVM and Scala.js without R.

The model is `prediction = beta(0) * feature(0) + ...`. Coefficients are not mixture weights.
Cellwise [surface regression](SURFACE_DECOMPOSITION.md) is a different estimand.

## Run the complete example

The exact [TemplateFitGuide.scala](../io/src/test/scala/eyes4s/examples/TemplateFitGuide.scala)
is compiled and tested on JVM and Scala.js. The [CLI](../tools/study-cli/TemplateFitCli.scala)
adds filesystem effects. From the repository root:

```sh
fit_dir=$(mktemp -d /tmp/eyes4s-template.XXXXXX)
sbt "ioJVM/Test/runMain eyes4s.examples.TemplateFitCli fit $fit_dir"
sbt "ioJVM/Test/runMain eyes4s.examples.TemplateFitCli evaluate $fit_dir"
```

`fit` validates the native fit and writes a versioned `recipe.json`; `evaluate` reopens the
recipe, reconstructs the checked split and refits from the saved training rows before evaluating.
It prints key `e`, fold `test`, prediction approximately `7`, residual approximately zero and
mean squared error approximately zero, within the named `1e-12` test tolerance. The recipe retains
both partitions; fitting accepts only the training capability. Files remain available for inspection.

The training rows are `(1,0) -> 1`, `(0,1) -> 2`, `(1,1) -> 3`, `(2,1) -> 4`, with coefficients
`(1,2)`. The held-out row `(3,2) -> 7` is never passed to the fitter. These are synthetic trial
observations, not acquired eye-tracking evidence.

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
4. Fit with `FittedTemplate.fitNoIntercept(split.training)` and evaluate with
   `model.evaluate(split.heldOut)`. Save inputs with `NativeTemplateRecipeCodec.of(schemaId,
   keyCodec)`; reopening reconstructs the same split, then uses the same native fit call.
   Allocate a distinct schema identity from a historical R-labelled recipe.

Choose folds at the appropriate independence unit (for example participant or stimulus), not
merely by row. The library cannot infer that unit from a label. **Features must be predeclared or
computed without held-out information.** This route does not certify externally prepared features
as leakage-free, nor does it implement nested tuning, learned templates or per-fold preprocessing.
The [learned mean-map recipe](TEMPLATE_CV.md) provides one such training-only
contract, with separate analytic and leakage evidence.

## What is checked and retained

The training digest includes the basis, ordered feature names, response unit, typed key digests,
folds, feature values and responses of training rows only. Editing held-out responses or features
does not change it. Training row order is part of this identity. The evaluation digest includes
held-out rows, so changing the held-out response changes the evaluation identity.

The saved recipe includes **both partitions**, their data and the explicit held-out fold set;
it is not the fitting request and should not be sent to the backend. Restore rejects unsupported
schema/method versions, duplicate keys/folds, invalid data and mismatched training identity.

The native method `eyes4s.no-intercept-scaled-householder-qr/1` uses unit-norm column scaling
and Householder QR at fixed relative rank tolerance `1e-12`. There is no implicit intercept,
silent row deletion or rank-deficient coefficient substitution. Rank, shape and arithmetic
errors retain the training identity and solver operands. The native codec records the method,
no-intercept convention and tolerance, and refuses changes to them.

Historical `TemplateRecipeCodec` v1 and `FittedTemplate.importNoIntercept` retain their original
R-labelled meaning and `1e-7` rank convention. `TemplateFitCsv` import/export remain optional
interop; they are not required for a native workflow. Neither codec silently accepts the other's
method. An imported receipt is trusted-backend provenance, not proof against forged coefficients.
The training digest is a content identity, not a cryptographic signature.

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
sum is 10. These coefficients are correct for that different basis. This historical fixture alone is not parity with `template_similarity_cv` or
`template_regression`. The [CV bridge and learned recipe](TEMPLATE_CV.md) and
[surface OLS evidence](SURFACE_DECOMPOSITION.md) qualify those separate bounded routes. Robust regression, NNLS, simplex surface fitting and partial association remain
unimplemented in this route; binomial regression on continuous mass is excluded.

```sh
python3 tools/r-parity/generate_template.py --eyesim /path/to/eyesim --check
sbt 'designJVM/testOnly eyes4s.design.TemplateFitSuite' 'designJS/testOnly eyes4s.design.TemplateFitSuite' \
  'ioJVM/testOnly eyes4s.io.TemplateFitSuite' 'ioJS/testOnly eyes4s.io.TemplateFitSuite'
```

For matched/control scores use [fixation studies](FIXATION_STUDIES.md); for explicit repetition
comparisons use [repetition studies](REPETITION_STUDIES.md). The [capability map](EYESIM_CAPABILITIES.md)
keeps the remaining template-method and surface-decomposition requirements visible.
