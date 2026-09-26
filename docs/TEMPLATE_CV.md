# Training-only learned templates and explicit folds

`LearnedTemplate` implements `eyes4s.training-mean-cosine-response/1`, a bounded
native recipe. Each observation is one trial, with a normalized `Mass` map on a
shared nominal grid, finite response, typed key, split-group label and match-group
label. The caller names the split unit (for example participant) and response unit.

For training maps m_i, the learned template is t = sum(m_i)/n. Every trial has
weight 1/n, including trials in groups of different sizes. The feature is
x_i = dot(m_i,t)/(norm(m_i)*norm(t)). The response model is y_i = beta*x_i,
with no intercept; beta is fitted by the shared native scaled Householder QR.
This is prediction across trials, not regression across cells, and beta is not
a mixture weight. There is no response weighting, implicit centering or tuning.

`MapTemplateObservation.of` validates each row. `MapTemplateSplit.of` takes an
explicit set of held-out split groups. Before fitting it excludes training
candidates whose match group occurs in held-out data. Every such row is retained
in `excluded` with `HeldOutMatchGroup`; no observation disappears. The resulting
training and held-out capabilities are separate types. Empty partitions, repeated
trial keys and incompatible grids are typed errors. Group membership must be
chosen at the appropriate independence unit; a label cannot establish that unit
scientifically.

`LearnedTemplate.fit(split.training)` can receive only training maps and responses.
It returns the mean map, ordered training features, slope and full training receipt.
The digest includes recipe identity, units, grid/frame specification, ordered keys,
groups, map values and responses. `evaluate(split.heldOut)` checks training identity
and retains every held-out key, group, response and prediction/residual or error.

`LearnedTemplateRecipeCodec.of(schema, keyCodec)` saves the complete input recipe,
including excluded rows and both partitions. Reopening reruns admission and
exclusion rules and checks the training digest. Refit reconstructs the learned mean,
features and slope; saved coefficients are not accepted as proof of native fitting.
The compiled `LearnedTemplateRecipeSuite` exercises save/reopen/refit/evaluate on
JVM and Scala.js, including invalid mass, wrong units and stale identities.

## Independent leakage evidence

The analytic case has training maps (1,0), (1,0), (0,1) and responses 2,2,1.
The expected mean is (2/3,1/3), the feature vector is (2,2,1)/sqrt(5), and beta
is sqrt(5). A held-out map (1/2,1/2) predicts 3/sqrt(2). Replacing it by (0,1)
and changing its response from 7 to 700 changes prediction to 1 and residual to
699 while preserving the learned map, features, slope and training digest.

A production mutation that admitted held-out rows into feature construction failed
three tests in `LearnedTemplateSuite`. Restoring the source restored passing results.
External-package compile probes also reject forged training capabilities and
passing held-out data to the fitter. This establishes this recipe's boundary;
it does not certify arbitrary externally prepared features.

## Measured eyesim bridge

The pinned exported `eyesim::template_similarity_cv` call uses cosine on supplied
normalized maps, explicit participant grouping, two folds, seed 42 and no controls
or learned similarity transform. Its actual fold assignment is recorded in
`tools/r-parity/fixtures/template-cv.json`, with per-fold training/evaluation counts
and overlap exclusions. Native `MapTemplateSplit`, `Pairing` and
`Distribution.cosine` reproduce the recorded folds' exclusions and scores within
absolute tolerance 1e-12. Each fold has a cross-fold repeated match key, so omission
of the exclusion check cannot pass from an empty-overlap fixture.

The native workflow accepts explicit fold membership. It does not emulate R's RNG.
The pinned R call changes the caller's RNG state (`rng_restored: false`); that
measured side effect is retained. R selects the first duplicate reference and warns
then drops unmatched source rows. Native pairing retains ambiguity and missing-key
records, with the different behavior tested explicitly. The fixture includes both
reference cases and their full output/warning records.

The learned mean/cosine response recipe is intentionally a different estimand from
that untransformed matched-map CV call. It is not parity with PCA, CORAL, CCA or
other learned density transforms. The separately measured `template_regression`
and `template_multireg` LM paths are covered by
[SurfaceDecomposition](SURFACE_DECOMPOSITION.md), including normalized predictors,
baseline/source slopes, explicit intercept policy and Signed fitted/residual maps.
Robust, NNLS and logistic variants remain outside this bounded baseline slice.

Regenerate the offline oracle with:

```sh
python3 tools/r-parity/generate_template_cv.py --eyesim /path/to/eyesim --check
sbt 'designJVM/testOnly *TemplateCvSuite *LearnedTemplateSuite' \
    'designJS/testOnly *TemplateCvSuite *LearnedTemplateSuite' \
    'codecJVM/testOnly *LearnedTemplateRecipeSuite' \
    'codecJS/testOnly *LearnedTemplateRecipeSuite'
```

Normal fitting and tests consume checked-in evidence and require no R installation.
