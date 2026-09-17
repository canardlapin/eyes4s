# Fit on training data; evaluate held-out trials

The supported basic route has fixed features and an explicit train/test partition. The statistical
unit is a trial, not a grid cell. Fitting runs in the external R adapter, keeping solver dependencies
out of the pure library.

1. Build a `TemplateBasis`, keyed `TemplateObservation`s and a `TemplateSplit`. The split gives
   separate training and held-out capabilities; exporting training cannot select held-out rows.
2. Persist the recipe with `TemplateRecipeCodec`. It retains keys, data identity, folds, method
   and the training identity to which a coefficient receipt must belong.
3. Export with `TemplateFitCsv`, run `tools/template-fit/fit.R`, then import its validated
   full-rank coefficient receipt against that training set.
4. Evaluate the `FittedTemplate` on held-out trials. Keep keyed predictions, residuals, errors and
   the strict descriptive MSE. Never refit to improve the held-out metric.

The repository's [complete runnable recipe](https://github.com/canardlapin/eyes4s/blob/main/docs/TEMPLATE_FITTING.md)
executes Scala export, R QR fitting and reimport. Its exact fixture has coefficients `(1,2)` and
held-out prediction `7`; replacing only the held-out response with `700` leaves coefficients
unchanged and changes evaluation error.

This does **not** prove learned-feature safety. If features or spatial templates are learned,
their fitting must also occur inside the training partition. Cross-fitted learned similarity and
typed surface regression remain unsupported. Intercept-free trial regression is not eyesim's
normalized-map regression; coefficient normalization alone does not produce mixture weights.

Return to [migration](migration.md) for the supported boundary.
