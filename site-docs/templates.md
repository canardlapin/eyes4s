# Fit on training data; evaluate held-out trials

The basic fixed-feature route has an explicit train/test partition and runs entirely in Scala.
The statistical unit is a trial. Native scaled Householder QR fits without an implicit intercept.

1. Build a `TemplateBasis`, keyed `TemplateObservation`s and a `TemplateSplit`. Its separate
   training and held-out capabilities prevent passing held-out rows to the fitter.
2. Call `fitNoIntercept` with the training capability. Rank and arithmetic failures remain
   typed errors carrying the training identity.
3. Persist with `NativeTemplateRecipeCodec`, reopen the checked inputs and refit deterministically.
   The native method and fitting conventions are distinct from historical R-labelled recipes.
4. Evaluate on `split.heldOut`. Retain keyed predictions, residuals, errors and strict descriptive MSE.

The [runnable recipe](https://github.com/canardlapin/eyes4s/blob/main/docs/TEMPLATE_FITTING.md)
saves and reopens without R. Its fixture has coefficients `(1,2)` and prediction `7`; changing
only the held-out response to `700` preserves the fit and changes the residual to `693`.

The [learned mean-map recipe](https://github.com/canardlapin/eyes4s/blob/main/docs/TEMPLATE_CV.md)
constructs a template only from training maps, uses cosine as its feature, and fits a response slope.
Its separate map/response perturbation tests detect leakage during feature construction. Cellwise surface OLS is a
different estimand from trial response prediction, and its coefficients are not mixture weights.

Return to [migration](migration.md) for the supported boundary.

## Fit and reopen a fixed-feature recipe

Here the feature vectors are fixed before splitting. The two training observations identify
coefficients 1 and 2; the held-out response is deliberately different from its prediction.
The response unit is `score`, and there is no implicit intercept.

```scala mdoc:silent
import eyes4s.design.*
import eyes4s.codec.*
import eyes4s.plan.*

val recipe = for
  basis <- TemplateBasis.of("guide-fixed/1", Vector("a", "b"), "score")
  a <- TemplateObservation.of("a", "train", Vector(1.0, 0.0), 1.0)
  b <- TemplateObservation.of("b", "train", Vector(0.0, 1.0), 2.0)
  held <- TemplateObservation.of("held", "test", Vector(3.0, 2.0), 700.0)
  split <- TemplateSplit.of(basis, Vector(a, b, held), Set("test"))
  schema <- DefinitionId.of("example.guide-native-template", 1)
  keys <- DefinitionId.of("example.guide-template-key", 1)
  codec = NativeTemplateRecipeCodec.of(schema, VersionedCodec.string(keys))
  json <- codec.encode(split)
  reopened <- codec.decode(json)
  fit <- FittedTemplate.fitNoIntercept(reopened.training)
  evaluation <- fit.evaluate(reopened.heldOut)
yield evaluation

assert(recipe.exists(_.rows.head.result.exists { case (prediction, residual) =>
  math.abs(prediction - 7.0) < 1e-12 && math.abs(residual - 693.0) < 1e-12
}))
assert(TemplateObservation.of("bad", "train", Vector(1.0), Double.NaN).isLeft)
```

```scala mdoc
recipe.map(_.rows.map(row => (row.key, row.result)))
```

For features learned from maps, use the separate learned mean-map recipe linked above.
Reusing a template estimated from all observations would leak held-out information.
