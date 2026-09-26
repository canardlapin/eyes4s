args <- commandArgs(trailingOnly = TRUE)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
input <- jsonlite::fromJSON(args[[2L]], simplifyVector = FALSE)
results <- lapply(input$cases, function(case) {
  norm <- function(x) { x <- unlist(x); x / sum(x) }
  a <- norm(case$a); b <- norm(case$b); y <- norm(case$y)
  maps <- tibble::tibble(response = list(list(z = matrix(y, nrow = 2))),
    a = list(list(z = matrix(a, nrow = 2))), b = list(list(z = matrix(b, nrow = 2))))
  multi <- eyesim::template_multireg(maps, "response", c("a", "b"),
    method = "lm", intercept = case$intercept)$multireg[[1L]]
  fit <- stats::lm(if (case$intercept) y ~ a + b else y ~ a + b - 1)
  # template_regression has an implicit intercept and reports only the two slopes.
  legacy <- if (case$intercept) {
    ref <- tibble::tibble(id = "one", density = list(list(z = matrix(b, nrow = 2))))
    source <- tibble::tibble(id = "one", group = "all", density = list(list(z = matrix(y, nrow = 2))))
    baseline <- tibble::tibble(group = "all", density = list(list(z = matrix(a, nrow = 2))))
    reg <- eyesim::template_regression(ref, source, "id", baseline, "group", method = "lm")
    c(reg$beta_baseline, reg$beta_source)
  } else NULL
  list(name = case$name, terms = multi$term, coefficients = multi$estimate,
    regression_slopes = legacy, fitted = unname(fitted(fit)), residuals = unname(residuals(fit)),
    r_squared = unname(summary(fit)$r.squared))
})
out <- list(results = results, R = as.character(getRversion()),
  packages = setNames(lapply(sort(loadedNamespaces()), function(p) as.character(utils::packageVersion(p))), sort(loadedNamespaces())))
jsonlite::write_json(out, args[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
