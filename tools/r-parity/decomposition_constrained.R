args <- commandArgs(trailingOnly = TRUE)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
input <- jsonlite::fromJSON(args[[2L]], simplifyVector = FALSE)
map <- function(x) list(z = matrix(unlist(x), nrow = 2))
nnls <- lapply(input$nnls, function(case) {
  covars <- names(case$maps)
  maps <- tibble::tibble(response = list(map(case$response)))
  for (k in covars) maps[[k]] <- list(map(case$maps[[k]]))
  # template_multireg(method = "nnls") ignores its intercept argument; record both calls.
  fit <- function(intercept) eyesim::template_multireg(maps, "response", covars,
    method = "nnls", intercept = intercept)$multireg[[1L]]
  with_intercept <- fit(TRUE)
  without <- fit(FALSE)
  list(name = case$name, terms = without$term, coefficients = unname(without$estimate),
    coefficients_intercept_true = unname(with_intercept$estimate))
})
rank <- lapply(input$rank, function(case) {
  ref <- tibble::tibble(id = "one", density = list(map(case$reference)))
  source <- tibble::tibble(id = "one", group = "all", density = list(map(case$source)))
  baseline <- tibble::tibble(group = "all", density = list(map(case$baseline)))
  reg <- eyesim::template_regression(ref, source, "id", baseline, "group", method = "rank")
  # The same layout eyesim builds, through ppcor's Pearson route as a backend reference.
  df1 <- data.frame(y = unlist(case$source), baseline = unlist(case$baseline),
    x2 = unlist(case$reference))
  pearson <- ppcor::pcor(df1, method = "pearson")$estimate[2:3, 1]
  list(name = case$name, beta_baseline = unname(reg$beta_baseline),
    beta_source = unname(reg$beta_source), pearson_baseline = unname(pearson[[1L]]),
    pearson_source = unname(pearson[[2L]]))
})
out <- list(nnls = nnls, rank = rank, R = as.character(getRversion()),
  packages = setNames(lapply(sort(loadedNamespaces()), function(p) as.character(utils::packageVersion(p))), sort(loadedNamespaces())))
jsonlite::write_json(out, args[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
