# Public repetition calls against the archived package installed by the generator.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
x <- read.csv(args[[2L]], stringsAsFactors = FALSE)
ids <- paste(x$participant, x$image, x$phase, sep = "/")
tab <- dplyr::bind_rows(lapply(unique(ids), function(id) {
  f <- x[ids == id, ]
  cells <- 1L + floor(f$x_px) + 2L * floor(f$y_px)
  w <- numeric(4); w[cells] <- f$duration_us
  tibble::tibble(id = id, participant = f$participant[[1]], phase = f$phase[[1]],
                 density = list(w / sum(w)))
}))
run <- function(t, pairwise) {
  warnings <- character()
  result <- withCallingHandlers(
    eyesim::repetitive_similarity(t, condition_var = "phase", method = "cosine", pairwise = pairwise),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(rows = lapply(seq_len(nrow(result)), function(i) {
    r <- list(id = result$id[[i]], same = result$repsim[[i]], other = result$othersim[[i]])
    if (pairwise) r$pairs <- unlist(result$pairwise_repsim[[i]], use.names = FALSE)
    r
  }), warnings = warnings)
}
packages <- sort(unique(c(loadedNamespaces(), "jsonlite")))
out <- list(
  runtime = list(R = as.character(getRversion()), packages = setNames(lapply(packages,
    function(p) as.character(utils::packageVersion(p))), packages)),
  pairwise = run(tab, TRUE), reduced = run(tab, FALSE),
  duplicate = run(dplyr::bind_rows(tab, tab[1, ]), TRUE),
  singleton = run(tab[1, ], TRUE), empty = run(tab[FALSE, ], TRUE))
jsonlite::write_json(out, args[[3]], pretty = TRUE, auto_unbox = TRUE, digits = NA, na = "string")
