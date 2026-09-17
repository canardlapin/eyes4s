# Pinned eyesim public coordinate-transform reference. Called by generate_transforms.py.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
options(digits = 17)
spec <- jsonlite::fromJSON(args[[2L]], simplifyVector = TRUE)$transforms
as_rows <- function(value) {
  # jsonlite returns a numeric matrix for a rectangular array and a list otherwise.
  if (is.list(value)) value <- do.call(rbind, lapply(value, as.numeric))
  matrix(as.numeric(value), ncol = 2L)
}
points <- as_rows(spec$points)
stopifnot(identical(points[1L, ], c(0, 0)), nrow(points) == 3L)
n <- nrow(points)
fg <- eyesim::fixation_group(
  x = points[, 1L], y = points[, 2L],
  onset = seq(0, by = 100, length.out = n), duration = rep(100, n)
)
coords <- function(group) unname(as.matrix(group[, c("x", "y")]))
bounds <- as.numeric(spec$bounds)
supplied <- list(
  center_supplied = coords(eyesim::center(fg, origin = as.numeric(spec$origin))),
  center_default = coords(eyesim::center(fg)),
  rescale = coords(eyesim::rescale(fg, sx = as.numeric(spec$rescale[[1L]]), sy = as.numeric(spec$rescale[[2L]]))),
  normalize = coords(eyesim::normalize(fg, xbounds = bounds[1:2], ybounds = bounds[3:4]))
)
# eyesim's affine_transform and contract_transform are fitted density-space maps:
# they take reference/source tables whose match column holds density objects.
# Handing them coordinate tables is the documented misuse; the observed failure
# is recorded so the boundary is pinned rather than asserted.
key <- c("a", "b", "c")
pairs <- spec$fit_pairs
sources <- as_rows(pairs$source)
targets <- as_rows(pairs$target)
stopifnot(nrow(sources) == 3L, nrow(targets) == 3L, identical(targets[1L, ], c(10, -4)))
coord_tab <- function(rows) {
  tibble::tibble(key = key, density = lapply(seq_len(nrow(rows)), function(i) rows[i, ]))
}
ref_tab <- coord_tab(targets)
source_tab <- coord_tab(sources)
observe <- function(call) {
  tryCatch({
    force(call)
    list(outcome = "value", detail = "returned without error")
  }, error = function(e) list(outcome = "error", detail = conditionMessage(e)))
}
fitted <- list(
  affine_transform = observe(eyesim::affine_transform(ref_tab, source_tab, match_on = "key")),
  contract_transform = observe(eyesim::contract_transform(ref_tab, source_tab, match_on = "key"))
)
meta <- utils::packageDescription("eyesim")
direct <- trimws(gsub("\\s*\\(.*?\\)", "", unlist(strsplit(paste(meta$Imports, meta$LinkingTo, sep = ","), ","))))
packages <- sort(unique(c(loadedNamespaces(), direct, "jsonlite")))
versions <- setNames(lapply(packages, function(p) as.character(utils::packageVersion(p))), packages)
out <- list(
  runtime = list(R = as.character(getRversion()), packages = versions),
  supplied = supplied,
  fitted = fitted
)
jsonlite::write_json(out, args[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = NA)
