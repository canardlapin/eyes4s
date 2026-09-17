# Public admission reference from the pinned package, not sourced checkout functions.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
spec <- jsonlite::fromJSON(args[[2L]])$admission
rows <- spec$rows
observe <- function(f) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(f(), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
  }), error = function(e) list(error = conditionMessage(e)))
  list(value = value, warnings = warnings)
}
group_value <- function(g) list(coords = unname(eyesim::coords(g)),
  onset = unname(g$onset), duration = unname(g$duration), index = unname(g$index))
table_value <- function(d, bounds = spec$clip_bounds, relative = TRUE) {
  t <- eyesim::eye_table("x", "y", "duration", "onset", c("participant", "trial"),
    data = d, clip_bounds = bounds, relative_coords = relative)
  list(origin = attr(t, "origin"), groups = lapply(seq_len(nrow(t)), function(i)
    list(participant = t$participant[[i]], trial = t$trial[[i]], data = group_value(t$fixgroup[[i]]))))
}
fg <- function(d) eyesim::fixation_group(d$x, d$y, duration = d$duration, onset = d$onset)
valid <- rows[c(1L, 4L), ]
duplicate <- rbind(valid, valid[1L, ])
negative <- rows; negative$duration[1L] <- -1
missing <- rows; missing$x[1L] <- NA_real_
boundary <- valid; boundary$x[1L] <- 100; boundary$y[1L] <- 50
shifted <- valid; shifted$x <- shifted$x + 100; shifted$y <- shifted$y + 50
out <- list(
  original = observe(function() table_value(rows)),
  reversed = observe(function() table_value(rows[nrow(rows):1L, ])),
  valid = observe(function() table_value(valid)),
  duplicate = observe(function() table_value(duplicate)),
  negative = observe(function() table_value(negative)),
  missing = observe(function() table_value(missing)),
  boundary = observe(function() table_value(boundary)),
  shifted = observe(function() table_value(shifted, c(100, 200, 50, 100))),
  flipped = observe(function() table_value(valid, c(100, 0, 50, 0))),
  empty = observe(function() table_value(rows[FALSE, ])),
  group = observe(function() group_value(fg(rows))),
  empty_group = observe(function() group_value(fg(rows[FALSE, ]))),
  negative_group = observe(function() group_value(fg(negative))),
  reclassify = observe(function() {
    x <- eyesim::as_eye_table(data.frame(unrelated = 42))
    list(class = class(x), names = names(x), rows = nrow(x))
  })
)
packages <- sort(unique(c(loadedNamespaces(), "jsonlite")))
out$runtime <- list(R = as.character(getRversion()), packages = setNames(
  lapply(packages, function(p) as.character(utils::packageVersion(p))), packages))
jsonlite::write_json(out, args[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = NA, na = "string")
