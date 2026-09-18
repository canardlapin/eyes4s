# Pinned eyesim public API reference. Called by generate_reference.py.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
future::plan(future::sequential)
x <- read.csv(args[[2L]], stringsAsFactors = FALSE)
trial_id <- paste(x$participant, x$image, x$phase, sep = "/")
rows <- lapply(unique(trial_id), function(id) {
  fix <- x[trial_id == id, ]
  # This fixture places one fixation at each cell centre, x varying fastest.
  cell <- 1L + floor(fix$x_px) + 2L * floor(fix$y_px)
  stopifnot(identical(as.integer(sort(cell)), 1:4))
  duration <- numeric(4)
  duration[cell] <- fix$duration_us
  tibble::tibble(
    participant = fix$participant[[1L]], image = fix$image[[1L]],
    phase = fix$phase[[1L]], key = paste(fix$participant[[1L]], fix$image[[1L]], sep = "/"),
    id = id, density = list(duration / sum(duration))
  )
})
trials <- dplyr::bind_rows(rows)
reference <- trials[trials$phase == "encode", ]
source <- trials[trials$phase == "recall", ]
run <- function(ref, src, match_on) {
  warnings <- character()
  result <- withCallingHandlers(
    eyesim::template_similarity(ref, src, match_on = match_on, permute_on = "participant",
                               method = "cosine", permutations = 100L),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  list(rows = as.data.frame(result[c("id", "eye_sim", "perm_sim", "eye_sim_diff", "n_perm")]),
       warnings = warnings)
}
correct <- run(reference, source, "key")
image_only <- run(reference, source, "image")
unmatched <- source[1L, ]
unmatched$participant <- "s3"; unmatched$key <- "s3/a"; unmatched$id <- "s3/a/recall"
missing_source <- run(reference, dplyr::bind_rows(source, unmatched), "key")
duplicate_reference <- run(dplyr::bind_rows(reference, reference[1L, ]), source, "key")
# Constant Pearson is a separately identified undefined-estimand case.
constant_pearson <- eyesim::similarity(rep(0.25, 4), rep(0.25, 4), method = "pearson")
# Include the JSON writer and build/import dependencies even if lazily unloaded.
meta <- utils::packageDescription("eyesim")
direct <- trimws(gsub("\\s*\\(.*?\\)", "", unlist(strsplit(paste(meta$Imports, meta$LinkingTo, sep = ","), ","))))
packages <- sort(unique(c(loadedNamespaces(), direct, "jsonlite", "future")))
versions <- setNames(lapply(packages, function(p) as.character(utils::packageVersion(p))), packages)
out <- list(
  runtime = list(R = as.character(getRversion()), packages = versions),
  correct = correct, image_only = image_only, missing_source = missing_source,
  duplicate_reference = duplicate_reference, constant_pearson = constant_pearson
)
jsonlite::write_json(out, args[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = NA)
