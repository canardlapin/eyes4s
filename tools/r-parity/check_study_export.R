# Independently read the actual Scala CSV with base R and compare pinned eyesim values.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
actual <- read.csv(args[[1]], check.names = FALSE, stringsAsFactors = FALSE)
reference <- jsonlite::fromJSON(args[[2]], simplifyVector = FALSE)
stopifnot(nrow(actual) == 6L, all(actual$status == "ok"),
          all(actual$matched_contributing == 1L), all(actual$control_contributing == 2L),
          all(actual$spatial_unit == "px"))
expected <- c("s1/a/recall" = 7/25, "s1/b/recall" = -1/10,
              "s1/c/recall" = -1/50, "s2/a/recall" = 1/50,
              "s2/b/recall" = 8/25, "s2/c/recall" = -1/50)
key <- paste(actual$participant, actual$stimulus, actual$phase, sep = "/")
stopifnot(!anyDuplicated(key), setequal(key, names(expected)),
          max(abs(actual$difference - unname(expected[key]))) < 1e-12)
observed_reference <- reference$correct$rows
reference_ids <- vapply(observed_reference, `[[`, character(1), "id")
ordered_reference <- observed_reference[match(key, reference_ids)]
stopifnot(all(vapply(ordered_reference, function(x) !is.null(x), logical(1))))
for (pair in list(c("matched", "eye_sim"), c("control", "perm_sim"), c("difference", "eye_sim_diff"))) {
  target <- vapply(ordered_reference, `[[`, numeric(1), pair[[2]])
  stopifnot(max(abs(actual[[pair[[1]]]] - target)) < 1e-12)
}
stopifnot(all(vapply(actual$plan_json, jsonlite::validate, logical(1))))
cat("Base R read 6 exported contrasts; keys, counts, units, JSON plans and rational targets agree.\n")
