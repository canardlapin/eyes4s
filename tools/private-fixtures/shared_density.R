# Explicitly invoked reference producer. All inputs and outputs are external to Git.
args <- commandArgs(trailingOnly = TRUE)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
library(jsonlite)
request <- fromJSON(args[[2L]], simplifyVector = FALSE)
out <- args[[3L]]
stopifnot(request$schema_version == 1L, length(request$templates) > 1L,
          length(request$sources) > 0L, request$min_fixations >= 2L)

density <- function(item) {
  f <- item$fixations
  x <- as.numeric(unlist(f$x)); y <- as.numeric(unlist(f$y))
  duration <- as.numeric(unlist(f$duration_ms)); onset <- as.numeric(unlist(f$onset_ms))
  stopifnot(length(x) == length(y), length(x) == length(duration),
            length(x) == length(onset), length(x) >= request$min_fixations,
            all(is.finite(c(x, y, duration, onset))), all(duration > 0))
  # Selection and crop exclusions must already be explicit in the private request.
  stopifnot(all(x >= request$x_min & x < request$x_max),
            all(y >= request$y_min & y < request$y_max))
  d <- eye_density(fixation_group(x, y, duration, onset),
    sigma = request$sigma_px, xbounds = c(request$x_min, request$x_max),
    ybounds = c(request$y_min, request$y_max), outdim = c(request$nx, request$ny),
    duration_weighted = TRUE, min_fixations = request$min_fixations,
    normalize = TRUE, kde_pkg = "ks")
  stopifnot(!is.null(d), all(is.finite(get_density(d))))
  d
}

templates <- request$templates
sources <- request$sources
template_ids <- vapply(templates, `[[`, "", "id")
source_ids <- vapply(sources, `[[`, "", "id")
stopifnot(!anyDuplicated(template_ids), !anyDuplicated(source_ids))
template_maps <- lapply(templates, density)
source_maps <- lapply(sources, density)
ref <- tibble::tibble(template = template_ids,
  participant = vapply(templates, `[[`, "", "participant"), density = template_maps)
src <- tibble::tibble(id = source_ids,
  template = vapply(sources, `[[`, "", "template"),
  participant = vapply(sources, `[[`, "", "participant"), density = source_maps)
indices <- match(src$template, ref$template)
stopifnot(!anyNA(indices), all(src$participant == ref$participant[indices]))
future::plan(future::sequential)
set.seed(0)
# Exhaustive distinct controls in this declared source cohort: no cross-RNG sampling comparison.
scores <- template_similarity(ref, src, match_on = "template", permute_on = "participant",
  method = "cosine", permutations = .Machine$integer.max)
stopifnot(nrow(scores) == nrow(src), identical(scores$id, src$id), all(scores$n_perm > 0L),
          all(is.finite(scores$eye_sim)), all(is.finite(scores$perm_sim)),
          all(is.finite(scores$eye_sim_diff)))
map_values <- function(d) as.numeric(get_density(d))
result <- list(nx = request$nx, ny = request$ny,
  templates = lapply(seq_along(templates), function(i) list(
    id = ref$template[[i]], participant = ref$participant[[i]], values = map_values(template_maps[[i]]))),
  sources = lapply(seq_along(sources), function(i) list(
    id = src$id[[i]], participant = src$participant[[i]], template = src$template[[i]],
    values = map_values(source_maps[[i]]), observed = scores$eye_sim[[i]],
    baseline = scores$perm_sim[[i]], difference = scores$eye_sim_diff[[i]], n_perm = scores$n_perm[[i]])))
write_json(result, file.path(out, "shared.json"), auto_unbox = TRUE, digits = NA)
writeLines(capture.output(sessionInfo()), file.path(out, "R-session.txt"))
