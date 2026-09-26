# Pinned eyesim fixation_entropy on derived inputs. Called by generate_derived_entropy.py.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
spec <- jsonlite::fromJSON(args[[2L]], simplifyVector = TRUE)
bases <- as.numeric(spec$log_bases)
stopifnot(length(bases) == 2L, bases[[1L]] == 2, bases[[2L]] == exp(1))
base_labels <- c("2", "e")

# R has four non-finite spellings that JSON has none of; each is named explicitly.
r_value <- function(v) {
  stopifnot(length(v) == 1L)
  if (is.nan(v)) list(kind = "NaN")
  else if (is.na(v)) list(kind = "NA")
  else if (v == Inf) list(kind = "Inf")
  else if (v == -Inf) list(kind = "-Inf")
  else list(kind = "finite", value = v)
}
# A value, a named per-scale vector, or an error with its message.
observe <- function(expr) {
  tryCatch({
    v <- force(expr)
    if (length(v) == 1L && is.null(names(v))) r_value(v)
    else list(kind = "vector", names = as.list(names(v)), values = lapply(as.numeric(v), r_value))
  }, error = function(e) list(kind = "error", detail = conditionMessage(e)))
}
# Every normalize/base combination of one call.
grid_of <- function(call) {
  out <- lapply(seq_along(bases), function(i) {
    list(raw = observe(call(FALSE, bases[[i]])), normalized = observe(call(TRUE, bases[[i]])))
  })
  names(out) <- base_labels
  out
}

g <- spec$fixation_group
fg <- fixation_group(x = g$x, y = g$y, onset = g$onset_ms, duration = g$duration_ms)
single <- fg[1L, ]
lat <- spec$explicit_lattice
dens <- spec$density
padded_lattices <- jsonlite::fromJSON(args[[2L]], simplifyVector = FALSE)$padded_lattices

# method = "grid": explicit bounds, eyesim's padded default bounds, and the
# duration_weighted flag the grid method does not read.
grid <- list(
  explicit = grid_of(function(n, b) fixation_entropy(
    fg, normalize = n, base = b, method = "grid", grid = lat$grid,
    xbounds = lat$xbounds, ybounds = lat$ybounds)),
  explicit_duration_weighted = grid_of(function(n, b) fixation_entropy(
    fg, normalize = n, base = b, method = "grid", grid = lat$grid,
    xbounds = lat$xbounds, ybounds = lat$ybounds, duration_weighted = TRUE)),
  padded = lapply(padded_lattices, function(lattice) {
    stopifnot(lattice$padding == 0.05)  # eyesim's fixed padding; not an argument.
    grid_of(function(n, b) fixation_entropy(
      fg, normalize = n, base = b, method = "grid", grid = as.numeric(unlist(lattice$grid))))
  }),
  single = grid_of(function(n, b) fixation_entropy(single, normalize = n, base = b, method = "grid"))
)

# method = "density" on explicit bounds, with the map eye_density builds from
# the same arguments, so the entropy step can be checked on eyesim's own map.
density_map <- function(weighted, sigma = dens$sigma) {
  d <- eye_density(fg, sigma = sigma, xbounds = dens$xbounds, ybounds = dens$ybounds,
                   outdim = dens$outdim, duration_weighted = weighted)
  list(x = as.numeric(d$x), y = as.numeric(d$y), z = as.numeric(d$z), sigma = d$sigma)
}
density <- lapply(c(unweighted = FALSE, duration_weighted = TRUE), function(weighted) {
  list(
    entropy = grid_of(function(n, b) fixation_entropy(
      fg, normalize = n, base = b, method = "density", sigma = dens$sigma,
      xbounds = dens$xbounds, ybounds = dens$ybounds, outdim = dens$outdim,
      duration_weighted = weighted)),
    map = density_map(weighted)
  )
})
x_pad <- range(fg$x) + c(-0.05, 0.05) * diff(range(fg$x))
y_pad <- range(fg$y) + c(-0.05, 0.05) * diff(range(fg$y))
density_default <- list(
  entropy = observe(fixation_entropy(fg, method = "density")),
  suggested_sigma_padded = observe(suggest_sigma(fg, xbounds = x_pad, ybounds = y_pad)),
  suggested_sigma_unclamped = observe(suggest_sigma(fg)),
  single_default = observe(fixation_entropy(single, method = "density")),
  single_explicit_sigma = observe(fixation_entropy(single, method = "density", sigma = dens$sigma))
)

# Multiscale: eye_density with a sigma vector, requested in descending order.
ms_sigmas <- as.numeric(spec$multiscale$sigmas)
ms <- eye_density(fg, sigma = ms_sigmas, xbounds = dens$xbounds, ybounds = dens$ybounds,
                  outdim = dens$outdim)
multiscale <- list(
  class = class(ms),
  sigmas_vector = as.numeric(attr(ms, "sigmas_vector")),
  maps = lapply(ms, function(d) list(sigma = d$sigma, z = as.numeric(d$z))),
  none = grid_of(function(n, b) fixation_entropy(ms, normalize = n, base = b, aggregate = "none")),
  mean = grid_of(function(n, b) fixation_entropy(ms, normalize = n, base = b, aggregate = "mean")),
  # The fixation_group method with a sigma vector: its only reduction is the mean.
  from_group = grid_of(function(n, b) fixation_entropy(
    fg, normalize = n, base = b, method = "density", sigma = ms_sigmas,
    xbounds = dens$xbounds, ybounds = dens$ybounds, outdim = dens$outdim)),
  from_group_none = observe(fixation_entropy(
    fg, method = "density", sigma = ms_sigmas, xbounds = dens$xbounds,
    ybounds = dens$ybounds, outdim = dens$outdim, aggregate = "none"))
)

# Supplied maps packaged the way eye_density packages a sigma vector, so the
# multiscale method dispatches as it would on an estimate. z[ix, iy] is the
# nx-by-ny matrix, so as.numeric(z) is column-major and cell k = iy * nx + ix.
sup <- spec$supplied_scales
supplied_map <- function(cells, sigma) {
  structure(
    list(x = as.numeric(sup$lattice$x), y = as.numeric(sup$lattice$y),
         z = matrix(as.numeric(cells), nrow = length(sup$lattice$x)),
         fixgroup = NULL, sigma = sigma),
    class = c("eye_density", "density", "list")
  )
}
multiscale_of <- function(maps, sigmas) {
  out <- lapply(seq_along(sigmas), function(i) supplied_map(maps[[i]], sigmas[[i]]))
  attr(out, "sigmas_vector") <- sigmas
  class(out) <- c("eye_density_multiscale", "list")
  out
}
sup_maps <- lapply(seq_len(nrow(sup$maps)), function(i) sup$maps[i, ])
sup_sigmas <- as.numeric(sup$sigmas)
supplied <- multiscale_of(sup_maps, sup_sigmas)
with_zero <- multiscale_of(c(sup_maps, list(rep(0, 4L))), c(sup_sigmas, sup$zero_sigma))
supplied_scales <- list(
  none = grid_of(function(n, b) fixation_entropy(supplied, normalize = n, base = b, aggregate = "none")),
  mean = grid_of(function(n, b) fixation_entropy(supplied, normalize = n, base = b, aggregate = "mean")),
  zero_none = grid_of(function(n, b) fixation_entropy(with_zero, normalize = n, base = b, aggregate = "none")),
  zero_mean = grid_of(function(n, b) fixation_entropy(with_zero, normalize = n, base = b, aggregate = "mean"))
)

meta <- utils::packageDescription("eyesim")
direct <- trimws(gsub("\\s*\\(.*?\\)", "", unlist(strsplit(paste(meta$Imports, meta$LinkingTo, sep = ","), ","))))
packages <- sort(unique(c(loadedNamespaces(), direct, "jsonlite")))
versions <- setNames(lapply(packages, function(p) as.character(utils::packageVersion(p))), packages)
out <- list(
  runtime = list(R = as.character(getRversion()), packages = versions),
  machine_eps = .Machine$double.eps,
  grid = grid,
  density = density,
  density_default = density_default,
  multiscale = multiscale,
  supplied_scales = supplied_scales
)
jsonlite::write_json(out, args[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = I(17))
