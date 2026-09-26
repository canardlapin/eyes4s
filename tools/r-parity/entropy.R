# Pinned eyesim entropy and map-arithmetic reference. Called by generate_entropy.py.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
spec <- jsonlite::fromJSON(args[[2L]], simplifyVector = TRUE)$entropy_and_arithmetic
lattice <- spec$lattice
nx <- length(lattice$x)
ny <- length(lattice$y)
stopifnot(nx == 2L, ny == 2L)
bases <- as.numeric(spec$log_bases)
stopifnot(length(bases) == 2L, bases[[1L]] == 2, bases[[2L]] == exp(1))
base_labels <- c("2", "e")

# An eye_density object with the supplied cells in place of a kernel estimate,
# built the way eye_density.fixation_group builds one (x, y, z, fixgroup, sigma)
# and carrying the same class vector, so fixation_entropy and Ops.eye_density
# dispatch exactly as they would on an estimated map. z[ix, iy] is the nx-by-ny
# matrix, so as.numeric(z) is column-major and cell k = iy * nx + ix.
density_map <- function(cells, x = lattice$x, y = lattice$y) {
  stopifnot(length(cells) == length(x) * length(y))
  structure(
    list(
      x = as.numeric(x), y = as.numeric(y),
      z = matrix(as.numeric(cells), nrow = length(x), ncol = length(y)),
      fixgroup = NULL, sigma = NA_real_
    ),
    class = c("eye_density", "density", "list")
  )
}

# R has four non-finite spellings that JSON has none of; each is named explicitly.
r_value <- function(v) {
  stopifnot(length(v) == 1L)
  if (is.nan(v)) list(kind = "NaN")
  else if (is.na(v)) list(kind = "NA")
  else if (v == Inf) list(kind = "Inf")
  else if (v == -Inf) list(kind = "-Inf")
  else list(kind = "finite", value = v)
}
r_values <- function(v) lapply(as.numeric(v), r_value)

p <- density_map(spec$mass_p)
q <- density_map(spec$mass_q)
r <- density_map(spec$mass_r)
counts <- density_map(spec$counts)
signed <- density_map(spec$signed)

observe <- function(expr) {
  tryCatch({
    force(expr)
    list(outcome = "value", detail = "returned without error")
  }, error = function(e) list(outcome = "error", detail = conditionMessage(e)))
}

# Ops.eye_density on the pinned lattice. `*` and a shifted lattice are
# observed failures; every other result is recorded cell by cell along with
# the class eyesim gives it and whether it still carries a sigma.
ops <- list(
  plus_p_q = p + q,
  minus_p_q = p - q,
  div_p_q = p / q,
  div_q_p = q / p,
  div_r_q = r / q,
  div_p_p = p / p
)
op_records <- lapply(ops, function(result) {
  list(
    cells = r_values(result$z),
    class = class(result),
    has_sigma = "sigma" %in% names(result),
    total = r_value(sum(result$z))
  )
})
outcomes <- list(
  product_p_q = observe(p * q),
  shifted_lattice_minus = observe(
    p - density_map(spec$mass_q, x = spec$shifted_lattice$x, y = spec$shifted_lattice$y)
  )
)

# fixation_entropy.eye_density, hence entropy_from_mass, on positive maps, on
# an unnormalised count map, and on every signed map the operators produced.
# The same cells under the bare "density" class reach fixation_entropy.density.
p_density_class <- structure(unclass(p), class = c("density", "list"))
entropy_inputs <- c(
  list(mass_p = p, mass_q = q, mass_r = r, counts = counts, signed = signed,
       mass_p_density_class = p_density_class),
  ops[c("minus_p_q", "div_p_q", "div_p_p")]
)
# An entropy that fails is pinned as an error with its message rather than stopping the script.
entropy_value <- function(map, normalize, base) {
  tryCatch(
    r_value(eyesim::fixation_entropy(map, normalize = normalize, base = base)),
    error = function(e) list(kind = "error", detail = conditionMessage(e))
  )
}
entropies <- lapply(entropy_inputs, function(map) {
  per_base <- lapply(seq_along(bases), function(i) {
    list(
      raw = entropy_value(map, FALSE, bases[[i]]),
      normalized = entropy_value(map, TRUE, bases[[i]])
    )
  })
  names(per_base) <- base_labels
  per_base
})

meta <- utils::packageDescription("eyesim")
direct <- trimws(gsub("\\s*\\(.*?\\)", "", unlist(strsplit(paste(meta$Imports, meta$LinkingTo, sep = ","), ","))))
packages <- sort(unique(c(loadedNamespaces(), direct, "jsonlite")))
versions <- setNames(lapply(packages, function(p) as.character(utils::packageVersion(p))), packages)
out <- list(
  runtime = list(R = as.character(getRversion()), packages = versions),
  machine_eps = .Machine$double.eps,
  entropy = entropies,
  ops = op_records,
  outcomes = outcomes
)
jsonlite::write_json(out, args[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = I(17))
