# Fixed-feature, intercept-free OLS backend for TemplateFitCsv.
# Deliberately consumes only the training export, not a combined study/recipe.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: Rscript --vanilla tools/template-fit/fit.R training.csv coefficients.csv")
if (file.exists(args[[2L]])) stop("Refusing to overwrite coefficient output: ", args[[2L]])
tab <- read.csv(args[[1L]], colClasses = "character", check.names = FALSE,
                stringsAsFactors = FALSE, na.strings = NULL)
prefix <- c("method", "training_hash", "basis", "response_unit", "row", "fold", "response")
if (ncol(tab) <= length(prefix) || !identical(names(tab)[seq_along(prefix)], prefix) ||
    anyDuplicated(names(tab)) || !all(startsWith(names(tab)[-seq_along(prefix)], "feature:")))
  stop("Invalid training header in ", args[[1L]])
features <- substring(names(tab)[-seq_along(prefix)], 9L)
if (any(!nzchar(trimws(features))) || anyDuplicated(features) || nrow(tab) < length(features))
  stop("Empty feature names or underdetermined training design in ", args[[1L]])
method <- "eyes4s.no-intercept-r-lm-qr/1"
if (!identical(unique(tab$method), method) ||
    length(unique(tab$training_hash)) != 1L ||
    !grepl("^[0-9a-f]{16}$", tab$training_hash[[1]]) ||
    length(unique(tab$basis)) != 1L || length(unique(tab$response_unit)) != 1L ||
    any(!nzchar(trimws(tab$basis))) || any(!nzchar(trimws(tab$response_unit))) ||
    any(!nzchar(trimws(tab$fold))) ||
    !identical(tab$row, as.character(seq_len(nrow(tab)) - 1L)))
  stop("Inconsistent training metadata in ", args[[1L]])
numeric_column <- function(v) {
  result <- suppressWarnings(as.numeric(v))
  if (any(!is.finite(result))) stop("Non-finite training value in ", args[[1L]])
  result
}
x <- as.matrix(as.data.frame(lapply(tab[-seq_along(prefix)], numeric_column), check.names = FALSE))
y <- numeric_column(tab$response)
# No intercept is appended, and no held-out data is available to lm.fit.
rank_tolerance <- 1e-7
fit <- stats::lm.fit(x = x, y = y, tol = rank_tolerance, singular.ok = FALSE)
if (fit$rank != ncol(x) || any(!is.finite(fit$coefficients)))
  stop("Rank-deficient or non-finite fit for training hash ", tab$training_hash[[1]])
out <- data.frame(method = method, training_hash = tab$training_hash[[1]],
  feature = features, coefficient = sprintf("%.17g", fit$coefficients),
  rank = fit$rank, observations = nrow(x),
  backend = paste0("R-", getRversion(), "/stats-lm.fit"),
  rank_tolerance = sprintf("%.17g", rank_tolerance), check.names = FALSE)
write.table(out, file = args[[2L]], sep = ",", quote = TRUE, row.names = FALSE,
            col.names = TRUE, eol = "\r\n", na = "NA")
