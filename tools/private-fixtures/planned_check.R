args <- commandArgs(TRUE)
.libPaths(c(args[[1]], .libPaths()))
library(jsonlite)
library(eyesim)
p <- fromJSON(file.path(args[[2]], 'verification.json'), simplifyVector=FALSE)
max_error <- 0
rank_count <- 0L
pair_count <- 0L
for (engine in c('eyes4s','eyesim')) {
  maps <- lapply(p$maps[[engine]], function(v) as.numeric(unlist(v)))
  for (id in names(maps)) {
    expected <- as.numeric(unlist(p$ranks[[engine]][[id]]))
    stopifnot(identical(rank(maps[[id]], ties.method='average'), expected))
    rank_count <- rank_count + 1L
  }
  phases <- unique(vapply(p$scores, `[[`, '', 'phase'))
  selected <- unlist(lapply(phases, function(ph) head(which(vapply(p$scores, `[[`, '', 'phase') == ph),4)),use.names=FALSE)
  templates <- setNames(vapply(p$sources, `[[`, '', 'template'),vapply(p$sources, `[[`, '', 'id'))
  for (index in selected) {
    s <- p$scores[[index]]
    refs <- as.character(unlist(s$controls))
    observed <- as.numeric(similarity(maps[[s$id]], maps[[templates[[s$id]]]], method='spearman'))
    baseline <- mean(vapply(refs,function(ref) as.numeric(similarity(maps[[s$id]],maps[[ref]],method='spearman')),0.0))
    actual <- c(observed=observed,baseline=baseline,difference=observed-baseline)
    expected <- as.numeric(unlist(s[[engine]][names(actual)]))
    stopifnot(all(is.finite(actual)),all(abs(actual-expected) <= 2e-12 + 2e-12*pmax(abs(actual),abs(expected))))
    max_error <- max(max_error,abs(actual-expected))
    pair_count <- pair_count + length(refs)+1L
  }
}
write_json(list(status='passed',checked_rank_vectors=rank_count,direct_pairs=pair_count,max_abs=max_error),file.path(args[[2]],'reference-check.json'),auto_unbox=TRUE,digits=NA)
