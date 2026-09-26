args <- commandArgs(trailingOnly = TRUE)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
input <- jsonlite::fromJSON(args[[2L]], simplifyVector = FALSE)
map <- function(values) {
  z <- matrix(unlist(values), nrow=1); z <- z/sum(z)
  structure(list(x=c(0,1), y=0, z=z), class="eye_density")
}
source <- tibble::tibble(key=vapply(input$source, `[[`, "", "key"),
  participant=vapply(input$source, `[[`, "", "group"),
  matched=vapply(input$source, `[[`, "", "match"),
  density=lapply(input$source, function(r) map(r$values)))
ref <- tibble::tibble(matched=vapply(input$reference, `[[`, "", "match"),
  density=lapply(input$reference, function(r) map(r$values)))
run <- function(ref, source) {
  warnings <- character()
  result <- withCallingHandlers(eyesim::template_similarity_cv(ref, source, "matched",
    method="cosine", permutations=0, split_on="participant", n_folds=input$n_folds,
    seed=input$seed), warning=function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")})
  list(rows=as.data.frame(result[c("key", "participant", "matched", ".cv_fold", "eye_sim")]),
    metadata=attr(result,"similarity_cv"), warnings=warnings)
}
set.seed(987); before <- .Random.seed
normal <- run(ref,source)
restored <- identical(before,.Random.seed)
duplicates <- run(dplyr::bind_rows(ref, tibble::tibble(matched="a",density=list(map(c(0,1))))), source)
missing <- run(ref[ref$matched != "a",], source)
jsonlite::write_json(list(normal=normal, duplicate_reference=duplicates, missing_reference=missing,
  rng_restored=restored, R=as.character(getRversion())),args[[3L]],pretty=TRUE,auto_unbox=TRUE,digits=NA,null="null")
