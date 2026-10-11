# Local-only reference: public multi_match, ungrouped, five components retained.
args <- commandArgs(trailingOnly=TRUE)
.libPaths(c(args[[1]], .libPaths()))
library(eyesim)
p <- jsonlite::fromJSON(args[[2]], simplifyVector=FALSE)
num <- function(x) as.numeric(unlist(x))
items <- c(p$templates, p$sources)
paths <- lapply(items, function(item) {
  f <- item$fixations
  tryCatch(scanpath(fixation_group(x=num(f$x), y=num(f$y),
    duration=num(f$duration_ms), onset=num(f$onset_ms))),
    error=function(e) list(error=conditionMessage(e)))
})
names(paths) <- vapply(items, `[[`, '', 'id')
stream <- file(args[[3]], 'wt')
components <- c('mm_vector','mm_direction','mm_length','mm_position','mm_duration')
for (pair in p$pairs) {
  warnings <- character()
  result <- tryCatch(withCallingHandlers({
    a <- paths[[pair$source]]; b <- paths[[pair$reference]]
    if (!is.null(a$error)) stop(a$error)
    if (!is.null(b$error)) stop(b$error)
    value <- multi_match(a, b, screensize=c(p$screen_width_px,p$screen_height_px))
    list(values=as.list(setNames(as.numeric(value[components]),
                               c('shape','direction','length','position','duration'))))
  }, warning=function(w) { warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning') }),
  error=function(e) list(error=conditionMessage(e)))
  writeLines(jsonlite::toJSON(c(pair,result,list(warnings=warnings)), auto_unbox=TRUE,
                             digits=NA, na='null',null='null'), stream)
}
close(stream)

writeLines(capture.output(sessionInfo()), file.path(dirname(args[[3]]), 'R-session.txt'))
