# Explicit external reference court: no dataset discovery and no plotting outside Intaglio.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==3L)
.libPaths(c(args[[1L]],.libPaths()))
library(eyesim)
library(jsonlite)
source(args[[3L]])
root <- normalizePath(args[[2L]],mustWork=TRUE)
manifest <- fromJSON(file.path(root,'manifest.json'),simplifyVector=FALSE)
future::plan(future::sequential)
started <- proc.time()[[3L]]
for (entry in manifest$participants) {
  request <- fromJSON(file.path(root,entry$file),simplifyVector=FALSE)
  out <- file.path(root,entry$output)
  stopifnot(!file.exists(out))
  maps <- list(); details <- list(); shared <- list(); warnings <- list()
  for (item in request$maps) {
    f <- item$fixations
    group <- fixation_group(as.numeric(unlist(f$x)),as.numeric(unlist(f$y)),
      as.numeric(unlist(f$duration_ms)),as.numeric(unlist(f$onset_ms)))
    d <- withCallingHandlers(eye_density(group,sigma=request$sigma_px,
      xbounds=c(0,request$width_px),ybounds=c(0,request$height_px),
      outdim=c(request$nx,request$ny),duration_weighted=TRUE,min_fixations=1L,
      normalize=TRUE,kde_pkg='ks',binned=FALSE,supp=3.7),warning=function(w){
        warnings[[length(warnings)+1L]] <<- list(id=item$id,message=conditionMessage(w))
        invokeRestart('muffleWarning')
      })
    stopifnot(!is.null(d),all(is.finite(get_density(d))))
    maps[[item$id]] <- d
    shared[[item$id]] <- as.numeric(unlist(item$eyes4s))
    details[[length(details)+1L]] <- list(id=item$id,x=d$x,y=d$y,values=as.numeric(get_density(d)))
  }
  score <- function(query,reference,use_shared=FALSE) {
    if(use_shared) similarity(shared[[query]],shared[[reference]],method='cosine')
    else similarity(maps[[query]],maps[[reference]],method='cosine')
  }
  scores <- lapply(request$scores,function(row){
    values <- function(shared) {
      m <- score(row$id,row$matched,shared)
      b <- mean(vapply(unlist(row$controls),function(ref)score(row$id,ref,shared),numeric(1)))
      stopifnot(is.finite(m),is.finite(b))
      list(observed=m,baseline=b,difference=m-b)
    }
    list(id=row$id,matched=row$matched,controls=row$controls,
      eyesim=values(FALSE),eyesim_on_eyes4s=values(TRUE))
  })
  write_evidence_json(list(participant=request$participant,maps=details,scores=scores,
    warnings=warnings,elapsed_seconds=proc.time()[[3L]]-started),out)
  cat('participant',entry$index,'maps',length(details),'scores',length(scores),'complete\n');flush.console()
}
writeLines(capture.output(sessionInfo()),file.path(root,'R-session.txt'))
cat('all participants complete\n')
