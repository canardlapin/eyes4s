args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==3L)
e <- new.env(parent=baseenv())
e$tibble <- tibble::tibble
e$assert_that <- assertthat::assert_that
sys.source(file.path(args[1], 'fixations.R'), envir=e)
sys.source(file.path(args[1], 'revisits.R'), envir=e)
cases <- jsonlite::fromJSON(args[2], simplifyVector=FALSE)$cases
default <- function(x, value) if(is.null(x)) value else x
results <- list()
for(c in cases) {
  # The R public API has an unweighted spatial reference; weighted cases are native only.
  if(!is.null(c$reference) && length(c$reference) && any(vapply(c$reference, function(p)p[[3]]!=1, logical(1)))) next
  xy <- do.call(rbind,lapply(c$points,unlist))
  n <- nrow(xy)
  fg <- e$fixation_group(xy[,1],xy[,2],duration=rep(10,n),onset=seq(0,by=20,length.out=n))
  ref <- if(is.null(c$reference)) NULL else if(!length(c$reference)) matrix(numeric(),0,2) else do.call(rbind,lapply(c$reference,function(p)unlist(p[1:2])))
  rv <- e$fixation_revisits.fixation_group(fg,radius=default(c$radius,10),min_lag=default(c$minimumLag,2),
      max_lag=default(c$maximumLag,Inf),require_departure=default(c$departure,TRUE),reference=ref,
      dist_method=default(c$distance,'euclidean'))
  s <- e$revisit_summary(rv)
  rows <- as.data.frame(rv[,c('opportunity','revisit','lag','match_index','distance','expected')])
  rows$match_index <- rows$match_index-1L
  results[[length(results)+1L]] <- list(case=c$name,rows=rows,rate=s$revisit_rate,expected_revisits=s$expected_revisits)
}
packages <- sort(setdiff(loadedNamespaces(), c('base','compiler','datasets','graphics','grDevices','methods','parallel','splines','stats','stats4','tcltk','tools','utils')))
versions <- setNames(lapply(packages,function(p)as.character(utils::packageVersion(p))),packages)
jsonlite::write_json(list(R_version=R.version.string,packages=versions,results=results),args[3],auto_unbox=TRUE,null='null',na='null',digits=17,pretty=TRUE)
