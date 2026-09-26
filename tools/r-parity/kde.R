args <- commandArgs(trailingOnly=TRUE)
.libPaths(c(args[[1L]],.libPaths()))
library(eyesim)
i <- jsonlite::fromJSON(args[[2L]],simplifyVector=TRUE)
attempt <- function(f) {
  warnings <- character(); messages <- character()
  value <- tryCatch(withCallingHandlers(f(),warning=function(w){warnings <<- c(warnings,conditionMessage(w));invokeRestart("muffleWarning")},message=function(m){messages <<- c(messages,conditionMessage(m));invokeRestart("muffleMessage")}),error=function(e)list(error=conditionMessage(e)))
  list(value=value,warnings=warnings,messages=messages)
}
fg <- eyesim::fixation_group(i$x,i$y,i$durations_us/1e6,i$onsets_us/1e6)
extract <- function(d) {
  if(is.null(d)) return(NULL)
  if(inherits(d,"eye_density_multiscale")) return(lapply(d,extract))
  list(x=d$x,y=d$y,z=as.vector(eyesim::get_density(d)),dim=dim(d$z),sigma=d$sigma)
}
estimates <- list()
for(backend in c("ks","MASS")) for(weighted in c(FALSE,TRUE)) for(normalized in c(FALSE,TRUE)) {
  name <- paste(backend,weighted,normalized,sep="-")
  estimates[[name]] <- attempt(function()extract(eyesim::eye_density(fg,sigma=i$sigma,xbounds=i$xbounds,ybounds=i$ybounds,outdim=c(i$nx,i$ny),duration_weighted=weighted,normalize=normalized,kde_pkg=backend)))
}
# Explicit weights, one per fixation; generate_kde.py checks them against a direct weighted Gaussian.
explicit <- attempt(function()extract(eyesim::eye_density(fg,sigma=i$sigma,xbounds=i$xbounds,ybounds=i$ybounds,outdim=c(i$nx,i$ny),weights=c(100,1,1),kde_pkg="MASS")))
scales <- attempt(function()extract(eyesim::eye_density(fg,sigma=c(1,2),xbounds=i$xbounds,ybounds=i$ybounds,outdim=c(i$nx,i$ny),kde_pkg="MASS")))
window <- attempt(function()extract(eyesim::eye_density(fg,sigma=1,window=c(0.1,0.2),min_fixations=2)))
groups_mass <- attempt(function() {
  tab <- tibble::tibble(group=c("full","short"),fixgroup=list(fg,fg[1,]))
  d <- eyesim::density_by(tab,"group",sigma=1,xbounds=i$xbounds,ybounds=i$ybounds,outdim=c(i$nx,i$ny),kde_pkg="MASS")
  list(groups=d$group,maps=lapply(d$density,extract))
})
groups <- attempt(function() {
  tab <- tibble::tibble(group=c("full","short"),fixgroup=list(fg,fg[1,]))
  d <- eyesim::density_by(tab,"group",sigma=1,xbounds=i$xbounds,ybounds=i$ybounds,outdim=c(i$nx,i$ny),kde_pkg="ks")
  list(groups=d$group,maps=lapply(d$density,extract))
})
defaults <- attempt(function()extract(eyesim::eye_density(fg,outdim=c(i$nx,i$ny))))
invalid_grid <- attempt(function()eyesim::gen_density(c(0,1),c(0,1),matrix(1,3,2)))
xs <- seq(0.5,4.5,length.out=i$nx); ys <- seq(0.5,2.5,length.out=i$ny)
d <- eyesim::gen_density(xs,ys,matrix(i$lookup_values,nrow=i$nx))
queries <- eyesim::fixation_group(i$query_x,i$query_y,rep(0.01,length(i$query_x)),seq_along(i$query_x))
lookup <- lapply(c("none","max","sum","zscore"),function(mode) attempt(function()eyesim::sample_density(d,queries,normalize=mode)))
names(lookup) <- c("none","max","sum","zscore")
timed <- attempt(function()eyesim::sample_density(d,fg,times=i$times_us/1e6,normalize="sum"))
facade <- attempt(function()eyesim::template_sample(tibble::tibble(fixgroup=list(fg),density=list(d)),"density",time=i$times_us/1e6)$sample_out[[1]])
constants <- lapply(c(0,3),function(value) {
  field <- eyesim::gen_density(xs,ys,matrix(value,nrow=i$nx,ncol=i$ny))
  setNames(lapply(c("none","max","sum","zscore"),function(mode) attempt(function()eyesim::sample_density(field,queries,normalize=mode))),c("none","max","sum","zscore"))
})
singleton <- attempt(function() eyesim::sample_density(eyesim::gen_density(0,0,matrix(1,1,1)),queries,normalize="zscore"))
suggested <- list(raw=eyesim::suggest_sigma(fg),bounded=eyesim::suggest_sigma(fg,xbounds=i$xbounds,ybounds=i$ybounds),constant=eyesim::suggest_sigma(rep(1,3),rep(1,3),xbounds=i$xbounds,ybounds=i$ybounds),singleton=eyesim::suggest_sigma(1,1))
jsonlite::write_json(list(estimates=estimates,explicit_weights=explicit,scales=scales,window=window,groups=groups,groups_mass=groups_mass,defaults=defaults,invalid_grid=invalid_grid,lookup=lookup,timed=timed,template_sample=facade,constants=constants,singleton=singleton,suggested=suggested,R=as.character(getRversion())),args[[3L]],pretty=TRUE,auto_unbox=TRUE,digits=NA,na="null",null="null")
