args <- commandArgs(trailingOnly=TRUE)
.libPaths(c(args[[1L]],.libPaths()))
library(eyesim)
input <- jsonlite::fromJSON(args[[2L]],simplifyVector=FALSE)
attempt <- function(f) {
  warnings <- character()
  result <- tryCatch(withCallingHandlers(f(), warning=function(w) {warnings <<- c(warnings,conditionMessage(w)); invokeRestart("muffleWarning")}), error=function(e) list(error=conditionMessage(e)))
  list(result=result,warnings=warnings)
}
trajectories <- lapply(input$trajectories,function(c) {
  run <- function(fast) attempt(function() {
    g <- eyesim::fixation_group(x=unlist(c$x),y=unlist(c$y),onset=unlist(c$onsets)/1e6,duration=unlist(c$durations)/1e6)
    r <- eyesim::sample_fixations(g,time=unlist(c$queries)/1e6,fast=fast)
    list(x=r$x,y=r$y,onset=r$onset,duration=r$duration)
  })
  list(name=c$name,fast=run(TRUE),slow=run(FALSE))
})
durations <- unlist(input$replication$durations)/1e6
replicated <- eyesim::rep_fixations(eyesim::fixation_group(seq_along(durations),seq_along(durations),duration=durations,onset=seq_along(durations)),resolution=input$replication$resolution_hz)
replication <- list(counts=as.integer(table(factor(replicated$index,levels=seq_along(durations)))),indices=replicated$index)
map <- function(v) {v <- unlist(v);structure(list(x=c(0,1),y=0,z=matrix(v/sum(v),nrow=1)),class="eye_density")}
source <- tibble::tibble(key=vapply(input$controls,`[[`,"","key"),matched=vapply(input$controls,`[[`,"","match"),stratum=vapply(input$controls,`[[`,"","stratum"),density=lapply(input$controls,function(r)map(r$values)))
ref <- source[!duplicated(source$matched),c("matched","stratum","density")]
matchind <- match(source$matched,ref$matched)
controls <- lapply(unlist(input$caps),function(cap) {
  set.seed(input$seed)
  result <- eyesim::template_similarity(ref,source,"matched",permute_on="stratum",method="cosine",permutations=cap)
  set.seed(input$seed)
  selected <- lapply(seq_len(nrow(source)),function(i) {
    if(cap<=0) return(integer())
    # The pinned draw: every copy of the true match is removed, each template counts once,
    # and only then are up to `cap` drawn without replacement.
    candidates <- matchind[source$stratum==source$stratum[i]]
    candidates <- unique(candidates[candidates!=matchind[i]])
    if(cap<length(candidates)) candidates <- sample(candidates,cap)
    candidates
  })
  list(cap=cap,rows=as.data.frame(result[setdiff(names(result),"density")]),selected_reference_indices=selected)
})
# Exercise the generic fixation facade as well as the optimized density cosine path.
source$fixgroup <- lapply(source$density,function(d)eyesim::fixation_group(rep(d$z[1,1]*10,2),c(0,0),c(0.1,0.1),c(0,1)))
ref$fixgroup <- lapply(ref$density,function(d)eyesim::fixation_group(rep(d$z[1,1]*10,2),c(0,0),c(0.1,0.1),c(0,1)))
fixation_controls <- lapply(unlist(input$caps),function(cap) attempt(function() {
  set.seed(input$seed)
  r <- eyesim::fixation_similarity(ref,source,"matched",permute_on="stratum",method="overlap",permutations=cap,time_samples=c(0,1),dthresh=1)
  as.data.frame(r[intersect(names(r),c("key","eye_sim","perm_sim","n_perm","eye_sim_diff"))])
}))
jsonlite::write_json(list(trajectories=trajectories,replication=replication,controls=controls,fixation_controls=fixation_controls,R=as.character(getRversion()),RNGkind=RNGkind(),seed=input$seed),args[[3L]],pretty=TRUE,auto_unbox=TRUE,digits=NA,na="null",null="null")
