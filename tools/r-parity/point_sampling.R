args<-commandArgs(trailingOnly=TRUE)
.libPaths(c(args[[1L]],.libPaths()));library(eyesim)
input<-jsonlite::fromJSON(args[[2L]],simplifyVector=FALSE)
num<-function(x) vapply(x,function(v)if(is.null(v))NA_real_ else as.numeric(v),0.0)
attempt<-function(f) {
 warnings<-character();r<-tryCatch(withCallingHandlers(f(),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)list(error=conditionMessage(e)))
 list(result=r,warnings=warnings)
}
map<-function(v)eyesim::gen_density(c(.5,1.5),c(.5,1.5),matrix(num(v),2))
ref<-tibble::tibble(matched=vapply(input$templates,`[[`,"","key"),stratum=vapply(input$templates,`[[`,"","stratum"),density=lapply(input$templates,function(r)map(r$values)))
source<-tibble::tibble(key=vapply(input$sources,`[[`,"","key"),matched=vapply(input$sources,`[[`,"","matched"),stratum=vapply(input$sources,`[[`,"","stratum"),fixgroup=lapply(input$sources,function(r)eyesim::fixation_group(num(r$x),num(r$y),num(r$duration),num(r$onset))))
run<-function(norm,cap,s=source,r=ref,times=num(input$queries),bins=num(input$bins))attempt(function() {
 set.seed(input$seed)
 out<-eyesim::sample_density_time(r,s,match_on="matched",times=times,time_bins=bins,permutations=cap,permute_on="stratum",normalize=norm)
 as.data.frame(out[setdiff(names(out),"fixgroup")])
})
normalizations<-setNames(lapply(unlist(input$normalizations),function(n)setNames(lapply(num(input$caps),function(cap)run(n,cap)),paste0("cap_",num(input$caps)))),unlist(input$normalizations))
# Reproduce only the declared finite selection to retain its exact candidate occurrences.
indices<-match(source$matched,ref$matched)
selected<-lapply(num(input$caps),function(cap){set.seed(input$seed);list(cap=cap,rows=lapply(seq_len(nrow(source)),function(i){mind<-indices[source$stratum==source$stratum[i]];mind<-mind[mind!=indices[i]];eligible<-mind;if(cap==0)mind<-integer() else if(cap<length(mind))mind<-sample(mind,cap);list(key=source$key[i],eligible=eligible,selected=mind)}))})
missing<-source[1,];missing$key<-"unmatched";missing$matched<-"absent"
duplicate<-rbind(ref,ref[1,]);duplicate$density[[nrow(duplicate)]]<-map(list(8,4,2,1))
boundaries<-list(extended=run("none",20,bins=num(input$extended_bins)),unmatched=run("none",20,s=rbind(source,missing)),duplicate=run("none",20,r=duplicate),empty_source=run("none",20,s=source[FALSE,]),empty_queries=run("none",20,times=numeric()),no_bins=run("none",20,bins=NULL))
# Missing cells produce unequal successful-control counts across times. Native checked fields
# reject nonfinite cells; retain the R result and qualify the aggregation independently.
partial<-ref;partial$density[[2]]<-map(list(2,NULL,3,9))
partial$density[[3]]<-map(list(9,3,NULL,1))
boundaries$partial_controls<-run("none",20,r=partial)
jsonlite::write_json(list(normalizations=normalizations,selected=selected,boundaries=boundaries,bin_membership=cut(num(input$queries),num(input$bins),right=FALSE,include.lowest=TRUE,labels=FALSE),R=as.character(getRversion()),RNGkind=RNGkind(),seed=input$seed),args[[3L]],auto_unbox=TRUE,pretty=TRUE,digits=NA,na="null",null="null")
