args <- commandArgs(trailingOnly=TRUE)
.libPaths(c(args[[1L]],.libPaths()))
library(eyesim)
input <- jsonlite::fromJSON(args[[2L]],simplifyVector=FALSE)
num <- function(v) vapply(v,function(x)if(is.null(x))NA_real_ else as.numeric(x),0.0)
attempt <- function(f) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(f(),warning=function(w){warnings <<- c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)list(error=conditionMessage(e)))
  if(is.atomic(value)) value <- list(values=as.vector(value),names=names(value))
  list(result=value,warnings=warnings)
}
methods <- unlist(input$methods)
records <- lapply(input$cases,function(c) {
  a<-num(c$a);b<-num(c$b)
  vector<-setNames(lapply(methods,function(m)attempt(function()eyesim::similarity(a,b,method=m))),methods)
  density<-if(length(a)>0 && length(a)==length(b)) {
    x<-eyesim::gen_density(seq_along(a),0,matrix(a,nrow=length(a)))
    y<-eyesim::gen_density(seq_along(b),0,matrix(b,nrow=length(b)))
    setNames(lapply(methods,function(m)attempt(function()eyesim::similarity(x,y,method=m))),methods)
  } else NULL
  list(name=c$name,vector=vector,density=density)
})
map <- function(v,sigma=1,x=c(0,1),y=c(0,1)) {
  d<-eyesim::gen_density(x,y,matrix(v,nrow=length(x)));d$sigma<-sigma
  class(d)<-c("eye_density","density","list");d
}
p<-c(.1,.2,.3,.4);q<-c(.4,.3,.2,.1)
pyramid <- function(xs)structure(xs,class=c("eye_density_multiscale","list"))
scale_cases<-list(
  ordered=list(x=pyramid(list(map(p,2),map(q,1))),y=pyramid(list(map(p,1),map(p,2)))),
  unmatched=list(x=pyramid(list(map(p,2),map(q,1))),y=pyramid(list(map(q,3),map(p,2)))),
  duplicate=list(x=pyramid(list(map(p,1),map(q,1))),y=pyramid(list(map(p,1)))),
  partial_failure=list(x=pyramid(list(map(rep(.25,4),1),map(p,2))),y=pyramid(list(map(p,1),map(p,2)))),
  none_common=list(x=pyramid(list(map(p,1))),y=pyramid(list(map(p,2)))),
  empty=list(x=pyramid(list()),y=pyramid(list(map(p,1)))))
scales<-lapply(scale_cases,function(c) setNames(lapply(methods,function(m)list(
  none=attempt(function()eyesim::similarity(c$x,c$y,method=m,multiscale_aggregation="none")),
  mean=attempt(function()eyesim::similarity(c$x,c$y,method=m,multiscale_aggregation="mean")))),methods))
transport<-list(
  dirac=attempt(function()eyesim::similarity(map(c(1,0,0,0)),map(c(0,0,0,1)),method="emd")),
  split=attempt(function()eyesim::similarity(map(c(.5,0,0,.5)),map(c(0,.5,.5,0)),method="emd")),
  backend=if(requireNamespace("emdist",quietly=TRUE))"emdist::emdw" else if(requireNamespace("T4transport",quietly=TRUE))"T4transport::sinkhornD" else "transport")
geometry<-setNames(lapply(methods,function(m)attempt(function()eyesim::similarity(map(p),map(p,x=c(100,101)),method=m))),methods)
jsonlite::write_json(list(records=records,scales=scales,transport=transport,geometry=geometry,R=as.character(getRversion())),args[[3L]],pretty=TRUE,auto_unbox=TRUE,digits=NA,na="null",null="null")
