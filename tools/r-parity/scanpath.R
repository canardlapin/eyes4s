args<-commandArgs(trailingOnly=TRUE)
.libPaths(c(args[[1L]],.libPaths()));library(eyesim)
input<-jsonlite::fromJSON(args[[2L]],simplifyVector=FALSE)
num<-function(x)as.numeric(unlist(x))
attempt<-function(f) {
 warnings<-character()
 r<-tryCatch(withCallingHandlers(f(),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)list(error=conditionMessage(e)))
 if(is.atomic(r))r<-list(values=as.vector(r),names=names(r))
 list(result=r,warnings=warnings)
}
fg<-function(p)eyesim::fixation_group(x=num(p$x),y=num(p$y),onset=num(p$onset),duration=num(p$duration))
a<-fg(input$paths[[1]]);b<-fg(input$paths[[2]]);screen<-num(input$screen)
paths<-lapply(input$paths,function(p)list(name=p$name,
 construction=attempt(function(){s<-eyesim::scanpath(fg(p));as.data.frame(s)}),
 direct=attempt(function()eyesim::multi_match(eyesim::scanpath(a),eyesim::scanpath(fg(p)),screensize=screen)),
 facade=attempt(function()eyesim::similarity(eyesim::scanpath(a),eyesim::scanpath(fg(p)),screensize=screen))))
base_table<-tibble::tibble(id="one",scanpath=list(eyesim::scanpath(a)))
other_table<-tibble::tibble(id="one",scanpath=list(eyesim::scanpath(b)))
scanpath_table<-attempt(function(){r<-eyesim::scanpath_similarity(base_table,other_table,"id",screensize=screen);as.data.frame(r[setdiff(names(r),"scanpath")])})
windows<-lapply(list(c(0,201),c(0,200),c(300,400)),function(w)attempt(function()eyesim::similarity(eyesim::scanpath(a),eyesim::scanpath(b),screensize=screen,window=w)))
tie_self<-attempt(function(){t<-eyesim::scanpath(fg(input$paths[[5]]));eyesim::multi_match(t,t,screensize=screen)})
missing_screen<-attempt(function()eyesim::similarity(eyesim::scanpath(a),eyesim::scanpath(b)))
overlap<-lapply(c("euclidean","manhattan"),function(m)list(method=m,rows=lapply(num(input$thresholds),function(t)list(threshold=t,out=attempt(function()eyesim::fixation_overlap(a,b,dthresh=t,time_samples=num(input$queries),dist_method=m))))))
defaults<-list(direct=attempt(function()eyesim::fixation_overlap(a,b)),facade_missing=attempt(function()eyesim::similarity(a,b,method="overlap")),facade=attempt(function()eyesim::similarity(a,b,method="overlap",time_samples=num(input$queries))),empty_queries=attempt(function()eyesim::fixation_overlap(a,b,time_samples=numeric())))
# Direct default ends at the LEFT final onset, not the union's final onset.
longer<-eyesim::fixation_group(c(130,330,530),c(140,240,140),c(50,50,50),c(0,100,400))
defaults$asymmetric_grid<-attempt(function()eyesim::fixation_overlap(a,longer))
x1<-eyesim::fixation_group(0,0,2,0);y1<-eyesim::fixation_group(3,4,7,100)
transport<-list(singleton=attempt(function()eyesim::similarity(x1,y1,method="sinkhorn",xdenom=10,ydenom=10,tdenom=100,tweight=1)),
 no_time=attempt(function()eyesim::similarity(x1,y1,method="sinkhorn",xdenom=10,ydenom=10,tdenom=100,tweight=0)),
 base=attempt(function()eyesim::similarity(a,b,method="sinkhorn")),
 lambdas=lapply(c(.01,.1,1),function(l)list(lambda=l,result=attempt(function()eyesim::similarity(a,b,method="sinkhorn",lambda=l)))),
 backend="T4transport::sinkhornD",default_p=2,
 two_point=attempt(function()T4transport::sinkhornD(matrix(c(0,1,1,0),2),wx=c(.5,.5),wy=c(.5,.5),lambda=.5)),
 one_step=attempt(function()T4transport::sinkhornD(matrix(c(0,1,1,0),2),wx=c(.8,.2),wy=c(.3,.7),lambda=.5,maxiter=1)),
 zero_duration=attempt(function()eyesim::similarity(fg(input$paths[[10]]),b,method="sinkhorn")))
transport$plans<-lapply(c(.01,.1,1),function(l) {
 d<-as.matrix(proxy::dist(cbind(a$x/1000,a$y/1000,a$onset/3000*.8),cbind(b$x/1000,b$y/1000,b$onset/3000*.8)))
 run<-function(iter) {r<-T4transport::sinkhornD(d,wx=a$duration/sum(a$duration),wy=b$duration/sum(b$duration),lambda=l,maxiter=iter)
 list(distance=r$distance,plan=r$plan,residual=max(abs(rowSums(r$plan)-a$duration/sum(a$duration)),abs(colSums(r$plan)-b$duration/sum(b$duration))))}
 list(lambda=l,default=run(496),extended=run(10000))
})
ref<-tibble::tibble(id="one",fixgroup=list(a));source<-tibble::tibble(id="one",fixgroup=list(b))
fixation_table<-attempt(function(){r<-eyesim::fixation_similarity(ref,source,"id",method="sinkhorn");as.data.frame(r[setdiff(names(r),"fixgroup")])})
jsonlite::write_json(list(paths=paths,tie_self=tie_self,scanpath_table=scanpath_table,windows=windows,missing_screen=missing_screen,overlap=overlap,defaults=defaults,transport=transport,fixation_table=fixation_table,R=as.character(getRversion())),args[[3L]],auto_unbox=TRUE,pretty=TRUE,digits=NA,na="null",null="null")
