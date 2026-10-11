# Optional external statistical handoff. Inputs and fitted models stay outside Git.
args <- commandArgs(TRUE)
if (nzchar(Sys.getenv('CI')) || nzchar(Sys.getenv('GITHUB_ACTIONS'))) stop('Private models are disabled in CI')
stopifnot(length(args)==5L)
outside_git <- function(path) {
  resolved <- normalizePath(path, mustWork=TRUE)
  parent <- resolved
  repeat {
    if (file.exists(file.path(parent,'.git'))) stop('Private model paths must stay outside Git')
    next_parent <- dirname(parent)
    if (identical(parent,next_parent)) break
    parent <- next_parent
  }
  resolved
}
args[[1]] <- outside_git(args[[1]])
args[[2]] <- outside_git(args[[2]])
if (file.exists(args[[3]])) stop('Output directory already exists; preserve each model attempt')
args[[3]] <- file.path(outside_git(dirname(args[[3]])),basename(args[[3]]))
library(brms)
library(jsonlite)
library(posterior)
protocol <- fromJSON(args[[1]],simplifyVector=TRUE)
data <- read.csv(args[[2]],stringsAsFactors=FALSE,na.strings=c('','NA'),colClasses=c(id='character',pid='character'))
output <- args[[3]]; engine <- args[[4]]; phase <- args[[5]]
stopifnot(engine %in% protocol$engines, phase %in% protocol$phases)
dir.create(output,recursive=TRUE,showWarnings=FALSE,mode='0700')
data <- data[data$engine==engine & data$phase==phase,]
stopifnot(nrow(data)>0,!anyDuplicated(data$id))
data$pid <- factor(data$pid)
data$object_type <- factor(data$object_type,levels=c('repeat','swapped','new'))
data$probe <- factor(data$probe,levels=c('target','lure','foil'))
options(mc.cores=4)
for (kind in c('vividness','accuracy')) {
  fields <- if (kind=='vividness') c('difference','object_type','vividness','pid') else c('accuracy','difference','probe','object_type','pid')
  included <- complete.cases(data[,fields])
  selected <- droplevels(data[included,])
  stopifnot(nrow(selected)>0,all(is.finite(selected$difference)))
  write.csv(data.frame(id=data$id,included=included,reason=ifelse(included,'included',paste('missing',paste(fields,collapse='/')))),file.path(output,paste0(kind,'-inclusion.csv')),row.names=FALSE)
  form <- as.formula(protocol$models[[kind]])
  priors <- if(kind=='vividness') c(prior(normal(0,0.5),class='b'),prior(normal(0,0.5),class='Intercept'),prior(exponential(2),class='sd'),prior(exponential(2),class='sigma'),prior(lkj(2),class='cor')) else c(prior(normal(0,5),class='b'),prior(normal(0,2),class='Intercept'),prior(exponential(1),class='sd'),prior(lkj(2),class='cor'))
  family <- if(kind=='vividness') gaussian() else bernoulli(link='logit')
  target <- file.path(output,kind)
  if(file.exists(paste0(target,'.rds'))) stop('Refusing to overwrite a fitted attempt')
  fit <- brm(form,data=selected,family=family,prior=priors,backend='rstan',seed=protocol$model_seed,
    chains=protocol$chains,iter=protocol$iterations,warmup=protocol$warmup,cores=4,
    control=list(adapt_delta=0.99,max_treedepth=12),refresh=200,file=target,file_refit='always')
  draws <- as_draws_array(fit)
  diagnostics <- summarise_draws(draws,'rhat','ess_bulk','ess_tail')
  write.csv(diagnostics,file.path(output,paste0(kind,'-diagnostics.csv')),row.names=FALSE)
  parameters <- rstan::get_sampler_params(fit$fit,inc_warmup=FALSE)
  divergences <- sum(vapply(parameters,function(x) sum(x[,'divergent__']),0.0))
  depths <- sum(vapply(parameters,function(x) sum(x[,'treedepth__'] >= 12),0.0))
  bfmi <- vapply(parameters,function(x) mean(diff(x[,'energy__'])^2)/var(x[,'energy__']),0.0)
  mat <- as_draws_matrix(fit)
  coefficients <- grep('^b_',colnames(mat),value=TRUE)
  summary <- do.call(rbind,lapply(coefficients,function(k) {
    v <- mat[,k]
    data.frame(parameter=k,median=median(v),lower=unname(quantile(v,.025)),upper=unname(quantile(v,.975)),probability_direction=max(mean(v>0),mean(v<0)))
  }))
  write.csv(summary,file.path(output,paste0(kind,'-posterior.csv')),row.names=FALSE)
  ok <- all(is.finite(diagnostics$rhat)) && max(diagnostics$rhat)<=1.01 && min(diagnostics$ess_bulk)>=400 && min(diagnostics$ess_tail)>=400 && divergences==0 && depths==0 && all(bfmi>=0.3)
  write_json(list(status=if(ok) 'passed' else 'diagnostics-failed',engine=engine,phase=phase,model=kind,formula=deparse(form),queries=nrow(selected),participants=nlevels(selected$pid),excluded=sum(!included),max_rhat=max(diagnostics$rhat),min_bulk_ess=min(diagnostics$ess_bulk),min_tail_ess=min(diagnostics$ess_tail),divergences=divergences,treedepth_hits=depths,bfmi=bfmi,seed=protocol$model_seed),file.path(output,paste0(kind,'-receipt.json')),auto_unbox=TRUE,digits=NA)
  png(file.path(output,paste0(kind,'-posterior-predictive.png')),width=1200,height=800,res=120)
  print(pp_check(fit,type=if(kind=='vividness') 'dens_overlay' else 'bars',ndraws=25))
  dev.off()
  rm(fit,draws,mat); gc()
}
writeLines(capture.output(sessionInfo()),file.path(output,'R-session.txt'))
