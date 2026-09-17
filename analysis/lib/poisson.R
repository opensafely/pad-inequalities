# Chunked Poisson fitting avoids a full patient-stratum x coefficient design matrix.
# All factor levels remain fixed across chunks; covariance is calendar-month HAC.
fit_poisson=function(q,vars,analysis,cohort,outcome,model,focus=NULL,min_months=24){
 q=q[is.finite(py)&py>0]
 if(nrow(q)==0)return(list(estimates=empty_estimates(),diagnostics=data.table(analysis,cohort,outcome,model,n=0,events=0,df=NA_integer_,status='No observable person-time',dispersion=NA_real_)))
 vars=vars[vapply(vars,function(v)uniqueN(q[[v]])>1,logical(1))]
 group_cols=unique(c('date',vars));q=q[,.(events=sum(events),py=sum(py)),by=group_cols]
 for(v in vars) if(is.character(q[[v]]))set(q,j=v,value=factor(q[[v]]))
 formula=as.formula(paste('events~',paste(vars,collapse='+'),'+offset(log(py))'))
 fit=NULL;warnings=character();status='ok';events=sum(q$events);n=uniqueN(q$date)
 cursor=1L;chunk_size=10000L
 next_chunk=function(reset=FALSE){
  if(reset){cursor<<-1L;return(invisible(NULL))}
  if(cursor>nrow(q))return(NULL)
  last=min(nrow(q),cursor+chunk_size-1L);chunk=as.data.frame(q[cursor:last]);cursor<<-last+1L;chunk
 }
 X0=model.matrix(formula,as.data.frame(q[seq_len(min(1000L,.N))]))
 start=setNames(rep(0,ncol(X0)),colnames(X0));start['(Intercept)']=log(max(events,.5)/sum(q$py))
 fit=tryCatch(withCallingHandlers(biglm::bigglm(formula,data=next_chunk,family=poisson(),start=start,maxit=30,tolerance=1e-8,quiet=FALSE),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')}),error=function(e)NULL)
 df=if(is.null(fit))NA_integer_ else length(coef(fit))
 if(is.null(fit)||!fit$converged||any(!is.finite(coef(fit)))||length(warnings))status='Non-estimable or convergence warning'
 if(status=='ok' && (events<10*df||n<min_months||any(q[,sum(events),by=date]$V1<=7)))status='Insufficient event/time support'
 dispersion=NA_real_
 if(status=='ok'){
  b=coef(fit);calendar=as.character(seq(min(q$date),max(q$date),by='month'))
  S=matrix(0,nrow=length(calendar),ncol=df,dimnames=list(calendar,names(b)))
  H=matrix(0,df,df);pearson=0;next_chunk(reset=TRUE)
  repeat{
   chunk=next_chunk();if(is.null(chunk))break
   X=model.matrix(formula,chunk);mu=exp(drop(X%*%b)+log(chunk$py))
   residual=chunk$events-mu;ss=rowsum(X*residual,as.character(chunk$date))
   S[rownames(ss),]=S[rownames(ss),,drop=FALSE]+ss
   H=H+crossprod(X,X*mu);pearson=pearson+sum(residual^2/mu)
  }
  dispersion=pearson/(nrow(q)-df);meat=crossprod(S)
  for(lag in 1:3)if(nrow(S)>lag){G=crossprod(S[(lag+1):nrow(S),,drop=FALSE],S[1:(nrow(S)-lag),,drop=FALSE]);meat=meat+(1-lag/4)*(G+t(G))}
  bread=tryCatch(solve(H),error=function(e)NULL)
  if(is.null(bread))status='Singular covariance' else {
   V=bread%*%meat%*%bread;se=sqrt(pmax(0,diag(V)))
   z=data.table(analysis,cohort,outcome,model,term=names(b),estimate=exp(b),lower=exp(b-1.96*se),upper=exp(b+1.96*se),p=2*pnorm(-abs(b/se)),n,events,df,status)
   if(any(!is.finite(z$estimate)|!is.finite(z$lower)|!is.finite(z$upper)))status='Unstable confidence intervals'
   if(!is.null(focus))z=z[grepl(paste(focus,collapse='|'),term)]
   if(status=='ok' && analysis=='segmented_ITS') {
    segments=list(pre='t',disruption=c('t','slope1'),recovery=c('t','slope1','slope2'))
    for(segment in names(segments))if(all(segments[[segment]]%in%names(b))) {
     contrast=setNames(rep(0,df),names(b));contrast[segments[[segment]]]=1
     log_slope=sum(contrast*b);slope_se=sqrt(drop(t(contrast)%*%V%*%contrast))
     if(!is.finite(slope_se)||slope_se<=0)next
     z=rbind(z,data.table(analysis,cohort,outcome,model,term=paste0('monthly_slope_',segment),estimate=exp(log_slope),lower=exp(log_slope-1.96*slope_se),upper=exp(log_slope+1.96*slope_se),p=2*pnorm(-abs(log_slope/slope_se)),n,events,df,status))
    }
   }
  }
 }
 if(status!='ok')z=empty_estimates()
 diagnostic=data.table(analysis,cohort,outcome,model,n,events,df,status,dispersion)
 list(estimates=z,diagnostics=diagnostic)
}
