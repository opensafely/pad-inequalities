source('analysis/lib/common.R')
curves=list(); estimates=list(); diagnostics=list()
for(cohort in c('incident','procedure')) {
 d=read_cohort(cohort)
 for(outcome in c('death','revasc','minor','major')) {
  q=copy(d); event_date=endpoint(q,outcome,cohort)
  # Pre-index outcomes are not a post-index event. Same-day index procedures belong to incident pathways.
  event_date[event_date<q$index_date]=NA
  q[,event:=!is.na(event_date)&event_date<=followup_end]
  q[,time:=pmax(0.5,as.numeric(pmin(event_date,followup_end,na.rm=TRUE)-index_date))]
  q[,dead_competing:=!event & !is.na(death_date)&death_date<=admin_end]
  if(outcome=='death') q[,dead_competing:=FALSE]
  q[,status:=factor(fcase(event,1L,dead_competing,2L,default=0L),levels=0:2,labels=c('censor','outcome','death'))]
  q[,covid_period:=paste(as.character(period),as.character(covid_at_index),sep=' / ')]
  curve_groups=c('period','imd','covid_period')
  if(cohort=='procedure') {
   q[,procedure_period:=paste(as.character(period),procedure_type,sep=' / ')]
   curve_groups=c(curve_groups,'procedure_period')
  }
  for(g in curve_groups) for(lev in unique(as.character(q[[g]]))) {
   sub=q[as.character(get(g))==lev];if(nrow(sub)<8) next
   sf=survfit(Surv(time,status)~1,data=sub)
   last=0
   for(t in c(30,90,365,1095,1825)) {
    # Fixed horizons only, no exact event times leave the secure environment.
    risk=sum(sub$time>=t);events=sum(sub$event & sub$time<=t)
    interval_events=sum(sub$event & sub$time>last & sub$time<=t)
    a=summary(sf,times=t,extend=TRUE)
    idx=match('outcome',sf$states)
    ci=if(!is.na(idx) && length(a$pstate)) a$pstate[1,idx] else NA_real_
    lo=if(!is.null(a$lower)) a$lower[1,idx] else NA_real_;hi=if(!is.null(a$upper)) a$upper[1,idx] else NA_real_
    curves[[length(curves)+1]]=data.table(cohort,outcome,group=g,level=lev,day=t,n=nrow(sub),risk,events,interval_events,estimate=ci,lower=lo,upper=hi)
    last=t
   }
  }
  # Primary and extended inequality models; 2-year registration sensitivity.
  model_names=c('demographic','clinical','registration_2yr','imd_period_interaction','anatomically_specific')
  if(cohort=='procedure')model_names=c(model_names,'post_revascularisation','post_amputation')
  for(model in model_names) {
   x=copy(q);if(model=='registration_2yr') x=x[registered_2yr==TRUE]
   # Amputation takes priority when both occur in the index spell; no ordering
   # within that spell or limb laterality is inferred from these categories.
   if(model=='post_revascularisation')x=x[procedure_type %in% c('Open and endovascular','Open revascularisation','Endovascular revascularisation')]
   if(model=='post_amputation')x=x[grepl('amputation',procedure_type,fixed=TRUE)]
   if(model=='anatomically_specific') {
    if(cohort=='procedure') x=x[procedure_specific==TRUE]
    if(outcome=='revasc') {
      dt=if(cohort=='incident')pmin(x$first_specific_open_date,x$first_specific_endo_date,na.rm=TRUE)else pmin(x$next_specific_open_date,x$next_specific_endo_date,na.rm=TRUE)
      dt[dt<x$index_date]=NA
      x[,event:=!is.na(dt)&dt<=followup_end];x[,time:=pmax(0.5,as.numeric(pmin(dt,followup_end,na.rm=TRUE)-index_date))]
    }
   }
   if(nrow(x)==0){diagnostics[[length(diagnostics)+1]]=data.table(cohort,outcome,model,n=0,events=0,df=NA_integer_,status='No eligible patients',ph_global=NA_real_);next}
   vars=c('age10','sex','imd','ethnicity','region','period')
   if(model!='demographic') vars=c(vars,'diabetes','ckd','chd','stroke','smoking','covid_at_index')
   if(cohort=='procedure' && model!='demographic')vars=c(vars,'procedure_type','emergency')
   # Keep all known/unknown categories; do not select complete cases silently.
   vars=vars[vapply(vars,function(v)uniqueN(x[[v]])>1,logical(1))]
   for(v in vars) if(is.factor(x[[v]]))set(x,j=v,value=droplevels(x[[v]]))
   rhs=if(length(vars))paste(vars,collapse='+')else'1';if(model=='imd_period_interaction' && all(c('imd','period')%in%vars)) rhs=paste(rhs,'imd:period',sep='+')
   rhs=gsub('age10','splines::ns(age,df=3)',rhs,fixed=TRUE)
   df=ncol(model.matrix(as.formula(paste('~',rhs)),x))-1
   status='ok';fit=NULL
   if(df==0 || sum(x$event)<max(30,10*df)||nrow(x)<20*df||!model_support(x,vars,x$event)) status='Insufficient model support'
   if(status=='ok') {
    warnings=character()
    fit=tryCatch(withCallingHandlers(coxph(as.formula(paste('Surv(time,event)~',rhs)),data=x,ties='efron',x=TRUE),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')}),error=function(e)NULL)
    if(is.null(fit)||any(!is.finite(coef(fit)))||length(warnings)) status='Non-estimable or convergence warning'
   }
   diagnostics[[length(diagnostics)+1]]=data.table(cohort,outcome,model,n=nrow(x),events=sum(x$event),df,status,ph_global=if(status=='ok')tryCatch(cox.zph(fit)$table['GLOBAL','p'],error=function(e)NA_real_) else NA_real_)
   if(status=='ok') {
    s=summary(fit)
    z=data.table(analysis='cause_specific_cox',cohort,outcome,model,term=rownames(s$coefficients),estimate=s$conf.int[,'exp(coef)'],lower=s$conf.int[,'lower .95'],upper=s$conf.int[,'upper .95'],p=s$coefficients[,'Pr(>|z|)'],n=nrow(x),events=sum(x$event),df,status)
    estimates[[length(estimates)+1]]=z
    # Precompute clinically useful interaction contrasts while the covariance
    # matrix is available; it is not part of the aggregate release.
    if(model=='imd_period_interaction' && all(c('imd','period')%in%vars) && '5'%in%levels(x$imd)) {
     for(per in levels(x$period))for(quintile in intersect(c('1','2','3','4'),levels(x$imd))) {
      support=x[period==per & imd %in% c('5',quintile)]
      if(uniqueN(support$imd)!=2 || !model_support(support,'imd',support$event))next
      nd=copy(x[rep(1L,2L)])
      nd[,imd:=factor(c('5',quintile),levels=levels(x$imd))]
      nd[,period:=factor(rep(per,2L),levels=levels(x$period))]
      design=model.matrix(fit,data=as.data.frame(nd));contrast=design[2,]-design[1,]
      log_hr=sum(contrast*coef(fit));se=sqrt(drop(t(contrast)%*%vcov(fit)%*%contrast))
      if(!is.finite(se)||se<=0)next
      estimates[[length(estimates)+1]]=data.table(analysis='cox_period_contrast',cohort,outcome,model,term=paste0('IMD_',quintile,'_vs_5_at_',per),estimate=exp(log_hr),lower=exp(log_hr-1.96*se),upper=exp(log_hr+1.96*se),p=2*pnorm(-abs(log_hr/se)),n=nrow(x),events=sum(x$event),df,status='ok')
     }
    }
   }
  }
 }
}
write_internal(rbindlist(curves,fill=TRUE),'curves')
write_internal(if(length(estimates))rbindlist(estimates,fill=TRUE)else empty_estimates(),'survival_estimates')
write_internal(rbindlist(diagnostics,fill=TRUE),'survival_diagnostics')
