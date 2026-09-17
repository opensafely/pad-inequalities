source('analysis/lib/common.R')
suppressPackageStartupMessages(library(sandwich))
x=rbindlist(lapply(Sys.glob('output/raw/trends_*.csv'),fread),fill=TRUE)
x=x[!grepl('_contributors$',measure)]
x[,`:=`(date=as.Date(interval_start),events=as.numeric(numerator),py=as.numeric(denominator)/365.25)]
x=x[py>0];x[,imd:=fifelse(grepl('^[1-5]',imd),substr(imd,1,1),'Unknown')]
x[,year:=format(date,'%Y')]
base=x[measure %in% c('incidence_any','incidence_gp','procedure_open','procedure_endo','procedure_major','procedure_minor')]
# Fixed 2017 age-sex distribution among incident-PAD risk time, summed across IMD.
w=base[measure=='incidence_any' & year=='2017',.(ref=sum(py)),by=.(age_band,sex)];w[,w:=ref/sum(ref)]
base=merge(base,w[,.(age_band,sex,w)],by=c('age_band','sex'),all.x=TRUE)
rates=list();models=list();diags=list()
aggregate_rates=function(d,group,frequency){
 z=d[,.(events=sum(events),py=sum(py)),by=.(measure,date,level=get(group))]
 z[,`:=`(group=group,frequency=frequency)];z
}
base[,Overall:='All']
for(g in c('Overall','imd')) {
 z=aggregate_rates(base,g,'month')
 fine=base[,.(events=sum(events),py=sum(py),w=first(w)),by=.(measure,date,level=get(g),age_band,sex)]
 a=fine[,.(asr=1e5*sum(w*events/py),asr_se=1e5*sqrt(sum(w^2*events/py^2)),weight_sum=sum(w)),by=.(measure,date,level)]
 z=merge(z,a,by=c('measure','date','level'));z[abs(weight_sum-1)>1e-6,`:=`(asr=NA_real_,asr_se=NA_real_)]
 rates[[length(rates)+1]]=z
}
# Annual marginal summaries; these have person-time, not distinct annual-person counts.
for(g in c('region','ethnicity')) {
 q=x[measure %in% c('incidence_any','incidence_gp')]
 q[,date:=as.Date(paste0(year,'-01-01'))]
 rates[[length(rates)+1]]=aggregate_rates(q,g,'year')
}
write_internal(rbindlist(rates,fill=TRUE),'rates')
# A Poisson mean model with calendar-month HAC covariance; score contributions are
# summed within month before applying Bartlett weights (3 monthly lags).
source('analysis/lib/poisson.R')
for(outcome in unique(base$measure)) for(g in c('All','1','2','3','4','5')) {
 q=base[measure==outcome];if(g!='All') q=q[imd==g]
 q=q[,.(events=sum(events),py=sum(py)),by=.(date,age_band,sex)]
 q=q[!format(date,'%Y-%m') %in% c('2020-03','2021-03')]
 q[,t:=as.integer(format(date,'%Y'))*12+as.integer(format(date,'%m'))-2017*12-1]
 q[,`:=`(level1=as.integer(date>=as.Date('2020-04-01')),slope1=pmax(0,t-39),level2=as.integer(date>=as.Date('2021-04-01')),slope2=pmax(0,t-51),sin1=sin(2*pi*t/12),cos1=cos(2*pi*t/12))]
 vars=c('t','level1','slope1','level2','slope2','sin1','cos1','age_band','sex')
 result=fit_poisson(q,vars,analysis='segmented_ITS',cohort=g,outcome=outcome,model='age_sex_season_adjusted',focus=c('t','level1','slope1','level2','slope2'),min_months=60)
 models[[length(models)+1]]=result$estimates;diags[[length(diags)+1]]=result$diagnostics
}
write_internal(rbindlist(models,fill=TRUE),'its_estimates');write_internal(rbindlist(diags,fill=TRUE),'its_diagnostics')
