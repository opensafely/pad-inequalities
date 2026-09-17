source('analysis/lib/common.R');source('analysis/lib/poisson.R')
x=readRDS('output/internal/covid_checked.rds')
x=x[measure=='covid_pad' & denominator>0]
x[,`:=`(events=as.numeric(numerator),py=as.numeric(denominator)/365.25,date=as.Date(interval_start))]
x[,imd:=fifelse(grepl('^[1-5]',imd),substr(imd,1,1),'Unknown')]
x[,covid:=factor(covid,levels=c('No recorded infection','28-89','90-364','365+'))]
x[,calendar_month:=factor(format(date,'%Y-%m'))]
rates=x[,.(events=sum(events),py=sum(py)),by=.(date=as.Date(paste0(format(date,'%Y'),'-01-01')),level=as.character(covid))]
rates[,`:=`(measure='covid_pad',group='covid',frequency='year')]
write_internal(rates,'covid_rates')
out=list();diag=list()
for(model in c('demographic','clinical','before_2022_04')) {
 q=copy(x);if(model=='before_2022_04')q=q[date<as.Date('2022-04-01')]
 q[,calendar_month:=droplevels(calendar_month)]
 vars=c('covid','age_band','sex','imd','ethnicity','region','calendar_month')
 if(model!='demographic') vars=c(vars,'diabetes','ckd','smoking')
 result=fit_poisson(q,vars,'monthly_landmark_IRR','PAD_free_adults','incident_PAD',model,focus='covid')
 # Covariate cells remain internal; exposure groups must themselves have >=8 events.
 support=q[,sum(events),by=covid]
 if(uniqueN(q$covid)<2 || any(support$V1<=7)) {result$estimates=empty_estimates();result$diagnostics[,status:='Insufficient exposure-group support']}
 out[[model]]=result$estimates;diag[[model]]=result$diagnostics
}
write_internal(rbindlist(out,fill=TRUE),'covid_estimates');write_internal(rbindlist(diag,fill=TRUE),'covid_diagnostics')
