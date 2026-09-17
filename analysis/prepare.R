source('analysis/lib/common.R')
flows=list(); checks=list()
for(cohort in c('incident','procedure')) {
 d=fread(paste0('output/raw/',cohort,'.csv.gz'),na.strings=c('','NA'))
 dates=grep('date$|discharge$|registration_end$',names(d),value=TRUE)
 for(v in dates) set(d,j=v,value=as.Date(d[[v]]))
 for(v in c('eligible','registered_1yr','registered_2yr','diabetes','ckd','chd','stroke','pad_on_procedure_spell','procedure_specific','index_has_open','index_has_endo','index_has_major','index_has_minor')) set(d,j=v,value=as.logical(d[[v]]))
 d[, exclusion:=fcase(is.na(age)|age<18|age>110,'Age outside 18-110',!registered_1yr,'Less than one year continuous registration',!is.na(death_date)&death_date<index_date,'Death before index',default='Included')]
 flow=d[,.(n=.N),by=.(category=exclusion)]
 flow[,`:=`(cohort=cohort,table='flow',variable='eligibility',period='All')]
 flows[[cohort]]=flow
 d=d[eligible==TRUE & exclusion=='Included']
 if(nrow(d)==0)stop(paste('No eligible',cohort,'cohort: check source coverage and definitions before analysis'))
 d[,period:=period(index_date)]
 d[,admin_end:=pmin(END,registration_end,na.rm=TRUE)]
 d[,followup_end:=pmin(admin_end,death_date,na.rm=TRUE)]
 d[,followup_days:=pmax(0,as.numeric(followup_end-index_date)+1)]
 d[,imd:=fifelse(grepl('^[1-5]',imd),substr(imd,1,1),'Unknown')]
 d[,imd:=factor(imd,levels=c('5','4','3','2','1','Unknown'))]
 d[,ethnicity:=factor(ethnicity,levels=c('1','2','3','4','5','Unknown'))]
 for(v in c('sex','region','smoking','period','age_band')) {d[is.na(get(v)),(v):='Unknown'];set(d,j=v,value=factor(d[[v]]))}
 d[,age10:=age/10]
 d[,procedure_type:=fcase(index_has_major==TRUE,'Major amputation (with/without other procedures)',index_has_minor==TRUE,'Minor amputation (with/without revascularisation)',index_has_open==TRUE & index_has_endo==TRUE,'Open and endovascular',index_has_open==TRUE,'Open revascularisation',index_has_endo==TRUE,'Endovascular revascularisation',default='No qualifying intervention')]
 d[,presentation:=fcase(gp_pad_date<procedure_date,'GP PAD recorded before procedure admission',procedure_type=='No qualifying intervention','No qualifying procedure',default='No GP PAD recorded before procedure admission')]
 d[,gp_to_procedure_band:=fcase(is.na(gp_pad_date),'No GP PAD record by study end',gp_pad_date>procedure_date,'GP record after procedure admission',gp_pad_date==procedure_date,'Same day',as.numeric(procedure_date-gp_pad_date)<=30,'1-30 days before',as.numeric(procedure_date-gp_pad_date)<=90,'31-90 days before',as.numeric(procedure_date-gp_pad_date)<=365,'91-365 days before',default='More than 365 days before')]
 d[,emergency:=fcase(grepl('^2',procedure_admission_method),'Emergency',procedure_admission_method %in% c('11','12','13'),'Elective',default='Other/unknown')]
 d[,diagnosis_source:=fcase(gp_pad_date==hospital_pad_date,'GP and hospital same day',gp_pad_date==index_date,'GP first',default='Hospital first')]
 d[,covid_at_index:=fcase(is.na(first_covid_date)|first_covid_date>index_date,'No recorded infection',as.numeric(index_date-first_covid_date)<28,'0-27 days',as.numeric(index_date-first_covid_date)<90,'28-89 days',as.numeric(index_date-first_covid_date)<365,'90-364 days',default='365+ days')]
 d[,covid_at_index:=factor(covid_at_index,levels=c('No recorded infection','0-27 days','28-89 days','90-364 days','365+ days'))]
 checks[[cohort]]=data.table(cohort=cohort,n=nrow(d),n_followup_zero=sum(d$followup_days<=0),n_death_discordant=sum(!is.na(d$ons_death_date)&!is.na(d$gp_death_date)&d$ons_death_date!=d$gp_death_date),n_index_after_end=sum(d$index_date>END))
 if(any(d$followup_days<=0)) stop('Invalid follow-up; inspect highly sensitive data-quality results')
 saveRDS(d,paste0('output/internal/',cohort,'.rds'))
}
write_internal(rbindlist(flows),'flow');write_internal(rbindlist(checks),'quality')
cat('Prepared both cohorts; see internal quality summaries.\n')
