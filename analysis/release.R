source('analysis/lib/common.R')
dir.create('output/release',recursive=TRUE,showWarnings=FALSE)
# Remove only previously generated CSV families so a smaller rerun cannot retain stale parts.
unlink(Sys.glob('output/release/0[1-4]_*.csv'))
# Only controlled aggregate candidates are moderately sensitive. A human must review
# the combined release (including earlier releases); automation cannot approve release.
write_release=function(x,name){
 x[,covid_definition:='GP diagnosis/positive result or hospital U07.1/U07.2; earliest recorded date']
 stopifnot(!any(grepl('patient_id|index_date|gp_pad_date|admission_date',names(x))))
 if(nrow(x)==0) stop(paste('Empty release table:',name))
 chunks=split(seq_len(nrow(x)),ceiling(seq_len(nrow(x))/4999))
 for(i in seq_along(chunks)) {
  suffix=if(length(chunks)>1)paste0('_',i)else ''
  path=paste0('output/release/',name,suffix,'.csv');fwrite(x[chunks[[i]]],path,na='')
  if(file.info(path)$size>16*1024^2) stop('Release file exceeds 16 MB')
 }
}
x=fread('output/internal/counts.csv',colClasses=list(character=c('category','period','variable')))
x[,suppressed:=is.na(n)|n<=7]
x[,suppressed:={s=suppressed;if(sum(s)==1 && .N>1){j=which(!s)[which.max(n[!s])];s[j]=TRUE};s},by=.(table,cohort,period,variable)]
x[,count:=fifelse(suppressed,NA_real_,round5(n))]
x[,percentage:=if(any(suppressed))NA_real_ else round(100*count/sum(count),1),by=.(table,cohort,period,variable)]
x[,status:=fifelse(suppressed,'Suppressed','Rounded to 5')]
write_release(x[,.(table,cohort,period,variable,category,count,percentage,status)],'01_population_pathways')
r=rbindlist(list(fread('output/internal/rates.csv'),fread('output/internal/covid_rates.csv')),fill=TRUE)
r[,suppressed:=is.na(events)|events<=7|py<8]
r[,`:=`(events=fifelse(suppressed,NA_real_,round5(events)),person_years=fifelse(suppressed,NA_real_,round5(py)))]
r[,rate:=events/person_years*1e5]
r[,`:=`(lower=qchisq(.025,2*events)/2/person_years*1e5,upper=qchisq(.975,2*(events+1))/2/person_years*1e5)]
r[,`:=`(standardised_rate=fifelse(suppressed,NA_real_,round(asr,1)),standardised_lower=fifelse(suppressed,NA_real_,round(pmax(0,asr-1.96*asr_se),1)),standardised_upper=fifelse(suppressed,NA_real_,round(asr+1.96*asr_se,1)),status=fifelse(suppressed,'Suppressed','Counts/person-years rounded to 5'))]
for(v in c('rate','lower','upper'))set(r,j=v,value=round(r[[v]],1))
write_release(r[,.(measure,date,frequency,group,level,events,person_years,rate,lower,upper,standardised_rate,standardised_lower,standardised_upper,status)],'02_rates')
c=fread('output/internal/curves.csv')
setorder(c,cohort,outcome,group,level,day)
c[,suppressed:=cummax(n<=7|risk<=7|events<=7|interval_events<=7|n-events<=7|is.na(estimate))>0,by=.(cohort,outcome,group,level)]
for(v in c('n','risk','events'))set(c,j=v,value=fifelse(c$suppressed,NA_real_,round5(c[[v]])))
for(v in c('estimate','lower','upper'))set(c,j=v,value=fifelse(c$suppressed,NA_real_,round(c[[v]],3)))
c[,status:=fifelse(suppressed,'Suppressed from this horizon onwards','Counts rounded to 5; probabilities to 0.001')]
write_release(c[,.(cohort,outcome,group,level,day,n,risk,events,estimate,lower,upper,status)],'03_outcome_curves')
e=rbindlist(lapply(c('survival','its','covid'),function(z)fread(paste0('output/internal/',z,'_estimates.csv'))),fill=TRUE)
d=rbindlist(lapply(c('survival','its','covid'),function(z)fread(paste0('output/internal/',z,'_diagnostics.csv'))),fill=TRUE)
d[,`:=`(analysis='model_diagnostic',term='model_status')]
e=rbindlist(list(e,d),fill=TRUE)
# Do not export estimates from unstable or insufficiently supported models.
e[status!='ok',c('estimate','lower','upper','p','ph_global','dispersion'):=NA_real_]
e[,n_unit:=fifelse(cohort %in% c('incident','procedure'),'patients','calendar_months')]
e[n_unit=='patients',n:=fifelse(n<=7,NA_real_,round5(n))]
e[,events:=fifelse(events<=7,NA_real_,round5(events))]
for(v in c('estimate','lower','upper','p','ph_global','dispersion'))set(e,j=v,value=signif(e[[v]],3))
write_release(e,'04_models_diagnostics')
write_internal(data.table(file=list.files('output/release',pattern='csv$',full.names=TRUE),review_status='Candidate only: combined human disclosure review required'),'release_manifest')
cat('Prepared aggregate release candidates; no release request was submitted.\n')
