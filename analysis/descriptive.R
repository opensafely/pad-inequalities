source('analysis/lib/common.R')
out=list(fread('output/internal/flow.csv'))
for(cohort in c('incident','procedure')) {
 d=read_cohort(cohort)
 vars=c('age_band','sex','imd','ethnicity','region','smoking','diabetes','ckd','chd','stroke','registered_2yr','covid_at_index')
 if(cohort=='incident') vars=c(vars,'diagnosis_source') else vars=c(vars,'procedure_type','presentation','emergency','procedure_specific','gp_to_procedure_band')
 out[[length(out)+1]]=long_counts(d,cohort,vars)
 if(cohort=='procedure') {
  z=d[,.(n=.N),by=.(period,imd,presentation,procedure_type)]
  z[,`:=`(table='pathway',cohort=cohort,variable=paste('IMD',imd,presentation,sep=' / '),category=procedure_type)]
  out[[length(out)+1]]=z[,.(table,cohort,period,variable,category,n)]
 }
 # Missingness is visible as Unknown in characteristics, not a duplicate marginal table.
}
write_internal(rbindlist(out,fill=TRUE),'counts')
