source('analysis/lib/common.R')
args=commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !args[1] %in% c('trends','covid')) stop('Specify trends or covid')
mode=args[1]
years=if(mode=='trends') 2017:2025 else 2020:2025
core=c('incidence_any','incidence_gp','procedure_open','procedure_endo','procedure_major','procedure_minor')
groups=if(mode=='trends') c('age_band','sex','imd','region','ethnicity') else
  c('age_band','sex','imd','ethnicity','diabetes','ckd','smoking','region','covid')
measures=if(mode=='trends') c(core,'incidence_any_region','incidence_gp_region',
  'incidence_any_ethnicity','incidence_gp_ethnicity') else 'covid_pad'
required=c('measure','interval_start','interval_end','ratio','numerator','denominator',groups)
fail=function(message) stop(paste(mode, 'measures:', message),call.=FALSE)
parts=lapply(years,function(year) {
  file=sprintf('output/raw/%s_%s.csv',mode,year)
  if(!file.exists(file)) fail(paste('missing annual input',year))
  # Preserve categorical codes (notably ethnicity 1-5) as labels for modelling.
  # Only counts and ratios are converted to numeric below.
  z=fread(file,colClasses='character',na.strings=c('','NA','nan','NaN'))
  if(!setequal(names(z),required)) fail(paste('unexpected columns in',year))
  if(!nrow(z) || !setequal(z$measure,measures)) fail(paste('missing or unexpected measures in',year))
  starts=seq(as.Date(sprintf('%s-01-01',year)),by='month',length.out=if(year==2025) 6L else 12L)
  ends=seq(as.Date(sprintf('%s-02-01',year)),by='month',length.out=length(starts))-1L
  expected=data.table(interval_start=as.character(starts),interval_end=as.character(ends))
  for(measure_name in measures) {
    q=z[measure==measure_name]
    if(!fsetequal(unique(q[,.(interval_start,interval_end)]),expected))
      fail(paste('incomplete or invalid monthly intervals:',year,measure_name))
    active=if(mode=='covid') groups else if(grepl('_region$',measure_name)) 'region' else
      if(grepl('_ethnicity$',measure_name)) 'ethnicity' else c('age_band','sex','imd')
    inactive=setdiff(groups,active)
    if(length(inactive) && any(!is.na(as.matrix(q[,..inactive]))))
      fail(paste('unexpected cross-classification:',year,measure_name))
    if(anyDuplicated(q[,c('interval_start','interval_end',active),with=FALSE]))
      fail(paste('duplicate strata:',year,measure_name))
  }
  for(column in c('numerator','denominator','ratio'))
    set(z,j=column,value=suppressWarnings(as.numeric(z[[column]])))
  for(column in c('numerator','denominator')) {
    values=z[[column]]
    if(any(!is.finite(values) | values<0 | values!=round(values)))
      fail(paste('invalid',column,'in',year))
  }
  if(any(z$numerator>z$denominator)) fail(paste('events exceed person-days in',year))
  positive=z$denominator>0
  if(any(!is.finite(z$ratio[positive])) ||
     any(abs(z$ratio[positive]-z$numerator[positive]/z$denominator[positive])>1e-12))
    fail(paste('ratio does not match counts in',year))
  z
})
x=rbindlist(parts,use.names=TRUE)
quality=x[,.(strata=.N,events=sum(numerator),person_days=sum(denominator),
  zero_denominator_strata=sum(denominator==0)),by=.(measure,interval_start,interval_end)]
if(any(quality$person_days<=0)) fail('a monthly measure has no eligible person-time')
if(mode=='trends') {
  # Each marginal partition must exhaust exactly the same eligible population.
  for(outcome in c('incidence_any','incidence_gp')) {
    ref=quality[measure==outcome][order(interval_start)]
    for(group in c('region','ethnicity')) {
      q=quality[measure==paste0(outcome,'_',group)][order(interval_start)]
      if(!identical(q$events,ref$events) || !identical(q$person_days,ref$person_days))
        fail(paste('marginal totals disagree:',outcome,group))
    }
  }
  any_pad=quality[measure=='incidence_any'][order(interval_start)]
  gp_pad=quality[measure=='incidence_gp'][order(interval_start)]
  if(any(gp_pad$events>any_pad$events) || !identical(gp_pad$person_days,any_pad$person_days))
    fail('GP-first incidence is inconsistent with combined-source incidence')
  procedure_time=dcast(quality[grepl('^procedure_',measure)],interval_start~measure,value.var='person_days')
  if(any(apply(as.matrix(procedure_time[,-1]),1,function(v) length(unique(v))!=1L)))
    fail('procedure denominators disagree')
}
dir.create('output/internal',recursive=TRUE,showWarnings=FALSE)
saveRDS(x,sprintf('output/internal/%s_checked.rds',mode))
write_internal(quality,paste0(mode,'_quality'))
message(mode, ' measures passed interval, strata and denominator checks')
