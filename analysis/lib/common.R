suppressPackageStartupMessages({library(data.table);library(survival)})
END <- as.Date('2025-06-30')
dir.create('output/internal', recursive=TRUE, showWarnings=FALSE)
period <- function(x) fifelse(x<as.Date('2020-03-26'),'1_pre',fifelse(x<=as.Date('2021-03-08'),'2_disruption','3_recovery'))
round5 <- function(x) floor(x/5+0.5)*5
read_cohort <- function(name) readRDS(paste0('output/internal/',name,'.rds'))
write_internal <- function(x,name) fwrite(x,paste0('output/internal/',name,'.csv'),na='')
counts <- function(d, vars, table, cohort) {
 z=d[,.(n=.N),by=vars]
 z[,`:=`(table=table,cohort=cohort)]
 z
}
# Each column is disjoint within its panel; complementary suppression is applied later.
long_counts <- function(d,cohort,vars,split='period',table='characteristics') {
 rbindlist(lapply(vars,function(v){
  x=d[,.(n=.N),by=c(split,v)];setnames(x,v,'category');x[,category:=as.character(category)]
  x[,`:=`(variable=v,table=table,cohort=cohort)];x
 }),fill=TRUE)
}
endpoint <- function(d,name,cohort) {
 if(name=='death') return(d$death_date)
 if(cohort=='incident') {
  if(name=='revasc') return(pmin(d$first_open_date,d$first_endo_date,na.rm=TRUE))
  return(d[[paste0('first_',name,'_date')]])
 }
 if(name=='revasc') return(pmin(d$next_open_date,d$next_endo_date,na.rm=TRUE))
 d[[paste0('next_',name,'_date')]]
}
empty_estimates <- function() data.table(analysis=character(),cohort=character(),outcome=character(),model=character(),term=character(),estimate=numeric(),lower=numeric(),upper=numeric(),p=numeric(),n=numeric(),events=numeric(),df=numeric(),status=character())
# Models must have adequate outcome/non-outcome support in every categorical level.
model_support <- function(d,vars,event) {
 all(vapply(vars,function(v){
  if(is.numeric(d[[v]])) return(TRUE)
  tab=table(d[[v]],factor(event,levels=c(FALSE,TRUE)))
  all(tab[ rowSums(tab)>0,,drop=FALSE]>=8)
 },logical(1)))

}
