# Run locally using only CSVs approved for release; third argument 'synthetic' labels validation figures.
suppressPackageStartupMessages({library(data.table);library(ggplot2)})
a=commandArgs(TRUE);input=if(length(a))a[1]else'output/released';out=if(length(a)>1)a[2]else'output/manuscript'
caption_text=if(length(a)>2 && a[3]=='synthetic')'SIMULATED DATA - workflow validation only; no research findings'else'Controlled aggregate outputs; suppressed values are not imputed.'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
read_parts=function(prefix)rbindlist(lapply(Sys.glob(file.path(input,paste0(prefix,'*.csv'))),fread),fill=TRUE)
measure_labels=c(incidence_any='PAD: GP or hospital first',incidence_gp='PAD: GP first',procedure_open='Open revascularisation',procedure_endo='Endovascular revascularisation',procedure_minor='Minor amputation',procedure_major='Major amputation')
period_labels=c('1_pre'='Before disruption','2_disruption'='Disruption','3_recovery'='Recovery')
r=read_parts('02_rates');r[,date:=as.Date(date)];r[,measure_label:=measure_labels[measure]]
p=ggplot(r[frequency=='month' & group=='Overall'],aes(date,standardised_rate))+geom_line(colour='#245B78',na.rm=TRUE)+facet_wrap(~measure_label,scales='free_y')+geom_vline(xintercept=as.numeric(as.Date(c('2020-03-26','2021-03-09'))),linetype=3,colour='grey50')+theme_bw(base_size=11)+labs(x=NULL,y='Age-sex standardised rate per 100,000 person-years',caption=caption_text)
ggsave(file.path(out,'figure1_trends.pdf'),p,width=11,height=7)
p=ggplot(r[measure=='incidence_any' & group=='imd'],aes(date,standardised_rate,colour=level))+geom_line(na.rm=TRUE)+theme_bw(base_size=11)+labs(x=NULL,y='Recorded PAD incidence per 100,000 person-years',colour='IMD quintile',caption=caption_text)
ggsave(file.path(out,'figure2_inequalities.pdf'),p,width=9,height=5)
c=read_parts('03_outcome_curves');c[,period_label:=period_labels[level]]
p=ggplot(c[group=='period'],aes(day,estimate,colour=period_label))+geom_line(na.rm=TRUE)+geom_point(na.rm=TRUE)+facet_grid(cohort~outcome)+theme_bw(base_size=11)+labs(x='Days since diagnosis / procedure-bearing admission',y='Cumulative incidence at fixed horizons',colour='Index period',caption=caption_text)
ggsave(file.path(out,'figure3_outcomes.pdf'),p,width=12,height=6)
e=read_parts('04_models_diagnostics');z=e[analysis!='model_diagnostic' & grepl('^imd|^covid',term) & model %in% c('clinical','age_sex_season_adjusted')]
p=ggplot(z,aes(estimate,interaction(cohort,outcome,term),xmin=lower,xmax=upper))+geom_vline(xintercept=1,linetype=2)+geom_pointrange()+scale_x_log10()+theme_bw(base_size=11)+labs(x='Hazard / incidence rate ratio (95% CI)',y=NULL,caption=caption_text)
ggsave(file.path(out,'figure4_associations.pdf'),p,width=10,height=max(6,nrow(z)*.16))
fwrite(read_parts('01_population_pathways'),file.path(out,'tables_population_pathways.csv'))
cat('Figures and table source saved locally to ',out,'\n')
