"""Generate model-exercising aggregate fixtures, NOT estimates derived from real people.

Native source-table runs separately validate ehrQL queries. These sufficiently large,
fully artificial measure inputs make the complete 102-month R/release graph economical
on an emulated laptop. They do not test query correctness or estimate scientific effects.
"""
from pathlib import Path
from collections import defaultdict
import csv,random,calendar,math,itertools
r=Path(__file__).resolve().parents[2];out=r/'dummy_measures';out.mkdir(exist_ok=True)
rng=random.Random(2100916)
ages=['18-39','40-49','50-59','60-69','70-79','80+'];sexes=['female','male']
imds=['1 (most deprived)','2','3','4','5 (least deprived)'];eths=['1','2','3','4','5']
regions=['North East','North West','Yorkshire and The Humber','East Midlands','West Midlands','East','London','South East','South West']
def poisson(lam):
    # Fixture intensities are modest; an exact Knuth draw is sufficient.
    limit=math.exp(-lam);p=1;k=0
    while p>limit:p*=rng.random();k+=1
    return k-1
basefields=['measure','interval_start','interval_end','ratio','numerator','denominator']
def row(measure,year,m,events,pd,values):
    return [measure,f'{year}-{m:02d}-01',f'{year}-{m:02d}-{calendar.monthrange(year,m)[1]}',events/pd,events,pd,*values]
def accumulate(cells,key,events,pd):
    cells[key][0]+=events;cells[key][1]+=pd
for year in range(2017,2026):
    fields=basefields+['age_band','sex','imd','region','ethnicity']
    with (out/f'trends_{year}.csv').open('w') as f:
        w=csv.writer(f);w.writerow(fields)
        for m in range(1,7 if year==2025 else 13):
            cells=defaultdict(lambda:[0,0])
            t=(year-2017)*12+m-1;shock=.8 if 39<=t<51 else 1;recovery=1+.002*max(0,t-51)
            for ai,age in enumerate(ages):
                for sex,ii,rep in itertools.product(sexes,range(5),range(3)):
                    vals=[age,sex,imds[ii],regions[(ai*3+ii+rep)%9],eths[(ii+rep)%5]]
                    n=1500;pd=n*calendar.monthrange(year,m)[1]
                    any_n=poisson((6+ai*1.5)*(1.3-ii*.08)*shock*recovery)
                    gp_n=sum(rng.random()<.7 for _ in range(any_n))
                    for measure,k in [('incidence_any',any_n),('incidence_gp',gp_n),('procedure_open',poisson(5+ai)),('procedure_endo',poisson(7+ai)),('procedure_major',poisson(4+ai)),('procedure_minor',poisson(6+ai))]:
                        accumulate(cells,(measure,*vals[:3],'',''),k,pd)
                        if measure.startswith('incidence_'):
                            accumulate(cells,(measure+'_region','','','',vals[3],''),k,pd)
                            accumulate(cells,(measure+'_ethnicity','','','','',vals[4]),k,pd)
            for (measure,*values),(events,pd) in sorted(cells.items()):
                w.writerow(row(measure,year,m,events,pd,values))
for year in range(2020,2026):
    fields=basefields+['age_band','sex','imd','ethnicity','diabetes','ckd','smoking','region','covid']
    with (out/f'covid_{year}.csv').open('w') as f:
        w=csv.writer(f);w.writerow(fields)
        for m in range(1,7 if year==2025 else 13):
            cells=defaultdict(lambda:[0,0])
            states=['No recorded COVID-19'] if year==2020 and m<=2 else ['No recorded COVID-19','28-89','90-364']+(['365+'] if year>=2021 else [])
            for i in range(480):
                ai=rng.randrange(6);ii=rng.randrange(5);state=rng.choice(states);dm=rng.random()<.35;ckd=rng.random()<.25
                values=[ages[ai],rng.choice(sexes),imds[ii],rng.choice(eths),'T' if dm else 'F','T' if ckd else 'F',rng.choice(['N','E','S']),rng.choice(regions),state]
                pd=900*calendar.monthrange(year,m)[1]
                intensity=(4+ai*.8)*(1.3-ii*.07)*(1.25 if dm else 1)*(1.15 if ckd else 1)*(1.15 if state!='No recorded COVID-19' else 1)
                accumulate(cells,tuple(values),poisson(intensity),pd)
            for values,(events,pd) in sorted(cells.items()):
                w.writerow(row('covid_pad',year,m,events,pd,values))
(out/'SYNTHETIC_ONLY.txt').write_text('Parametric aggregate fixtures for workflow execution; not patient data, not query validation, not research findings.\n')
print('Generated full-period synthetic measure fixtures.')
