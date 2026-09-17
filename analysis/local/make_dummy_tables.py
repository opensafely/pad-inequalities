"""Deterministic synthetic source tables, not a mock extracted dataset or clinical findings."""
from pathlib import Path
import csv, random, datetime, argparse, json
p=argparse.ArgumentParser();p.add_argument('--n',type=int,default=16000);a=p.parse_args()
r=Path(__file__).resolve().parents[2];out=r/'dummy_tables';out.mkdir(exist_ok=True)
rng=random.Random(2102026);D=datetime.date
start=D(2017,1,1);end=D(2025,6,30)
def plus(d,n):return d+datetime.timedelta(days=n)
def dt():return plus(start,rng.randrange((end-start).days+1))
from functools import lru_cache
@lru_cache(None)
def lst(name):
 files={
  'ethnicity':'opensafely-ethnicity-snomed-0removed',
  'smoking':'opensafely-smoking-clear',
  'diabetes':'bristol-multimorbidity_diabetes',
  'ckd':'primis-covid19-vacc-uptake-ckd35',
  'chd':'bristol-multimorbidity_coronary-heart-disease',
  'stroke':'bristol-multimorbidity_stroketransient-ischemic-attack',
 }
 return list(csv.DictReader((r/'codelists'/f'{files[name]}.csv').open()))
eth={i:[x['code'] for x in lst('ethnicity') if x['Grouping_6']==str(i)][0] for i in range(1,6)}
smoke={x['Category']:x['CTV3Code'] for x in lst('smoking')}
rows={k:[] for k in ['patients','practice_registrations','addresses','clinical_events','apcs','ons_deaths','sgss_covid_all_tests']}
def clinical(i,d,s='',c=''):rows['clinical_events'].append([i,d,s,c,''])
spell=0
def hospital(i,d,proc='',diag='I702',method='21'):
 global spell
 spell+=1;rows['apcs'].append([i,spell,d,plus(d,rng.randint(1,10)),diag,diag,proc,method])
for i in range(1,a.n+1):
 age=rng.randint(25,92);dob=D(2017-age,rng.randint(1,12),1)
 index=dt();has_pad=rng.random()<0.68
 reg=D(2010,1,1) if i%53 else plus(index,-100)
 reg_end=plus(index,rng.randint(30,1400)) if i%19==0 else None
 death=plus(index,rng.randint(0,1600)) if rng.random()<.35 else None
 if death and death>end:death=None
 rows['patients'].append([i,dob,rng.choice(['female','male']),death])
 rows['practice_registrations'].append([i,reg,reg_end,i%100+1,rng.choice(['North East','North West','Yorkshire and The Humber','East Midlands','West Midlands','East','London','South East','South West'])])
 rows['addresses'].append([i,i,D(2009,1,1),'',rng.randint(1,328)])
 rows['addresses'][-1][-1]*=100
 clinical(i,D(2010,1,2),eth[rng.randint(1,5)])
 clinical(i,D(2015,1,2),c=smoke[rng.choice(['S','E','N'])])
 for com,prob in [('diabetes',.35),('ckd',.25),('chd',.25),('stroke',.2)]:
  if rng.random()<prob:clinical(i,D(2015,2,1),lst(com)[0]['code'])
 covid=plus(D(2020,3,1),rng.randint(0,1850)) if rng.random()<.75 else None
 if covid and (not death or covid<=death):
  rows['sgss_covid_all_tests'].append([i,covid,'T',plus(covid,1)])
  if i%3==0:clinical(i,covid,'840539006')
 if has_pad:
  if i%3:clinical(i,index,'840580004')
  else:hospital(i,index,diag='I739')
  if rng.random()<.70:
   pd=plus(index,rng.randint(0,500));proc=rng.choice(['L591','L541','X093','X102','L591||L541'])
   if not death or pd<=death:
    hospital(i,pd,proc,method=rng.choice(['11','21']))
    for j in range(rng.randint(0,3)):
     nxt=plus(pd,rng.randint(15,1250))
     if nxt<=end and (not death or nxt<=death):hospital(i,nxt,rng.choice(['L635','L592','X095','X111']))
 if death:rows['ons_deaths'].append([i,death,'I259'])
headers={'patients':['patient_id','date_of_birth','sex','date_of_death'],'practice_registrations':['patient_id','start_date','end_date','practice_pseudo_id','practice_nuts1_region_name'],'addresses':['patient_id','address_id','start_date','end_date','imd_rounded'],'clinical_events':['patient_id','date','snomedct_code','ctv3_code','numeric_value'],'apcs':['patient_id','apcs_ident','admission_date','discharge_date','primary_diagnosis','all_diagnoses','all_procedures','admission_method'],'ons_deaths':['patient_id','date','underlying_cause_of_death'],'sgss_covid_all_tests':['patient_id','specimen_taken_date','is_positive','lab_report_date']}
for name,data in rows.items():
 with (out/f'{name}.csv').open('w') as f:
  w=csv.writer(f);w.writerow(headers[name]);w.writerows(data)
(out/'SYNTHETIC_ONLY.json').write_text(json.dumps({'seed':2102026,'n':a.n,'purpose':'Pipeline execution only; deliberately enriched events; never research results'}))
print(f'Created {a.n} fully synthetic patients in {out}')
