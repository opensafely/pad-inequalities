"""Unrounded INTERNAL monthly sums. No event-level data extraction is used."""
from ehrql import Measures, INTERVAL, get_parameter, months, days, case, when, minimum_of, maximum_of
from analysis.definitions import *
year=get_parameter('year',type=int,default=2017)
mode=get_parameter('mode',default='trends')
measures=Measures()
measures.configure_dummy_data(population_size=1000)
measures.configure_disclosure_control(enabled=False)
start=INTERVAL.start_date; end=INTERVAL.end_date
intervals=months(6 if year==2025 else 12).starting_on(f'{year}-01-01')
cov=covariates(start)
base=eligible(start)
stop=minimum_of(end,death,registration(start).end_date)
pad_free=first_pad.is_null() | (first_pad>=start)

def add(name,event_date,groups,incident=False,covid=False):
    valid=base & (pad_free if incident else base)
    finish=stop
    if covid:
        # Acute infection is omitted at the landmark; unexposed time ends BEFORE infection.
        valid=valid & (first_covid.is_null() | (first_covid > start) | (first_covid <= start-days(28)))
        finish=minimum_of(finish,case(when(first_covid>start).then(first_covid-days(1))))
    if incident: finish=minimum_of(finish,first_pad)
    event=valid & event_date.is_on_or_between(start,finish)
    py=case(when(valid).then(maximum_of(0,(finish-start).days+1)),otherwise=0)
    measures.define_measure(name=name,numerator=event,denominator=py,group_by=groups,intervals=intervals)

if mode=='trends':
    groups={k:cov[k] for k in ['age_band','sex','imd','region','ethnicity']}
    # GP-first includes a GP/hospital same-day tie, but not GP codes after hospital PAD.
    gp_incident=case(when(first_gp==first_pad).then(first_gp))
    for name,dt in [('incidence_any',first_pad),('incidence_gp',gp_incident)]:
        add(name,dt,groups,incident=True)
    for name,frame in procedures.items():
        dt=frame.where(apcs.admission_date>=start).sort_by(apcs.admission_date,apcs.apcs_ident).first_for_patient().admission_date
        add('procedure_'+name,dt,groups)
else:
    since=(start-first_covid).days
    exposed=case(when(since>=365).then('365+'),when(since>=90).then('90-364'),when(since>=28).then('28-89'),otherwise='No recorded infection')
    groups={k:cov[k] for k in ['age_band','sex','imd','ethnicity','diabetes','ckd','smoking','region']}
    groups['covid']=exposed
    add('covid_pad',first_pad,groups,incident=True,covid=True)
