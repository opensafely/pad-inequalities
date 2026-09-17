"""Candidate cohorts include eligibility flags for transparent exclusion flows."""
from ehrql import create_dataset, get_parameter, minimum_of, years
from analysis.definitions import *
mode=get_parameter('cohort',default='incident')
index=first_pad if mode=='incident' else first_procedure.admission_date
dataset=create_dataset()
dataset.configure_dummy_data(population_size=1000)
dataset.define_population(index.is_on_or_between(START,END))
dataset.index_date=index
dataset.eligible=eligible(index)
dataset.registered_1yr=practice_registrations.spanning(index-years(1),index).exists_for_patient()
dataset.registered_2yr=practice_registrations.spanning(index-years(2),index).exists_for_patient()
dataset.death_date=death
dataset.gp_pad_date=first_gp
dataset.hospital_pad_date=first_hospital
dataset.first_covid_date=first_covid
dataset.registration_end=minimum_of(registration(index).end_date,END)
dataset.gp_death_date=bounded_date(patients.date_of_death)
dataset.ons_death_date=bounded_date(ons_deaths.date)
for name,value in covariates(index).items(): dataset.add_column(name,value)
dataset.procedure_date=first_procedure.admission_date
dataset.procedure_discharge=minimum_of(first_procedure.discharge_date,END)
dataset.procedure_admission_method=first_procedure.admission_method
for name,frame in procedures.items():
    # Search from the cohort index: a pre-index diabetic-circulatory spell must
    # not hide a later qualifying outcome after the first recorded PAD diagnosis.
    post_index=frame.where(apcs.admission_date>=index)
    dataset.add_column('first_'+name+'_date',post_index.sort_by(apcs.admission_date,apcs.apcs_ident).first_for_patient().admission_date)
    # A new spell after discharge, not another episode in the index spell.
    after=frame.where(apcs.admission_date > minimum_of(first_procedure.discharge_date.when_null_then(first_procedure.admission_date),END)).where(apcs.apcs_ident != first_procedure.apcs_ident)
    dataset.add_column('next_'+name+'_date',after.sort_by(apcs.admission_date,apcs.apcs_ident).first_for_patient().admission_date)
    dataset.add_column('n_'+name+'_admissions_1yr',frame.where(apcs.admission_date>=index).where(apcs.admission_date<=minimum_of(index+years(1),death,registration(index).end_date,END)).count_for_patient())
# For data-quality checks only; censoring is applied consistently in R.
dataset.pad_on_procedure_spell=first_procedure.all_diagnoses.contains_any_of(pad_hospital)

# Anatomically explicit sensitivity avoids unlinked generic artery procedure codes.
dataset.procedure_specific=first_procedure.all_procedures.contains_any_of(codes("revasc_open_specific")+codes("revasc_endo_specific")+codes("major_amputation")+codes("minor_amputation"))
for name,frame in specific_procedures.items():
    post_index=frame.where(apcs.admission_date>=index)
    dataset.add_column("first_specific_"+name+"_date",post_index.sort_by(apcs.admission_date,apcs.apcs_ident).first_for_patient().admission_date)
    after=frame.where(apcs.admission_date>first_procedure.discharge_date.when_null_then(first_procedure.admission_date))
    dataset.add_column("next_specific_"+name+"_date",after.sort_by(apcs.admission_date,apcs.apcs_ident).first_for_patient().admission_date)

for name in ["open","endo","major","minor"]:
    codefile={"open":"revasc_open","endo":"revasc_endo","major":"major_amputation","minor":"minor_amputation"}[name]
    dataset.add_column("index_has_"+name,first_procedure.all_procedures.contains_any_of(codes(codefile)))
