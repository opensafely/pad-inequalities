"""Shared, date-anchored definitions; all patient records remain highly sensitive."""
from ehrql import case, when, codelist_from_csv, minimum_of, years
from ehrql.tables.tpp import patients, clinical_events, apcs, practice_registrations, addresses, ons_deaths
START='2017-01-01'
END='2025-06-30'
clinical_events=clinical_events.where(clinical_events.date<=END)
apcs=apcs.where(apcs.admission_date<=END)
def bounded_date(value):
    return case(when(value<=END).then(value))
def codes(name):
    return codelist_from_csv('codelists/local/'+name+'.csv', column='code')
def condition(name):
    files={
        'diabetes':'bristol-multimorbidity_diabetes',
        'ckd':'primis-covid19-vacc-uptake-ckd35',
        'chd':'bristol-multimorbidity_coronary-heart-disease',
        'stroke':'bristol-multimorbidity_stroketransient-ischemic-attack',
    }
    return codelist_from_csv('codelists/'+files[name]+'.csv', column='code')
pad_codes=codes('pad_primary')
pad_hospital=codes('pad_hospital')
gp_pad=clinical_events.where(clinical_events.snomedct_code.is_in(pad_codes))
first_gp=gp_pad.sort_by(clinical_events.date).first_for_patient().date
hospital_pad=apcs.where(apcs.all_diagnoses.contains_any_of(pad_hospital))
first_hospital=hospital_pad.sort_by(apcs.admission_date, apcs.apcs_ident).first_for_patient().admission_date
first_pad=minimum_of(first_gp,first_hospital)
death=bounded_date(minimum_of(patients.date_of_death,ons_deaths.date))
# Recorded COVID: explicit GP diagnosis/positive result or any-position hospital
# U07.1/U07.2 (including clinically diagnosed COVID without laboratory confirmation).
# GP subset excludes test procedures without a positive result, antibody-only,
# organism/substance, medication, severity/rehabilitation and ongoing-symptom codes.
# These dates are recording/admission dates, not infection-onset dates; absence of
# a qualifying record does not establish absence of infection. No laboratory feed.
covid_hospital_codes=codelist_from_csv('codelists/opensafely-covid-identification.csv',column='icd10_code')
first_covid=minimum_of(
 clinical_events.where(clinical_events.snomedct_code.is_in(codes('covid_confirmed'))).where(clinical_events.date >= '2020-01-01').sort_by(clinical_events.date).first_for_patient().date,
 apcs.where(apcs.all_diagnoses.contains_any_of(covid_hospital_codes)).where(apcs.admission_date >= '2020-01-01').sort_by(apcs.admission_date,apcs.apcs_ident).first_for_patient().admission_date,
)
# Require recorded PAD by the spell; remove competing primary indications only.
context=(apcs.all_diagnoses.contains_any_of(pad_hospital+["E105","E115","E135","E145"]) | (first_gp <= apcs.admission_date))
nonvascular=apcs.primary_diagnosis.is_in(['C400','C401','C402','C403','C408','C409','C410','C411','C412','C413','C414','C418','C419']) | apcs.primary_diagnosis.is_in(['S780','S781','S789','S880','S881','S889','S980','S981','S982','S983','S984'])
procedures={k:apcs.where(context).except_where(nonvascular).where(apcs.all_procedures.contains_any_of(codes(v))) for k,v in {'open':'revasc_open','endo':'revasc_endo','major':'major_amputation','minor':'minor_amputation'}.items()}
proc_condition=apcs.all_procedures.contains_any_of(codes('revasc_open')+codes('revasc_endo')+codes('major_amputation')+codes('minor_amputation'))
all_procedures=apcs.where(context).except_where(nonvascular).where(proc_condition)
specific_procedures={k:apcs.where(context).except_where(nonvascular).where(apcs.all_procedures.contains_any_of(codes('revasc_'+k+'_specific'))) for k in ['open','endo']}
first_procedure=all_procedures.sort_by(apcs.admission_date,apcs.apcs_ident).first_for_patient()
ethnicity_codes=codelist_from_csv('codelists/opensafely-ethnicity-snomed-0removed.csv',column='code',category_column='Grouping_6')
smoking_codes=codelist_from_csv('codelists/opensafely-smoking-clear.csv',column='CTV3Code',category_column='Category')
def registration(date):
    return practice_registrations.for_patient_on(date)
def eligible(date):
    return ((patients.age_on(date)>=18) & (patients.age_on(date)<=110)) & practice_registrations.spanning(date-years(1),date).exists_for_patient() & (death.is_null() | (death >= date))
def covariates(date):
    age=patients.age_on(date)
    eth=clinical_events.where(clinical_events.snomedct_code.is_in(ethnicity_codes)).where(clinical_events.date<=date).sort_by(clinical_events.date,clinical_events.snomedct_code).last_for_patient().snomedct_code.to_category(ethnicity_codes)
    smoke=clinical_events.where(clinical_events.ctv3_code.is_in(smoking_codes)).where(clinical_events.date<=date).sort_by(clinical_events.date,clinical_events.ctv3_code).last_for_patient().ctv3_code.to_category(smoking_codes)
    out={'age':age,'age_band':case(when(age<40).then('18-39'),when(age<50).then('40-49'),when(age<60).then('50-59'),when(age<70).then('60-69'),when(age<80).then('70-79'),otherwise='80+'),'sex':patients.sex,'imd':addresses.for_patient_on(date).imd_quintile,'region':registration(date).practice_nuts1_region_name.when_null_then('Unknown'),'ethnicity':eth.when_null_then('Unknown'),'smoking':smoke.when_null_then('Unknown')}
    # ckd is the PRIMIS stage 3-5 diagnosis list; eGFR measurement codes are not diagnoses.
    for name in ['diabetes','ckd','chd','stroke']:
        out[name]=clinical_events.where(clinical_events.snomedct_code.is_in(condition(name))).where(clinical_events.date<=date).exists_for_patient()
    return out
