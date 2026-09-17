"""Run the real OpenSAFELY graph locally; restore the server YAML even on failure.

Default: ehrQL source tables for the two cohorts, aggregate fixtures for measures.
--measures-from-tables: native ehrQL evaluation for measures as well (much slower).
--fresh: force requested actions and their dependencies; default reuses completed jobs.
"""
from pathlib import Path
import subprocess,sys,hashlib,json,datetime,re,argparse
r=Path(__file__).resolve().parents[2];p=r/'project.yaml'
a=argparse.ArgumentParser();a.add_argument('--fresh',action='store_true');a.add_argument('--measures-from-tables',action='store_true');a.add_argument('--concurrency',type=int,default=1);a.add_argument('targets',nargs='*');opts=a.parse_args()
original=p.read_bytes()
if b'--dummy' in original:raise SystemExit('Production YAML unexpectedly has a dummy override')
text=original.decode().replace(' -- --cohort',' --dummy-tables dummy_tables -- --cohort')
if opts.measures_from_tables:
 text=text.replace(' -- --year',' --dummy-tables dummy_tables -- --year')
else:
 if not (r/'dummy_measures/trends_2017.csv').exists():subprocess.run([sys.executable,str(r/'analysis/local/make_dummy_measures.py')],check=True)
 text=re.sub(r'(--output output/raw/((?:trends|covid)_\d{4}\.csv))\s+-- --year',r'\1 --dummy-data-file dummy_measures/\2 -- --year',text)
expected=original.decode().count('ehrql:v1 generate-measures')
if opts.measures_from_tables:
 assert text.count('--dummy-tables')==expected+2, 'Not every ehrQL action received its dummy source override'
else:
 assert text.count('--dummy-data-file')==expected, 'Not every measures action received its aggregate fixture'
 assert text.count('--dummy-tables')==2, 'Both cohort source-table overrides are required'
meta=r/'metadata/local_validation';meta.mkdir(parents=True,exist_ok=True)
command=['opensafely','run','--concurrency',str(opts.concurrency),'--memory','8G',*(['-f'] if opts.fresh else []),*(opts.targets or ['prepare_release'])]
result=None
try:
 p.write_text(text)
 with (meta/'opensafely-run.log').open('w') as f:
  result=subprocess.run(command,cwd=r,stdout=f,stderr=subprocess.STDOUT)
finally:
 p.write_bytes(original)
log=(meta/'opensafely-run.log').read_text()
completed=sorted(set(re.findall(r'^([a-z_0-9]+): Completed successfully$',log,re.M)))
expected_actions=re.findall(r'^  ([a-z_0-9]+):$',original.decode(),re.M) if (opts.targets or ['prepare_release'])==['prepare_release'] else opts.targets
if opts.fresh and not set(expected_actions).issubset(completed):
 if result:result.returncode=1
record={'completed_actions':completed,'utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'command':' '.join(command),'exit_code':result.returncode if result else None,'measure_input':'native source tables' if opts.measures_from_tables else 'synthetic aggregate fixtures','production_yaml_sha256':hashlib.sha256(original).hexdigest(),'synthetic_only':True,'dummy_override_removed':b'--dummy' not in p.read_bytes()}
(meta/'execution.json').write_text(json.dumps(record,indent=2));print(json.dumps(record,indent=2));sys.exit(result.returncode if result else 1)
