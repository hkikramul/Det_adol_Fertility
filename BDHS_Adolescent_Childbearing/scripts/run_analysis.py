"""Run all Python analyses and regenerate figures/table inputs."""
from pathlib import Path
import sys,subprocess
root=Path(__file__).resolve().parents[1]
if len(sys.argv)!=2:raise SystemExit('Usage: python scripts/run_analysis.py /path/to/BDIR81FL.SAV')
raw=Path(sys.argv[1]).resolve()
if not raw.is_file():raise SystemExit('Raw SAV input does not exist; obtain authorised DHS data separately.')
for script,args in [('analyze.py',[str(raw),str(root/'results')]),('policy_descriptives.py',[str(raw),str(root/'results')]),('build_figures_and_tables.py',[str(raw)])]:
 subprocess.run([sys.executable,str(root/'scripts'/script),*args],check=True,cwd=root)
print('Analysis, aggregate results and figures rebuilt. See docs/analysis_status.md before submission.')
