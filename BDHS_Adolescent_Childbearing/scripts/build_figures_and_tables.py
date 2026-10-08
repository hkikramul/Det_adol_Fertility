from pathlib import Path
import pandas as pd,numpy as np,pyreadstat,matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import sys
root=Path(__file__).resolve().parents[1]; p=root/'manuscript'; r=pd.read_csv(root/'results/model_results.csv'); prev=pd.read_csv(root/'results/policy_descriptive_estimates.csv')
main=r[r.model=='main_no_work'].set_index('term')
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':11,'axes.spines.top':False,'axes.spines.right':False,'pdf.fonttype':42})
terms=['C(cohab)[T.2.0]','C(cohab)[T.3.0]','C(partner)[T.3.0]','C(gapgroup)[T.3.0]']; labels=['Cohabitation 15–17 vs <15','Cohabitation 18–19 vs <15','Higher husband education vs none','Spousal age gap ≥11 vs ≤5']
f,ax=plt.subplots(figsize=(8,3.5));ys=np.arange(4)[::-1]
for y,k in zip(ys,terms):
 z=main.loc[k];ax.errorbar(z.estimate,y,xerr=[[z.estimate-z.lower],[z.upper-z.estimate]],fmt='o',color='#153954',capsize=4,lw=1.7);ax.text(3.3,y,f'{z.estimate:.3f} ({z.lower:.3f}–{z.upper:.3f})',va='center',fontsize=10)
ax.axvline(1,color='0.45',ls='--',lw=1);ax.set_xscale('log');ax.set_xlim(.01,12);ax.set_xticks([.01,.03,.1,.3,1,3]);ax.set_xticklabels(['0.01','0.03','0.1','0.3','1','3']);ax.set_yticks(ys,labels);ax.set_xlabel('Adjusted odds ratio (logarithmic scale)');ax.set_ylim(-.6,3.6);f.tight_layout();f.savefig(p/'figures/forest.pdf',bbox_inches='tight');plt.close(f)
f,ax=plt.subplots(figsize=(6.8,3.6));z=prev[prev.variable=='V012'];ax.errorbar(z.category,z.weighted_prevalence*100,yerr=[(z.weighted_prevalence-z.lower)*100,(z.upper-z.weighted_prevalence)*100],fmt='o-',color='#153954',capsize=4,lw=1.5);ax.set_xticks(range(15,20));ax.set_ylim(0,100);ax.set_xlabel('Current age (years)');ax.set_ylabel('Birth or current pregnancy (%)');ax.grid(axis='y',alpha=.15);f.tight_layout();f.savefig(p/'figures/age_prevalence.pdf');plt.close(f)
f,ax=plt.subplots(figsize=(7.5,4.3));ax.axis('off')
boxes=[(.5,.88,'Ever-married adolescents aged 15–19\nn = 2,449'),(.5,.62,'Long questionnaire\nn = 1,635'),(.5,.36,'Currently married, long questionnaire\nn = 1,603'),(.5,.10,'Complete analytical sample\nn = 1,601')]
for x,y,txt in boxes:ax.text(x,y,txt,ha='center',va='center',bbox=dict(boxstyle='round,pad=.5',fc='#eef3f6',ec='#153954'),fontsize=11)
for y in [.78,.52,.26]:ax.annotate('',xy=(.5,y-.06),xytext=(.5,y),arrowprops=dict(arrowstyle='->',color='#153954'))
for y,txt in [(.73,'814 short questionnaire'),(.47,'32 formerly married'),(.21,'2 unknown husband education')]:ax.text(.76,y,txt,ha='left',va='center',fontsize=9)
f.tight_layout();f.savefig(p/'figures/flow.pdf',bbox_inches='tight');plt.close(f)
# Raw-data sample table and labels.
cols=['V012','V013','V502','SQTYPE','V005','V106','V190','V511','V701','V730','V714','V025','V024']
d,m=pyreadstat.read_sav(sys.argv[1],usecols=cols);a=d[(d.V012.between(15,19))&(d.V502==1)&(d.SQTYPE==1)&d.V701.isin([0,1,2,3])].copy();a['cohab']=np.select([a.V511<15,a.V511<=17,a.V511<=19],[1,2,3]);a['gapgroup']=np.select([(a.V730-a.V012)<=5,(a.V730-a.V012)<=10],[1,2],default=3)
labelsmap={'V012':{i:str(i) for i in range(15,20)},'V106':{0:'None',1:'Primary',2:'Secondary',3:'Higher'},'V190':{1:'Poorest',2:'Poorer',3:'Middle',4:'Richer',5:'Richest'},'cohab':{1:'Below 15',2:'15--17',3:'18--19'},'V701':{0:'None',1:'Primary',2:'Secondary',3:'Higher'},'gapgroup':{1:r'$\leq5$',2:'6--10',3:r'$\geq11$'},'V025':{1:'Urban',2:'Rural'},'V024':{1:'Barishal',2:'Chattogram',3:'Dhaka',4:'Khulna',5:'Mymensingh',6:'Rajshahi',7:'Rangpur',8:'Sylhet'}}
names={'V012':'Current age','V106':'Respondent education','V190':'Household wealth','cohab':'First cohabitation age','V701':'Husband education','gapgroup':'Signed spousal age gap','V025':'Residence','V024':'Division'}
rows=[]
for v in names:
 for j,(code,g) in enumerate(a.groupby(v)):
  rows.append(f"{names[v] if j==0 else ''} & {labelsmap[v][int(code)]} & {len(g)} & {100*g.V005.sum()/a.V005.sum():.1f} \\\\")
(p/'table1.tex').write_text('\n'.join(rows))
term_labels={'Intercept':'Intercept'}
for v,n in [('V012','Age'),('V106','Respondent education'),('V190','Wealth'),('cohab','Cohabitation age'),('partner','Husband education'),('gapgroup','Spousal age gap'),('V025','Residence'),('V024','Division')]:
 raw='V701' if v=='partner' else v
 for code,label in labelsmap[raw].items():term_labels[f'C({v})[T.{float(code)}]']=f'{n}: {label}'
def pv(x):return '$<0.001$' if x<.001 else f'{x:.3f}'
rows=[]
for k,z in main.iterrows():
 if k=='Intercept':continue
 rows.append(f"{term_labels.get(k,k)} & {z.estimate:.3f} & {z.lower:.3f}--{z.upper:.3f} & {pv(z.p)} \\\\")
(p/'table3.tex').write_text('\n'.join(rows))
rows=[]
for k,label in zip(terms,labels):
 short=label.replace('–','--').replace('≥',r'$\geq$').replace('≤',r'$\leq$').replace('<',r'$<$')
 vals=[]
 for model in ['historical_six_predictors','age_geo','main_no_work','candidate_primary_age_geography']:
  zz=r[(r.model==model)&(r.term==k)]; vals.append('--' if len(zz)==0 else f'{zz.iloc[0].estimate:.3f} ({zz.iloc[0].lower:.3f}--{zz.iloc[0].upper:.3f})')
 rows.append(short+' & '+' & '.join(vals)+r' \\')
(p/'sensitivity.tex').write_text('\n'.join(rows))
print('figures and tables created')

import shutil
shutil.copytree(p/'figures',root/'figures',dirs_exist_ok=True)
