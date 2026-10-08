"""Reproducible BDHS audit and exploratory analysis. No raw respondent data exported.
Usage: python analyze.py /path/to/BDIR81FL.SAV /path/to/output
Dependencies: pyreadstat pandas numpy scipy statsmodels patsy
Design variance: ultimate-cluster Taylor sandwich, stratum centered PSU totals,
with zero scores for PSUs outside the analytical domain; no FPC.
Weights remain provisional pending confirmation of long-form weight guidance.
"""
import sys,json
from pathlib import Path
import pyreadstat,pandas as pd,numpy as np,statsmodels.api as sm
from patsy import dmatrix
from scipy.stats import t
raw=Path(sys.argv[1]); out=Path(sys.argv[2]);out.mkdir(exist_ok=True,parents=True)
assets=['V119','V120','V121','V122','V123','V124','V125','V153']
cols=assets+['V191','V012','V013','V502','SQTYPE','V005','V021','V023','V025','V024','V201','V213','V511','V509','V008','V011','V701','V730','V106','V190','V714']
d,meta=pyreadstat.read_sav(str(raw),usecols=cols)
d['w']=d.V005/1e6; d['outcome']=((d.V201>0)|(d.V213==1)).astype(float)
d.loc[d.V201.isna()|d.V213.isna(),'outcome']=np.nan
d['cohab']=np.select([d.V511<15,d.V511.between(15,17),d.V511.between(18,19)],[1.,2.,3.],default=np.nan)
d['partner']=d.V701.where(d.V701.isin([0,1,2,3]));d['gap']=d.V730.where(d.V730<96)-d.V012
d['gapgroup']=np.select([d.gap<=5,d.gap.between(6,10),d.gap>=11],[1.,2.,3.],default=np.nan)
d['wealth3']=np.select([d.V190.isin([1,2]),d.V190==3,d.V190.isin([4,5])],[1.,2.,3.],default=np.nan)
d['duration']=(d.V008-d.V509)/12;d['wealth_z']=(d.V191-d.V191.mean())/d.V191.std()
elig=d[d.V012.between(15,19)&d.V502.isin([1,2])];long=elig[elig.SQTYPE==1];target=long[long.V502==1]
a=target.dropna(subset=['outcome','cohab','partner','gapgroup','V106','V714','V190','V025','V024'])
flow={'all_ever_married_adolescents':len(elig),'short_questionnaire':int((elig.SQTYPE==2).sum()),'long_questionnaire':len(long),'long_formerly_married':int((long.V502==2).sum()),'long_currently_married':len(target),'unknown_partner_education_excluded':len(target)-len(a),'analysis':len(a),'events':int(a.outcome.sum()),'nonevents':int((1-a.outcome).sum()),'full_PSUs':d.V021.nunique(),'strata':d.V023.nunique(),'analysis_PSUs':a.V021.nunique(),'negative_signed_age_gaps':int((a.gap<0).sum())}
(out/'sample_flow.json').write_text(json.dumps(flow,indent=2))
psu_index=pd.MultiIndex.from_frame(d[['V023','V021']].drop_duplicates())
allresults=[]
def fit(name,data,formula,family='logit'):
 X=dmatrix(formula,data,return_type='dataframe');sub=data.loc[X.index];y=sub.outcome.to_numpy();w=sub.w.to_numpy();M=X.to_numpy()
 fam=sm.families.Binomial() if family=='logit' else sm.families.Poisson()
 f=sm.GLM(y,X,family=fam,freq_weights=w).fit(maxiter=200)
 p=f.fittedvalues.to_numpy(); h=w*p*(1-p) if family=='logit' else w*p
 B=np.linalg.inv(M.T@(h[:,None]*M));scores=pd.DataFrame(M*(w*(y-p))[:,None],index=sub.index);scores['V021']=sub.V021;scores['V023']=sub.V023
 S=scores.groupby(['V023','V021']).sum().reindex(psu_index,fill_value=0);meat=np.zeros_like(B)
 for _,g in S.groupby(level=0):
  z=g.to_numpy();z-=z.mean(axis=0);meat+=len(z)/(len(z)-1)*(z.T@z)
 cov=B@meat@B;se=np.sqrt(np.maximum(0,np.diag(cov)));df=len(psu_index)-d.V023.nunique()-(X.shape[1]-1);q=t.ppf(.975,df)
 for i,term in enumerate(X.columns):
  b=f.params.iloc[i];allresults.append(dict(model=name,n=len(sub),term=term,measure='AOR' if family=='logit' else 'APR',estimate=np.exp(b),lower=np.exp(b-q*se[i]),upper=np.exp(b+q*se[i]),p=2*t.sf(abs(b/se[i]),df),design_df=df))
 # Apparent discrimination: descriptive only, not model adequacy or validation.
 pos=y==1;neg=y==0;order=np.argsort(p);r=pd.DataFrame({'p':p,'w':w,'y':y}).groupby('p').apply(lambda z:pd.Series({'pos':z.loc[z.y==1,'w'].sum(),'neg':z.loc[z.y==0,'w'].sum()}),include_groups=False).sort_index();below=r['neg'].cumsum()-r['neg'];auc=(r['pos']*(below+.5*r['neg'])).sum()/(r['pos'].sum()*r['neg'].sum())
 return {'n':len(sub),'parameters':X.shape[1],'converged':bool(f.converged),'condition_number':float(np.linalg.cond(M.T@(h[:,None]*M))),'apparent_weighted_AUC':float(auc),'predicted_min':float(p.min()),'predicted_max':float(p.max())}
base='C(V106)+C(wealth3)+C(cohab)+C(partner)+C(gapgroup)+C(V714)'
primary='C(V012)+C(V106)+C(V190)+C(cohab)+C(partner)+C(gapgroup)+C(V714)+C(V025)+C(V024)'
diag={}
diag['historical']=fit('historical_six_predictors',a,base)
diag['age_adjusted']=fit('age_adjusted_six_predictors',a,base+'+C(V012)')
diag['candidate_primary']=fit('candidate_primary_age_geography',a,primary)
diag['main_no_work']=fit('main_no_work',a,primary.replace('+C(V714)',''))
diag['age_geo']=fit('age_geo',a,'C(V012)+C(cohab)+C(V025)+C(V024)')
diag['prevalence_ratio']=fit('candidate_primary_APR',a,primary,'poisson')
# Functional-form check: nonlinear exact duration is a distinct timing parameterization,
# not adjustment simultaneously for cohabitation age and its mechanically linked duration.
durformula='C(V012)+bs(duration,df=3,degree=3)+C(V106)+C(V190)+C(partner)+C(gapgroup)+C(V714)+C(V025)+C(V024)'
diag['duration']=fit('timing_duration_spline_exploratory',a,durformula)
# PCA is an exploratory asset construct, not supervised confounder selection.
Z=a[assets].where(a[assets].isin([0,1]));pcdata=a.loc[Z.dropna().index].copy();Z=Z.loc[pcdata.index].to_numpy();w=pcdata.w.to_numpy();w=w/w.sum();mean=w@Z;sd=np.sqrt(w@((Z-mean)**2));keep=sd>0;Z=(Z[:,keep]-mean[keep])/sd[keep];asset_corr=Z.T@(w[:,None]*Z);eig,vec=np.linalg.eigh(asset_corr);order=np.argsort(eig)[::-1];eig=eig[order];vec=vec[:,order];v=vec[:,0]
if v[list(np.array(assets)[keep]).index('V122')]<0:v=-v
score=Z@v;pcdata['asset_pc1']=score/np.sqrt(eig[0]);pd.DataFrame({'variable':np.array(assets)[keep],'label':[meta.column_names_to_labels[x] for x in np.array(assets)[keep]],'PC1_eigenvector':v,'weighted_asset_prevalence':mean[keep]}).to_csv(out/'pca_loadings.csv',index=False)
pd.DataFrame({'component':np.arange(1,len(eig)+1),'eigenvalue':eig,'variance_fraction':eig/eig.sum()}).to_csv(out/'pca_variance.csv',index=False)
pcformula=primary.replace('C(V190)','asset_pc1')
diag['pca_model']=fit('exploratory_asset_PC1',pcdata,pcformula)
diag['pca_same_sample_control']=fit('official_wealth_PCA_matched_sample',pcdata,primary)
(out/'diagnostics.json').write_text(json.dumps(diag,indent=2))
pd.DataFrame(allresults).to_csv(out/'model_results.csv',index=False)
checks=[]
for v in ['V012','V106','V190','cohab','partner','gapgroup','V714','V025','V024']:
 for cat,g in a.groupby(v): checks.append({'variable':v,'category':cat,'n':len(g),'events':int(g.outcome.sum()),'nonevents':int((1-g.outcome).sum()),'weighted_percent_outcome':100*np.average(g.outcome,weights=g.w)})
pd.DataFrame(checks).to_csv(out/'category_counts.csv',index=False)
pd.crosstab(a.V012,a.cohab).to_csv(out/'age_cohabitation_overlap.csv')
print(json.dumps(flow));print('PCA n',len(pcdata),'PC1 variance',eig[0]/eig.sum());print(pd.DataFrame(allresults).query("term.str.contains('cohab|partner.*3|gapgroup.*3') and model.str.contains('candidate_primary_age|age_adjusted_six')",engine='python').to_string(index=False))
