"""Survey-domain descriptive estimates; uses original V005, PSU and strata."""
import sys
from pathlib import Path
import pyreadstat,pandas as pd,numpy as np
from scipy.stats import t
raw=Path(sys.argv[1]); output=Path(sys.argv[2]);output.mkdir(exist_ok=True,parents=True)
d,_=pyreadstat.read_sav(str(raw),usecols=['V012','V502','V201','V213','V005','V021','V023','V024','V025'])
d['y']=((d.V201>0)|(d.V213==1)).astype(float);d.loc[d.V201.isna()|d.V213.isna(),'y']=np.nan
d['w']=d.V005/1e6;d['domain']=d.V012.between(15,19)&d.V502.isin([1,2])&d.y.notna()
idx=pd.MultiIndex.from_frame(d[['V023','V021']].drop_duplicates());out=[]
for var,cat in [('all',0)]+[('V012',v) for v in range(15,20)]+[('V024',v) for v in range(1,9)]+[('V025',v) for v in [1,2]]:
 mask=d.domain if var=='all' else d.domain&(d[var]==cat);s=d.loc[mask];W=s.w.sum();mu=np.average(s.y,weights=s.w)
 z=pd.DataFrame({'V021':s.V021,'V023':s.V023,'score':s.w*(s.y-mu)/W}).groupby(['V023','V021']).score.sum().reindex(idx,fill_value=0);variance=0
 for _,g in z.groupby(level=0):variance+=len(g)/(len(g)-1)*((g-g.mean())**2).sum()
 se=np.sqrt(variance);q=t.ppf(.975,len(idx)-d.V023.nunique());logit=np.log(mu/(1-mu));se_logit=se/(mu*(1-mu));lo=1/(1+np.exp(-(logit-q*se_logit)));hi=1/(1+np.exp(-(logit+q*se_logit)))
 out.append(dict(variable=var,category=cat,n=len(s),weighted_prevalence=mu,lower=lo,upper=hi))
pd.DataFrame(out).to_csv(output/'policy_descriptive_estimates.csv',index=False)
