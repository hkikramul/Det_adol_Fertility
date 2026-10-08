# Editable ggplot2 counterpart; NOT executed in this release.
# Run from extracted manuscript directory after installing ggplot2 and readr.
library(ggplot2)
library(readr)
r <- read_csv('results/model_results.csv',show_col_types=FALSE)
terms <- c('C(cohab)[T.2.0]','C(cohab)[T.3.0]','C(partner)[T.3.0]','C(gapgroup)[T.3.0]')
labels <- c('Cohabitation 15–17 vs <15','Cohabitation 18–19 vs <15','Higher husband education vs none','Spousal age gap ≥11 vs ≤5')
z <- r[r$model=='main_no_work' & r$term %in% terms,]
z$comparison <- factor(labels[match(z$term,terms)],levels=rev(labels))
p <- ggplot(z,aes(x=estimate,y=comparison))+geom_vline(xintercept=1,linetype=2,colour='grey50')+
 geom_segment(aes(x=lower,xend=upper,yend=comparison),linewidth=.7,colour='#153954')+
 geom_point(size=2.8,colour='#153954')+scale_x_log10(breaks=c(.01,.03,.1,.3,1,3))+
 labs(x='Adjusted odds ratio (95% CI)',y=NULL)+theme_classic(base_size=12)
ggsave('figures/forest_ggplot2.pdf',p,width=8,height=3.5)
v <- read_csv('results/policy_descriptive_estimates.csv',show_col_types=FALSE)
v <- v[v$variable=='V012',]
p <- ggplot(v,aes(category,100*weighted_prevalence))+geom_line(colour='#153954')+
 geom_errorbar(aes(ymin=100*lower,ymax=100*upper),width=.1,colour='#153954')+
 geom_point(size=2.5,colour='#153954')+scale_x_continuous(breaks=15:19)+
 coord_cartesian(ylim=c(0,100))+labs(x='Current age (years)',y='Birth or current pregnancy (%)')+
 theme_classic(base_size=12)
ggsave('figures/age_prevalence_ggplot2.pdf',p,width=6.8,height=3.6)
