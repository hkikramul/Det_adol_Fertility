# Independent verification script. Not executed in this release: R is unavailable.
# Run: Rscript reproduce_in_R.R /path/to/BDIR81FL.SAV /path/to/output
library(haven)
library(survey)
a <- commandArgs(trailingOnly=TRUE)
stopifnot(length(a)==2)
d <- as.data.frame(read_sav(a[1])); dir.create(a[2],recursive=TRUE,showWarnings=FALSE)
d$w <- as.numeric(d$V005)/1e6
d$outcome <- as.numeric(d$V201>0 | d$V213==1)
d$cohab <- factor(ifelse(d$V511<15,1,ifelse(d$V511<=17,2,ifelse(d$V511<=19,3,NA))),levels=1:3)
d$partner <- factor(ifelse(d$V701 %in% 0:3,as.numeric(d$V701),NA),levels=0:3)
d$gap <- ifelse(d$V730<96,as.numeric(d$V730)-as.numeric(d$V012),NA)
d$gapgroup <- factor(ifelse(d$gap<=5,1,ifelse(d$gap<=10,2,3)),levels=1:3)
for(v in c('V012','V106','V190','V714','V025','V024')) d[[v]] <- factor(as.numeric(d[[v]]))
d$eligible <- as.numeric(d$V013)==1 & as.numeric(d$V502)==1 & as.numeric(d$SQTYPE)==1
required <- c('outcome','cohab','partner','gapgroup','V012','V106','V190','V714','V025','V024')
d$analysis <- d$eligible & complete.cases(d[required])
# No PSUs repeat across strata in supplied file; nest=TRUE is explicit.
# Confirm V005 applicability to long-questionnaire inference before final submission.
design <- svydesign(ids=~V021,strata=~V023,weights=~w,data=d,nest=TRUE)
analytic <- subset(design,analysis)
stopifnot(nrow(analytic$variables)==1601)
model <- svyglm(outcome~V012+V106+V190+cohab+partner+gapgroup+V025+V024,
                design=analytic,family=quasibinomial())
s <- coef(summary(model)); ci <- confint(model)
write.csv(data.frame(term=rownames(s),AOR=exp(s[,1]),lower=exp(ci[,1]),upper=exp(ci[,2]),p=s[,4]),file.path(a[2],'R_primary_results.csv'),row.names=FALSE)
capture.output(summary(model),file=file.path(a[2],'R_model_summary.txt'))
capture.output(lapply(c('V012','V106','V190','cohab','partner','gapgroup','V714','V025','V024'),function(v)regTermTest(model,as.formula(paste('~',v)))),file=file.path(a[2],'R_joint_tests.txt'))
capture.output(sessionInfo(),file=file.path(a[2],'R_session_info.txt'))
