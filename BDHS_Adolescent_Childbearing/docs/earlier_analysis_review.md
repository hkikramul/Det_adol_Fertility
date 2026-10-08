# BDHS adolescent childbearing analysis — research review release

Prepared 8 October 2026. Status: independently executed candidate analysis; not a certified final journal submission. All model choices are post-review exploratory decisions and must be described honestly. No multiple imputation. No raw respondent records included.

## Research question and scope
Among currently married women aged 15–19 who received the 2022 BDHS long questionnaire, how are cohabitation timing and partner characteristics associated with ever having had a live birth or being currently pregnant? This is a cumulative prevalence outcome, not an adolescent birth rate or causal effect. Births before age 15 are included.

## Verified population
2,449 ever-married adolescents; 814 short questionnaire; 1,635 long questionnaire; 32 formerly married long-form respondents excluded by the target definition; 1,603 currently married long-form respondents; two unknown partner education; 1,601 analysed (960 outcome-positive, 641 negative). The four signed age gaps below zero remain in the <=5 category.

## Candidate main model
Survey-weighted logistic regression: categorical current age, respondent education, five original wealth quintiles, cohabitation groups, husband education, signed spousal age-gap groups, working status, urban/rural residence and division. All included simultaneously; no p-value screening. Reference categories are the smallest raw code: age 15, no education, poorest wealth, cohabitation <15, no partner education, gap <=5, not working, urban, Barishal.

Age and geography are justified as measured background characteristics, not selected for significance. Working status, wealth and education are measured at interview and may be consequences of childbearing. Conditional coefficients are therefore not total causal effects. A DAG and a clearer primary exposure should precede any causal claims.

## Selected executed results
| Comparison | AOR | 95% CI |
|---|---:|---|
| C(cohab)[T.2.0] | 0.170 | 0.113–0.256 |
| C(cohab)[T.3.0] | 0.026 | 0.015–0.045 |
| C(partner)[T.3.0] | 0.612 | 0.338–1.108 |
| C(gapgroup)[T.3.0] | 1.752 | 1.242–2.473 |

Husband higher education is inconclusive under this specification; do not retain the original statistically significant education headline. See model_results.csv for every coefficient and every sensitivity model.

## Survey estimation and limitations
Python GLM point estimates with inverse-probability weights; manually implemented stratified ultimate-PSU Taylor sandwich. PSU score totals are centered within each stratum; PSUs outside the analytic domain contribute zero. No finite population correction. t inference uses 674 minus 16 minus the number of non-intercept parameters. Weight scaling equals V005/1,000,000. This accounts for the supplied first-stage design under the ultimate-cluster approximation; it does not independently validate every aspect of the long-questionnaire subsampling variance. Confirm survey documentation and weight applicability before claiming final national inference.

R is unavailable in this runtime. reproduce_in_R.R is provided for independent survey-package verification and joint term tests; it has NOT been executed. The manual covariance reproduces selected historical table intervals closely but does not substitute for verifying all final outputs in survey.

## PCA assessment
An exploratory weighted PCA was executed on eight binary asset items after excluding codes outside 0/1. It uses respondent weights and estimates asset covariance in this selected adolescent population, not a nationally recalibrated household wealth measure. Complete valid asset data exist for 1,299 of 1,601 respondents: 302 analysed respondents have not-de-jure codes. PC1 explains 19.29% of standardized asset variation. Rare radio, telephone and car items have unstable potential influence. Eigenvalues and loadings are exported. Sign oriented to positive refrigerator loading.

The PCA model replaces official quintiles with PC1 only as a sensitivity analysis. An official-wealth model on the exact same 1,299 respondents is included to distinguish changed sampling from changed wealth measurement. Do not present this as validated wealth, as a mechanism, or as supervised selection of valuable covariates. Official DHS wealth already uses asset PCA and is retained in the candidate primary model.

## Executed sensitivity analyses
Historical six-predictor model; age-adjusted historical model; candidate age/geography model; robust Poisson prevalence-ratio model; duration-spline timing model; asset-PC1 model and matched-sample official-wealth comparator.

The Poisson model produced fitted values up to 1.487, so it is not recommended as the main probability model. Its coefficient table is retained for transparency. The duration spline replaces cohabitation groups and addresses a different timing parameterization; its high information-matrix condition number and near-one fitted values warrant further stability checks. No claim of superiority based on AUC.

## Diagnostics actually completed
Convergence, one-variable outcome cell counts, age/cohabitation overlap, information-matrix condition numbers, fitted ranges and apparent weighted AUC. All models converged. Candidate primary apparent AUC = 0.811; this is training-sample discrimination, not external validation or proof of adequacy. Sparse no-education reference contains only 25 women and six negative outcomes. Cohabitation at 18–19 has structural zero cells at younger current ages; do not standardize that category over ages 15–17.

## Required before submission
1. Verify long-form weight and design guidance and run the supplied R verification.
2. Agree on primary exposure/estimand and document post-review specification changes.
3. Check influence by PSU, nonlinear age/cohabitation sensitivity, and exclusion of post-outcome working status.
4. Evaluate urban/rural and education sensitivities, avoiding an unreported search for significance.
5. If presenting adjusted probabilities, restrict comparisons to feasible current ages (for all three cohabitation groups, ages 18–19) and use survey-based confidence intervals.
6. Replace determinants/protective-effect wording with associations; no intervention efficacy claims.
7. Remove every imputation claim and unsupported diagnostic statement from the manuscript.
8. Consider first-birth timing analysis only after validating birth dates, union dates and event-before-union handling; pregnancy is a different event and cannot simply be treated as a live birth.

## Publication framing
Suggested title: Cohabitation timing and adolescent childbearing among currently married women in Bangladesh: a complex-survey analysis of the 2022 BDHS. Statistical complexity does not supply novelty. A strong journal submission needs a focused public-health contribution, transparent reporting, and conclusions commensurate with the cross-sectional evidence. Acceptance is not guaranteed.

## Reproduce
Install pyreadstat, pandas, numpy, scipy, statsmodels and patsy. Run: python analyze.py /path/to/BDIR81FL.SAV /path/to/output. Model tables contain unrounded values. The source raw dataset is required but deliberately excluded from this package.

## Method references
https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Wealth_Quintiles.htm
https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svyprcomp.html
https://r-survey.r-forge.r-project.org/survey/html/regTermTest.html
https://dhsprogram.com/pubs/pdf/FR386/FR386.pdf

## Policy analysis addition
Policy descriptive estimates use all 2,449 ever-married adolescents across both questionnaires, independent of partner-model exclusions. policy_descriptive_estimates.csv reports weighted outcome prevalence and logit-transformed Taylor confidence intervals overall, by current age, division and residence. Overall prevalence is 59.66% (95% CI 57.62–61.67%). These are estimates for EVER-MARRIED adolescents, not all girls in Bangladesh. No ranking of districts or causal program effect is supported. Geographic comparisons are descriptive and not adjusted intervention targets.

For actionable policy inference, specify an intervention and estimand first: for example incidence of first birth before age 20 under a feasible delayed-union strategy. This cross-sectional ever-married sample excludes never-married girls and conditions on a selection process related to union timing; it cannot identify that population intervention effect. Evaluate legislation with longitudinal or repeated pre/post data and an identified comparison design, not this regression. For Lancet-family positioning, journal scope, novelty and independent substantive validation remain unresolved; no claim of readiness or acceptance is made.
