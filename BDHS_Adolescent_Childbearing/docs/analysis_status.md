# Analysis status and choices
The current manuscript main model is `main_no_work`: age, respondent education, five wealth quintiles, cohabitation group, partner education, signed age-gap group, residence and division. Working status appears only in sensitivity specifications.

Completed: raw recoding verification; sample flow; descriptive prevalence; stratified PSU Taylor covariance; regression and timing/PCA sensitivity models; category counts; overlap; convergence; fitted ranges; apparent weighted AUC; compiled and visually reviewed manuscript.

Not completed: confirmation of long-questionnaire subsampling weights/variance; independent execution of R verification; PSU influence; external validation; causal identification. R and ggplot2 scripts are supplied but were not executed. No multiple imputation.

The manual covariance uses the full 674-PSU, 16-stratum structure with zero analytic-domain score contributions outside the subset. Estimates are provisional until questionnaire-specific design guidance is checked.

All specifications were developed after reviewing original results. They are exploratory, not a preregistered analysis. The historical review in docs is retained for provenance and predates the main model excluding work; use the current manuscript and `main_no_work` rows for current results.

PCA models include work, matching the expanded-with-work sensitivity comparator. Both PCA and matched official-wealth models use 1,299 identical respondents. Neither is the main manuscript specification. The first asset component explains 19.3% of variation and is not a validated substitute for official wealth.

Do not infer intervention effects or describe statistical nonsignificance as absence of association. Apparent AUC is not out-of-sample validation. The Poisson fitted values exceeded one; this model is retained for transparency, not adopted as the main model.
