# Adolescent childbearing in Bangladesh — BDHS 2022

Reproducible research analysis of live-birth experience or current pregnancy among women aged 15–19. This is a research-review release, not a final validated journal submission or causal policy evaluation.

## Population
- Descriptive: 2,449 ever-married adolescents across both questionnaire forms.
- Long questionnaire: 1,635; currently married: 1,603.
- Regression: 1,601 after excluding two unknown husband-education responses.
- Short-questionnaire exclusions and formerly married respondents are not ordinary item nonresponse. No imputation.

## Run from the analysis folder
Requires Python 3.12 (the version used for this release). Obtain the raw DHS file separately.

```bash
cd BDHS_Adolescent_Childbearing
python -m venv .venv
source .venv/bin/activate
python -m pip install -r requirements.txt
python scripts/run_analysis.py data/BDIR81FL.SAV
```

The command rebuilds model tables, policy descriptive estimates, figures and LaTeX table inputs. It does not automatically compile the PDF.

```bash
cd manuscript
pdflatex -interaction=nonstopmode -halt-on-error Manuscript_Revised.tex
pdflatex -interaction=nonstopmode -halt-on-error Manuscript_Revised.tex
```

## Optional independent R verification
Requires haven and survey. This script has not been executed in this release.

```r
install.packages(c("haven", "survey", "ggplot2", "readr"))
```

```bash
Rscript scripts/verify_in_R.R data/BDIR81FL.SAV results/R_verification
Rscript scripts/figures_ggplot2.R
```

PDF figures included here were generated with Matplotlib; the editable ggplot2 counterpart is supplied for R workflows.

## Outputs
- `results/model_results.csv`: all specifications, estimates, design-based intervals, coefficient p-values and sample sizes.
- `results/policy_descriptive_estimates.csv`: overall and age/division/residence prevalence and intervals.
- `results/category_counts.csv`: observed positive/negative counts and weighted prevalence by category.
- `results/age_cohabitation_overlap.csv`: overlap/structural zero audit.
- `results/pca_loadings.csv`, `pca_variance.csv`: exploratory asset PCA.
- `results/diagnostics.json`, `sample_flow.json`: completed diagnostics and population counts.
- `figures/`: publication-quality vector graphics.
- `manuscript/`: editable LaTeX, table inputs, figures and review PDF.

## Interpretation
The current main model is `main_no_work`. The official DHS wealth measure remains in the main model. PCA is exploratory and cannot select causal confounders. See `docs/analysis_status.md` for completed and outstanding checks. No claim of national inference for all adolescent girls or an intervention effect is justified.

## Data and permissions
No raw respondent data, identifiers or credentials are distributed. Obtain data through https://dhsprogram.com/data/ under the applicable access conditions. The parent repository contains an MIT software licence; see `../LICENSE`. DHS data remain subject to their separate access conditions. Authors are responsible for manuscript rights and coauthor approval.

## AI disclosure
ChatGPT/Codex assisted with review, code, figures and drafting. Authors are responsible for independent verification and the final manuscript. AI is not an author.
