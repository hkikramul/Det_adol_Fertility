# Load libraries
library(haven)
library(dplyr)
library(survey)
library(srvyr)
library(broom)
library(flextable)
library(officer)


# Open the dataset
BDHS <- read_sav("adolescent fertility new_1.SAV")
#Recode the adolescent fertility using new variable V213 as currenly pregnant status

BDHS <- BDHS %>%
  mutate(adol_fertility = ifelse(V201 >= 1 | V213 == 1, 1, 0))
BDHS
BDHS <- BDHS %>%
  mutate(
    adol_fertility = as.numeric(adol_fertility)
  )

# Prepare variables
BDHS <- BDHS %>%
  mutate(
    education = as_factor(V106),
    partner_education = as_factor(V701),
    division = as_factor(V024),
    residence = as_factor(V025),
    religion = as_factor(V130),
    wealth = as_factor(V190),
    age = as_factor(V012),
    age_gap = as_factor(Age_Gap),
    contraceptive_status = as_factor(V312New),
    WomenEmpowerment = as_factor(WomenEmpowerment),
    weight = V005 / 1000000
  )

# Relevel the reference category

BDHS <- BDHS %>%
  mutate(
    education = relevel(education, ref = "Higher"),
    partner_education = relevel(partner_education, ref = "Higher"),
    age_gap = relevel(age_gap, ref = "<=5"),
    wealth = relevel(wealth, ref = "Rich")
  )

options(survey.lonely.psu = "adjust")
# Create survey design object
bdhs_design <- BDHS %>%
  as_survey_design(
    ids = V001,
    strata = V023,
    weights = weight,
    nest = TRUE
  )


# 1. Fit survey-weighted logistic regression model and check overdispersion
model_binom <- svyglm(
  adol_fertility ~ education + partner_education + division + residence +
    religion + wealth + age + age_gap + contraceptive_status + WomenEmpowerment,
  design = bdhs_design,
  family = binomial()
)


# 2. Check model summary
summary(model_binom)

# 3. Check for overdispersion manually
res_dev <- deviance(model_binom)          # residual deviance
df_res <- df.residual(model_binom)        # degrees of freedom
dispersion <- res_dev / df_res            # dispersion statistic

# 4. Print dispersion value
dispersion

# Run univariate predictor model for calculate wald chi-square test 
predictors <- c(
  "education", "partner_education", "division",
  "residence", "religion", "wealth",
  "age", "age_gap", "contraceptive_status", "WomenEmpowerment"
)


# Run univariate surveys and get Wald p-values
uv_results <- lapply(predictors, function(v) {
  fit_uv <- svyglm(
    formula = as.formula(paste("adol_fertility ~", v)),
    design = bdhs_design,
    family = quasibinomial()
)
pval <- summary(fit_uv)$coef[2, "Pr(>|t|)"]
data.frame(variable = v, wald_p = pval)
})

uv_df <- do.call(rbind, uv_results)
print(uv_df)

# Assuming uv_df contains your results:
uv_df <- data.frame(
  variable = c(
    "education", "partner_education", "division",
    "residence", "religion", "wealth",
    "age", "age_gap", "contraceptive_status",
    "WomenEmpowerment"
  ),
  wald_p = c(
    1.016847e-02, 6.252774e-04, 4.098811e-03, 0.4225585,
    0.43829, 1.308803e-05, 2.185662e-03, 1.424685e-02,
    0.9427551, 0.2883230
  )
)

uv_df <- uv_df %>%
  mutate(
    wald_p_3dig = sprintf("%.4f", wald_p)
  )

print(uv_df)



# Fit weighted logistic for over-disperse data

model <- svyglm(
  adol_fertility ~ education + partner_education + division+ wealth + age + age_gap,
  design = bdhs_design,
  family = quasibinomial()
)

# Tidy and exponentiate coefficients
results <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
  rename(
    AOR = estimate,
    `2.5%` = conf.low,
    `97.5%` = conf.high,
    `p-value` = p.value
  ) %>%
  select(term, AOR, `2.5%`, `97.5%`, `p-value`)


# Tidy and exponentiate coefficients
results <- results %>%
  rename(
    `Predictor` = term,
    `AOR` = AOR,
    `95% CI Lower` = `2.5%`,
    `95% CI Upper` = `97.5%`,
    `P-value` = `p-value`
  )

results <- results %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Create flextable
ft <- flextable(results) %>%
  autofit() %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  bold(i = ~ `P-value` < 0.05, bold = TRUE, part = "body")  # bold significant

# Create new Word doc and add table
doc <- read_docx() %>%
  body_add_par("Table X: Adjusted Odds Ratios for Adolescent Fertility", style = "heading 1") %>%
  body_add_flextable(ft)

# Save to Word file with new name
print(doc, target = "Adjusted_OR_Table_Adolescent_Fertility.docx")

# Evaluate model:
#  a) Wald tests for predictor contribution
summary(model)
#  b) Overdispersion check (dispersion > 1 indicates overdispersion)
dispersion <- summary(model)$dispersion
cat("Dispersion (phi):", round(dispersion, 2), "\n")
#  c) Model calibration with Hosmer–Lemeshow test
library(generalhoslem)
hl <- logitgof(BDHS$adol_fertility, fitted(model), g = 10)
print(hl)





