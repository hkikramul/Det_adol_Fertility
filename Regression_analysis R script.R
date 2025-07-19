# Load libraries
library(haven)
library(dplyr)
library(survey)
library(srvyr)
# Open the dataset
BDHS <- read_sav("adolescent fertility new_1.SAV")
#Recode the adolescent fertility using new variable V213 as currenly pregnant status

BDHS <- BDHS %>%
  mutate(adol_fertility = ifelse(V201 >= 1 | V213 == 1, 1, 0))
BDHS

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
# Create survey design object
bdhs_design <- BDHS %>%
  as_survey_design(
    ids = V001,
    strata = V023,
    weights = weight,
    nest = TRUE
  )

# For strata with single PSU
options(survey.lonely.psu = "adjust")
# Variables to summarize
vars_to_summarize <- c(
  "education", "partner_education", "division", "residence", "religion",
  "wealth", "contraceptive_status", "age", "age_gap", "WomenEmpowerment"
)

# Function to summarize each variable
get_summary <- function(var) {
  bdhs_design %>%
    group_by(value = .data[[var]]) %>%
    summarise(
      n = unweighted(n()),
      percent = survey_mean(proportion = TRUE, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(variable = var) %>%
    select(variable, category = value, n, percent)
}

# Apply and combine
descriptive_table <- bind_rows(lapply(vars_to_summarize, get_summary))
# View table
print(descriptive_table)

# Export as CSV
write.csv(descriptive_table, "descriptive_table.csv", row.names = FALSE)

# Create Word document and add formatted table
doc <- read_docx() %>%
  body_add_par("Descriptive Table: Weighted Percentage and Unweighted Frequency", style = "heading 1") %>%
  body_add_flextable(flextable(descriptive_table))
print(doc, target = "Descriptive_Table.docx")


# Fix lonely PSU issue
options(survey.lonely.psu = "adjust")
library(tidyr)

# Background variables
vars_to_check <- c(
  "education", "partner_education", "division", "residence", "religion",
  "wealth", "contraceptive_status", "age", "age_gap", "WomenEmpowerment"
)

# Function to create table for each variable
generate_table <- function(varname) {
  # Unweighted counts
  unweighted_counts <- BDHS %>%
    filter(!is.na(.data[[varname]]), !is.na(adol_fertility)) %>%
    count(!!sym(varname), adol_fertility) %>%
    pivot_wider(names_from = adol_fertility, values_from = n, values_fill = 0) %>%
    rename(`No Child (n)` = `0`, `Had Child (n)` = `1`) %>%
    mutate(Category = as.character(!!sym(varname))) %>%
    select(Category, `Had Child (n)`, `No Child (n)`)
  
  # Weighted percentages
  tab <- svytable(as.formula(paste("~", varname, "+ adol_fertility")), bdhs_design)
  prop_tab <- prop.table(tab, margin = 1) * 100
  perc_tab <- round(as.data.frame.matrix(prop_tab), 1)
  perc_tab$Category <- rownames(perc_tab)
  rownames(perc_tab) <- NULL
  names(perc_tab) <- c("No Child (%)", "Had Child (%)", "Category")
  
  # Chi-square p-value
  pval <- tryCatch({
    svychisq(as.formula(paste("~", varname, "+ adol_fertility")), bdhs_design)$p.value
  }, error = function(e) NA)
  pval_str <- format.pval(pval, digits = 3)
  
  # Merge and format final table
  final <- left_join(unweighted_counts, perc_tab, by = "Category") %>%
    mutate(
      Variable = varname,
      `p-value` = c(pval_str, rep("", n() - 1))
    ) %>%
    select(Variable, Category,
           `Had Child (n)`, `Had Child (%)`,
           `No Child (n)`, `No Child (%)`,
           `p-value`)
  
  return(final)
}

# Apply function across all variables
full_table <- bind_rows(lapply(vars_to_check, generate_table))

# Export to Word document
doc <- read_docx() %>%
  body_add_par("Frequency (%) Distribution of Ever-Married Adolescent Women by Fertility with p-value (χ² Test)", style = "heading 1") %>%
  body_add_flextable(flextable(full_table))

print(doc, target = "Fertility_Distribution_by_Covariates_new.docx")

