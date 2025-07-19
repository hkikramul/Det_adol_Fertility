#load different libraries

library(haven)
library(dplyr)
library(survey)
library(srvyr)
library(glmmTMB)
library(lme4)

# Open the dataset
BDHS <- read_sav("adolescent fertility new_1.SAV")

#Recode the adolescent fertility using new variable V213 as currenly pregnant status

BDHS <- BDHS %>%
  mutate(adol_fertility = ifelse(V201 >= 1 | V213 == 1, 1, 0))


# Prepare variables
BDHS <- BDHS %>%
  rename(
    
    cluster_id = V001
  )
# Fit a null (intercept-only) multilevel logistic model

null_model <- glmer(adol_fertility ~ 1 + (1 | cluster_id),
                    data = BDHS,
                    family = binomial(link = "logit"),
                    control = glmerControl(optimizer = "bobyqa"))

summary(null_model)




