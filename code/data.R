library(cardioStatsUSA)
library(tidyverse)
library(survey)

colnames(nhanes_data)
help(nhanes_data)


table(nhanes_data$svy_subpop_htn)
table(nhanes_data$demo_age_cat)
table(nhanes_data$bp_control_140_90)
table(nhanes_data$htn_jnc7)
dim(nhanes_data)

df = nhanes_data[svy_subpop_htn == 1 & htn_jnc7 == "Yes"]
dim(df)

summary(df$demo_age_years)


vars_survey <- c(
  "svy_id", "svy_psu", "svy_strata",
  "svy_weight_mec", "svy_year"
)

vars_demo <- c(
  "demo_age_years", "demo_age_cat",
  "demo_gender", "demo_race"
)

vars_behavior <- c(
  "cc_smoke"
)

vars_prior_dx <- c(
  "cc_diabetes", "cc_ckd", "cc_bmi", "cc_cvd_any"
)

vars_med_use <- c(
  "bp_med_use",
  "bp_med_n_class",
  "bp_med_n_pills",
  "bp_med_combination",
  # drug classes (optional - complete list)
  "bp_med_ace", "bp_med_angioten", "bp_med_beta",
  "bp_med_ccb", "bp_med_ccb_dh", "bp_med_ccb_ndh",
  "bp_med_diur_thz", "bp_med_diur_loop",
  "bp_med_diur_Ksparing", "bp_med_alpha",
  "bp_med_central", "bp_med_aldo",
  "bp_med_vasod", "bp_med_renin_inhibitors"
)

vars_med_history <- c(
  "bp_sys_mean", "bp_dia_mean",
  "htn_jnc7", "htn_accaha", "htn_escesh",
  "htn_aware",
  "htn_resistant_jnc7", "htn_resistant_accaha"
)

vars_outcome <- c(
  "bp_control_140_90"
)

# Combine all variable groups
vars_all <- c(
  vars_survey,
  vars_demo,
  vars_behavior,
  vars_prior_dx,
  vars_med_use,
  vars_med_history,
  vars_outcome
)

df_analysis <- df[, ..vars_all]

dim(df_analysis)
colnames(df_analysis)

svy <- svydesign(
  id = ~svy_psu,
  strata = ~svy_strata,
  weights = ~svy_weight_mec,
  data = df_analysis,
  nest = TRUE
)

svyby(~bp_control_140_90, ~svy_year, svy, svymean)
