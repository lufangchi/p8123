library(cardioStatsUSA)
library(tidyverse)
library(survey)
library(ggplot)

df = read.csv("./data/df_analysis.csv")
colnames(df)
str(df)

vars_to_check <- c(
  "svy_year",
  "demo_age_cat", "demo_gender", "demo_race",
  "cc_smoke", "cc_diabetes", "cc_ckd", "cc_bmi", "cc_cvd_any",
  "bp_med_use", "bp_med_n_class", "bp_med_n_pills", "bp_med_combination",
  "htn_aware", "htn_resistant_jnc7", "htn_resistant_accaha"
)


for (v in vars_to_check) {
  cat("----------", v, "----------\n")
  
  if (is.numeric(df[[v]]) && !all(df[[v]] %% 1 == 0, na.rm = TRUE)) {
    print(summary(df[[v]]))
    
  } else if (is.numeric(df[[v]])) {
    print(table(df[[v]], useNA = "ifany"))
    
  } else {
    print(table(df[[v]], useNA = "ifany"))
  }
  
  cat("\n")
}

df$svy_year <- factor(df$svy_year)

df$demo_age_cat <- factor(
  df$demo_age_cat,
  levels = c("18 to 44", "45 to 64", "65 to 74", "75+"),
  ordered = TRUE
)

df$demo_gender <- factor(df$demo_gender, levels=c("Men","Women"))

df$demo_race <- factor(
  df$demo_race,
  levels=c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Non-Hispanic Asian","Other")
)

df$cc_smoke <- factor(
  df$cc_smoke,
  levels=c("Never","Former","Current")
)

df$cc_bmi <- factor(
  df$cc_bmi,
  levels=c("<25","25 to <30","30 to <35","35+"),
  ordered = TRUE
)

df$bp_med_n_class <- factor(
  df$bp_med_n_class,
  levels = c("None","One","Two","Three","Four or more")
)

df$bp_med_n_pills <- factor(
  df$bp_med_n_pills,
  levels = c("None","One","Two","Three","Four or more")
)


binary_vars <- c(
  "cc_diabetes", "cc_ckd", "cc_cvd_any",
  "bp_med_use", "bp_med_combination",
  "htn_aware",
  "htn_resistant_jnc7", "htn_resistant_accaha"
)

for (v in binary_vars) {
  df[[v]] <- factor(df[[v]], levels = c("No", "Yes"))
}

str(df)


svy <- svydesign(
  id = ~svy_psu,
  strata = ~svy_strata,
  weights = ~svy_weight_mec,
  data = df,
  nest = TRUE
)



svyby(~bp_control_140_90, ~svy_year, svy, svymean)
bp_trend <- svyby(~bp_control_140_90, ~svy_year, svy, svymean, vartype = "ci")

bp_trend <- bp_trend %>%
  rename(
    mean  = `bp_control_140_90Yes`,
    ci_l  = `ci_l.bp_control_140_90Yes`,
    ci_u  = `ci_u.bp_control_140_90Yes`
  )
ggplot(bp_trend, aes(x = svy_year, y = mean)) +
  geom_point() +
  geom_line(group = 1) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.1) +
  theme_minimal() +
  labs(
    x = "Survey Cycle",
    y = "BP Control Rate (Proportion)",
    title = "Survey-Weighted Trend in BP Control Over Time"
  )




