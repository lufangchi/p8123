library(cardioStatsUSA)
library(tidyverse)
library(survey)

df = read.csv("./data/df_analysis.csv")
colnames(df)

#create factor versions with labels
table(df$svy_year)
df$svy_year <- factor(df_analysis$svy_year)
table(df$demo_age_cat)
df$demo_age_cat <- factor(
  df$demo_age_cat,
  levels = c("18 to 44", "45 to 64", "65 to 74", "75+"),
  ordered = TRUE
)

table(df$demo_gender)
df$demo_gender <- factor(df$demo_gender, levels = 1:2, labels=c("Male","Female"))

table(df$demo_race)
df$demo_race <- factor(
  df$demo_race,
  levels = c("Non-Hispanic White",
             "Non-Hispanic Black",
             "Hispanic",
             "Non-Hispanic Asian",
             "Other")
)

table(df$cc_smoke)
df$cc_smoke <- factor(
  df$cc_smoke,
  levels = c("Never", "Former", "Current")
)



svy <- svydesign(
  id = ~svy_psu,
  strata = ~svy_strata,
  weights = ~svy_weight_mec,
  data = df_analysis,
  nest = TRUE
)

svyby(~bp_control_140_90, ~svy_year, svy, svymean)
