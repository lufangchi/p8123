# Exploratory Data Analysis (EDA)
library(tidyverse)
library(survey)
library(gridExtra)
library(grid)
source("./code/import_transform_data.R")

# Create output directory for figures
if (!dir.exists("./figure")) {
  dir.create("./figure")
}

cat("EXPLORATORY DATA ANALYSIS\n")

# TABLE 1: Baseline Characteristics
cat("TABLE 1: Baseline Characteristics (Survey-Weighted)\n")

# Create summary table
table1_data <- data.frame(
  Characteristic = character(),
  Value = character(),
  stringsAsFactors = FALSE
)

# Age
age_mean <- svymean(~demo_age_years, svy)
age_se <- SE(svymean(~demo_age_years, svy))
table1_data <- rbind(table1_data, 
                     data.frame(Characteristic = "Age (years), mean (SE)", 
                                Value = sprintf("%.1f (%.1f)", coef(age_mean), age_se)))

# Age category
age_cat <- svymean(~demo_age_cat, svy)
for (i in seq_along(coef(age_cat))) {
  table1_data <- rbind(table1_data,
                       data.frame(Characteristic = paste0("  ", names(coef(age_cat))[i]),
                                  Value = sprintf("%.1f%%", coef(age_cat)[i] * 100)))
}

# Gender
gender <- svymean(~demo_gender, svy)
for (i in seq_along(coef(gender))) {
  table1_data <- rbind(table1_data,
                       data.frame(Characteristic = paste0("Gender: ", names(coef(gender))[i]),
                                  Value = sprintf("%.1f%%", coef(gender)[i] * 100)))
}

# Race
race <- svymean(~demo_race, svy)
for (i in seq_along(coef(race))) {
  table1_data <- rbind(table1_data,
                       data.frame(Characteristic = paste0("Race: ", names(coef(race))[i]),
                                  Value = sprintf("%.1f%%", coef(race)[i] * 100)))
}

# BMI
bmi <- svymean(~cc_bmi, svy)
for (i in seq_along(coef(bmi))) {
  table1_data <- rbind(table1_data,
                       data.frame(Characteristic = paste0("BMI: ", names(coef(bmi))[i]),
                                  Value = sprintf("%.1f%%", coef(bmi)[i] * 100)))
}

# Diabetes
diabetes <- svymean(~cc_diabetes, svy)
table1_data <- rbind(table1_data,
                     data.frame(Characteristic = "Diabetes: Yes",
                                Value = sprintf("%.1f%%", coef(diabetes)["cc_diabetesYes"] * 100)))

# CKD
ckd <- svymean(~cc_ckd, svy)
table1_data <- rbind(table1_data,
                     data.frame(Characteristic = "CKD: Yes",
                                Value = sprintf("%.1f%%", coef(ckd)["cc_ckdYes"] * 100)))

# CVD
cvd <- svymean(~cc_cvd_any, svy)
table1_data <- rbind(table1_data,
                     data.frame(Characteristic = "CVD: Yes",
                                Value = sprintf("%.1f%%", coef(cvd)["cc_cvd_anyYes"] * 100)))

# BP Medication Use
med_use <- svymean(~bp_med_use, svy)
table1_data <- rbind(table1_data,
                     data.frame(Characteristic = "BP Medication Use: Yes",
                                Value = sprintf("%.1f%%", coef(med_use)["bp_med_useYes"] * 100)))

# Number of medication classes
med_class <- svymean(~bp_med_n_class, svy, na.rm = TRUE)
for (i in seq_along(coef(med_class))) {
  table1_data <- rbind(table1_data,
                       data.frame(Characteristic = paste0("Med Classes: ", names(coef(med_class))[i]),
                                  Value = sprintf("%.1f%%", coef(med_class)[i] * 100)))
}

# Systolic BP
sbp_mean <- svymean(~bp_sys_mean, svy)
sbp_se <- SE(svymean(~bp_sys_mean, svy))
table1_data <- rbind(table1_data,
                     data.frame(Characteristic = "Systolic BP (mmHg), mean (SE)",
                                Value = sprintf("%.1f (%.1f)", coef(sbp_mean), sbp_se)))

# Diastolic BP
dbp_mean <- svymean(~bp_dia_mean, svy)
dbp_se <- SE(svymean(~bp_dia_mean, svy))
table1_data <- rbind(table1_data,
                     data.frame(Characteristic = "Diastolic BP (mmHg), mean (SE)",
                                Value = sprintf("%.1f (%.1f)", coef(dbp_mean), dbp_se)))

# Hypertension awareness
aware <- svymean(~htn_aware, svy)
table1_data <- rbind(table1_data,
                     data.frame(Characteristic = "Hypertension Awareness: Yes",
                                Value = sprintf("%.1f%%", coef(aware)["htn_awareYes"] * 100)))

print(table1_data, row.names = FALSE)
cat("\n")

# Save Table 1 as image
table1_grob <- tableGrob(table1_data, 
                         theme = ttheme_default(
                           core = list(
                             fg_params = list(fontsize = 9, hjust = 0, x = 0.05),
                             bg_params = list(fill = "white", col = "white")
                           ),
                           colhead = list(
                             fg_params = list(fontsize = 10, fontface = "bold", hjust = 0, x = 0.05),
                             bg_params = list(fill = "white", col = "white")
                           ),
                           rowhead = list(
                             bg_params = list(fill = "white", col = "white")
                           ),
                           base_colour = "black",
                           base_size = 9,
                           padding = unit(c(4, 4), "mm")
                         ))

# Create title as a proper grob with padding
title1 <- textGrob("Table 1: Baseline Characteristics (Survey-Weighted)", 
                   gp = gpar(fontsize = 14, fontface = "bold"),
                   x = unit(0.05, "npc"),
                   hjust = 0,
                   vjust = 0.5)

# Use grid.arrange for better layout control
png("./figure/table1_baseline_characteristics.png", 
    width = 12, height = 12, units = "in", res = 300, bg = "white")
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1, 
                                           heights = unit.c(unit(0.8, "in"), unit(1, "null")),
                                           widths = unit(1, "npc"))))
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.draw(title1)
popViewport()
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.draw(table1_grob)
popViewport(2)
dev.off()
cat("Saved: ./figure/table1_baseline_characteristics.png\n\n")

# TABLE 2: BP Control Rates by Key Predictors
cat("TABLE 2: BP Control Rates by Key Predictors (Survey-Weighted)\n")

# Create table 2
table2_data <- data.frame(
  Predictor = character(),
  Category = character(),
  BP_Control_Rate = character(),
  CI_95 = character(),
  stringsAsFactors = FALSE
)

# By survey year
bp_by_year <- svyby(~bp_control_140_90, ~svy_year, svy, svymean, vartype = "ci")
for (i in seq_len(nrow(bp_by_year))) {
  table2_data <- rbind(table2_data,
                       data.frame(Predictor = ifelse(i == 1, "Survey Year", ""),
                                  Category = as.character(bp_by_year$svy_year[i]),
                                  BP_Control_Rate = sprintf("%.1f%%", bp_by_year$bp_control_140_90[i] * 100),
                                  CI_95 = sprintf("(%.1f-%.1f)", bp_by_year$ci_l[i] * 100, bp_by_year$ci_u[i] * 100)))
}

# By age category
bp_by_age <- svyby(~bp_control_140_90, ~demo_age_cat, svy, svymean, vartype = "ci")
for (i in seq_len(nrow(bp_by_age))) {
  table2_data <- rbind(table2_data,
                       data.frame(Predictor = ifelse(i == 1, "Age Category", ""),
                                  Category = as.character(bp_by_age$demo_age_cat[i]),
                                  BP_Control_Rate = sprintf("%.1f%%", bp_by_age$bp_control_140_90[i] * 100),
                                  CI_95 = sprintf("(%.1f-%.1f)", bp_by_age$ci_l[i] * 100, bp_by_age$ci_u[i] * 100)))
}

# By gender
bp_by_gender <- svyby(~bp_control_140_90, ~demo_gender, svy, svymean, vartype = "ci")
for (i in seq_len(nrow(bp_by_gender))) {
  table2_data <- rbind(table2_data,
                       data.frame(Predictor = ifelse(i == 1, "Gender", ""),
                                  Category = as.character(bp_by_gender$demo_gender[i]),
                                  BP_Control_Rate = sprintf("%.1f%%", bp_by_gender$bp_control_140_90[i] * 100),
                                  CI_95 = sprintf("(%.1f-%.1f)", bp_by_gender$ci_l[i] * 100, bp_by_gender$ci_u[i] * 100)))
}

# By race
bp_by_race <- svyby(~bp_control_140_90, ~demo_race, svy, svymean, vartype = "ci")
for (i in seq_len(nrow(bp_by_race))) {
  table2_data <- rbind(table2_data,
                       data.frame(Predictor = ifelse(i == 1, "Race/Ethnicity", ""),
                                  Category = as.character(bp_by_race$demo_race[i]),
                                  BP_Control_Rate = sprintf("%.1f%%", bp_by_race$bp_control_140_90[i] * 100),
                                  CI_95 = sprintf("(%.1f-%.1f)", bp_by_race$ci_l[i] * 100, bp_by_race$ci_u[i] * 100)))
}

# By diabetes
bp_by_diabetes <- svyby(~bp_control_140_90, ~cc_diabetes, svy, svymean, vartype = "ci")
for (i in seq_len(nrow(bp_by_diabetes))) {
  table2_data <- rbind(table2_data,
                       data.frame(Predictor = ifelse(i == 1, "Diabetes", ""),
                                  Category = as.character(bp_by_diabetes$cc_diabetes[i]),
                                  BP_Control_Rate = sprintf("%.1f%%", bp_by_diabetes$bp_control_140_90[i] * 100),
                                  CI_95 = sprintf("(%.1f-%.1f)", bp_by_diabetes$ci_l[i] * 100, bp_by_diabetes$ci_u[i] * 100)))
}

# By medication use
bp_by_med_use <- svyby(~bp_control_140_90, ~bp_med_use, svy, svymean, vartype = "ci")
for (i in seq_len(nrow(bp_by_med_use))) {
  table2_data <- rbind(table2_data,
                       data.frame(Predictor = ifelse(i == 1, "BP Medication Use", ""),
                                  Category = as.character(bp_by_med_use$bp_med_use[i]),
                                  BP_Control_Rate = sprintf("%.1f%%", bp_by_med_use$bp_control_140_90[i] * 100),
                                  CI_95 = sprintf("(%.1f-%.1f)", bp_by_med_use$ci_l[i] * 100, bp_by_med_use$ci_u[i] * 100)))
}

print(table2_data, row.names = FALSE)
cat("\n")

# Save Table 2 as image
table2_grob <- tableGrob(table2_data, 
                         theme = ttheme_default(
                           core = list(
                             fg_params = list(fontsize = 9, hjust = 0, x = 0.05),
                             bg_params = list(fill = "white", col = "white")
                           ),
                           colhead = list(
                             fg_params = list(fontsize = 10, fontface = "bold", hjust = 0, x = 0.05),
                             bg_params = list(fill = "white", col = "white")
                           ),
                           rowhead = list(
                             bg_params = list(fill = "white", col = "white")
                           ),
                           base_colour = "black",
                           base_size = 9,
                           padding = unit(c(4, 4), "mm")
                         ))

# Create title as a proper grob with padding
title2 <- textGrob("Table 2: BP Control Rates by Key Predictors (Survey-Weighted)", 
                   gp = gpar(fontsize = 14, fontface = "bold"),
                   x = unit(0.05, "npc"),
                   hjust = 0,
                   vjust = 0.5)

# Use grid.arrange for better layout control
png("./figure/table2_bp_control_by_predictors.png", 
    width = 14, height = 14, units = "in", res = 300, bg = "white")
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1, 
                                           heights = unit.c(unit(0.8, "in"), unit(1, "null")),
                                           widths = unit(1, "npc"))))
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.draw(title2)
popViewport()
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.draw(table2_grob)
popViewport(2)
dev.off()
cat("Saved: ./figure/table2_bp_control_by_predictors.png\n\n")

# FIGURE 1: BP Control Trend Over Time
cat("FIGURE 1: BP Control Trend Over Time\n")

bp_trend <- svyby(~bp_control_140_90, ~svy_year, svy, svymean, vartype = "ci")
bp_trend <- bp_trend %>%
  rename(mean = bp_control_140_90)

p1 <- ggplot(bp_trend, aes(x = svy_year, y = mean)) +
  geom_point(size = 3) +
  geom_line(group = 1, linewidth = 1) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.1, linewidth = 0.8) +
  theme_minimal() +
  labs(
    x = "Survey Cycle",
    y = "BP Control Rate (Proportion)",
    title = "Survey-Weighted Trend in BP Control Over Time"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("./figure/figure1_bp_control_trend.png", p1, width = 10, height = 6, dpi = 300)
cat("Saved: ./figure/figure1_bp_control_trend.png\n\n")

# FIGURE 2: BP Control by Race/Ethnicity
cat("FIGURE 2: BP Control by Race/Ethnicity\n")

bp_by_race_viz <- svyby(~bp_control_140_90, ~demo_race, svy, svymean, vartype = "ci")
bp_by_race_viz <- bp_by_race_viz %>%
  rename(mean = bp_control_140_90)

p2 <- ggplot(bp_by_race_viz, aes(x = reorder(demo_race, mean), y = mean)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2, linewidth = 0.8) +
  theme_minimal() +
  labs(
    x = "Race/Ethnicity",
    y = "BP Control Rate (Proportion)",
    title = "BP Control Rate by Race/Ethnicity"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

ggsave("./figure/figure2_bp_control_by_race.png", p2, width = 10, height = 6, dpi = 300)
cat("Saved: ./figure/figure2_bp_control_by_race.png\n")

cat("EDA COMPLETE\n")
cat("2 tables and 2 figures saved to ./figure/\n")

