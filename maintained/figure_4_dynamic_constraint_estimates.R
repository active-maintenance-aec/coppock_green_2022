# coppock_green_2022/maintained/figure_4_dynamic_constraint_estimates.R
# Output: output/figure_4_dynamic_constraint_estimates.pdf, .png, .csv
# Depends on: helpers.R
# Description: Figure 4, wave 1 treatment effects on all 16 outcomes in three
#   samples. The CSV of plotted estimates is also the input to
#   text_significance_counts.R and text_amtrak_largest_effects.R, so the in-text
#   counts and the figure cannot disagree. Replaces the archive's position_dodgev
#   (ggstance) with position_dodge and geom_errorbarh with geom_linerange.

source(here::here("maintained", "helpers.R"))

dv_df <- read_dv_df()

study_1_mturk <- read_study_1("study_1_mturk_cleaned.rds")
study_1_elite <- read_study_1("study_1_elite_cleaned.rds")
study_1_lucid <- read_study_1("studies_1_2_replications_lucid_cleaned.rds")

gg_df <- bind_rows(
  `MTurk Sample` = fit_all_outcomes(study_1_mturk, dv_df, "w1"),
  `Elite Sample` = fit_all_outcomes(study_1_elite, dv_df, "w1"),
  `Lucid Sample` = fit_all_outcomes(study_1_lucid, dv_df, "w1"),
  .id = "Study"
) |>
  mutate(Study = factor(Study, levels = c("MTurk Sample", "Elite Sample", "Lucid Sample")))

g <- constraint_plot(gg_df, colors = bpr_colors, shapes = c(16, 17, 15))

ggsave(file.path(out_dir, "figure_4_dynamic_constraint_estimates.pdf"),
       plot = g, height = 5, width = 6.5)
ggsave(file.path(out_dir, "figure_4_dynamic_constraint_estimates.png"),
       plot = g, height = 5, width = 6.5, dpi = 300)

gg_df |>
  select(Study, treatment, dv_cat_label, dv_labels, outcome, term, target,
         estimate, std.error, p.value, conf.low, conf.high, df) |>
  write_csv(file.path(out_dir, "figure_4_dynamic_constraint_estimates.csv"))
