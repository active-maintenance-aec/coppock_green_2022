# coppock_green_2022/maintained/figure_b3_wave3_estimates.R
# Output: output/figure_b3_wave3_estimates.pdf, .png, .csv
# Depends on: helpers.R
# Description: Appendix figure B3, wave 3 treatment effects (MTurk sample only;
#   only MTurk respondents were reinterviewed a third time).

source(here::here("maintained", "helpers.R"))

dv_df <- read_dv_df()

study_1_mturk <- read_study_1("study_1_mturk_cleaned.rds")

gg_df <- bind_rows(
  `MTurk Sample` = fit_all_outcomes(study_1_mturk, dv_df, "w3"),
  .id = "Study"
) |>
  mutate(Study = factor(Study, levels = "MTurk Sample"))

g <- constraint_plot(gg_df, colors = bpr_colors[1], shapes = 16)

ggsave(file.path(out_dir, "figure_b3_wave3_estimates.pdf"),
       plot = g, height = 5, width = 6.5)
ggsave(file.path(out_dir, "figure_b3_wave3_estimates.png"),
       plot = g, height = 5, width = 6.5, dpi = 300)

gg_df |>
  select(Study, treatment, dv_cat_label, dv_labels, outcome, term, target,
         estimate, std.error, p.value, conf.low, conf.high, df) |>
  write_csv(file.path(out_dir, "figure_b3_wave3_estimates.csv"))
