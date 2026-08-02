# coppock_green_2022/maintained/table_4_correction_experiment.R
# Output: output/table_4_correction_experiment.csv, .tex
# Depends on: helpers.R
# Description: Table 4, the consistency correction experiment, run among the
#   respondents whose stated preference for the size of government conflicts
#   with their stated preference for education spending, split by which size of
#   government they prefer. gov_size is numeric in the original sample (1 =
#   smaller, 2 = larger) and character in the replication. The published table
#   labels the top panel "N (MTurk)", but it is computed from the same study 2
#   sample that table 2 labels "N (Lucid original)"; the sample labels here
#   follow the data, and the discrepancy is documented in the report.

source(here::here("maintained", "helpers.R"))

study_2 <- read_rds(file.path(data_dir, "study_2_lucid_clean.rds"))
study_2_replication <- read_rds(file.path(data_dir, "studies_1_2_replications_lucid_cleaned.rds"))

fit_correction <- function(data, sample_label, gov_pref) {
  fit <- lm_robust(spend_outcomes_average ~ Z_correction, data = data)
  tidy(fit) |>
    mutate(
      sample = sample_label,
      gov_pref = gov_pref,
      term = if_else(term == "(Intercept)", "Constant (control mean)",
                     "Correction treatment"),
      n = fit$nobs
    )
}

tab_4 <- bind_rows(
  fit_correction(filter(study_2, trap_set == 1, gov_size == 1),
                 "Original", "Prefer small government"),
  fit_correction(filter(study_2, trap_set == 1, gov_size == 2),
                 "Original", "Prefer large government"),
  fit_correction(filter(study_2_replication, trap_set == 1,
                        gov_size == "Smaller government with fewer services"),
                 "Replication", "Prefer small government"),
  fit_correction(filter(study_2_replication, trap_set == 1,
                        gov_size == "Larger government with more services"),
                 "Replication", "Prefer large government")
) |>
  as_tibble() |>
  select(sample, gov_pref, term, estimate, std.error, p.value, conf.low, conf.high, n)

write_csv(tab_4, file.path(out_dir, "table_4_correction_experiment.csv"))

kbl_out <- tab_4 |>
  mutate(cell = est_se_cell(estimate, std.error, p.value)) |>
  select(sample, term, gov_pref, cell) |>
  pivot_wider(names_from = gov_pref, values_from = cell) |>
  kable("latex", booktabs = TRUE,
        caption = "Correction Experiment",
        label = "correction") |>
  kable_styling(latex_options = "hold_position")

write_lines(kbl_out, file.path(out_dir, "table_4_correction_experiment.tex"))

print(tab_4, n = nrow(tab_4))
