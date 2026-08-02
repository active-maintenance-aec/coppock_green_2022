# coppock_green_2022/maintained/table_2_employment_discrimination.R
# Output: output/table_2_employment_discrimination.csv, .tex
# Depends on: helpers.R
# Description: Table 2, the study 2 religious exemption experiment in the
#   original sample and its replication. N is the regression sample size, which
#   is what the published table reports and which differs per outcome; counting
#   non-missing outcome values instead would ignore missingness on the treatment
#   indicator. Replaces the archive's stargazer plus starprep table.

source(here::here("maintained", "helpers.R"))

study_2 <- read_rds(file.path(data_dir, "study_2_lucid_clean.rds"))
study_2_replication <- read_rds(file.path(data_dir, "studies_1_2_replications_lucid_cleaned.rds"))

outcome_levels <- c("gays_employers", "ssm_exempt", "ssm_ammend",
                    "prayer_football", "prayer_ammend")
outcome_labels <- c("Employers", "Local Officials", "SSM Amend",
                    "Prayer", "Prayer Amend")

# Z_gays is numeric 0/1 in both samples, so the coefficient is named Z_gays.
fit_study_2 <- function(data, sample_label) {
  outcome_levels |>
    set_names() |>
    map(~ lm_robust(reformulate("Z_gays", .x), data = data)) |>
    map(~ tidy(.x) |> mutate(n = .x$nobs)) |>
    bind_rows(.id = "outcome") |>
    mutate(sample = sample_label)
}

tab_2 <- bind_rows(
  fit_study_2(study_2, "Original"),
  fit_study_2(study_2_replication, "Replication")
) |>
  mutate(
    outcome = factor(outcome, levels = outcome_levels, labels = outcome_labels),
    term = if_else(term == "(Intercept)", "Constant (control mean)", "Religious exemption")
  ) |>
  as_tibble() |>
  select(sample, outcome, term, estimate, std.error, p.value, conf.low, conf.high, n)

write_csv(tab_2, file.path(out_dir, "table_2_employment_discrimination.csv"))

kbl_out <- tab_2 |>
  mutate(cell = est_se_cell(estimate, std.error, p.value)) |>
  select(sample, term, outcome, cell) |>
  pivot_wider(names_from = outcome, values_from = cell) |>
  kable("latex", booktabs = TRUE,
        caption = "Employment Discrimination Experimental Results",
        label = "employment") |>
  kable_styling(latex_options = "hold_position")

write_lines(kbl_out, file.path(out_dir, "table_2_employment_discrimination.tex"))

print(tab_2, n = nrow(tab_2))
