# coppock_green_2022/maintained/table_c4_welfare_experiment.R
# Output: output/table_c4_welfare_experiment.csv, .tex
# Depends on: helpers.R
# Description: Appendix table C4, the welfare wording experiment in study 2 and
#   its replication. Z_welfare is a character 0/1 variable in both samples, so
#   the treatment coefficient is named Z_welfare1.

source(here::here("maintained", "helpers.R"))

study_2 <- read_rds(file.path(data_dir, "study_2_lucid_clean.rds"))
study_2_replication <- read_rds(file.path(data_dir, "studies_1_2_replications_lucid_cleaned.rds"))

outcome_levels <- c("welfare_good", "gov_reduce_ineq", "medicaid_good")
outcome_labels <- c("Too Little on Welfare", "Gov Should Reduce Ineq",
                    "Medicaid a Good Thing")

fit_welfare <- function(data, sample_label) {
  outcome_levels |>
    set_names() |>
    map(~ lm_robust(reformulate("Z_welfare", .x), data = data)) |>
    map(~ tidy(.x) |> mutate(n = .x$nobs)) |>
    bind_rows(.id = "outcome") |>
    mutate(sample = sample_label)
}

tab_c4 <- bind_rows(
  fit_welfare(study_2, "Original"),
  fit_welfare(study_2_replication, "Replication")
) |>
  mutate(
    outcome = factor(outcome, levels = outcome_levels, labels = outcome_labels),
    term = if_else(term == "(Intercept)", "Constant (control mean)",
                   "Welfare wording")
  ) |>
  as_tibble() |>
  select(sample, outcome, term, estimate, std.error, p.value, conf.low, conf.high, n)

write_csv(tab_c4, file.path(out_dir, "table_c4_welfare_experiment.csv"))

kbl_out <- tab_c4 |>
  mutate(cell = est_se_cell(estimate, std.error, p.value)) |>
  select(sample, term, outcome, cell) |>
  pivot_wider(names_from = outcome, values_from = cell) |>
  kable("latex", booktabs = TRUE,
        caption = "Welfare Wording Experiment",
        label = "welfare") |>
  kable_styling(latex_options = "hold_position")

write_lines(kbl_out, file.path(out_dir, "table_c4_welfare_experiment.tex"))

print(tab_c4, n = nrow(tab_c4))
