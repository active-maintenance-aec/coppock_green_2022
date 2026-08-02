# coppock_green_2022/maintained/table_3_inequality.R
# Output: output/table_3_inequality.csv, .tex
# Depends on: helpers.R
# Description: Table 3, the income inequality experiment: a reanalysis of Trump
#   and White (2018) in the top panel and the Lucid replication, which adds a
#   pure control arm, in the bottom panel. N is the regression sample size. In
#   the original study only a random third of respondents were asked the
#   economic system justification question, so its N is much smaller than the
#   manipulation check's.

source(here::here("maintained", "helpers.R"))

original <- read_rds(file.path(data_dir, "original_trump_white_clean.rds"))

# The replication's assignment variable is character; control is the reference
# category, as in the published table.
replication <- read_rds(file.path(data_dir, "replication_trump_white_clean.rds")) |>
  mutate(tw_Z = factor(tw_Z, levels = c("control", "treatment", "pure_control")))

outcome_levels <- c("tw_manip_1", "ESJ_s")
outcome_labels <- c("Belief in Inequality (Binary)",
                    "Econ. System Justification (Standard Units)")

fit_study_3 <- function(data, sample_label) {
  outcome_levels |>
    set_names() |>
    map(~ lm_robust(reformulate("tw_Z", .x), data = data)) |>
    map(~ tidy(.x) |> mutate(n = .x$nobs)) |>
    bind_rows(.id = "outcome") |>
    mutate(sample = sample_label)
}

tab_3 <- bind_rows(
  fit_study_3(original, "Original"),
  fit_study_3(replication, "Replication")
) |>
  mutate(
    outcome = factor(outcome, levels = outcome_levels, labels = outcome_labels),
    term = case_when(
      term == "(Intercept)" ~ "Constant (control mean)",
      term == "tw_Ztreatment" ~ "Inequality treatment",
      term == "tw_Zpure_control" ~ "Pure control"
    )
  ) |>
  as_tibble() |>
  select(sample, outcome, term, estimate, std.error, p.value, conf.low, conf.high, n)

write_csv(tab_3, file.path(out_dir, "table_3_inequality.csv"))

kbl_out <- tab_3 |>
  mutate(cell = est_se_cell(estimate, std.error, p.value)) |>
  select(sample, term, outcome, cell) |>
  pivot_wider(names_from = outcome, values_from = cell) |>
  kable("latex", booktabs = TRUE,
        caption = "Inequality Experimental Results",
        label = "inequality") |>
  kable_styling(latex_options = "hold_position")

write_lines(kbl_out, file.path(out_dir, "table_3_inequality.tex"))

print(tab_3, n = nrow(tab_3))
