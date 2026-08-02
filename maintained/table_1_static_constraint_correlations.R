# coppock_green_2022/maintained/table_1_static_constraint_correlations.R
# Output: output/table_1_static_constraint_correlations.csv, .tex
# Depends on: helpers.R
# Description: Table 1, average within- and across-domain wave 1 correlations in
#   each sample. The archive averaged over all 240 ordered off-diagonal pairs of
#   the 16 by 16 correlation matrix; this averages over the 120 unordered pairs
#   the paper describes ("16 choose 2 = 120 pairs"). The correlation matrix is
#   symmetric, so the two averages are identical, and the pair count is written
#   to the CSV so the text's 120 traces to data.

source(here::here("maintained", "helpers.R"))

dv_df <- read_dv_df()

study_1_mturk <- read_study_1("study_1_mturk_cleaned.rds")
study_1_elite <- read_study_1("study_1_elite_cleaned.rds")
study_1_lucid <- read_study_1("studies_1_2_replications_lucid_cleaned.rds")

pairs_df <- expand_grid(
  var_1 = paste0(dv_df$dvs, "_w1"),
  var_2 = paste0(dv_df$dvs, "_w1")
) |>
  filter(var_1 < var_2) |>
  left_join(transmute(dv_df, var_1 = paste0(dvs, "_w1"), cat_1 = dv_cat), by = "var_1") |>
  left_join(transmute(dv_df, var_2 = paste0(dvs, "_w1"), cat_2 = dv_cat), by = "var_2") |>
  mutate(same_domain = if_else(cat_1 == cat_2, "within_domain", "across_domain"))

avg_cors <- function(data, sample_label) {
  pairs_df |>
    mutate(
      sample = sample_label,
      cor = map2_dbl(var_1, var_2, ~ cor(data[[.x]], data[[.y]], use = "complete.obs"))
    ) |>
    group_by(sample, same_domain) |>
    summarize(avg_cor = mean(cor), n_pairs = n(), .groups = "drop")
}

tab_1 <- bind_rows(
  avg_cors(study_1_mturk, "MTurk sample"),
  avg_cors(study_1_elite, "Elite sample"),
  avg_cors(study_1_lucid, "Lucid sample")
) |>
  pivot_wider(names_from = same_domain, values_from = c(avg_cor, n_pairs)) |>
  select(sample, across_domain = avg_cor_across_domain,
         within_domain = avg_cor_within_domain,
         n_pairs_across = n_pairs_across_domain,
         n_pairs_within = n_pairs_within_domain) |>
  mutate(n_pairs_total = n_pairs_across + n_pairs_within)

write_csv(tab_1, file.path(out_dir, "table_1_static_constraint_correlations.csv"))

kbl_out <- tab_1 |>
  transmute(
    Sample = sample,
    `Across Outcome Family` = sprintf("%.2f", across_domain),
    `Within Outcome Family` = sprintf("%.2f", within_domain)
  ) |>
  kable("latex", booktabs = TRUE,
        caption = "Levels of Static Constraint by Study Sample",
        label = "static_constraint") |>
  kable_styling(latex_options = "hold_position")

write_lines(kbl_out, file.path(out_dir, "table_1_static_constraint_correlations.tex"))

print(tab_1)
