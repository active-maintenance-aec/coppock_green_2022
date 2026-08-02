# coppock_green_2022/maintained/text_significance_counts.R
# Output: output/text_significance_counts.csv
# Depends on: helpers.R, figure_4_dynamic_constraint_estimates.R output
# Description: The in-text counts of significant treatment effects in study 1:
#   "out of the 48 estimated treatment effects on target issues, 39 are
#   statistically significant at p < .05 or better. If we apply the Benjamini and
#   Hochberg (1995) multiple comparisons correction to the associated p-values,
#   35 estimated effects remain significant" and "out of 144 estimated effects on
#   nontarget issues, only nine of the effects are significant, and when we apply
#   the Benjamini-Hochberg correction, none of these estimates is deemed
#   significant". The deposited archive computes none of these; they are read off
#   the same wave 1 estimates that figure 4 plots.
#
#   The correction is applied within the target set and within the nontarget set
#   separately, which is what the two sentences describe.

source(here::here("maintained", "helpers.R"))

estimates <- read_csv(file.path(out_dir, "figure_4_dynamic_constraint_estimates.csv"),
                      show_col_types = FALSE)

counts <- estimates |>
  mutate(issue_type = if_else(target, "Target issues", "Nontarget issues")) |>
  group_by(issue_type) |>
  mutate(p_bh = p.adjust(p.value, method = "BH")) |>
  summarize(
    n_estimates = n(),
    n_significant = sum(p.value < 0.05),
    n_significant_bh = sum(p_bh < 0.05),
    .groups = "drop"
  )

write_csv(counts, file.path(out_dir, "text_significance_counts.csv"))

print(counts)
