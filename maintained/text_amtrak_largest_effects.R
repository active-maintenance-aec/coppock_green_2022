# coppock_green_2022/maintained/text_amtrak_largest_effects.R
# Output: output/text_amtrak_largest_effects.csv
# Depends on: helpers.R, figure_4_dynamic_constraint_estimates.R output
# Description: The two in-text effect sizes singled out in the study 1 results:
#   "treatment effects on the third question in the Amtrak set of outcomes
#   (regarding how subjects expect new taxes to be spent) were especially large,
#   at 0.52 standard deviations among elites and 0.73 standard deviations on
#   MTurk". Written out with every Amtrak-on-Amtrak estimate so the two quoted
#   values can be located in context rather than taken on trust.

source(here::here("maintained", "helpers.R"))

estimates <- read_csv(file.path(out_dir, "figure_4_dynamic_constraint_estimates.csv"),
                      show_col_types = FALSE)

amtrak <- estimates |>
  filter(treatment == "Treatment: Amtrak", dv_cat_label == "Outcomes: Amtrak") |>
  select(Study, dv_labels, outcome, estimate, std.error, p.value,
         conf.low, conf.high) |>
  arrange(dv_labels, Study)

write_csv(amtrak, file.path(out_dir, "text_amtrak_largest_effects.csv"))

print(amtrak, n = nrow(amtrak))
