# coppock_green_2022/maintained/text_precision_weighted_averages.R
# Output: output/text_precision_weighted_averages.csv,
#   output/text_precision_weighted_averages_draws.csv,
#   output/text_precision_weighted_averages_endpoint_ranges.csv
# Depends on: helpers.R
# Description: The in-text precision-weighted average treatment effects on target
#   and nontarget outcomes in each study 1 sample, the ratio of the two, and
#   respondent-level bootstrap uncertainty for all three. This is the slow step,
#   roughly six minutes: 1,000 resamples by three samples by 16 outcomes.
#
#   Two departures from the deposited in_text_numbers.R. The archive ships
#   sims <- 10 uncommented with sims <- 1000 commented out one line above; the
#   rewrite uses 1,000. The archive also asks for the 0.9755 quantile where 0.975
#   was meant; the rewrite uses 0.975 and writes the archive's quantile alongside
#   so the size of that typo is measured rather than asserted.
#
#   Neither the archive nor the article sets a seed, so no published interval
#   endpoint in this paper is a fixed quantity: it is one draw of a bootstrap
#   quantile. Each endpoint is therefore reported with the range it takes across
#   resamples of the draws, at both resample counts: 1,000, which this script
#   uses, and 10, which is what the deposit ships. That range is what a published
#   endpoint has to fall inside to count as reproduced, and the two counts give
#   very different ranges, so which setting produced the published intervals is a
#   question the ranges answer rather than one to be assumed.
#
#   The individual bootstrap draws are written out because the pooled
#   meta-analysis in text_pooled_ratio_metaanalysis.R needs them to do the same
#   thing for the pooled estimate.

source(here::here("maintained", "helpers.R"))

set.seed(12345)

sims <- 1000

# The resample count the deposited in_text_numbers.R ships.
sims_archive <- 10

dv_df <- read_dv_df()

samples <- list(
  MTurk = read_study_1("study_1_mturk_cleaned.rds"),
  Elite = read_study_1("study_1_elite_cleaned.rds"),
  Lucid = read_study_1("studies_1_2_replications_lucid_cleaned.rds")
)

estimates <- samples |>
  map(~ pwa(.x, dv_df)) |>
  bind_rows(.id = "sample")

boot_draws <- samples |>
  map(function(data) {
    bootstraps(data, times = sims)$splits |>
      map(~ pwa(rsample::analysis(.x), dv_df)) |>
      bind_rows()
  }) |>
  bind_rows(.id = "sample")

boot_long <- boot_draws |>
  pivot_longer(c(`Across Domain`, `Within Domain`, ratio),
               names_to = "quantity", values_to = "draw")

write_csv(boot_long, file.path(out_dir, "text_precision_weighted_averages_draws.csv"))

# How far a bootstrap interval endpoint moves from one unseeded run to the next,
# measured by resampling the draws and recomputing the endpoint. Each endpoint
# gets mc_reps resampled values; the 2.5th and 97.5th percentiles of those are
# the range a published endpoint has to fall inside to be reproduced. A resample
# of size 10 drawn from an independent pool is distributed as 10 fresh bootstrap
# draws, so the sims = 10 rows describe what the deposited script produces.
mc_reps <- 2000

endpoint_ranges <- expand_grid(
  distinct(boot_long, sample, quantity),
  sims_setting = c(sims_archive, sims),
  endpoint = c("conf_low", "conf_high")
) |>
  mutate(
    prob = if_else(endpoint == "conf_low", 0.025, 0.975),
    replicated = pmap(
      list(sample, quantity, sims_setting, prob),
      function(s, q, n, p) {
        pool <- boot_long$draw[boot_long$sample == s & boot_long$quantity == q]
        map_dbl(seq_len(mc_reps), ~ quantile(sample(pool, n, replace = TRUE), p))
      }
    ),
    mc_low = map_dbl(replicated, ~ quantile(.x, 0.025)),
    mc_high = map_dbl(replicated, ~ quantile(.x, 0.975))
  ) |>
  select(sample, quantity, sims_setting, endpoint, mc_low, mc_high)

write_csv(endpoint_ranges, file.path(out_dir, "text_precision_weighted_averages_endpoint_ranges.csv"))

boot_summary <- boot_long |>
  group_by(sample, quantity) |>
  summarize(
    boot_sd = sd(draw),
    conf_low = quantile(draw, 0.025),
    conf_high = quantile(draw, 0.975),
    conf_high_archive_quantile = quantile(draw, 0.9755),
    .groups = "drop"
  )

pwa_results <- estimates |>
  pivot_longer(c(`Across Domain`, `Within Domain`, ratio),
               names_to = "quantity", values_to = "estimate") |>
  left_join(boot_summary, by = c("sample", "quantity")) |>
  mutate(
    sample = factor(sample, levels = c("MTurk", "Elite", "Lucid")),
    quantity = factor(quantity, levels = c("Within Domain", "Across Domain", "ratio"))
  ) |>
  arrange(sample, quantity)

write_csv(pwa_results, file.path(out_dir, "text_precision_weighted_averages.csv"))

print(pwa_results, n = nrow(pwa_results))
