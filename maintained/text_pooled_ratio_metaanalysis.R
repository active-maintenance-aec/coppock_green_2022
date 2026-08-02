# coppock_green_2022/maintained/text_pooled_ratio_metaanalysis.R
# Output: output/text_pooled_ratio_metaanalysis.csv,
#   output/text_pooled_ratio_dispersion.csv
# Depends on: helpers.R, text_precision_weighted_averages.R output
# Description: Pools the three samples' indirect-to-direct effect ratios, the
#   number the paper reports as "if we pool the three studies together, our
#   estimate is 1.4% (95% CI: -8%, 6%)". The deposited in_text_numbers.R did this
#   with rmeta::meta.summaries, which has not been updated since March 2018;
#   metafor::rma(method = "FE") is the maintained equivalent and returns the same
#   pooled estimate, standard error, and interval on the same inputs. The
#   random-effects fit is reported alongside as an addition, not a correction: it
#   shows what the fixed-effect assumption costs, and here it costs almost
#   nothing.
#
#   The three study-level ratios are fixed quantities but the three weights are
#   not: each is one over the square of an unseeded bootstrap standard deviation.
#   The pooled estimate is therefore a draw, and how wide a draw depends on how
#   many bootstrap resamples went into each standard deviation. The dispersion
#   file reports that spread at both resample counts, the 1,000 this rewrite uses
#   and the 10 the deposit ships, by resampling the saved draws.

source(here::here("maintained", "helpers.R"))

set.seed(54321)

pwa_results <- read_csv(file.path(out_dir, "text_precision_weighted_averages.csv"),
                        show_col_types = FALSE)

draws <- read_csv(file.path(out_dir, "text_precision_weighted_averages_draws.csv"),
                  show_col_types = FALSE)

sample_order <- c("MTurk", "Elite", "Lucid")

ratios <- pwa_results |>
  filter(quantity == "ratio") |>
  arrange(factor(sample, levels = sample_order))

fit_fe <- rma(yi = ratios$estimate, sei = ratios$boot_sd, method = "FE")
fit_reml <- rma(yi = ratios$estimate, sei = ratios$boot_sd, method = "REML")

pooled <- bind_rows(
  tibble(model = "Fixed effects", estimate = fit_fe$beta[1], std.error = fit_fe$se,
         conf_low = fit_fe$ci.lb, conf_high = fit_fe$ci.ub,
         p.value = fit_fe$pval, tau2 = 0,
         q_stat = fit_fe$QE, q_df = fit_fe$k - 1, q_p = fit_fe$QEp),
  tibble(model = "Random effects (REML)", estimate = fit_reml$beta[1],
         std.error = fit_reml$se, conf_low = fit_reml$ci.lb, conf_high = fit_reml$ci.ub,
         p.value = fit_reml$pval, tau2 = fit_reml$tau2,
         q_stat = fit_reml$QE, q_df = fit_reml$k - 1, q_p = fit_reml$QEp)
) |>
  mutate(k = nrow(ratios), .after = model)

write_csv(pooled, file.path(out_dir, "text_pooled_ratio_metaanalysis.csv"))

# How much the pooled estimate moves from one unseeded run to the next ----
# The study-level ratios are held at their point estimates; only the three
# bootstrap standard deviations are redrawn, because that is the only thing the
# absent seed touches.
ratio_draws <- draws |>
  filter(quantity == "ratio") |>
  group_by(sample) |>
  summarize(pool = list(draw), .groups = "drop") |>
  arrange(factor(sample, levels = sample_order))

mc_reps <- 4000

pooled_draw <- function(size) {
  sei <- map_dbl(ratio_draws$pool, ~ sd(sample(.x, size, replace = TRUE)))
  fit <- rma(yi = ratios$estimate, sei = sei, method = "FE")
  tibble(estimate = as.numeric(fit$beta), conf_low = fit$ci.lb, conf_high = fit$ci.ub)
}

dispersion <- c(10, 1000) |>
  set_names() |>
  map(function(size) {
    map(seq_len(mc_reps), ~ pooled_draw(size)) |>
      bind_rows() |>
      pivot_longer(everything(), names_to = "quantity", values_to = "replicated") |>
      group_by(quantity) |>
      summarize(
        mc_reps = mc_reps,
        mean = mean(replicated),
        mc_low = quantile(replicated, 0.025),
        mc_high = quantile(replicated, 0.975),
        share_positive = mean(replicated > 0),
        .groups = "drop"
      )
  }) |>
  bind_rows(.id = "sims_setting") |>
  mutate(sims_setting = as.integer(sims_setting), .before = 1)

write_csv(dispersion, file.path(out_dir, "text_pooled_ratio_dispersion.csv"))

print(ratios)
print(pooled)
print(dispersion)
