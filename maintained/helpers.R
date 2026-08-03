# coppock_green_2022/maintained/helpers.R
# Output: (sourced by every other script)
# Depends on: (none)
# Description: Packages, paths, and shared constants for the maintained rewrite.

library(here)
library(tidyverse)
library(estimatr)
library(rsample)
library(metafor)
library(janitor)
library(kableExtra)
library(knitr)

# Anchors a script run on its own rather than through run_all.R.
here::i_am("maintained/helpers.R")

data_dir <- here::here("original")
out_dir <- here::here("maintained", "output")

# Sample colors carried over from the deposited archive's helper_functions.R.
bpr_colors <- c("#1F3A93", "#7C2C55", "#D91E18")

# The four op-ed treatments and the four outcome families they target. Study 1
# also randomized a climate change op-ed, which the paper sets aside; every
# script that uses these drops it the same way.
treatment_levels <- c("Zpaul", "Zwallstreet", "Zveterans", "Zamtrak")
treatment_labels <- c("Treatment: Flat Tax", "Treatment: Wall Street",
                      "Treatment: Veterans", "Treatment: Amtrak")
dv_cat_levels <- c("flat", "wall", "vets", "amtrak")
dv_cat_labels <- c("Outcomes: Flat Tax", "Outcomes: Wall Street",
                   "Outcomes: Veterans", "Outcomes: Amtrak")

# Panels on the diagonal of figure 4 are the within-domain (target) cells and are
# shaded in the published figure.
highlight_df <- tibble(
  treatment = c("Treatment: Wall Street", "Treatment: Amtrak",
                "Treatment: Veterans", "Treatment: Flat Tax"),
  dv_cat_label = c("Outcomes: Wall Street", "Outcomes: Amtrak",
                   "Outcomes: Veterans", "Outcomes: Flat Tax")
)

# Study 1 analysis frames, with the climate arm dropped.
read_study_1 <- function(file) {
  read_rds(file.path(data_dir, file)) |>
    filter(Z != "climate")
}

read_dv_df <- function() {
  read_rds(file.path(data_dir, "dv_df.rds")) |>
    filter(dv_cat != "climate")
}

# The elite sample's five-category ideology, rebuilt from the deposited wave 1
# raw file. The deposited study_1_elite_cleaned.rds carries a broken version of
# this column: the cleaning script writes the recode in car's specification-string
# syntax without attaching car, so dplyr::recode reads the whole specification as
# a single positional replacement and every respondent ends up "Other" or missing.
# The intended recode is unambiguous, it is written out below, and it returns the
# published distribution exactly. Rebuilding it takes only the deposit's own files.
read_elite_ideology <- function() {
  raw <- read_csv(file.path(data_dir, "study_1_elite_wave_1_raw.csv"),
                  show_col_types = FALSE)
  raw |>
    transmute(
      subject_id,
      ideo_5 = factor(
        case_when(
          ideology %in% c(1, 9) ~ "Liberal",
          ideology == 2 ~ "Moderate",
          ideology == 3 ~ "Conservative",
          ideology == 4 ~ "Libertarian",
          .default = "Other"
        ),
        levels = c("Liberal", "Moderate", "Libertarian", "Conservative", "Other")
      )
    )
}

# The elite sample's two political engagement items, rebuilt from the deposited
# wave 1 raw file for the same reason ideo_5 is: study_1_elite_cleaning.R writes
# all three recodes in car's specification-string syntax without attaching car,
# so dplyr::recode stores the whole specification string in every row. Q55 and Q56
# are the news and political conversation frequency items; the deposit's own
# specification leaves Q56 = 5 unmapped, which is preserved here as NA rather than
# guessed at.
read_elite_engagement <- function() {
  raw <- read_csv(file.path(data_dir, "study_1_elite_wave_1_raw.csv"),
                  show_col_types = FALSE)
  raw |>
    transmute(
      subject_id,
      news_how_often = case_when(
        Q55 == 7 ~ "Several times an hour",
        Q55 == 1 ~ "Several times a day",
        Q55 == 2 ~ "Once a day",
        Q55 == 3 ~ "Several times a week",
        Q55 == 4 ~ "Several times a month",
        Q55 == 6 ~ "Never"
      ),
      talk_politics_how_often = case_when(
        Q56 == 1 ~ "Several times a day",
        Q56 == 2 ~ "Once a day",
        Q56 == 3 ~ "Several times a week",
        Q56 == 4 ~ "Several times a month",
        Q56 == 6 ~ "Never"
      )
    )
}

# Every treatment effect for one sample at one wave: 4 treatment contrasts by 16
# outcomes. Figure 4, appendix figures B2 and B3, the precision-weighted
# averages, and the in-text significance counts all read the same estimates.
fit_all_outcomes <- function(data, dv_df, wave) {
  dv_df$dvs |>
    map(~ formula(paste0(.x, "_", wave, " ~ Z"))) |>
    map(~ lm_robust(.x, data = data)) |>
    map(tidy) |>
    bind_rows() |>
    mutate(dvs = str_remove(outcome, paste0("_", wave))) |>
    filter(term != "(Intercept)") |>
    left_join(dv_df, by = "dvs") |>
    mutate(
      treatment = factor(term, levels = treatment_levels, labels = treatment_labels),
      dv_cat_label = factor(dv_cat, levels = dv_cat_levels, labels = dv_cat_labels),
      dv_labels_rev = factor(dv_labels, levels = rev(levels(dv_labels))),
      target = str_remove(as.character(dv_cat_label), "^Outcomes: ") ==
        str_remove(as.character(treatment), "^Treatment: "),
      same_cat = if_else(target, "Within Domain", "Across Domain")
    )
}

# The coefficient plot shared by figure 4 and appendix figures B2 and B3.
constraint_plot <- function(gg_df, colors, shapes) {
  ggplot(gg_df, aes(x = estimate, y = dv_labels_rev,
                    group = Study, color = Study, shape = Study)) +
    geom_rect(
      data = highlight_df,
      aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
          x = NULL, y = NULL, group = NULL, color = NULL, shape = NULL),
      fill = "grey", alpha = 0.4, show.legend = FALSE
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                   position = position_dodge(width = 0.5)) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    scale_x_continuous(breaks = c(0, 0.33, 0.67)) +
    facet_grid(dv_cat_label ~ treatment, scales = "free_y") +
    coord_cartesian(xlim = c(-0.2, 0.8)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = "bottom",
      strip.background = element_blank(),
      legend.key.width = unit(4, "lines"),
      text = element_text(size = 8)
    )
}

# Precision-weighted average treatment effect within and across domains, and the
# ratio of the two. This is the quantity the paper calls the size of the average
# indirect effect as a proportion of the average direct effect.
pwa <- function(data, dv_df) {
  fit_all_outcomes(data, dv_df, "w1") |>
    group_by(same_cat) |>
    summarize(pwa = weighted.mean(estimate, w = 1 / std.error^2), .groups = "drop") |>
    pivot_wider(names_from = same_cat, values_from = pwa) |>
    mutate(ratio = `Across Domain` / `Within Domain`)
}

# Column-percentage crosstab of one demographic against the sample column, in the
# form the appendix demographic tables use.
pct_by_sample <- function(data, var, label) {
  data |>
    tabyl(!!sym(var), sample) |>
    adorn_percentages("col") |>
    adorn_pct_formatting() |>
    rename(category = 1) |>
    mutate(variable = label)
}

pid_7_labels <- c("Strong Democrat", "Not very strong Democrat", "Lean Democrat",
                  "Independent", "Lean Republican", "Not very strong Republican",
                  "Strong Republican")

# Estimate, significance star, and standard error in one display cell.
est_se_cell <- function(estimate, std.error, p.value) {
  paste0(sprintf("%.3f", estimate), if_else(p.value < 0.05, "*", ""),
         "\n(", sprintf("%.3f", std.error), ")")
}
