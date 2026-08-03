# coppock_green_2022/maintained/text_study_2_correlations.R
# Output: output/text_study_2_correlations.csv
# Depends on: helpers.R
# Description: The pairwise correlations among study 2's five outcome measures,
#   which the article describes but never tabulates: "Responses to these nontarget
#   outcomes are positively correlated among each other and with the target
#   outcome of the manipulation (correlations range from 0.3 to 0.7)". The claim
#   is about the original study 2 sample, so that is the one the range is read
#   off; the replication is written out beside it because the same five questions
#   were asked there. The deposit computes neither.

source(here::here("maintained", "helpers.R"))

study_2 <- read_rds(file.path(data_dir, "study_2_lucid_clean.rds"))
study_2_replication <- read_rds(file.path(data_dir, "studies_1_2_replications_lucid_cleaned.rds"))

outcome_levels <- c("gays_employers", "ssm_exempt", "ssm_ammend",
                    "prayer_football", "prayer_ammend")
outcome_labels <- c("Employers", "Local Officials", "SSM Amend",
                    "Prayer", "Prayer Amend")

# Pairs are taken in the order the published table lists the five outcomes, not in
# alphabetical order of the variable names.
outcome_pairs <- expand_grid(index_1 = seq_along(outcome_levels),
                             index_2 = seq_along(outcome_levels)) |>
  filter(index_1 < index_2) |>
  transmute(outcome_1 = outcome_levels[index_1], outcome_2 = outcome_levels[index_2])

pairwise_correlations <- function(data, sample_label) {
  outcome_pairs |>
    mutate(
      sample = sample_label,
      target_pair = outcome_1 == "gays_employers" | outcome_2 == "gays_employers",
      correlation = map2_dbl(outcome_1, outcome_2,
                             ~ cor(data[[.x]], data[[.y]], use = "complete.obs"))
    ) |>
    mutate(across(c(outcome_1, outcome_2),
                  ~ factor(.x, levels = outcome_levels, labels = outcome_labels))) |>
    select(sample, outcome_1, outcome_2, target_pair, correlation)
}

correlations <- bind_rows(
  pairwise_correlations(study_2, "Original"),
  pairwise_correlations(study_2_replication, "Replication")
) |>
  arrange(sample, desc(correlation), .locale = "en")

write_csv(correlations, file.path(out_dir, "text_study_2_correlations.csv"))

print(correlations, n = nrow(correlations))
