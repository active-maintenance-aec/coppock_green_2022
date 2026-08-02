# coppock_green_2022/maintained/text_inconsistency_rates.R
# Output: output/text_inconsistency_rates.csv
# Depends on: helpers.R
# Description: The share of respondents whose stated preference for the size of
#   government conflicts with their stated preference for education spending, the
#   subgroup the study 4 correction experiment was run among: "Among the MTurk
#   sample, 30% of the respondents were inconsistent; this figure was 24% on
#   Lucid". The deposited archive computes neither figure. trap_set is the
#   indicator the cleaning scripts build for that inconsistency.

source(here::here("maintained", "helpers.R"))

study_2 <- read_rds(file.path(data_dir, "study_2_lucid_clean.rds"))
study_2_replication <- read_rds(file.path(data_dir, "studies_1_2_replications_lucid_cleaned.rds"))

inconsistency_rate <- function(data, sample_label) {
  tibble(
    sample = sample_label,
    n_respondents = nrow(data),
    n_measured = sum(!is.na(data$trap_set)),
    n_inconsistent = sum(data$trap_set == 1, na.rm = TRUE),
    share_inconsistent = mean(data$trap_set == 1, na.rm = TRUE)
  )
}

rates <- bind_rows(
  inconsistency_rate(study_2, "Original"),
  inconsistency_rate(study_2_replication, "Replication")
)

write_csv(rates, file.path(out_dir, "text_inconsistency_rates.csv"))

print(rates)
