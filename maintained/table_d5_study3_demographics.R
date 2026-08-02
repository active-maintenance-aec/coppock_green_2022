# coppock_green_2022/maintained/table_d5_study3_demographics.R
# Output: output/table_d5_study3_demographics.csv, .tex
# Depends on: helpers.R
# Description: Appendix table D5, study 3 sample demographics as column
#   percentages. The seven-point party identification variable is named pid_7n
#   in the Trump and White (2018) data and pid_7 in the replication.

source(here::here("maintained", "helpers.R"))

original <- read_rds(file.path(data_dir, "original_trump_white_clean.rds"))
replication <- read_rds(file.path(data_dir, "replication_trump_white_clean.rds"))

study_3_stacked <- bind_rows(
  Original = select(original, age_5, educ_5, race_4, pid_7 = pid_7n, female),
  Replication = select(replication, age_5, educ_5, race_4, pid_7, female),
  .id = "sample"
) |>
  mutate(
    sample = factor(sample, levels = c("Original", "Replication")),
    pid_7 = factor(pid_7, levels = 1:7, labels = pid_7_labels),
    female = factor(female, levels = c(0, 1), labels = c("Male", "Female"))
  )

n_tab <- study_3_stacked |>
  mutate(N = "N") |>
  tabyl(N, sample) |>
  rename(category = N) |>
  mutate(variable = "n", across(c(Original, Replication), as.character))

tab_d5 <- bind_rows(
  pct_by_sample(study_3_stacked, "age_5", "age"),
  pct_by_sample(study_3_stacked, "educ_5", "education"),
  pct_by_sample(study_3_stacked, "race_4", "race"),
  pct_by_sample(study_3_stacked, "pid_7", "party_id"),
  pct_by_sample(study_3_stacked, "female", "gender"),
  n_tab
) |>
  select(variable, category, Original, Replication)

write_csv(tab_d5, file.path(out_dir, "table_d5_study3_demographics.csv"))

kbl_out <- tab_d5 |>
  kable("latex", booktabs = TRUE,
        caption = "Study 3 Sample Demographics",
        label = "study3_demo") |>
  kable_styling(latex_options = c("hold_position", "scale_down"))

write_lines(kbl_out, file.path(out_dir, "table_d5_study3_demographics.tex"))

print(tab_d5, n = nrow(tab_d5))
