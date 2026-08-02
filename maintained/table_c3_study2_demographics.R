# coppock_green_2022/maintained/table_c3_study2_demographics.R
# Output: output/table_c3_study2_demographics.csv, .tex
# Depends on: helpers.R
# Description: Appendix table C3, study 2 sample demographics as column
#   percentages for the original sample and its replication.

source(here::here("maintained", "helpers.R"))

study_2 <- read_rds(file.path(data_dir, "study_2_lucid_clean.rds"))
study_2_replication <- read_rds(file.path(data_dir, "studies_1_2_replications_lucid_cleaned.rds"))

demographics <- c("age_5", "educ_5", "pid_7", "female")

study_2_stacked <- bind_rows(
  Original = select(study_2, all_of(demographics)),
  Replication = select(study_2_replication, all_of(demographics)),
  .id = "sample"
) |>
  mutate(
    sample = factor(sample, levels = c("Original", "Replication")),
    pid_7 = factor(pid_7, levels = 1:7, labels = pid_7_labels),
    female = factor(female, levels = c(0, 1), labels = c("Male", "Female"))
  )

n_tab <- study_2_stacked |>
  mutate(N = "N") |>
  tabyl(N, sample) |>
  rename(category = N) |>
  mutate(variable = "n", across(c(Original, Replication), as.character))

tab_c3 <- bind_rows(
  pct_by_sample(study_2_stacked, "age_5", "age"),
  pct_by_sample(study_2_stacked, "educ_5", "education"),
  pct_by_sample(study_2_stacked, "pid_7", "party_id"),
  pct_by_sample(study_2_stacked, "female", "gender"),
  n_tab
) |>
  select(variable, category, Original, Replication)

write_csv(tab_c3, file.path(out_dir, "table_c3_study2_demographics.csv"))

kbl_out <- tab_c3 |>
  kable("latex", booktabs = TRUE,
        caption = "Study 2 Sample Demographics",
        label = "study2_demo") |>
  kable_styling(latex_options = c("hold_position", "scale_down"))

write_lines(kbl_out, file.path(out_dir, "table_c3_study2_demographics.tex"))

print(tab_c3, n = nrow(tab_c3))
