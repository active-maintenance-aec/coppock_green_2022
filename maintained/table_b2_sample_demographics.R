# coppock_green_2022/maintained/table_b2_sample_demographics.R
# Output: output/table_b2_sample_demographics.csv, .tex
# Depends on: helpers.R
# Description: Appendix table B2, study 1 sample demographics as column
#   percentages for the MTurk, elite, and Lucid samples. The elite ideology
#   column is rebuilt from the deposited wave 1 raw file, because the deposited
#   elite clean file carries a broken copy of it; see read_elite_ideology in
#   helpers.R and the errata section of the report.

source(here::here("maintained", "helpers.R"))

study_1_mturk <- read_study_1("study_1_mturk_cleaned.rds")
study_1_lucid <- read_study_1("studies_1_2_replications_lucid_cleaned.rds")

study_1_elite <- read_study_1("study_1_elite_cleaned.rds") |>
  select(-ideo_5) |>
  left_join(read_elite_ideology(), by = "subject_id")

demographics <- c("age_5", "educ_5", "race_4", "pid_7", "ideo_5", "female")

study_1_stacked <- bind_rows(
  MTurk = select(study_1_mturk, all_of(demographics)),
  Elites = select(study_1_elite, all_of(demographics)),
  Lucid = select(study_1_lucid, all_of(demographics)),
  .id = "sample"
) |>
  mutate(
    sample = factor(sample, levels = c("MTurk", "Elites", "Lucid")),
    pid_7 = factor(pid_7, levels = 1:7, labels = pid_7_labels),
    female = factor(female, levels = c(0, 1), labels = c("Male", "Female"))
  )

n_tab <- study_1_stacked |>
  mutate(N = "N") |>
  tabyl(N, sample) |>
  rename(category = N) |>
  mutate(variable = "n", across(c(MTurk, Elites, Lucid), as.character))

tab_b2 <- bind_rows(
  pct_by_sample(study_1_stacked, "age_5", "age"),
  pct_by_sample(study_1_stacked, "educ_5", "education"),
  pct_by_sample(study_1_stacked, "race_4", "race"),
  pct_by_sample(study_1_stacked, "pid_7", "party_id"),
  pct_by_sample(study_1_stacked, "ideo_5", "ideology"),
  pct_by_sample(study_1_stacked, "female", "gender"),
  n_tab
) |>
  select(variable, category, MTurk, Elites, Lucid)

write_csv(tab_b2, file.path(out_dir, "table_b2_sample_demographics.csv"))

kbl_out <- tab_b2 |>
  kable("latex", booktabs = TRUE,
        caption = "Study 1 Sample Demographics",
        label = "study1_demo") |>
  kable_styling(latex_options = c("hold_position", "scale_down"))

write_lines(kbl_out, file.path(out_dir, "table_b2_sample_demographics.tex"))

print(tab_b2, n = nrow(tab_b2))
