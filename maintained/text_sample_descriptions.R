# coppock_green_2022/maintained/text_sample_descriptions.R
# Output: output/text_sample_descriptions.csv
# Depends on: helpers.R
# Description: The study 1 sample composition figures the article states in prose
#   rather than in a table: "A full 96% of the elite sample hold a college degree,
#   compared with 51% of the MTurk sample and 44% of the Lucid sample; 71% of the
#   elites hold a graduate degree, compared with 13% for the MTurk sample and 15%
#   for Lucid. Nearly half - 44% - of the elite sample describe themselves as
#   'strong' partisans, but only a quarter of the MTurk sample do so. Lucid is
#   more similar to the elite sample than MTurk on this dimension at 46% strong
#   partisans", and "97% say they read the news at least daily, and 64% say they
#   talk about politics with their family, friends, or coworkers at least once per
#   day". Appendix table B.2 carries the underlying education and party
#   identification distributions but none of these collapsed shares, and the two
#   elite engagement items appear nowhere in the paper's tables. The deposit
#   computes none of them.
#
#   Every share takes the sample's full N as its denominator, which is the
#   denominator appendix table B.2 uses, and each row also carries the count of
#   respondents who gave an answer at all. The two differ only for the two
#   engagement items, where the deposit's own recode leaves one response code on
#   the conversation item unlabelled, so those respondents fall out of that item's
#   answered count. The full-sample denominator is the one the article's printed
#   figures follow, and both are written out so the difference is visible.

source(here::here("maintained", "helpers.R"))

study_1_mturk <- read_study_1("study_1_mturk_cleaned.rds")
study_1_lucid <- read_study_1("studies_1_2_replications_lucid_cleaned.rds")

study_1_elite <- read_study_1("study_1_elite_cleaned.rds") |>
  select(-news_how_often, -talk_politics_how_often) |>
  left_join(read_elite_engagement(), by = "subject_id")

# "College degree" is the College and Graduate School categories of educ_5
# together; "strong partisan" is the two endpoints of the seven-point pid scale;
# "at least daily" is every response option at or above once a day, which is three
# options for the news item and two for the conversation item, whose scale has no
# hourly option.
share_row <- function(data, sample_label, quantity_label, indicator, answered) {
  tibble(
    sample = sample_label,
    quantity = quantity_label,
    n_sample = nrow(data),
    n_answered = sum(answered),
    n_yes = sum(indicator & answered),
    share_of_sample = n_yes / n_sample,
    share_of_answered = n_yes / n_answered
  )
}

composition_rows <- function(data, sample_label) {
  bind_rows(
    share_row(data, sample_label, "College degree or more",
              data$educ_5 %in% c("College", "Graduate School"), !is.na(data$educ_5)),
    share_row(data, sample_label, "Graduate degree",
              data$educ_5 == "Graduate School", !is.na(data$educ_5)),
    share_row(data, sample_label, "Strong partisan",
              data$pid_7 %in% c(1, 7), !is.na(data$pid_7))
  )
}

descriptions <- bind_rows(
  composition_rows(study_1_mturk, "MTurk"),
  composition_rows(study_1_elite, "Elite"),
  composition_rows(study_1_lucid, "Lucid"),
  share_row(study_1_elite, "Elite", "Reads the news at least daily",
            study_1_elite$news_how_often %in%
              c("Several times an hour", "Several times a day", "Once a day"),
            !is.na(study_1_elite$news_how_often)),
  share_row(study_1_elite, "Elite", "Talks politics at least once a day",
            study_1_elite$talk_politics_how_often %in%
              c("Several times a day", "Once a day"),
            !is.na(study_1_elite$talk_politics_how_often))
)

write_csv(descriptions, file.path(out_dir, "text_sample_descriptions.csv"))

print(descriptions, n = nrow(descriptions), width = 200)
