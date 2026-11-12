# coppock_green_2022/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_green_2022_ground_truth.csv,
#   ground_truth/float_coverage.csv
# Depends on: maintained/output/ (run run_all.R first), ground_truth/archive_values.csv
# Description: Assemble the ground truth table. value_paper is typed from the
#   article and the online appendix and is only ever compared against.
#   value_script is read out of ground_truth/archive_values.csv, which
#   run_original_archive.R produces by running the deposited scripts and reading
#   the LaTeX they write. value_rewrite is read out of maintained/output/. No
#   published number is an input to any computation, here or in maintained/.
#
#   value_paper is carried as the string the article prints, not as a number,
#   because a double cannot record how many decimals were printed: 0.80 and 0.8
#   are the same double, and a tolerance read off the double would compare a
#   published 0.80 at five hundredths. The tolerance is derived from the string.
#
#   Every quantity is carried in the units the article prints it in. Where the
#   article gives a percentage the rewrite's proportion is multiplied by 100 at
#   the point of comparison, so the printed precision and the tolerance line up.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

out <- function(file) read_csv(here::here("maintained", "output", file), show_col_types = FALSE)

archive <- read_csv(here::here("ground_truth", "archive_values.csv"), show_col_types = FALSE)

pct <- function(x) as.numeric(str_remove(x, "%"))

# Published precision ----
paper_number <- function(x) as.numeric(str_remove_all(x, "[,%]"))

paper_decimals <- function(x) {
  stripped <- str_remove_all(x, "[,%]")
  if_else(str_detect(stripped, fixed(".")),
          nchar(str_remove(stripped, "^[^.]*\\.")),
          0L)
}

agrees <- function(value, paper) {
  case_when(
    is.na(value) | is.na(paper) ~ NA_real_,
    abs(value - paper_number(paper)) <= 0.5 * 10^(-paper_decimals(paper)) ~ 1,
    .default = 0
  )
}

# Joins ----
# Every join below carries a published value on one side and a pipeline value on
# the other, so a key that is not unique silently multiplies rows and a key that
# matches nothing silently produces NA. Both are checked at every join.
join_checked <- function(x, y, by, label) {
  if (anyDuplicated(select(x, all_of(by))) != 0) stop(str_glue("{label}: key not unique on the left"))
  if (anyDuplicated(select(y, all_of(by))) != 0) stop(str_glue("{label}: key not unique on the right"))
  joined <- left_join(x, y, by = by)
  if (nrow(joined) != nrow(x)) stop(str_glue("{label}: join changed the row count"))
  joined
}

# Demographic tables ----
# Three appendix tables with the same shape: column percentages by sample, one
# published table, one set of deposited xtable fragments, one rewrite CSV.
demographic_block <- function(label, paper_wide, rewrite_file, sample_cols, file_map, note) {
  paper_long <- paper_wide |>
    pivot_longer(all_of(sample_cols), names_to = "sample", values_to = "value_paper")

  rewrite_long <- out(rewrite_file) |>
    pivot_longer(all_of(sample_cols), names_to = "sample", values_to = "value_rewrite") |>
    mutate(value_rewrite = pct(value_rewrite))

  archive_long <- archive |>
    filter(file %in% names(file_map)) |>
    mutate(variable = unname(file_map[file]), sample = sample_cols[column]) |>
    select(variable, category = term, sample, value_script = value)

  paper_long |>
    join_checked(rewrite_long, c("variable", "category", "sample"),
                 str_glue("{label} rewrite")) |>
    join_checked(archive_long, c("variable", "category", "sample"),
                 str_glue("{label} archive")) |>
    transmute(
      table_figure = label,
      claim = str_glue("{variable}: {category} ({sample})") |> as.character(),
      value_script, value_paper, value_rewrite,
      notes = note
    )
}

paper_b2 <- tribble(
  ~variable, ~category, ~MTurk, ~Elites, ~Lucid,
  "age", "18 - 29", "45.1%", "7.2%", "18.0%",
  "age", "30 - 39", "30.7%", "17.6%", "19.7%",
  "age", "40 - 49", "12.2%", "19.2%", "18.9%",
  "age", "50 - 59", "8.8%", "25.0%", "16.6%",
  "age", "60+", "3.2%", "31.1%", "26.7%",
  "gender", "Male", "52.1%", "67.9%", "49.0%",
  "gender", "Female", "47.9%", "32.1%", "51.0%",
  "race", "Black", "6.5%", "3.1%", "12.0%",
  "race", "Hispanic", "5.8%", "4.1%", "7.9%",
  "race", "Other", "10.2%", "9.7%", "5.0%",
  "race", "White", "77.5%", "83.0%", "75.0%",
  "education", "Less than High School", "0.5%", "0.0%", "1.3%",
  "education", "High School", "9.8%", "0.5%", "21.2%",
  "education", "Some College", "38.3%", "3.1%", "33.0%",
  "education", "College", "39.0%", "24.9%", "29.4%",
  "education", "Graduate School", "12.4%", "71.6%", "15.1%",
  "party_id", "Strong Democrat", "19.8%", "31.6%", "24.1%",
  "party_id", "Not very strong Democrat", "25.4%", "14.3%", "14.4%",
  "party_id", "Lean Democrat", "15.5%", "10.1%", "6.7%",
  "party_id", "Independent", "15.4%", "11.9%", "15.1%",
  "party_id", "Lean Republican", "7.1%", "7.0%", "5.7%",
  "party_id", "Not very strong Republican", "11.0%", "12.2%", "12.2%",
  "party_id", "Strong Republican", "5.7%", "12.7%", "21.8%",
  "ideology", "Conservative", "18.2%", "16.8%", "30.4%",
  "ideology", "Liberal", "44.5%", "37.3%", "25.8%",
  "ideology", "Libertarian", "6.2%", "10.2%", "1.7%",
  "ideology", "Moderate", "26.5%", "32.0%", "34.0%",
  "ideology", "Other", "4.6%", "3.7%", "8.1%",
  "n", "N", "3001", "2181", "2524"
)

paper_c3 <- tribble(
  ~variable, ~category, ~Original, ~Replication,
  "age", "18 - 29", "15.8%", "18.3%",
  "age", "30 - 39", "17.7%", "19.8%",
  "age", "40 - 49", "16.2%", "18.8%",
  "age", "50 - 59", "20.0%", "16.3%",
  "age", "60+", "30.4%", "26.8%",
  "gender", "Male", "37.7%", "48.7%",
  "gender", "Female", "62.3%", "51.3%",
  "education", "Less than High School", "3.3%", "1.4%",
  "education", "High School", "20.3%", "21.0%",
  "education", "Some College", "39.8%", "33.3%",
  "education", "College", "23.8%", "29.3%",
  "education", "Graduate School", "12.7%", "14.9%",
  "party_id", "Strong Democrat", "20.5%", "24.0%",
  "party_id", "Not very strong Democrat", "16.7%", "14.3%",
  "party_id", "Lean Democrat", "5.4%", "6.8%",
  "party_id", "Independent", "17.7%", "14.9%",
  "party_id", "Lean Republican", "5.2%", "6.0%",
  "party_id", "Not very strong Republican", "13.5%", "12.2%",
  "party_id", "Strong Republican", "21.0%", "21.7%",
  "n", "N", "1087", "2976"
)

paper_d5 <- tribble(
  ~variable, ~category, ~Original, ~Replication,
  "age", "18-29", "14.4%", "22.7%",
  "age", "30-39", "13.6%", "18.7%",
  "age", "40-49", "12.6%", "18.7%",
  "age", "50-99", "21.2%", "14.9%",
  "age", "60+", "38.1%", "25.0%",
  "gender", "Male", "50.1%", "46.9%",
  "gender", "Female", "49.9%", "53.1%",
  "race", "Black", "8.5%", "10.6%",
  "race", "Hispanic", "9.2%", "12.9%",
  "race", "Other", "7.1%", "9.0%",
  "race", "White", "75.2%", "67.5%",
  "education", "Less than High School", "7.7%", "2.7%",
  "education", "High School", "30.3%", "29.3%",
  "education", "Some College", "29.3%", "23.3%",
  "education", "College", "18.6%", "31.2%",
  "education", "Graduate School", "14.0%", "13.5%",
  "party_id", "Strong Democrat", "15.0%", "28.3%",
  "party_id", "Not very strong Democrat", "10.9%", "11.2%",
  "party_id", "Lean Democrat", "18.4%", "7.4%",
  "party_id", "Independent", "5.8%", "13.9%",
  "party_id", "Lean Republican", "20.2%", "6.7%",
  "party_id", "Not very strong Republican", "13.1%", "9.9%",
  "party_id", "Strong Republican", "16.6%", "22.6%",
  "n", "N", "1020", "1187"
)

b2_rows <- demographic_block(
  "Table B.2", paper_b2, "table_b2_sample_demographics.csv",
  c("MTurk", "Elites", "Lucid"),
  c(study_1_age.tex = "age", study_1_female.tex = "gender", study_1_race.tex = "race",
    study_1_educ.tex = "education", study_1_pid.tex = "party_id",
    study_1_ideo.tex = "ideology", study_1_n.tex = "n"),
  "Appendix table B.2, column percentages by sample"
)

c3_rows <- demographic_block(
  "Table C.3", paper_c3, "table_c3_study2_demographics.csv",
  c("Original", "Replication"),
  c(study_2_age.tex = "age", study_2_female.tex = "gender",
    study_2_educ.tex = "education", study_2_pid.tex = "party_id", study_2_n.tex = "n"),
  "Appendix table C.3, column percentages by sample"
)

d5_rows <- demographic_block(
  "Table D.5", paper_d5, "table_d5_study3_demographics.csv",
  c("Original", "Replication"),
  c(study_3_age.tex = "age", study_3_female.tex = "gender", study_3_race.tex = "race",
    study_3_educ.tex = "education", study_3_pid.tex = "party_id", study_3_n.tex = "n"),
  "Appendix table D.5, column percentages by sample"
)

# Regression tables ----
# Same shape again: a published panel of estimates, standard errors and sample
# sizes, a deposited stargazer table, a rewrite CSV of the same fits.
regression_block <- function(label, paper_wide, rewrite_long, file_map, note) {
  paper_long <- paper_wide |>
    pivot_longer(c(estimate, std.error, n), names_to = "stat", values_to = "value_paper") |>
    filter(!is.na(value_paper))

  archive_long <- archive |>
    filter(file %in% names(file_map$file)) |>
    mutate(
      panel = unname(file_map$file[file]),
      column_label = file_map$columns[column],
      term = if_else(term == "N", "N", unname(file_map$terms[term]))
    ) |>
    select(panel, column_label, term, stat, value_script = value)

  paper_long |>
    join_checked(rewrite_long, c("panel", "column_label", "term", "stat"),
                 str_glue("{label} rewrite")) |>
    join_checked(archive_long, c("panel", "column_label", "term", "stat"),
                 str_glue("{label} archive")) |>
    transmute(
      table_figure = label,
      claim = str_glue("{panel}, {column_label}, {term}: {stat}") |> as.character(),
      value_script, value_paper, value_rewrite,
      notes = note
    )
}

# The rewrite writes estimates and standard errors in long form already; N is a
# column on each row, so it becomes its own stat.
rewrite_regression_long <- function(file, panel_col, column_col) {
  raw <- out(file)
  bind_rows(
    raw |>
      transmute(panel = .data[[panel_col]], column_label = as.character(.data[[column_col]]),
                term, estimate, std.error) |>
      pivot_longer(c(estimate, std.error), names_to = "stat", values_to = "value_rewrite"),
    raw |>
      distinct(panel = .data[[panel_col]], column_label = as.character(.data[[column_col]]), n) |>
      transmute(panel, column_label, term = "N", stat = "n", value_rewrite = n)
  )
}

t2_columns <- c("Employers", "Local Officials", "SSM Amend", "Prayer", "Prayer Amend")

paper_t2 <- tribble(
  ~panel, ~column_label, ~term, ~estimate, ~std.error, ~n,
  "Original", "Employers", "Religious exemption", ".444", ".133", NA,
  "Original", "Local Officials", "Religious exemption", "-.003", ".029", NA,
  "Original", "SSM Amend", "Religious exemption", ".217", ".143", NA,
  "Original", "Prayer", "Religious exemption", "-.075", ".115", NA,
  "Original", "Prayer Amend", "Religious exemption", ".087", ".131", NA,
  "Original", "Employers", "Constant (control mean)", "3.016", ".090", NA,
  "Original", "Local Officials", "Constant (control mean)", ".381", ".021", NA,
  "Original", "SSM Amend", "Constant (control mean)", "3.679", ".099", NA,
  "Original", "Prayer", "Constant (control mean)", "5.115", ".081", NA,
  "Original", "Prayer Amend", "Constant (control mean)", "4.164", ".093", NA,
  "Original", "Employers", "N", NA, NA, "1,087",
  "Original", "Local Officials", "N", NA, NA, "1,086",
  "Original", "SSM Amend", "N", NA, NA, "1,087",
  "Original", "Prayer", "N", NA, NA, "1,087",
  "Original", "Prayer Amend", "N", NA, NA, "1,085",
  "Replication", "Employers", "Religious exemption", ".410", ".080", NA,
  "Replication", "Local Officials", "Religious exemption", "-.038", ".018", NA,
  "Replication", "SSM Amend", "Religious exemption", ".049", ".085", NA,
  "Replication", "Prayer", "Religious exemption", "-.014", ".073", NA,
  "Replication", "Prayer Amend", "Religious exemption", ".019", ".079", NA,
  "Replication", "Employers", "Constant (control mean)", "3.061", ".056", NA,
  "Replication", "Local Officials", "Constant (control mean)", ".383", ".013", NA,
  "Replication", "SSM Amend", "Constant (control mean)", "3.634", ".060", NA,
  "Replication", "Prayer", "Constant (control mean)", "4.788", ".051", NA,
  "Replication", "Prayer Amend", "Constant (control mean)", "4.041", ".056", NA,
  "Replication", "Employers", "N", NA, NA, "2,954",
  "Replication", "Local Officials", "N", NA, NA, "2,950",
  "Replication", "SSM Amend", "N", NA, NA, "2,953",
  "Replication", "Prayer", "N", NA, NA, "2,950",
  "Replication", "Prayer Amend", "N", NA, NA, "2,953"
)

t2_rows <- regression_block(
  "Table 2", paper_t2,
  rewrite_regression_long("table_2_employment_discrimination.csv", "sample", "outcome"),
  file_map = list(
    file = c(study_2_ssm.tex = "Original", study_2_replication_ssm.tex = "Replication"),
    columns = t2_columns,
    terms = c(`Religious Exemption` = "Religious exemption",
              `Constant (Control Mean)` = "Constant (control mean)")
  ),
  note = "Table 2, employment discrimination experiment"
)

t3_columns <- c("Belief in Inequality (Binary)", "Econ. System Justification (Standard Units)")

paper_t3 <- tribble(
  ~panel, ~column_label, ~term, ~estimate, ~std.error, ~n,
  "Original", t3_columns[1], "Inequality treatment", ".261", ".030", NA,
  "Original", t3_columns[2], "Inequality treatment", "-.181", ".103", NA,
  "Original", t3_columns[1], "Constant (control mean)", ".394", ".022", NA,
  "Original", t3_columns[2], "Constant (control mean)", "-.000", ".077", NA,
  "Original", t3_columns[1], "N", NA, NA, "1,020",
  "Original", t3_columns[2], "N", NA, NA, "336",
  "Replication", t3_columns[1], "Inequality treatment", ".229", ".037", NA,
  "Replication", t3_columns[2], "Inequality treatment", "-.071", ".066", NA,
  "Replication", t3_columns[1], "Pure control", ".122", ".039", NA,
  "Replication", t3_columns[2], "Pure control", "-.063", ".068", NA,
  "Replication", t3_columns[1], "Constant (control mean)", ".446", ".029", NA,
  "Replication", t3_columns[2], "Constant (control mean)", "-.254", ".050", NA,
  "Replication", t3_columns[1], "N", NA, NA, "1,041",
  "Replication", t3_columns[2], "N", NA, NA, "1,026"
)

t3_rows <- regression_block(
  "Table 3", paper_t3,
  rewrite_regression_long("table_3_inequality.csv", "sample", "outcome"),
  file_map = list(
    file = c(study_3.tex = "Original", study_3_replication.tex = "Replication"),
    columns = t3_columns,
    terms = c(`Inequality Treatment` = "Inequality treatment",
              `Pure control` = "Pure control",
              `Constant (Control Mean)` = "Constant (control mean)")
  ),
  note = "Table 3, income inequality experiment"
)

t4_columns <- c("Prefer small government", "Prefer large government")

paper_t4 <- tribble(
  ~panel, ~column_label, ~term, ~estimate, ~std.error, ~n,
  "Original", t4_columns[1], "Correction treatment", "-.172", ".047", NA,
  "Original", t4_columns[2], "Correction treatment", ".174", ".224", NA,
  "Original", t4_columns[1], "Constant (control mean)", ".316", ".030", NA,
  "Original", t4_columns[2], "Constant (control mean)", ".043", ".174", NA,
  "Original", t4_columns[1], "N", NA, NA, "299",
  "Original", t4_columns[2], "N", NA, NA, "26",
  "Replication", t4_columns[1], "Correction treatment", "-.153", ".030", NA,
  "Replication", t4_columns[2], "Correction treatment", ".077", ".074", NA,
  "Replication", t4_columns[1], "Constant (control mean)", ".232", ".021", NA,
  "Replication", t4_columns[2], "Constant (control mean)", ".064", ".049", NA,
  "Replication", t4_columns[1], "N", NA, NA, "609",
  "Replication", t4_columns[2], "N", NA, NA, "111"
)

t4_rows <- regression_block(
  "Table 4", paper_t4,
  rewrite_regression_long("table_4_correction_experiment.csv", "sample", "gov_pref"),
  file_map = list(
    file = c(study_4_correction.tex = "Original",
             study_4_correction_replication.tex = "Replication"),
    columns = t4_columns,
    terms = c(`Correction Treatment` = "Correction treatment",
              `Constant (Control Mean)` = "Constant (control mean)")
  ),
  note = "Table 4, consistency correction experiment"
)

c4_columns <- c("Too Little on Welfare", "Gov Should Reduce Ineq", "Medicaid a Good Thing")

paper_c4 <- tribble(
  ~panel, ~column_label, ~term, ~estimate, ~std.error, ~n,
  "Original", c4_columns[1], "Welfare wording", "-0.593", "0.046", NA,
  "Original", c4_columns[2], "Welfare wording", "-0.241", "0.130", NA,
  "Original", c4_columns[3], "Welfare wording", "-0.002", "0.024", NA,
  "Original", c4_columns[1], "Constant (control mean)", "2.383", "0.033", NA,
  "Original", c4_columns[2], "Constant (control mean)", "4.678", "0.093", NA,
  "Original", c4_columns[3], "Constant (control mean)", "0.813", "0.017", NA,
  "Original", c4_columns[1], "N", NA, NA, "1,087",
  "Original", c4_columns[2], "N", NA, NA, "1,086",
  "Original", c4_columns[3], "N", NA, NA, "1,084",
  "Replication", c4_columns[1], "Welfare wording", "-0.457", "0.026", NA,
  "Replication", c4_columns[2], "Welfare wording", "-0.123", "0.074", NA,
  "Replication", c4_columns[3], "Welfare wording", "-0.017", "0.014", NA,
  "Replication", c4_columns[1], "Constant (control mean)", "2.471", "0.018", NA,
  "Replication", c4_columns[2], "Constant (control mean)", "4.439", "0.053", NA,
  "Replication", c4_columns[3], "Constant (control mean)", "0.820", "0.010", NA,
  "Replication", c4_columns[1], "N", NA, NA, "2,954",
  "Replication", c4_columns[2], "N", NA, NA, "2,952",
  "Replication", c4_columns[3], "N", NA, NA, "2,949"
)

c4_rows <- regression_block(
  "Table C.4", paper_c4,
  rewrite_regression_long("table_c4_welfare_experiment.csv", "sample", "outcome"),
  file_map = list(
    file = c(welfare.tex = "Original", welfare_replication.tex = "Replication"),
    columns = c4_columns,
    terms = c(`Welfare Wording` = "Welfare wording",
              `Constant (Aid to the Poor)` = "Constant (control mean)")
  ),
  note = "Appendix table C.4, welfare wording experiment"
)

# Table 1 ----
t1 <- out("table_1_static_constraint_correlations.csv")
t1_archive <- archive |> filter(file == "static_cor.tex")

paper_t1 <- tribble(
  ~sample, ~scope, ~value_paper,
  "MTurk sample", "across_domain", ".06",
  "MTurk sample", "within_domain", ".30",
  "Elite sample", "across_domain", ".22",
  "Elite sample", "within_domain", ".39",
  "Lucid sample", "across_domain", ".00",
  "Lucid sample", "within_domain", ".12"
)

t1_rows <- paper_t1 |>
  join_checked(
    t1 |>
      pivot_longer(c(across_domain, within_domain), names_to = "scope",
                   values_to = "value_rewrite") |>
      select(sample, scope, value_rewrite),
    c("sample", "scope"), "Table 1 rewrite"
  ) |>
  join_checked(
    t1_archive |>
      transmute(sample = str_replace(term, "Sample", "sample"),
                scope = if_else(column == 1, "across_domain", "within_domain"),
                value_script = value),
    c("sample", "scope"), "Table 1 archive"
  ) |>
  transmute(
    claim_id = str_glue("t1_{str_to_lower(word(sample))}_{str_remove(scope, '_domain')}") |>
      as.character(),
    table_figure = "Table 1",
    claim = str_glue("{sample}: average {str_replace(scope, '_', ' ')} correlation") |>
      as.character(),
    value_script, value_paper, value_rewrite, digits = 2L,
    notes = "Table 1, average Pearson correlation over the 120 unordered outcome pairs"
  )

# Figures ----
# The three coefficient plots print no numbers, so there is nothing in them to
# compare against cell by cell. What can be checked is how many estimates each
# carries, and for figure 4 the article states that count in the results section.
# Each row reads its own figure's CSV, addressed by the figure's own stem.
figure_file <- function(stem) {
  file <- str_glue("{stem}.csv") |> as.character()
  stopifnot(file.exists(here::here("maintained", "output", file)))
  out(file)
}

f4 <- figure_file("figure_4_dynamic_constraint_estimates")
fb2 <- figure_file("figure_b2_wave2_estimates")
fb3 <- figure_file("figure_b3_wave3_estimates")

figure_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "Figure 4", "Treatment effects plotted, target issues", NA, "48", sum(f4$target),
    "Article: 'out of the 48 estimated treatment effects on target issues'. The deposit plots the same estimates but prints no count.",
  "Figure 4", "Treatment effects plotted, nontarget issues", NA, "144", sum(!f4$target),
    "Article: 'out of 144 estimated effects on nontarget issues'."
)

# Float coverage ----
# The covered fraction of each published float, not the presence of at least one
# row. value_paper is the number of cells the article prints, counted off the
# page; value_rewrite is the number the pipeline writes. The three coefficient
# plots print no numbers on their faces, so what they assert is a count of
# plotted estimates: 16 outcomes by 4 treatments by however many samples the
# panel carries.
regression_cells <- function(file, panel_col, column_col) {
  raw <- out(file)
  2 * nrow(raw) + nrow(distinct(raw, .data[[panel_col]], .data[[column_col]]))
}

demographic_cells <- function(file, sample_cols) nrow(out(file)) * length(sample_cols)

float_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_paper, ~value_rewrite,
  "float_table_1", "Table 1", "Published cells covered", "6", 2 * nrow(t1),
  "float_table_2", "Table 2", "Published cells covered", "50",
    regression_cells("table_2_employment_discrimination.csv", "sample", "outcome"),
  "float_table_3", "Table 3", "Published cells covered", "24",
    regression_cells("table_3_inequality.csv", "sample", "outcome"),
  "float_table_4", "Table 4", "Published cells covered", "20",
    regression_cells("table_4_correction_experiment.csv", "sample", "gov_pref"),
  "float_table_b2", "Table B.2", "Published cells covered", "87",
    demographic_cells("table_b2_sample_demographics.csv", c("MTurk", "Elites", "Lucid")),
  "float_table_c3", "Table C.3", "Published cells covered", "40",
    demographic_cells("table_c3_study2_demographics.csv", c("Original", "Replication")),
  "float_table_c4", "Table C.4", "Published cells covered", "30",
    regression_cells("table_c4_welfare_experiment.csv", "sample", "outcome"),
  "float_table_d5", "Table D.5", "Published cells covered", "48",
    demographic_cells("table_d5_study3_demographics.csv", c("Original", "Replication")),
  "float_figure_4", "Figure 4", "Estimates plotted", "192", nrow(f4),
  "float_figure_b2", "Figure B.2", "Estimates plotted", "128", nrow(fb2),
  "float_figure_b3", "Figure B.3", "Estimates plotted", "64", nrow(fb3)
) |>
  mutate(
    value_script = NA_real_,
    digits = 0L,
    notes = "Coverage of the float rather than one of its cells: the number of values the article prints in it against the number the pipeline writes. Each cell is compared separately in the rows above."
  )

# In-text quantities ----
pwa <- out("text_precision_weighted_averages.csv")
ranges <- out("text_precision_weighted_averages_endpoint_ranges.csv")
counts <- out("text_significance_counts.csv")
amtrak <- out("text_amtrak_largest_effects.csv")
inconsistency <- out("text_inconsistency_rates.csv")
pooled <- out("text_pooled_ratio_metaanalysis.csv")
pooled_dispersion <- out("text_pooled_ratio_dispersion.csv")

pwa_value <- function(sample_name, quantity_name, column) {
  value <- pwa[[column]][pwa$sample == sample_name & pwa$quantity == quantity_name]
  stopifnot(length(value) == 1)
  value
}

amtrak_value <- function(study) {
  value <- amtrak$estimate[amtrak$Study == study & str_detect(amtrak$dv_labels, "^Taxes")]
  stopifnot(length(value) == 1)
  value
}

# The deposit's own ideology table carries three rows with no label, so a logical
# subset on term picks up NAs; the lookup filters instead.
archive_value <- function(term_name) {
  value <- archive |> filter(term == term_name) |> pull(value)
  stopifnot(length(value) == 1)
  value
}

# Sample ratios and the pooled ratio are printed as percentages in the article.
as_percent <- function(x) 100 * x

meta_agreement <- archive_value("rmeta versus metafor, largest absolute difference")

endpoint_range <- function(sample_name, endpoint_name, sims) {
  row <- ranges |>
    filter(sample == sample_name, quantity == "ratio", endpoint == endpoint_name,
           sims_setting == sims)
  stopifnot(nrow(row) == 1)
  c(low = as_percent(row$mc_low), high = as_percent(row$mc_high))
}

# Whether the published value falls in the range decides the verdict, so the note
# reports it rather than describing it, and note and verdict cannot drift apart.
verdict_clause <- function(published, range) {
  value <- paper_number(published)
  inside <- value >= range["low"] & value <= range["high"]
  str_glue("The published {published} lies {if_else(inside, 'inside', 'outside')} that range.")
}

endpoint_note <- function(sample_name, endpoint_name, published) {
  archive_sims <- endpoint_range(sample_name, endpoint_name, 10)
  rewrite_sims <- endpoint_range(sample_name, endpoint_name, 1000)
  str_glue(
    "Bootstrap interval endpoint from an unseeded procedure, so it is compared against its own \\
     distribution rather than to printed precision. Across resamples the endpoint runs from \\
     {round(archive_sims['low'], 1)}% to {round(archive_sims['high'], 1)}% at the 10 resamples the \\
     deposit ships, and from {round(rewrite_sims['low'], 1)}% to {round(rewrite_sims['high'], 1)}% \\
     at the 1,000 the rewrite uses. {verdict_clause(published, archive_sims)}"
  ) |> as.character()
}

pooled_range <- function(quantity_name, sims) {
  row <- pooled_dispersion |> filter(quantity == quantity_name, sims_setting == sims)
  stopifnot(nrow(row) == 1)
  c(low = as_percent(row$mc_low), high = as_percent(row$mc_high))
}

pooled_note <- function(quantity_name, published) {
  archive_sims <- pooled_range(quantity_name, 10)
  rewrite_sims <- pooled_range(quantity_name, 1000)
  str_glue(
    "The three study-level ratios are fixed, but their weights are unseeded bootstrap \\
     variances, so this is a draw. Over resamples it runs from {round(archive_sims['low'], 1)}% \\
     to {round(archive_sims['high'], 1)}% at the 10 resamples the deposit ships and from \\
     {round(rewrite_sims['low'], 1)}% to {round(rewrite_sims['high'], 1)}% at the 1,000 the \\
     rewrite uses. {verdict_clause(published, archive_sims)} On identical inputs \\
     rmeta::meta.summaries and metafor::rma(method = 'FE') return pooled estimates, standard \\
     errors and interval endpoints agreeing to {signif(meta_agreement, 2)}."
  ) |> as.character()
}

text_rows <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~digits, ~notes,
  "s1_pairs_total", "Text", "Outcome pairs compared (16 choose 2)", NA, "120",
    t1$n_pairs_total[1], 0L,
    "Article: '16 choose 2 = 120 pairs'. The deposit averages over the 240 ordered pairs instead and prints no count; the correlation matrix is symmetric, so the average is the same.",
  "s1_mturk_within_text", "Text", "MTurk average within-domain correlation, as given in the body text",
    t1_archive |> filter(term == "MTurk Sample", column == 2) |> pull(value), ".29",
    t1$within_domain[t1$sample == "MTurk sample"], 2L,
    "The body text says .29 where table 1 says .30. The quantity is 0.2966, so table 1 rounds it correctly and the sentence does not.",
  "s1_pwa_target_mturk", "Text", "MTurk precision-weighted average, target outcomes", NA, "0.42",
    pwa_value("MTurk", "Within Domain", "estimate"), 2L,
    "The deposited in_text_numbers.R computes this and prints nothing when sourced.",
  "s1_pwa_target_elite", "Text", "Elite precision-weighted average, target outcomes", NA, "0.22",
    pwa_value("Elite", "Within Domain", "estimate"), 2L,
    "The deposited in_text_numbers.R computes this and prints nothing when sourced.",
  "s1_pwa_target_lucid", "Text", "Lucid precision-weighted average, target outcomes", NA, "0.245",
    pwa_value("Lucid", "Within Domain", "estimate"), 3L,
    "The deposited in_text_numbers.R computes this and prints nothing when sourced.",
  "s1_pwa_nontarget_mturk", "Text", "MTurk precision-weighted average, nontarget outcomes", NA, "0.004",
    pwa_value("MTurk", "Across Domain", "estimate"), 3L,
    "The deposited in_text_numbers.R computes this and prints nothing when sourced.",
  "s1_pwa_nontarget_elite", "Text", "Elite precision-weighted average, nontarget outcomes", NA, "0.01",
    pwa_value("Elite", "Across Domain", "estimate"), 2L,
    "The deposited in_text_numbers.R computes this and prints nothing when sourced.",
  "s1_pwa_nontarget_lucid", "Text", "Lucid precision-weighted average, nontarget outcomes", NA, "-0.02",
    pwa_value("Lucid", "Across Domain", "estimate"), 2L,
    "The deposited in_text_numbers.R computes this and prints nothing when sourced.",
  "s1_ratio_mturk", "Text", "MTurk indirect-to-direct effect ratio", NA, "0.8%",
    as_percent(pwa_value("MTurk", "ratio", "estimate")), 1L,
    "Article: 'on MTurk, our best guess is that the average indirect effect is 0.8% of the average direct effect'.",
  "s1_ratio_elite", "Text", "Elite indirect-to-direct effect ratio", NA, "6.4%",
    as_percent(pwa_value("Elite", "ratio", "estimate")), 1L,
    "Article: 'indirect effects are 6.4% the size of the direct effects on average'.",
  "s1_ratio_lucid", "Text", "Lucid indirect-to-direct effect ratio", NA, "-9.4%",
    as_percent(pwa_value("Lucid", "ratio", "estimate")), 1L,
    "Article: 'on Lucid, the fraction is in fact negative, at -9.4%'.",
  "s1_ratio_mturk_low", "Text", "MTurk ratio, 95% interval lower bound", NA, "-8.1%",
    as_percent(pwa_value("MTurk", "ratio", "conf_low")), 1L, endpoint_note("MTurk", "conf_low", "-8.1%"),
  "s1_ratio_mturk_high", "Text", "MTurk ratio, 95% interval upper bound", NA, "8.7%",
    as_percent(pwa_value("MTurk", "ratio", "conf_high")), 1L, endpoint_note("MTurk", "conf_high", "8.7%"),
  "s1_ratio_elite_low", "Text", "Elite ratio, 95% interval lower bound", NA, "-25.7%",
    as_percent(pwa_value("Elite", "ratio", "conf_low")), 1L, endpoint_note("Elite", "conf_low", "-25.7%"),
  "s1_ratio_elite_high", "Text", "Elite ratio, 95% interval upper bound", NA, "26.2%",
    as_percent(pwa_value("Elite", "ratio", "conf_high")), 1L, endpoint_note("Elite", "conf_high", "26.2%"),
  "s1_ratio_lucid_low", "Text", "Lucid ratio, 95% interval lower bound", NA, "-24%",
    as_percent(pwa_value("Lucid", "ratio", "conf_low")), 0L, endpoint_note("Lucid", "conf_low", "-24%"),
  # +5%, not 15%. The article sets this endpoint's plus sign in a symbol font
  # that the text extraction this repository was built against mangles: the
  # minus of -24% comes off the page as a 2 and the plus of +5% as a 1. The
  # first transcription read "1 5%" as fifteen per cent, and the disagreement
  # that produced was published as entry 7 of the errata note, which is
  # removed. What found it was an audit of the published PDF by font name: a
  # glyph the extractor mangles comes from a face the body text does not use.
  "s1_ratio_lucid_high", "Text", "Lucid ratio, 95% interval upper bound", NA, "+5%",
    as_percent(pwa_value("Lucid", "ratio", "conf_high")), 0L, endpoint_note("Lucid", "conf_high", "+5%"),
  "s1_ratio_pooled", "Text", "Pooled indirect-to-direct ratio",
    as_percent(archive_value("Pooled indirect-to-direct ratio")), "1.4%",
    as_percent(pooled$estimate[pooled$model == "Fixed effects"]), 1L,
    pooled_note("estimate", "1.4%"),
  "s1_ratio_pooled_low", "Text", "Pooled ratio, 95% interval lower bound",
    as_percent(archive_value("Pooled ratio, 95% interval lower bound")), "-8%",
    as_percent(pooled$conf_low[pooled$model == "Fixed effects"]), 0L,
    pooled_note("conf_low", "-8%"),
  "s1_ratio_pooled_high", "Text", "Pooled ratio, 95% interval upper bound",
    as_percent(archive_value("Pooled ratio, 95% interval upper bound")), "6%",
    as_percent(pooled$conf_high[pooled$model == "Fixed effects"]), 0L,
    pooled_note("conf_high", "6%"),
  "s1_target_estimates", "Text", "Estimated treatment effects on target issues", NA, "48",
    counts$n_estimates[counts$issue_type == "Target issues"], 0L,
    "Read off the wave 1 estimates figure 4 plots; the deposit prints no count.",
  "s1_target_significant", "Text", "Target-issue effects significant at p < 0.05", NA, "39",
    counts$n_significant[counts$issue_type == "Target issues"], 0L,
    "Read off the wave 1 estimates figure 4 plots; the deposit prints no count.",
  "s1_target_significant_bh", "Text", "Target-issue effects significant after Benjamini-Hochberg", NA, "35",
    counts$n_significant_bh[counts$issue_type == "Target issues"], 0L,
    "Read off the wave 1 estimates figure 4 plots; the deposit prints no count.",
  "s1_nontarget_estimates", "Text", "Estimated treatment effects on nontarget issues", NA, "144",
    counts$n_estimates[counts$issue_type == "Nontarget issues"], 0L,
    "Read off the wave 1 estimates figure 4 plots; the deposit prints no count.",
  "s1_nontarget_significant", "Text", "Nontarget-issue effects significant at p < 0.05", NA, "9",
    counts$n_significant[counts$issue_type == "Nontarget issues"], 0L,
    "Read off the wave 1 estimates figure 4 plots; the deposit prints no count.",
  "s1_nontarget_significant_bh", "Text", "Nontarget-issue effects significant after Benjamini-Hochberg", NA, "0",
    counts$n_significant_bh[counts$issue_type == "Nontarget issues"], 0L,
    "Read off the wave 1 estimates figure 4 plots; the deposit prints no count.",
  "s1_amtrak_elite", "Text", "Amtrak treatment on the third Amtrak outcome, elite sample", NA, "0.52",
    amtrak_value("Elite Sample"), 2L,
    "Article: 'especially large, at 0.52 standard deviations among elites'. The article calls it the third question in the Amtrak set, and it is the third of the four Amtrak outcomes in the deposit's own ordering.",
  "s1_amtrak_mturk", "Text", "Amtrak treatment on the third Amtrak outcome, MTurk sample", NA, "0.73",
    amtrak_value("MTurk Sample"), 2L,
    "Article: 'and 0.73 standard deviations on MTurk'.",
  "disc_inconsistent_original", "Text", "Share of the study 2 original sample holding inconsistent preferences", NA, "30%",
    as_percent(inconsistency$share_inconsistent[inconsistency$sample == "Original"]), 0L,
    "Article: 'Among the MTurk sample, 30% of the respondents were inconsistent'. The inconsistency indicator exists only in the study 2 samples, both of which the article elsewhere says were recruited on Lucid, so that sentence attaches the rate to the wrong sample label. The rate itself is the study 2 original sample's. The deposit computes no rate.",
  "disc_inconsistent_replication", "Text", "Share of the study 2 replication sample holding inconsistent preferences", NA, "24%",
    as_percent(inconsistency$share_inconsistent[inconsistency$sample == "Replication"]), 0L,
    "Article: 'this figure was 24% on Lucid'. The deposit computes no rate.",
  "s2_n_original", "Text", "Study 2 original sample size", NA, "1,087",
    inconsistency$n_respondents[inconsistency$sample == "Original"], 0L,
    "Article: 'The 1,087 subjects for this experiment were also recruited via Lucid'."
)

# Claims stated in prose ----
# Everything the article asserts numerically outside a float. Each row carries the
# claim_id that ties it to ground_truth/published_claims.csv and to the block in
# maintained/in_text_claims.R that recomputes it by its own path. Rows flagged
# descriptive make a claim about shape, sign, count or order rather than naming a
# value, so their verdict is carried in holds and not in match_rewrite; rows
# flagged approximate name a rough magnitude the article itself hedges, and return
# no verdict at all.
descriptions <- out("text_sample_descriptions.csv")
study_2_cors <- out("text_study_2_correlations.csv")
t2_out <- out("table_2_employment_discrimination.csv")
t3_out <- out("table_3_inequality.csv")
t4_out <- out("table_4_correction_experiment.csv")
c4_out <- out("table_c4_welfare_experiment.csv")
b2_out <- out("table_b2_sample_demographics.csv")
c3_out <- out("table_c3_study2_demographics.csv")

description_share <- function(sample_name, quantity_name) {
  row <- descriptions |> filter(sample == sample_name, quantity == quantity_name)
  stopifnot(nrow(row) == 1)
  100 * row$n_yes / row$n_sample
}

description_n <- function(sample_name) {
  n <- unique(descriptions$n_sample[descriptions$sample == sample_name])
  stopifnot(length(n) == 1)
  n
}

regression_value <- function(tab, panel, column, term_name, statistic) {
  value <- tab[[statistic]][tab[[1]] == panel & tab[[2]] == column & tab$term == term_name]
  stopifnot(length(value) == 1)
  value
}

demographic_n <- function(tab, column) {
  value <- tab[[column]][tab$variable == "n"]
  stopifnot(length(value) == 1)
  as.numeric(value)
}

# Target-cell averages, the quantity every descriptive claim about the shaded
# diagonal of figure 4 is made against.
target_means <- f4 |>
  filter(target) |>
  summarize(mean_estimate = mean(estimate), .by = c(treatment, Study))

wall_street_means <- target_means$mean_estimate[target_means$treatment == "Treatment: Wall Street"]
sample_leaders <- target_means |>
  slice_max(mean_estimate, n = 1, by = Study) |>
  pull(treatment)
amtrak_means <- target_means$mean_estimate[target_means$treatment == "Treatment: Amtrak" &
                                             target_means$Study != "Lucid Sample"]
nontarget_estimates <- f4$estimate[!f4$target]
s2_nontarget <- t2_out |> filter(term == "Religious exemption", outcome != "Employers")
s2_original_cors <- study_2_cors$correlation[study_2_cors$sample == "Original"]
t4_treatment <- t4_out |> filter(term == "Correction treatment")
t4_precision <- t4_treatment |>
  select(sample, gov_pref, std.error) |>
  pivot_wider(names_from = gov_pref, values_from = std.error)
pooled_se <- as_percent(pooled$std.error[pooled$model == "Fixed effects"])
half_widths <- (f4$conf.high - f4$conf.low) / (2 * f4$std.error)

prose_rows <- tribble(
  ~claim_id, ~claim, ~value_paper, ~value_rewrite, ~digits, ~descriptive, ~approximate, ~holds, ~notes,
  "s1_times_conducted", "Samples study 1 was run in", "3", ncol(b2_out) - 2, 0L, FALSE, FALSE, NA,
    "Article: 'The experiment was conducted three times'.",
  "s1_n_mturk", "MTurk sample size, stated in prose", "3,001", description_n("MTurk"), 0L, FALSE, FALSE, NA,
    "The same quantity appendix table B.2 prints in its N row, transcribed a second time from the body text.",
  "s1_n_elite", "Elite sample size, stated in prose", "2,181", description_n("Elite"), 0L, FALSE, FALSE, NA,
    "As above.",
  "s1_n_lucid", "Lucid sample size, stated in prose", "2,524", description_n("Lucid"), 0L, FALSE, FALSE, NA,
    "As above.",
  "s1_college_elite", "Elite sample holding a college degree", "96%",
    description_share("Elite", "College degree or more"), 0L, FALSE, FALSE, NA,
    "College and Graduate School together. Appendix table B.2 prints the two categories separately and never their sum, so nothing in the article checks this figure against a table.",
  "s1_college_mturk", "MTurk sample holding a college degree", "51%",
    description_share("MTurk", "College degree or more"), 0L, FALSE, FALSE, NA, "As above.",
  "s1_college_lucid", "Lucid sample holding a college degree", "44%",
    description_share("Lucid", "College degree or more"), 0L, FALSE, FALSE, NA, "As above.",
  "s1_graduate_elite", "Elite sample holding a graduate degree", "71%",
    description_share("Elite", "Graduate degree"), 0L, FALSE, FALSE, NA,
    "Appendix table B.2 prints 71.6% for the same category, which rounds to 72%, so the sentence truncates where the table rounds.",
  "s1_graduate_mturk", "MTurk sample holding a graduate degree", "13%",
    description_share("MTurk", "Graduate degree"), 0L, FALSE, FALSE, NA,
    "Appendix table B.2 prints 12.4% for the same category, which rounds to 12%.",
  "s1_graduate_lucid", "Lucid sample holding a graduate degree", "15%",
    description_share("Lucid", "Graduate degree"), 0L, FALSE, FALSE, NA,
    "Appendix table B.2 prints 15.1% for the same category.",
  "s1_strong_partisan_elite", "Elite sample describing themselves as strong partisans", "44%",
    description_share("Elite", "Strong partisan"), 0L, FALSE, FALSE, NA,
    "The two endpoints of the seven-point party identification scale in appendix table B.2.",
  "s1_strong_partisan_mturk", "MTurk sample describing themselves as strong partisans", "25",
    description_share("MTurk", "Strong partisan"), 0L, TRUE, FALSE,
    as.numeric(round(description_share("MTurk", "Strong partisan")) == 25),
    "The article says 'only a quarter of the MTurk sample do so', so the test is against a quarter rather than a printed figure.",
  "s1_strong_partisan_lucid", "Lucid sample describing themselves as strong partisans", "46%",
    description_share("Lucid", "Strong partisan"), 0L, FALSE, FALSE, NA, "As above.",
  "s1_news_daily", "Elite sample reading the news at least daily", "97%",
    description_share("Elite", "Reads the news at least daily"), 0L, FALSE, FALSE, NA,
    "Appears in no table. The deposited elite clean file stores this item as an unevaluated recode specification string, so it is rebuilt from the deposit's own wave 1 raw file the same way the ideology column is.",
  "s1_talk_politics_daily", "Elite sample talking politics at least once a day", "64%",
    description_share("Elite", "Talks politics at least once a day"), 0L, FALSE, FALSE, NA,
    "As above.",
  "s1_conditions", "Experimental conditions in study 1", "5",
    n_distinct(f4$term) + 1, 0L, FALSE, FALSE, NA,
    "Four op-ed treatment contrasts plus the control they are taken against.",
  "s1_opeds", "Op-ed treatments analyzed", "4", n_distinct(f4$treatment), 0L, FALSE, FALSE, NA,
    "The climate change arm is set aside by the article and by every script here.",
  "s1_questions_per_attitude", "Outcome questions per issue area", "4",
    n_distinct(f4$outcome) / n_distinct(f4$dv_cat_label), 0L, FALSE, FALSE, NA, "",
  "s1_outcomes", "Outcome questions in total", "16", n_distinct(f4$outcome), 0L, FALSE, FALSE, NA, "",
  "s1_pairs_choose", "Outcome questions entering the pairwise correlations", "16",
    (1 + sqrt(1 + 8 * t1$n_pairs_total[1])) / 2, 0L, FALSE, FALSE, NA,
    "Recovered from the pair count rather than counted again: 120 unordered pairs implies 16 questions.",
  "s1_panels", "Panels in figure 4", "16", nrow(distinct(f4, treatment, dv_cat_label)), 0L, FALSE, FALSE, NA, "",
  "s1_attitudes", "Attitudes the rows of panels correspond to", "4",
    n_distinct(f4$dv_cat_label), 0L, FALSE, FALSE, NA, "",
  "s1_ci_level", "Confidence level of the intervals figure 4 draws", "95",
    100 * mean(2 * pnorm(half_widths) - 1), 0L, FALSE, FALSE, NA,
    "Stated nowhere in the deposit's code and recovered from the plotted half widths, which run at 1.96 standard errors.",
  "s1_elite_within_text", "Elite within-domain correlation, restated in prose", "0.39",
    t1$within_domain[t1$sample == "Elite sample"], 2L, FALSE, FALSE, NA, "",
  "s1_elite_across_text", "Elite across-domain correlation, restated in prose", "0.22",
    t1$across_domain[t1$sample == "Elite sample"], 2L, FALSE, FALSE, NA, "",
  "s1_mturk_across_text", "MTurk across-domain correlation, restated in prose", "0.06",
    t1$across_domain[t1$sample == "MTurk sample"], 2L, FALSE, FALSE, NA, "",
  "s1_lucid_within_text", "Lucid within-domain correlation, restated in prose", "0.12",
    t1$within_domain[t1$sample == "Lucid sample"], 2L, FALSE, FALSE, NA, "",
  "s1_lucid_across_text", "Lucid across-domain correlation, restated in prose", "0.00",
    t1$across_domain[t1$sample == "Lucid sample"], 2L, FALSE, FALSE, NA, "",
  "s1_wallstreet_largest", "The Wall Street treatment has the largest target-issue effect of the four", NA,
    sum(sample_leaders == "Treatment: Wall Street"), 0L, TRUE, FALSE,
    as.numeric(all(sample_leaders == "Treatment: Wall Street")),
    "True in all three samples: the Wall Street target-cell average leads in each.",
  "s1_wallstreet_magnitude", "The Wall Street target effects average nearly 0.5 standard deviations in all three samples",
    "0.5", min(wall_street_means), 2L, TRUE, FALSE, NA,
    "Hedged by the article, so no verdict is returned. The three averages are the elite, Lucid and MTurk target-cell means; the smallest is the Lucid one, which is the one furthest from 0.5.",
  "s1_amtrak_magnitude", "The Amtrak target effects hover around one-third of a standard deviation",
    "0.33", mean(amtrak_means), 2L, TRUE, FALSE, NA,
    "Hedged by the article, so no verdict is returned. The claim is made about elites and MTurk only; the value here is the mean of those two target-cell averages.",
  "s1_amtrak_question_index", "Position of the Amtrak question with the largest effects", "3",
    as.numeric(str_extract(unique(amtrak$outcome[str_detect(amtrak$dv_labels, "^Taxes")]),
                           "(?<=amtrak_)\\d+")), 0L, FALSE, FALSE, NA,
    "The article calls it the third question in the Amtrak set; it is the third of the four in the deposit's own variable ordering, and the appendix questionnaire's third Amtrak item is the one about how new taxes would be spent.",
  "s1_nontarget_near_zero", "In nearly every case the nontarget estimates are very close to zero", NA,
    max(abs(nontarget_estimates)), 2L, TRUE, FALSE, NA,
    "Not reducible to a verdict, so none is returned. The largest of the 144 nontarget estimates in absolute value is recorded here; the block in in_text_claims.R prints the whole distribution.",
  "s1_power_low", "The study is powered to detect indirect effects of 10 per cent of the direct effect",
    "10", paper_number("10%") / pooled_se, 1L, TRUE, FALSE,
    as.numeric(paper_number("10%") / pooled_se > 1.96),
    "Expressed as the number of pooled standard errors in a ratio of 10 per cent, which is what 'well powered to detect' has to mean here.",
  "s1_power_high", "The study is powered to detect indirect effects of 20 per cent of the direct effect",
    "20", paper_number("20%") / pooled_se, 1L, TRUE, FALSE,
    as.numeric(paper_number("20%") / pooled_se > 1.96), "As above.",
  "s2_nontarget_measures_text", "Nontarget outcome measures in study 2, as the design paragraph counts them",
    "3", n_distinct(t2_out$outcome) - 1, 0L, FALSE, FALSE, NA,
    "The design paragraph describes one measure plus 'the other two'. Table 2 reports four, and the results paragraph two paragraphs later says four.",
  "s2_nontarget_measures_table", "Nontarget outcome measures in study 2, as the results paragraph counts them",
    "4", n_distinct(t2_out$outcome) - 1, 0L, FALSE, FALSE, NA, "",
  "s2_correlation_low", "Lower end of the correlations among the five study 2 outcomes", "0.3",
    min(s2_original_cors), 1L, TRUE, FALSE,
    as.numeric(round(min(s2_original_cors), 1) == 0.3),
    "Ten pairwise correlations in the original sample. The smallest is 0.26 and the largest 0.65, and the article's stated range holds at the one decimal place it prints.",
  "s2_correlation_high", "Upper end of the same range", "0.7", max(s2_original_cors), 1L, TRUE, FALSE,
    as.numeric(round(max(s2_original_cors), 1) == 0.7), "As above.",
  "s2_effect_original", "Religious exemption effect on the employers question, restated in prose", "0.44",
    regression_value(t2_out, "Original", "Employers", "Religious exemption", "estimate"), 2L,
    FALSE, FALSE, NA, "",
  "s2_effect_replication", "The same effect in the replication, restated in prose", "0.41",
    regression_value(t2_out, "Replication", "Employers", "Religious exemption", "estimate"), 2L,
    FALSE, FALSE, NA, "",
  "s2_sign_split_original", "Two of the four nontarget estimates are positive in the original sample", NA,
    sum(s2_nontarget$estimate[s2_nontarget$sample == "Original"] > 0), 0L, TRUE, FALSE,
    as.numeric(sum(s2_nontarget$estimate[s2_nontarget$sample == "Original"] > 0) == 2), "",
  "s2_sign_split_replication", "The same split in the replication", NA,
    sum(s2_nontarget$estimate[s2_nontarget$sample == "Replication"] > 0), 0L, TRUE, FALSE,
    as.numeric(sum(s2_nontarget$estimate[s2_nontarget$sample == "Replication"] > 0) == 2), "",
  "s3_belief_effect_original", "Effect on belief in rising inequality, original study", "25",
    100 * regression_value(t3_out, "Original", "Belief in Inequality (Binary)",
                           "Inequality treatment", "estimate"), 1L, TRUE, TRUE, NA,
    "The article writes 'approximately 25 percentage points in both samples', so this is compared as a rough magnitude and returns no verdict.",
  "s3_belief_effect_replication", "The same effect in the replication", "25",
    100 * regression_value(t3_out, "Replication", "Belief in Inequality (Binary)",
                           "Inequality treatment", "estimate"), 1L, TRUE, TRUE, NA, "As above.",
  "s3_se_bound", "The standard errors of that effect are smaller than 4 percentage points", "4",
    100 * max(regression_value(t3_out, "Original", "Belief in Inequality (Binary)",
                               "Inequality treatment", "std.error"),
              regression_value(t3_out, "Replication", "Belief in Inequality (Binary)",
                               "Inequality treatment", "std.error")), 1L, TRUE, FALSE,
    as.numeric(100 * max(regression_value(t3_out, "Original", "Belief in Inequality (Binary)",
                                          "Inequality treatment", "std.error"),
                         regression_value(t3_out, "Replication", "Belief in Inequality (Binary)",
                                          "Inequality treatment", "std.error")) < 4),
    "A threshold claim, so the comparison is an inequality rather than an equality. The larger of the two standard errors is the one recorded.",
  "s3_esj_original", "Economic system justification effect in the original study, restated in prose", "-0.18",
    regression_value(t3_out, "Original", "Econ. System Justification (Standard Units)",
                     "Inequality treatment", "estimate"), 2L, FALSE, FALSE, NA, "",
  "s3_esj_original_se", "Its standard error, restated in prose", "0.10",
    regression_value(t3_out, "Original", "Econ. System Justification (Standard Units)",
                     "Inequality treatment", "std.error"), 2L, FALSE, FALSE, NA, "",
  "s3_esj_replication", "Economic system justification effect in the replication, restated in prose", "-0.07",
    regression_value(t3_out, "Replication", "Econ. System Justification (Standard Units)",
                     "Inequality treatment", "estimate"), 2L, FALSE, FALSE, NA, "",
  "s3_esj_replication_se", "Its standard error, restated in prose", "0.06",
    regression_value(t3_out, "Replication", "Econ. System Justification (Standard Units)",
                     "Inequality treatment", "std.error"), 2L, FALSE, FALSE, NA,
    "Table 3 prints .066 for the same standard error, which rounds to 0.07, so the sentence truncates where the table rounds.",
  "t3_note_third", "A random third of original study respondents answered the system justification question", "3",
    regression_value(t3_out, "Original", "Belief in Inequality (Binary)", "Inequality treatment", "n") /
      regression_value(t3_out, "Original", "Econ. System Justification (Standard Units)",
                       "Inequality treatment", "n"), 0L, FALSE, FALSE, NA,
    "Recorded as the denominator the table 3 note implies: original study respondents per system justification respondent.",
  "disc_target_significant", "Target-issue effects that were significant, restated in the discussion", "39",
    counts$n_significant[counts$issue_type == "Target issues"], 0L, FALSE, FALSE, NA, "",
  "disc_target_estimates", "Target-issue opportunities, restated in the discussion", "48",
    counts$n_estimates[counts$issue_type == "Target issues"], 0L, FALSE, FALSE, NA, "",
  "disc_nontarget_significant", "Nontarget-issue effects that were significant, restated in the discussion", "9",
    counts$n_significant[counts$issue_type == "Nontarget issues"], 0L, FALSE, FALSE, NA, "",
  "disc_nontarget_estimates", "Nontarget-issue opportunities, restated in the discussion", "144",
    counts$n_estimates[counts$issue_type == "Nontarget issues"], 0L, FALSE, FALSE, NA, "",
  "disc_spillover_ratio", "Average spillover effect as a share of the direct effect, restated in the discussion",
    "1.4%", as_percent(pooled$estimate[pooled$model == "Fixed effects"]), 1L, FALSE, FALSE, NA,
    "The same pooled quantity the results section prints, and wrong in the same way and the same place.",
  "disc_inconsistent_sample_label", "The inconsistency rates come from the MTurk sample", NA,
    inconsistency$n_respondents[inconsistency$sample == "Original"], 0L, TRUE, FALSE, 0,
    "The 30 per cent is computed over the study 2 original sample, which the body text says was 'also recruited via Lucid' and which appendix table C.3 heads 'Lucid Original'. The trap_set indicator that defines inconsistency appears in no MTurk data file in the deposit, so the denominator recorded here is the one the rate is taken over.",
  "disc_correction_sample_label", "Table 4's top panel is the MTurk sample", NA,
    sum(t4_out$n[t4_out$sample == "Original" & t4_out$term == "Correction treatment"]), 0L,
    TRUE, FALSE, 0,
    "Table 4's top panel is fitted on the inconsistent subset of that same study 2 original sample, and its two column sample sizes add to the figure recorded here out of the 327 inconsistent respondents in it. The published row label reads 'N (MTurk)'.",
  "disc_spending_categories", "Spending items averaged into the study 4 outcome", "5",
    inconsistency$n_spending_items[inconsistency$sample == "Original"], 0L, FALSE, FALSE, NA, "",
  "disc_half_assigned", "Half of the inconsistent respondents were assigned the correction query", "2",
    inconsistency$n_inconsistent[inconsistency$sample == "Original"] /
      inconsistency$n_assigned_query[inconsistency$sample == "Original"], 0L, FALSE, FALSE, NA,
    "Recorded as the denominator the sentence implies: inconsistent respondents per respondent assigned the query.",
  "disc_correction_small", "Correction effect among small-government respondents, original sample", "-0.172",
    regression_value(t4_treatment, "Original", "Prefer small government", "Correction treatment", "estimate"),
    3L, FALSE, FALSE, NA,
    "The article gives this as the MTurk figure. It is computed from the study 2 original sample, which the body text and appendix table C.3 both identify as Lucid.",
  "disc_correction_small_replication", "Correction effect among small-government respondents, replication", "-0.153",
    regression_value(t4_treatment, "Replication", "Prefer small government", "Correction treatment", "estimate"),
    3L, FALSE, FALSE, NA, "",
  "disc_large_gov_imprecise", "Estimates are less precise among respondents preferring larger government", NA,
    sum(t4_precision$`Prefer large government` > t4_precision$`Prefer small government`), 0L, TRUE, FALSE,
    as.numeric(all(t4_precision$`Prefer large government` > t4_precision$`Prefer small government`)),
    "True in both samples.",
  "app_c_n_study_1", "Study 1 Lucid sample size, restated in the appendix", "2,524",
    demographic_n(b2_out, "Lucid"), 0L, FALSE, FALSE, NA, "",
  "app_c_n_study_2", "Study 2 replication sample size", "2,976",
    demographic_n(c3_out, "Replication"), 0L, FALSE, FALSE, NA, "",
  "app_c_welfare_original", "Welfare wording effect in the original sample, restated in prose", "0.59",
    abs(regression_value(c4_out, "Original", "Too Little on Welfare", "Welfare wording", "estimate")),
    2L, FALSE, FALSE, NA, "The appendix states the magnitude without its sign.",
  "app_c_welfare_replication", "The same effect in the replication, restated in prose", "0.46",
    abs(regression_value(c4_out, "Replication", "Too Little on Welfare", "Welfare wording", "estimate")),
    2L, FALSE, FALSE, NA, "",
  "app_d_outcomes_used", "Outcomes taken from Trump and White (2018)", "2",
    n_distinct(t3_out$outcome), 0L, FALSE, FALSE, NA, ""
) |>
  mutate(table_figure = "Text", value_script = NA_real_, .before = claim)

gt <- bind_rows(t1_rows, t2_rows, t3_rows, t4_rows, b2_rows, c3_rows, c4_rows, d5_rows,
                figure_rows, float_rows, text_rows, prose_rows)

# A published value with no rewrite counterpart stops the build ----
# A mislabelled join would otherwise leave value_rewrite empty, and the row would
# land in the unverifiable bucket looking like a quantity the pipeline does not
# produce rather than like a defect in this script.
orphaned <- gt |> filter(!is.na(value_paper), is.na(value_rewrite))
if (nrow(orphaned) > 0) {
  print(orphaned |> select(table_figure, claim, value_paper), n = nrow(orphaned))
  stop(str_glue("{nrow(orphaned)} published values have no rewrite counterpart."))
}

# Coverage against the published float list ----
# Taken from the article and the appendix rather than from what the pipeline
# happens to produce. The floats carrying no data are the theoretical diagrams,
# the enumeration of the 32 causal models, and the reproduced treatment images.
floats <- tribble(
  ~float, ~carries_data,
  "Figure 1", FALSE, "Figure 2", FALSE, "Figure 3", FALSE, "Figure 4", TRUE,
  "Table 1", TRUE, "Table 2", TRUE, "Table 3", TRUE, "Table 4", TRUE,
  "Table A.1", FALSE, "Figure A.1", FALSE,
  "Figure B.2", TRUE, "Figure B.3", TRUE, "Table B.2", TRUE,
  "Table C.3", TRUE, "Table C.4", TRUE,
  "Figure D.4", FALSE, "Table D.5", TRUE
)

uncovered <- setdiff(floats$float[floats$carries_data], gt$table_figure)
if (length(uncovered) > 0) {
  stop(str_glue("Published floats with no ground truth row: {str_c(uncovered, collapse = ', ')}"))
}

# Agreement at the precision the article prints ----
# A descriptive claim names a shape, a sign, a count or an ordering rather than a
# value, and an approximate claim names a rough magnitude the article itself
# hedges. Neither is testable by comparing printed digits, so match and
# match_rewrite stay NA on both and the descriptive verdict lives in holds. That
# is three states, not two, and the locus gate below reads all three.
gt <- gt |>
  mutate(
    paper_id = "coppock_green_2022",
    descriptive = replace_na(descriptive, FALSE),
    approximate = replace_na(approximate, FALSE),
    comparable = !descriptive & !approximate,
    match = if_else(comparable, agrees(value_script, value_paper), NA_real_),
    match_rewrite = if_else(comparable, agrees(value_rewrite, value_paper), NA_real_)
  )

# The nine quantities that come out of the unseeded bootstrap are draws, not
# fixed numbers, so agreement to printed precision is the wrong test for them.
# Each is scored against the range its own resampling produces at the deposit's
# sims = 10, which is the setting that shipped. The archive column takes the same
# test: the deposit runs the same estimator at the same setting with no seed.
stochastic <- bind_rows(
  expand_grid(sample_name = c("mturk", "elite", "lucid"),
              endpoint_name = c("conf_low", "conf_high")) |>
    mutate(claim_id = str_glue("s1_ratio_{sample_name}_{str_remove(endpoint_name, 'conf_')}") |>
             as.character(),
           range = map2(str_to_title(sample_name), endpoint_name,
                        ~ endpoint_range(if_else(.x == "Mturk", "MTurk", .x), .y, 10))),
  tibble(claim_id = c("s1_ratio_pooled", "s1_ratio_pooled_low", "s1_ratio_pooled_high"),
         range = map(c("estimate", "conf_low", "conf_high"), ~ pooled_range(.x, 10)))
) |>
  select(claim_id, range) |>
  left_join(select(gt, claim_id, value_paper, value_script), by = "claim_id") |>
  mutate(
    published = paper_number(value_paper),
    inside = as.numeric(published >= map_dbl(range, "low") & published <= map_dbl(range, "high"))
  )

stopifnot(nrow(stochastic) == 9, !any(is.na(stochastic$value_paper)))

gt <- gt |>
  rows_update(
    transmute(stochastic, claim_id,
              match = if_else(is.na(value_script), NA_real_, inside),
              match_rewrite = inside),
    by = "claim_id"
  )

# The pooled ratio is restated in the discussion, where the same number is wrong
# in the same way, so the discussion row takes the same verdict as the results
# row rather than being compared to printed precision a second time.
gt <- gt |>
  rows_update(
    tibble(claim_id = "disc_spillover_ratio",
           match_rewrite = gt$match_rewrite[gt$claim_id == "s1_ratio_pooled" &
                                              !is.na(gt$claim_id)]),
    by = "claim_id"
  )

# What the article prints, stored as the article prints it ----
# The comparison above needs a number; the committed file needs the string. Both
# requirements are met in sequence: compare numerically, then render value_paper
# back at the digit count each row already records, then write. The render is the
# stricter of the two checks, because a row whose digits is right about the value
# and wrong about the precision passes numeric equality and fails here.
normalize_paper <- function(x) {
  stripped <- str_remove_all(x, "[,%]")
  if_else(str_starts(stripped, fixed(".")), str_c("0", stripped),
          if_else(str_starts(stripped, fixed("-.")), str_replace(stripped, fixed("-."), "-0."),
                  stripped))
}

# A sign the article prints and sprintf does not reproduce is restored: a minus
# on a value that rounds to zero, and the plus the Lucid interval's upper
# endpoint carries on p. 732.
render_paper <- function(x, digits) {
  rendered <- sprintf(paste0("%.", digits, "f"), paper_number(x))
  sign <- replace_na(str_extract(normalize_paper(x), "^[-+]"), "")
  if_else(sign != "" & !str_starts(rendered, fixed(sign)), str_c(sign, rendered), rendered)
}

precision <- gt |>
  filter(!is.na(claim_id), !is.na(value_paper), comparable) |>
  mutate(rendered = render_paper(value_paper, digits),
         normalized = normalize_paper(value_paper)) |>
  filter(rendered != normalized)

if (nrow(precision) > 0) {
  print(precision |> select(claim_id, value_paper, rendered), n = nrow(precision))
  stop(str_glue("{nrow(precision)} rows record a digit count the published string does not have."))
}

# Where the fault lies on every row that carries an adverse verdict ----
# A locus is required whenever either verdict is zero, not only when the rewrite
# fails. match == 0 with match_rewrite == 1 is the signature of a deposit that no
# longer reproduces its own published table, and it needs naming as much as the
# other direction does. A descriptive claim carries its verdict in holds, so that
# column is read too. Each pattern names one failure; a row matching none stops
# the build.
locus_patterns <- tribble(
  ~pattern, ~defect_locus,
  "^ideology: ", "archive",
  "^s1_mturk_within_text$", "paper_internal",
  "^s1_graduate_", "paper_internal",
  "^s3_esj_replication_se$", "paper_internal",
  "^s2_nontarget_measures_text$", "paper_internal",
  "^disc_spillover_ratio$", "paper_internal",
  "_sample_label$", "paper_internal",
  "^s1_ratio_pooled", "paper_internal",
  "^s1_ratio_(mturk|elite|lucid)_(low|high)$", "paper_internal"
)

locus_for <- function(key) {
  hit <- locus_patterns$defect_locus[map_lgl(locus_patterns$pattern, ~ str_detect(key, .x))]
  if (length(hit) == 0) NA_character_ else hit[1]
}

adverse <- function(x) !is.na(x) & x == 0

gt <- gt |>
  mutate(
    locus_key = coalesce(claim_id, claim),
    defect_locus = if_else(adverse(match) | adverse(match_rewrite) | adverse(holds),
                           map_chr(locus_key, locus_for), NA_character_)
  )

unexplained <- gt |>
  filter(adverse(match) | adverse(match_rewrite) | adverse(holds), is.na(defect_locus))
if (nrow(unexplained) > 0) {
  print(unexplained |> select(claim_id, table_figure, claim, value_paper, value_rewrite,
                              match, match_rewrite, holds),
        n = nrow(unexplained))
  stop(str_glue("{nrow(unexplained)} rows carrying an adverse verdict have no recorded defect locus."))
}

stopifnot(all(is.na(gt$defect_locus[!adverse(gt$match) & !adverse(gt$match_rewrite) &
                                      !adverse(gt$holds)])))

# A note may not contradict its own verdict ----
# Generating value_rewrite closes one drift channel and leaves this one open: a
# note that says a value disagrees, on a row scored as agreeing.
contradiction <- gt |>
  filter(!is.na(match_rewrite), match_rewrite == 1,
         str_detect(notes, regex("disagree|does not match|differs|mismatch|cannot be reproduced|is wrong|lies outside",
                                 ignore_case = TRUE)))
if (nrow(contradiction) > 0) {
  print(contradiction |> select(claim, notes), n = nrow(contradiction))
  stop("A row scored as matching carries a note saying it does not.")
}

# Coverage, checked against the extraction and against the second instrument ----
# published_claims.csv is the hand-reviewed extraction of every numeric token in
# the article and the published appendix. value_paper is forced to character in
# every reader, here included: a column whose entries all look numeric is guessed
# as double, and that silently turns the article's 0.020 into 0.02 and destroys
# the printed precision every comparison here depends on.
claims <- read_csv(here::here("ground_truth", "published_claims.csv"),
                   col_types = cols(.default = col_character(),
                                    value_paper = col_character())) |>
  mutate(needs_block = needs_block == "TRUE")

stopifnot(!any(duplicated(claims$claim_id)), !any(duplicated(na.omit(gt$claim_id))))

# Every pipeline and descriptive claim needs a ground truth row, and every
# claim_id in the ground truth must name a claim the article actually makes.
required_rows <- claims$claim_id[claims$claim_type %in% c("pipeline", "descriptive")]
missing_gt_rows <- setdiff(required_rows, gt$claim_id)
unknown_gt_ids <- setdiff(na.omit(gt$claim_id), claims$claim_id)
if (length(missing_gt_rows) > 0) print(missing_gt_rows)
if (length(unknown_gt_ids) > 0) print(unknown_gt_ids)
stopifnot(length(missing_gt_rows) == 0, length(unknown_gt_ids) == 0)

# The extraction and the ground truth are two hand transcriptions of the same
# pages, and nothing else compares them.
transcription <- claims |>
  select(claim_id, extracted = value_paper) |>
  inner_join(select(gt, claim_id, value_paper), by = "claim_id") |>
  filter(!identical(TRUE, NA), xor(is.na(extracted), is.na(value_paper)) |
           (!is.na(extracted) & !is.na(value_paper) &
              normalize_paper(extracted) != normalize_paper(value_paper)))

if (nrow(transcription) > 0) {
  print(transcription, n = nrow(transcription))
  stop(str_glue("{nrow(transcription)} claims are transcribed differently in the extraction and the ground truth."))
}

# Errata spine gate ----
# Every claim id an errata entry names has to exist here. A missing one is a typo or a
# claim that has since been renamed, and a published correction pointing at a row that is
# not in the table is a dangling reference the build should refuse to carry.
#
# The spine is read before the second instrument rather than after it, because the
# verdict check below needs to know which claims the published note corrects. Only an
# entry of class quantity changes a number; the rest correct a label, a cross-reference
# or a word and leave every value where the article put it.
errata_spine <- read_csv(here::here("errata_entries.csv"), show_col_types = FALSE,
                         col_types = cols(.default = col_character()))

spine_ids <- function(rows) {
  ids <- rows$claim_ids |> str_split(";") |> unlist() |> str_trim()
  ids[!is.na(ids) & ids != ""]
}

cited_claim_ids <- spine_ids(errata_spine)
published_corrections <- spine_ids(filter(errata_spine, class == "quantity"))

if (length(setdiff(cited_claim_ids, gt$claim_id)) > 0) {
  print(setdiff(cited_claim_ids, gt$claim_id))
}
stopifnot(length(setdiff(cited_claim_ids, gt$claim_id)) == 0)

# Every quantity the note corrects has to be a quantity that differs. The claims file
# asserts the same property from the other side, on the value it computes itself; this
# asserts it on the value recorded here, which is reached by a different path.
uncorrected <- gt |> filter(claim_id %in% published_corrections, match_rewrite != 0)
if (nrow(uncorrected) > 0) {
  print(uncorrected |> select(claim_id, claim, value_paper, value_rewrite, match_rewrite),
        n = nrow(uncorrected))
  stop("The errata note corrects claims that reproduce as published.")
}

# And every quantity that differs because the article is wrong has to be one the note
# corrects. This is the direction that says the published note is COMPLETE, and it is
# the reason defect_locus is read here: a difference whose fault lies with the deposit or
# the environment is not something an erratum about the article could report.
unpublished <- gt |>
  filter(!is.na(claim_id), defect_locus == "paper_internal", adverse(match_rewrite),
         !claim_id %in% cited_claim_ids)
if (nrow(unpublished) > 0) {
  print(unpublished |> select(claim_id, table_figure, claim, value_paper, value_rewrite),
        n = nrow(unpublished))
  stop("Claims differ from the article through the article's own fault and no errata entry says so.")
}

# The second instrument is read as a program, not as text: a block that errors at
# runtime, or that ends in a bare expression and therefore prints nothing under
# source(), would satisfy any check that merely looked for its claim id in the
# file. It is sourced into its own environment because the two files necessarily
# read the same outputs and name objects for what they hold, so a bare source()
# would replace this script's own objects with the claims file's and the gate
# would then run against the wrong ones.
claims_log <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env())
)

printed <- str_match(claims_log, "^CLAIM (\\S+) = (\\S+) \\|\\| \\[(\\w+)\\] (.*)$")
printed <- tibble(claim_id = printed[, 2], printed_value = printed[, 3],
                  verdict = printed[, 4], label = printed[, 5]) |>
  filter(!is.na(claim_id))

declared <- claims$claim_id[claims$needs_block]
if (!setequal(declared, printed$claim_id)) {
  print(list(declared_not_printed = setdiff(declared, printed$claim_id),
             printed_not_declared = setdiff(printed$claim_id, declared)))
}
stopifnot(
  !any(duplicated(printed$claim_id)),
  nrow(printed) == length(declared),
  length(setdiff(declared, printed$claim_id)) == 0,
  length(setdiff(printed$claim_id, declared)) == 0
)

# Two instruments, compared value by value. build_ground_truth.R and
# in_text_claims.R reach each number by separate paths from the same outputs, so
# a disagreement between them is a finding in its own right.
cross_check <- printed |>
  inner_join(select(gt, claim_id, value_rewrite, digits), by = "claim_id") |>
  mutate(
    gt_printed = if_else(is.na(value_rewrite), "NA",
                         sprintf(paste0("%.", digits, "f"), value_rewrite)),
    agrees = printed_value == gt_printed
  )

if (any(!cross_check$agrees)) print(cross_check |> filter(!agrees), n = Inf, width = 200)
stopifnot(length(setdiff(required_rows, cross_check$claim_id)) == 0,
          all(cross_check$agrees))

# Two instruments, compared verdict by verdict ----
# The cross-check above puts the two files on the same number. This puts them on the
# same STATEMENT ABOUT that number, which is the half that used to live only here.
# in_text_claims.R decides what to expect of a claim from the extraction and the errata
# spine; this script decides it from its own descriptive and approximate flags, its
# stochastic list and the comparison it performs itself. Neither can see the other's
# reasoning, so a claim the two classify differently is a finding of the same kind as a
# value they compute differently.
#
# The claims file's five unasserted verdicts collapse to two questions here: is the claim
# comparable to a printed digit at all, and is it one of the nine whose value is a
# bootstrap draw scored against a range instead.
expected_verdict <- gt |>
  filter(!is.na(claim_id), claim_id %in% printed$claim_id) |>
  mutate(
    corrected = claim_id %in% published_corrections,
    expected = case_when(
      claim_id %in% stochastic$claim_id & !corrected ~ "range",
      !comparable ~ "unasserted",
      corrected ~ "corrected",
      match_rewrite == 1 ~ "match",
      TRUE ~ "MISMATCH"
    )
  ) |>
  select(claim_id, table_figure, claim, value_paper, value_rewrite, comparable,
         match_rewrite, expected)

verdict_check <- printed |>
  select(claim_id, printed_value, verdict) |>
  inner_join(expected_verdict, by = "claim_id") |>
  mutate(
    # The claims file distinguishes three reasons for not asserting; this script has one
    # flag for all three, so the comparison is made at the coarser of the two grains.
    verdict_here = if_else(verdict %in% c("hedged", "shape", "derived", "absent"),
                           "unasserted", verdict),
    # The second instrument may assert where this script declines to compare, but never the
    # reverse. A descriptive claim that prints a truth value, or one whose hedge the
    # pipeline happens to hit exactly, is comparable there and not here, and an assertion
    # this script cannot make is a strengthening rather than a disagreement. A claim this
    # script DOES compare and the claims file declines to assert is the direction that
    # loses coverage, and it still fails.
    agrees = verdict_here == expected | (expected == "unasserted" & verdict == "match")
  )

if (any(!verdict_check$agrees)) {
  print(verdict_check |> filter(!agrees), n = Inf, width = 250)
  stop(str_glue("{sum(!verdict_check$agrees)} claims are classified differently by the ",
                "two instruments."))
}
stopifnot(nrow(verdict_check) == nrow(printed))

# Claim type and printed precision, carried from the extraction ----
# A row with a claim id takes both from published_claims.csv. A float cell has no
# extraction row of its own (the extraction summarises each float in one coverage
# row), so it is a pipeline quantity and its precision is read off the string the
# article prints, unless the row above already carries a digit count of its own.
gt <- gt |>
  left_join(select(claims, claim_id, claim_type, extracted_digits = digits,
                   extracted_location = location),
            by = "claim_id") |>
  mutate(
    extracted_digits = as.integer(extracted_digits),
    digits = as.integer(digits)
  )

# The two transcriptions of a claim's precision have to agree wherever both exist.
digit_disagreement <- gt |>
  filter(!is.na(claim_id), !is.na(digits), !is.na(extracted_digits),
         digits != extracted_digits)
if (nrow(digit_disagreement) > 0) {
  print(digit_disagreement |> select(claim_id, value_paper, digits, extracted_digits))
  stop(str_glue("{nrow(digit_disagreement)} claims carry a digit count the extraction does not."))
}

gt <- gt |>
  mutate(
    digits = if_else(is.na(claim_id), digits, extracted_digits),
    digits = if_else(is.na(digits), excheckr::claim_digits(value_paper), digits),
    claim_type = if_else(is.na(claim_id), "pipeline", claim_type)
  )

stopifnot(!anyNA(gt$claim_type), !anyNA(gt$extracted_location[!is.na(gt$claim_id)]))

# Disposition: what became of each defect ----
# corrected_here is computed from the errata spine and never typed. Every other
# judgment is taken from what the README already says happened, keyed the way
# defect_locus is keyed; a defect nobody has judged stays NA.
disposition_judgments <- tribble(
  ~pattern, ~judgment, ~disposition_reason,
  "^ideology: ", "documented_only",
    "The README describes the deposit's dplyr::recode failure and the rewrite rebuilds the column from the wave 1 raw file; the published table is right, so the article needs no erratum."
)

judgment_for <- function(key, column) {
  hit <- disposition_judgments[[column]][map_lgl(disposition_judgments$pattern,
                                                 ~ str_detect(key, .x))]
  if (length(hit) == 0) NA_character_ else hit[1]
}

gt <- gt |>
  mutate(
    corrected_here = !is.na(claim_id) & claim_id %in% cited_claim_ids,
    disposition = case_when(
      is.na(defect_locus) ~ NA_character_,
      corrected_here ~ "corrected_here",
      .default = map_chr(locus_key, judgment_for, column = "judgment")
    ),
    disposition_reason = if_else(
      !is.na(disposition) & disposition != "corrected_here",
      map_chr(locus_key, judgment_for, column = "disposition_reason"),
      NA_character_
    )
  )

undisposed <- gt |> filter(defect_locus == "paper_internal", is.na(disposition))
if (nrow(undisposed) > 0) {
  print(undisposed |> select(claim_id, table_figure, claim), n = nrow(undisposed))
  stop(str_glue("{nrow(undisposed)} paper_internal rows carry no disposition."))
}
print(gt |> filter(!is.na(defect_locus)) |> count(defect_locus, disposition))

# Float coverage ----
# One row per published float. A table's published_numbers is the cell count the
# article prints, the value_paper of its coverage row; covered counts the cells
# compared above, and the two reproduced columns count the cells each pipeline
# matches. A figure prints no numbers, so what it carries is a count of plotted
# estimates, reproduced when the rewrite plots as many as the article does.
float_cells <- gt |>
  filter(table_figure %in% floats$float, str_starts(table_figure, "Table"),
         is.na(claim_id) | !str_starts(claim_id, "float_"))

cell_counts <- float_cells |>
  summarize(
    covered = sum(!is.na(value_paper)),
    reproduced_by_rewrite = sum(match_rewrite == 1, na.rm = TRUE),
    reproduced_by_archive = if_else(all(is.na(match)), NA_integer_,
                                    sum(match == 1, na.rm = TRUE)),
    .by = table_figure
  )

coverage_rows <- gt |>
  filter(str_starts(claim_id, "float_")) |>
  transmute(table_figure, coverage_paper = as.integer(value_paper),
            coverage_reproduced = match_rewrite == 1)

no_data_notes <- c(
  "Figure 1" = "A theoretical diagram; prints no numbers.",
  "Figure 2" = "A theoretical diagram; prints no numbers.",
  "Figure 3" = "A theoretical diagram; prints no numbers.",
  "Table A.1" = "Enumerates the 32 causal models; carries no data.",
  "Figure A.1" = "A causal graph elaborating the basic model; prints no numbers.",
  "Figure D.4" = "Reproduces the treatment images; prints no numbers."
)

float_coverage <- floats |>
  left_join(cell_counts, by = c("float" = "table_figure")) |>
  left_join(coverage_rows, by = c("float" = "table_figure")) |>
  mutate(
    is_figure = str_starts(float, "Figure"),
    published_numbers = if_else(carries_data & !is_figure, coverage_paper, 0L),
    covered = replace_na(covered, 0L),
    reproduced_by_rewrite = replace_na(reproduced_by_rewrite, 0L),
    reproduced_by_archive = if_else(carries_data & !is_figure, reproduced_by_archive, 0L),
    plotted = if_else(carries_data & is_figure, coverage_paper, NA_integer_),
    plotted_reproduced = if_else(carries_data & is_figure,
                                 if_else(coverage_reproduced, coverage_paper, 0L), NA_integer_),
    note = case_when(
      !carries_data ~ unname(no_data_notes[float]),
      is_figure ~ "Prints no numbers on its face; plotted counts 16 outcomes by 4 treatments by the samples the panel carries.",
      .default = NA_character_
    )
  ) |>
  select(float, published_numbers, covered, reproduced_by_rewrite, reproduced_by_archive,
         plotted, plotted_reproduced, note)

# Every table carrying data has to be covered cell for cell.
stopifnot(
  nrow(float_coverage) == nrow(floats),
  !anyNA(float_coverage$note[!floats$carries_data]),
  with(filter(float_coverage, str_starts(float, "Table"), published_numbers > 0),
       all(covered == published_numbers))
)

write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))
print(float_coverage, n = nrow(float_coverage), width = 200)

# The ground truth, in the canonical column order ----
# A row the extraction names takes the extraction's location; a float cell keeps the
# float's label as printed.
gt <- gt |>
  mutate(across(c(match, match_rewrite, holds), as.logical),
         location = if_else(is.na(claim_id), table_figure, extracted_location)) |>
  select(paper_id, claim_id, location, claim_type, claim, value_paper,
         digits, value_script, match, value_rewrite, match_rewrite, holds, defect_locus,
         disposition, disposition_reason, notes)

write_csv(gt, here::here("ground_truth", "coppock_green_2022_ground_truth.csv"))

print(gt |> select(claim_id, location, claim, value_script, value_paper, match,
                   value_rewrite, match_rewrite, holds, defect_locus, disposition),
      n = nrow(gt), width = 250)

print(tibble(
  quantity = c("rows", "archive comparable", "archive matching", "archive differing",
               "rewrite comparable", "rewrite matching", "rewrite differing",
               "descriptive claims judged", "descriptive claims holding",
               "extraction rows", "extraction rows needing a block",
               "claims printed by in_text_claims.R",
               "claims compared across the two instruments",
               "claims classified across the two instruments",
               "claims the second instrument asserts",
               "claims the second instrument declines to assert",
               "quantities the errata note corrects"),
  value = c(nrow(gt), sum(!is.na(gt$match)), sum(gt$match == 1, na.rm = TRUE),
            sum(gt$match == 0, na.rm = TRUE),
            sum(!is.na(gt$match_rewrite)), sum(gt$match_rewrite == 1, na.rm = TRUE),
            sum(gt$match_rewrite == 0, na.rm = TRUE),
            sum(!is.na(gt$holds)), sum(gt$holds == 1, na.rm = TRUE),
            nrow(claims), length(declared), nrow(printed), nrow(cross_check),
            nrow(verdict_check),
            sum(verdict_check$verdict %in% c("match", "corrected")),
            sum(!verdict_check$verdict %in% c("match", "corrected")),
            length(published_corrections))
), n = Inf)
