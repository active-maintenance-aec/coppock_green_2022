# coppock_green_2022/ground_truth/run_original_archive.R
# Output: ground_truth/archive_values.csv, ground_truth/archive_run_status.csv,
#   ground_truth/archive_overwrites.csv
# Depends on: original/ (run download_original.R first)
# Description: Runs every deposited script and reads the numbers they write back
#   out of their own LaTeX output, so the archive column of the ground truth is
#   measured rather than transcribed.
#
#   Three things this script has to work around. The deposit is flat, but every
#   script addresses programs/analysis/, programs/cleaning/, programs/helpers/,
#   data/raw/ and data/clean/ and writes to ../Drafts/tables/ and
#   ../Drafts/figures/, so the tree has to be rebuilt before anything runs. One
#   cleaning script reads data/raw/Trump605/, a folder the deposit flattens away,
#   so that has to be rebuilt too. And the archive writes into that tree, which is
#   why the whole thing is assembled in a temporary directory: several deposited
#   scripts write files whose names are deposited files, so running the deposit
#   inside original/ would overwrite part of the deposit.
#
#   Every script is run twice. The stripped pass gives the deposit only its raw
#   data and its code, which is the only way to see whether a script passes on its
#   own work or on an intermediate the deposit ships. The deposited pass restores
#   those intermediates and is the run the ground truth reads. The two exit
#   statuses are recorded per script, one row per script per pass, with the
#   deposited pass named as_shipped.
#
#   This script is not part of run_all.R. It needs the packages the deposit was
#   written against (stargazer, xtable, coefplot, psych, car, rmeta), which the
#   maintained rewrite no longer uses, and its output is committed so that
#   build_ground_truth.R can read it without them.

library(here)
library(tidyverse)

here::i_am("ground_truth/run_original_archive.R")

deposit <- here::here("original")

# Files the deposit ships that deposited code also writes ----
# Six analysis-ready data sets plus one factor-analysis cache. Under the stripped
# pass these are withheld, so a script that needs one has to make it first.
generated_clean <- c("study_1_mturk_cleaned.rds", "study_1_elite_cleaned.rds",
                     "study_2_lucid_clean.rds", "original_trump_white_clean.rds",
                     "replication_trump_white_clean.rds",
                     "studies_1_2_replications_lucid_cleaned.rds")
generated_raw <- "factor_analysis_models.rdata"

cleaning_scripts <- c("study_1_mturk_cleaning.R", "study_1_elite_cleaning.R",
                      "study_2_lucid_cleaning.R", "study_3_original_cleaning.R",
                      "study_3_lucid_cleaning.R",
                      "studies_1_2_replications_lucid_cleaning.R")

analysis_scripts <- c("in_text_numbers.R", "study_1_figure_4.R", "study_1_table_1.R",
                      "study_2_table_2.R", "study_3_table_3.R", "study_4_table_4.R",
                      "appendix_figure_B2.R", "appendix_figure_B3.R",
                      "appendix_table_B2.R", "appendix_table_C3.R",
                      "appendix_table_C4.R", "appendix_table_D5.R")

# Rebuild the tree the deposit assumes ----
stage <- function(label, include_generated) {
  # normalizePath, because tempdir() can carry a doubled slash and macOS resolves
  # it through /private, either of which would defeat the path scrubbing below.
  root <- file.path(normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
                    paste0("coppock_green_2022_", label))
  unlink(root, recursive = TRUE)
  archive <- file.path(root, "archive")
  walk(
    file.path(archive, c("programs/cleaning", "programs/analysis", "programs/helpers",
                         "data/raw/Trump605", "data/clean")),
    dir.create, recursive = TRUE
  )
  walk(file.path(root, c("Drafts/tables", "Drafts/figures")), dir.create, recursive = TRUE)

  file.copy(file.path(deposit, "helper_functions.R"), file.path(archive, "programs/helpers"))
  file.copy(file.path(deposit, analysis_scripts), file.path(archive, "programs/analysis"))
  file.copy(file.path(deposit, cleaning_scripts), file.path(archive, "programs/cleaning"))

  raw_files <- c("dv_df.rds",
                 basename(list.files(deposit, pattern = "\\.(csv|xlsx)$")))
  file.copy(file.path(deposit, raw_files), file.path(archive, "data/raw"))

  # The deposit flattens the TESS folder the cleaning script addresses.
  file.copy(file.path(deposit, "TESS3 174 - Trump_Client.sav"),
            file.path(archive, "data/raw/Trump605"))

  # The four .rds files no deposited script reads travel with the clean data.
  unread <- c("dc_replication_cleaned.rds", "elite_opeds_cleaned.rds",
              "lucid_cleaned.rds", "mturk_opeds_cleaned.rds")
  file.copy(file.path(deposit, unread), file.path(archive, "data/clean"))

  if (include_generated) {
    file.copy(file.path(deposit, generated_clean), file.path(archive, "data/clean"))
    file.copy(file.path(deposit, generated_raw), file.path(archive, "data/raw"))
  }

  list(root = root, archive = archive)
}

# Run each script in its own process ----
# Every deposited script opens with rm(list = ls()), which would empty this
# session's environment if they were sourced into it.
run_one <- function(tree, script, subdir) {
  log_file <- file.path(tree$root, str_replace(script, "\\.R$", ".log"))
  # print.eval reproduces what a reader running the script interactively sees.
  # Several deposited scripts end in bare object names and in_text_numbers.R
  # reports its meta-analysis that way, so without it those numbers never appear.
  expr <- paste0("set.seed(20220401); setwd(", deparse(tree$archive), "); source(",
                 deparse(file.path("programs", subdir, script)),
                 ", print.eval = TRUE)")
  status <- system2(file.path(R.home("bin"), "Rscript"), args = c("-e", shQuote(expr)),
                    stdout = log_file, stderr = log_file)
  # An rlang condition puts its detail on the lines after "Error in `mutate()`:",
  # so the message is taken as a short block rather than a single line, with the
  # scratch tree's path removed so the record is not machine specific.
  lines <- read_lines(log_file)
  first <- which(str_starts(lines, "Error"))
  message <- if (length(first) == 0) {
    NA_character_
  } else {
    lines[first[1]:min(first[1] + 3, length(lines))] |>
      discard(~ str_detect(.x, "Execution halted")) |>
      str_c(collapse = " ") |>
      # Both spellings of the scratch path are removed, so nothing machine
      # specific reaches the committed record.
      str_replace_all(fixed(tree$archive), "<archive>") |>
      str_replace_all(fixed(str_remove(tree$archive, "^/private")), "<archive>") |>
      str_squish() |>
      str_trunc(220)
  }
  tibble(script = script, exit_status = status, error = message, log = log_file)
}

run_all_scripts <- function(tree) {
  bind_rows(
    map(cleaning_scripts, ~ run_one(tree, .x, "cleaning")) |> bind_rows(),
    map(analysis_scripts, ~ run_one(tree, .x, "analysis")) |> bind_rows()
  )
}

# The stripped pass ----
stripped_tree <- stage("stripped", include_generated = FALSE)
stripped <- run_all_scripts(stripped_tree)

# What the deposit's own cleaning code rebuilds, and whether it rebuilds it ----
# A cleaning script that fails on its last line can still have written earlier
# files, so this is checked per file rather than inferred from an exit status.
same_object <- function(a, b) {
  isTRUE(all.equal(a, b, check.attributes = FALSE, tolerance = 1e-12))
}

matches_deposited_copy <- function(file, path) {
  if (!file.exists(path)) return(NA)
  if (str_ends(file, "\\.rds")) {
    same_object(read_rds(path), read_rds(file.path(deposit, file)))
  } else {
    fresh <- new.env(); deposited <- new.env()
    load(path, envir = fresh); load(file.path(deposit, file), envir = deposited)
    same_object(mget(ls(fresh), fresh), mget(ls(deposited), deposited))
  }
}

generated_path <- function(tree) {
  c(file.path(tree$archive, "data/clean", generated_clean),
    file.path(tree$archive, "data/raw", generated_raw))
}

generated_files <- tibble(file = c(generated_clean, generated_raw)) |>
  mutate(
    stripped_rebuilt = file.exists(generated_path(stripped_tree)),
    stripped_matches_deposit = map2_lgl(file, generated_path(stripped_tree),
                                        matches_deposited_copy)
  )

# The deposited pass ----
# The deposited copies of these seven files start the pass byte-identical to the
# deposit, so a changed modification time is the record of a deposited file being
# written over. Nothing else measures that, and it is the reason the deposit is
# never run in place.
deposited_tree <- stage("deposited", include_generated = TRUE)
mtime_before <- file.mtime(generated_path(deposited_tree))

deposited <- run_all_scripts(deposited_tree)

generated_files <- generated_files |>
  mutate(
    deposited_overwritten = file.mtime(generated_path(deposited_tree)) != mtime_before,
    deposited_matches_deposit = map2_lgl(file, generated_path(deposited_tree),
                                         matches_deposited_copy)
  )

print(generated_files)

# The two archive tables, in the canonical long form ----
# Every shipped intermediate has a row under the stripped pass: rebuilt if the deposit's
# own code wrote it again, withheld if it did not. Under the as-shipped pass a file has a
# row only where the run wrote over it.
script_roles <- tibble(
  script = c(cleaning_scripts, analysis_scripts),
  role = rep(c("cleaning", "analysis"),
             times = c(length(cleaning_scripts), length(analysis_scripts)))
)

run_status <- bind_rows(stripped = stripped, as_shipped = deposited, .id = "pass") |>
  left_join(script_roles, by = "script") |>
  transmute(pass, script, role, exit_status,
            outcome = if_else(exit_status == 0, "clean", "error"),
            stopped_at = error)

stopifnot(nrow(run_status) == 2 * nrow(script_roles), !anyNA(run_status$role))

overwrites <- bind_rows(
  generated_files |>
    transmute(pass = "stripped", file,
              effect = if_else(stripped_rebuilt, "rebuilt", "withheld"),
              matches_deposit = if_else(stripped_rebuilt, stripped_matches_deposit, NA)),
  generated_files |>
    filter(deposited_overwritten) |>
    transmute(pass = "as_shipped", file, effect = "overwritten",
              matches_deposit = deposited_matches_deposit)
)

write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))
write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))
print(run_status |> select(pass, script, role, outcome), n = nrow(run_status))
print(overwrites)

if (any(deposited$exit_status[deposited$script %in% analysis_scripts] != 0)) {
  failed <- deposited |> filter(script %in% analysis_scripts, exit_status != 0)
  walk(failed$log, ~ print(read_lines(.x)))
  stop("A deposited analysis script failed against the archive as deposited.")
}

# Read the numbers back out of the LaTeX the deposit wrote ----
tables <- file.path(deposited_tree$root, "Drafts", "tables")

clean_cell <- function(x) {
  x |>
    str_remove_all(fixed("$^{*}$")) |>
    str_replace_all(fixed("$-$"), "-") |>
    str_remove_all("\\\\%") |>
    str_remove_all(",") |>
    str_remove_all("[()]") |>
    str_trim()
}

body_rows <- function(file) {
  read_lines(file) |>
    keep(~ str_detect(.x, fixed("\\\\"))) |>
    str_remove("\\\\\\\\\\s*$") |>
    str_split(fixed("&")) |>
    map(str_trim) |>
    keep(~ length(.x) > 1 && !str_detect(.x[1], "multicolumn|\\[-1\\.8ex\\]"))
}

# In a stargazer table an unlabelled row carries the standard errors of the row
# above it, and the N row is labelled N.
parse_stargazer <- function(file) {
  rows <- body_rows(file)
  parsed <- list()
  current <- NA_character_
  for (row in rows) {
    label <- row[1]
    values <- suppressWarnings(as.numeric(clean_cell(row[-1])))
    if (all(is.na(values))) next
    if (label == "") {
      parsed[[length(parsed) + 1]] <-
        tibble(term = current, stat = "std.error", column = seq_along(values), value = values)
    } else if (label == "N") {
      parsed[[length(parsed) + 1]] <-
        tibble(term = "N", stat = "n", column = seq_along(values), value = values)
    } else {
      current <- label
      parsed[[length(parsed) + 1]] <-
        tibble(term = label, stat = "estimate", column = seq_along(values), value = values)
    }
  }
  bind_rows(parsed) |> mutate(file = basename(file), .before = 1)
}

parse_xtable <- function(file) {
  body_rows(file) |>
    map(function(row) {
      values <- suppressWarnings(as.numeric(clean_cell(row[-1])))
      tibble(term = row[1], stat = "value", column = seq_along(values), value = values)
    }) |>
    bind_rows() |>
    mutate(file = basename(file), .before = 1)
}

stargazer_files <- c("study_2_ssm.tex", "study_2_replication_ssm.tex", "study_3.tex",
                     "study_3_replication.tex", "study_4_correction.tex",
                     "study_4_correction_replication.tex", "welfare.tex",
                     "welfare_replication.tex")

xtable_files <- setdiff(basename(list.files(tables, pattern = "\\.tex$")), stargazer_files)

archive_values <- bind_rows(
  map(file.path(tables, stargazer_files), parse_stargazer) |> bind_rows(),
  map(file.path(tables, xtable_files), parse_xtable) |> bind_rows()
) |>
  filter(!is.na(value)) |>
  arrange(file, term, stat, column)

# The pooled estimate, and what replacing rmeta with metafor costs ----
# in_text_numbers.R writes no file, so its one number has to be taken from a
# session that has run it. That same session has the two pooling functions and the
# identical inputs in hand, which is the only place the substitution the rewrite
# makes can be measured rather than asserted. The deposit sets no seed, so one is
# set here and the recorded value is one draw of an unseeded procedure.
pool_log <- file.path(deposited_tree$root, "pooling_comparison.log")
pool_expr <- paste0(
  "set.seed(20220401); setwd(", deparse(deposited_tree$archive), "); ",
  "source('programs/analysis/in_text_numbers.R'); ",
  "d <- as.numeric(c(mturk_est[1, 3], elite_est[1, 3], lucid_est[1, 3])); ",
  "se <- as.numeric(c(mturk_uncertainty[3, 2], elite_uncertainty[3, 2], lucid_uncertainty[3, 2])); ",
  "m <- rmeta::meta.summaries(d = d, se = se); ",
  "f <- metafor::rma(yi = d, sei = se, method = 'FE'); ",
  "z <- qnorm(0.975); ",
  "cat(sprintf('POOL rmeta %.17g %.17g %.17g %.17g\\n', m$summary, m$se.summary, ",
  "m$summary - z * m$se.summary, m$summary + z * m$se.summary)); ",
  "cat(sprintf('POOL metafor %.17g %.17g %.17g %.17g\\n', as.numeric(f$beta), f$se, f$ci.lb, f$ci.ub))"
)
pool_status <- system2(file.path(R.home("bin"), "Rscript"), args = c("-e", shQuote(pool_expr)),
                       stdout = pool_log, stderr = pool_log)
if (pool_status != 0) {
  print(read_lines(pool_log))
  stop("The pooling comparison run failed.")
}

pool_numbers <- function(package) {
  read_lines(pool_log) |>
    keep(~ str_starts(.x, paste("POOL", package))) |>
    str_remove(paste("POOL", package, "")) |>
    str_split("\\s+") |>
    pluck(1) |>
    as.numeric()
}

pool_rmeta <- pool_numbers("rmeta")
pool_metafor <- pool_numbers("metafor")
stopifnot(length(pool_rmeta) == 4, length(pool_metafor) == 4)

archive_values <- bind_rows(
  archive_values,
  tibble(
    file = "in_text_numbers.R, pooling comparison",
    term = c("Pooled indirect-to-direct ratio",
             "Pooled ratio, 95% interval lower bound",
             "Pooled ratio, 95% interval upper bound",
             "rmeta versus metafor, largest absolute difference"),
    stat = c("estimate", "conf_low", "conf_high", "difference"),
    column = 1,
    value = c(pool_rmeta[1], pool_rmeta[3], pool_rmeta[4],
              max(abs(pool_rmeta - pool_metafor)))
  )
)

write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

print(archive_values, n = nrow(archive_values))
print(str_glue("{nrow(archive_values)} values read from {n_distinct(archive_values$file)} deposited outputs."))
