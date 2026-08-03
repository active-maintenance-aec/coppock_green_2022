# coppock_green_2022/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# then every published table and figure, then the in-text quantities.
# Every script is self-contained and can also be run on its own, except that the
# three scripts noted below read another script's output.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Tables ----
source(here::here("maintained", "table_1_static_constraint_correlations.R"))
source(here::here("maintained", "table_2_employment_discrimination.R"))
source(here::here("maintained", "table_3_inequality.R"))
source(here::here("maintained", "table_4_correction_experiment.R"))
source(here::here("maintained", "table_b2_sample_demographics.R"))
source(here::here("maintained", "table_c3_study2_demographics.R"))
source(here::here("maintained", "table_c4_welfare_experiment.R"))
source(here::here("maintained", "table_d5_study3_demographics.R"))

# Figures ----
source(here::here("maintained", "figure_4_dynamic_constraint_estimates.R"))
source(here::here("maintained", "figure_b2_wave2_estimates.R"))
source(here::here("maintained", "figure_b3_wave3_estimates.R"))

# In-text quantities ----
# text_significance_counts.R and text_amtrak_largest_effects.R read the wave 1
# estimates that figure 4 writes, so they run after it.
source(here::here("maintained", "text_significance_counts.R"))
source(here::here("maintained", "text_amtrak_largest_effects.R"))
source(here::here("maintained", "text_inconsistency_rates.R"))
source(here::here("maintained", "text_sample_descriptions.R"))
source(here::here("maintained", "text_study_2_correlations.R"))

# text_precision_weighted_averages.R bootstraps 1,000 resamples of three samples
# and takes around six minutes; text_pooled_ratio_metaanalysis.R reads its output.
source(here::here("maintained", "text_precision_weighted_averages.R"))
source(here::here("maintained", "text_pooled_ratio_metaanalysis.R"))

# Deposited archive, again ----
# The check at the top is a precondition. A script that damages original/ part way
# through a run would pass it and go unnoticed until the next run, which may be
# months away, so the archive is verified again once everything has run.
source(here::here("download_original.R"))

# Ground truth ----
# Rebuilds the comparison against the published article from the outputs above,
# so it cannot go stale. The deposited archive's own numbers come from
# ground_truth/archive_values.csv, which ground_truth/run_original_archive.R
# produces; that script needs the packages the deposit was written against and
# is deliberately not part of this run.
#
# build_ground_truth.R also runs maintained/in_text_claims.R under capture.output
# to check coverage, so the run below is the second of two and exists to put the
# claim lines in the console log where a reader can see them.
source(here::here("ground_truth", "build_ground_truth.R"))

# In-text claims ----
# One block per claim the article makes in prose, each carrying the article's own
# sentence and recomputing the number from maintained/output/ by a path of its
# own. Where this file and build_ground_truth.R disagree, one of them is wrong.
source(here::here("maintained", "in_text_claims.R"))
