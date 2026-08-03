# coppock_green_2022/maintained/in_text_claims.R
# Output: printed CLAIM lines only; no files
# Depends on: maintained/output/, ground_truth/published_claims.csv, helpers.R
# Description: One block per claim the published article makes, each carrying the
#   article's own sentence verbatim and then recomputing the number from the
#   pipeline's committed output. This is the second instrument. It never reads
#   ground_truth/coppock_green_2022_ground_truth.csv, it never fits a model, and
#   it reaches every quantity by its own path, so where it and
#   build_ground_truth.R disagree one of them is wrong.
#
#   Every block prints a line of the form
#
#       CLAIM <claim_id> = <value> || <label>
#
#   which is what build_ground_truth.R parses to check coverage against
#   ground_truth/published_claims.csv. The value is printed at the precision the
#   article prints, so it can be laid against the page without re-rounding.
#
#   Two reads are not from maintained/output/. published_claims.csv is the
#   extraction, which the coverage rules allow, and a block that has to state a
#   threshold the article names ("on the order of 10% to 20%") takes it from
#   there rather than typing it. Everything else comes out of maintained/output/.
#
#   Transcription note: the published PDF's text layer renders the minus sign as
#   the digit 2, the equals sign as p, and the less-than sign as an exclamation
#   mark. Every quote below was checked against a rendered page, and the quotes
#   are what the article prints today, however wrong. Where a sentence is wrong
#   that is recorded in the ground truth and in coppock_green_2022_errata.pdf,
#   never by editing the quote.

source(here::here("maintained", "helpers.R"))

options(width = 200)

out <- function(file) read_csv(file.path(out_dir, file), show_col_types = FALSE)

t1 <- out("table_1_static_constraint_correlations.csv")
t2 <- out("table_2_employment_discrimination.csv")
t3 <- out("table_3_inequality.csv")
t4 <- out("table_4_correction_experiment.csv")
b2 <- out("table_b2_sample_demographics.csv")
c3 <- out("table_c3_study2_demographics.csv")
c4 <- out("table_c4_welfare_experiment.csv")
d5 <- out("table_d5_study3_demographics.csv")
f4 <- out("figure_4_dynamic_constraint_estimates.csv")
fb2 <- out("figure_b2_wave2_estimates.csv")
fb3 <- out("figure_b3_wave3_estimates.csv")
pwa_results <- out("text_precision_weighted_averages.csv")
pooled <- out("text_pooled_ratio_metaanalysis.csv")
counts <- out("text_significance_counts.csv")
amtrak <- out("text_amtrak_largest_effects.csv")
inconsistency <- out("text_inconsistency_rates.csv")
descriptions <- out("text_sample_descriptions.csv")
study_2_cors <- out("text_study_2_correlations.csv")

published <- read_csv(here::here("ground_truth", "published_claims.csv"),
                      col_types = cols(.default = col_character(),
                                       value_paper = col_character()))

# claim: print one claim line in the form the coverage gate parses. digits is the
# number of decimal places the article prints, so a value printed here can be laid
# against the page without being rounded a second time.
claim <- function(id, value, label, digits = 0) {
  printed <- if (length(value) != 1 || is.na(value)) {
    "NA"
  } else {
    sprintf(paste0("%.", digits, "f"), value)
  }
  cat("CLAIM ", id, " = ", printed, " || ", label, "\n", sep = "")
}

# stated: the article's own transcribed value for a claim, used only where the
# sentence names a threshold and the block has to compare against it. Reading the
# extraction is not reading the comparison.
stated <- function(id) {
  value <- published$value_paper[published$claim_id == id]
  stopifnot(length(value) == 1, !is.na(value))
  as.numeric(str_remove_all(value, "[,%]"))
}

# one: pull a single value out of a filtered frame, asserting it is a scalar. A
# widened output file would otherwise turn a scalar into a vector and a claim into
# a silently empty line.
one <- function(x) {
  stopifnot(length(x) == 1, !is.na(x))
  x
}

share <- function(sample_name, quantity_name) {
  100 * one(descriptions$share_of_sample[descriptions$sample == sample_name &
                                           descriptions$quantity == quantity_name])
}

pwa_value <- function(sample_name, quantity_name, column) {
  one(pwa_results[[column]][pwa_results$sample == sample_name &
                              pwa_results$quantity == quantity_name])
}

pct_cell <- function(tab, sample_col, variable_name, category_name) {
  cell <- tab[[sample_col]][tab$variable == variable_name & tab$category == category_name]
  as.numeric(str_remove(one(cell), "%"))
}

t2_value <- function(sample_name, outcome_name, term_name, column) {
  one(t2[[column]][t2$sample == sample_name & t2$outcome == outcome_name &
                     t2$term == term_name])
}

t3_value <- function(sample_name, outcome_name, term_name, column) {
  one(t3[[column]][t3$sample == sample_name & str_starts(t3$outcome, outcome_name) &
                     t3$term == term_name])
}

# Target-cell averages, the quantity behind every descriptive claim about the
# shaded diagonal of figure 4.
target_means <- f4 |>
  filter(target) |>
  summarize(mean_estimate = mean(estimate), .by = c(treatment, Study))

# Study 1: samples and their composition ----

# "The experiment was conducted three times: first with a sample of 3,001
#  Mechanical Turk (MTurk) respondents, again with a sample of 2,181 'elite'
#  respondents, and a final time with 2,524 Lucid respondents (Coppock and
#  McClellan 2019) as a preregistered, out-of-sample test suggested during the
#  review process."
claim("s1_times_conducted", n_distinct(f4$Study),
      "samples study 1 was run in", 0)
claim("s1_n_mturk", pct_cell(b2, "MTurk", "n", "N"),
      "MTurk respondents, stated in prose and in the appendix table B.2 N row", 0)
claim("s1_n_elite", pct_cell(b2, "Elites", "n", "N"),
      "elite respondents, stated in prose and in the appendix table B.2 N row", 0)
claim("s1_n_lucid", pct_cell(b2, "Lucid", "n", "N"),
      "Lucid respondents, stated in prose and in the appendix table B.2 N row", 0)

# "A full 96% of the elite sample hold a college degree, compared with 51% of the
#  MTurk sample and 44% of the Lucid sample; 71% of the elites hold a graduate
#  degree, compared with 13% for the MTurk sample and 15% for Lucid."
claim("s1_college_elite", share("Elite", "College degree or more"),
      "per cent of the elite sample holding a college degree", 0)
claim("s1_college_mturk", share("MTurk", "College degree or more"),
      "per cent of the MTurk sample holding a college degree", 0)
claim("s1_college_lucid", share("Lucid", "College degree or more"),
      "per cent of the Lucid sample holding a college degree", 0)
claim("s1_graduate_elite", share("Elite", "Graduate degree"),
      "per cent of the elite sample holding a graduate degree; appendix table B.2 prints 71.6%", 0)
claim("s1_graduate_mturk", share("MTurk", "Graduate degree"),
      "per cent of the MTurk sample holding a graduate degree; appendix table B.2 prints 12.4%", 0)
claim("s1_graduate_lucid", share("Lucid", "Graduate degree"),
      "per cent of the Lucid sample holding a graduate degree; appendix table B.2 prints 15.1%", 0)

# "Nearly half - 44% - of the elite sample describe themselves as 'strong'
#  partisans, but only a quarter of the MTurk sample do so. Lucid is more similar
#  to the elite sample than MTurk on this dimension at 46% strong partisans."
claim("s1_strong_partisan_elite", share("Elite", "Strong partisan"),
      "per cent of the elite sample describing themselves as strong partisans", 0)
claim("s1_strong_partisan_mturk", share("MTurk", "Strong partisan"),
      "per cent of the MTurk sample describing themselves as strong partisans, against a quarter", 0)
claim("s1_strong_partisan_lucid", share("Lucid", "Strong partisan"),
      "per cent of the Lucid sample describing themselves as strong partisans", 0)

# "The most distinctive attribute of the elite sample is its extremely high level
#  of engagement with political news: 97% say they read the news at least daily,
#  and 64% say they talk about politics with their family, friends, or coworkers
#  at least once per day."
claim("s1_news_daily", share("Elite", "Reads the news at least daily"),
      "per cent of the elite sample reading the news at least daily", 0)
claim("s1_talk_politics_daily", share("Elite", "Talks politics at least once a day"),
      "per cent of the elite sample talking politics at least once a day", 0)

# Study 1: design ----

# "All three sets of respondents were randomly assigned to read one of four op-eds
#  or a control condition, before answering a series of policy attitude questions
#  related to each of the four op-eds."
claim("s1_conditions", n_distinct(f4$term) + 1,
      "experimental conditions: four op-ed treatment contrasts plus the control they are taken against", 0)
claim("s1_opeds", n_distinct(f4$treatment),
      "op-ed treatments the article analyzes", 0)

# "Each treatment op-ed is associated with a 'target' attitude that is measured
#  with four outcome questions. ... It describes how respondents in each sample
#  were randomly assigned to the five experimental conditions before answering
#  four attitudinal questions in each issue area, for a total of 16 outcomes."
claim("s1_questions_per_attitude",
      one(unique(count(distinct(f4, dv_cat_label, outcome), dv_cat_label)$n)),
      "outcome questions per issue area, identical across all four areas", 0)
claim("s1_outcomes", n_distinct(f4$outcome),
      "outcome questions in total", 0)

# Study 1: static constraint ----

# "Table 1 shows the average Pearson correlations for all pairwise combinations of
#  the 16 outcome questions (16 choose 2 = 120 pairs)."
claim("s1_pairs_choose", n_distinct(f4$outcome),
      "outcome questions entering the pairwise correlations", 0)
claim("s1_pairs_total", one(unique(t1$n_pairs_total)),
      "unordered pairs of outcome questions", 0)

# Table 1: "Levels of Static Constraint by Study Sample. MTurk sample .06 .30;
#  Elite sample .22 .39; Lucid sample .00 .12"
claim("t1_mturk_across", one(t1$across_domain[t1$sample == "MTurk sample"]),
      "table 1, MTurk across-family correlation", 2)
claim("t1_mturk_within", one(t1$within_domain[t1$sample == "MTurk sample"]),
      "table 1, MTurk within-family correlation", 2)
claim("t1_elite_across", one(t1$across_domain[t1$sample == "Elite sample"]),
      "table 1, Elite across-family correlation", 2)
claim("t1_elite_within", one(t1$within_domain[t1$sample == "Elite sample"]),
      "table 1, Elite within-family correlation", 2)
claim("t1_lucid_across", one(t1$across_domain[t1$sample == "Lucid sample"]),
      "table 1, Lucid across-family correlation", 2)
claim("t1_lucid_within", one(t1$within_domain[t1$sample == "Lucid sample"]),
      "table 1, Lucid within-family correlation", 2)

# "In the Elite sample, the average within-domain correlation was 0.39, while the
#  average across domains was 0.22. On MTurk, the average correlation between
#  attitudes in the same domain was smaller at 0.29, and the average correlation
#  between attitudes in different domains was very small at 0.06. On Lucid, these
#  low correlations are even lower, 0.12 and 0.00, respectively."
claim("s1_elite_within_text", one(t1$within_domain[t1$sample == "Elite sample"]),
      "elite within-domain correlation as the body text gives it", 2)
claim("s1_elite_across_text", one(t1$across_domain[t1$sample == "Elite sample"]),
      "elite across-domain correlation as the body text gives it", 2)
claim("s1_mturk_within_text", one(t1$within_domain[t1$sample == "MTurk sample"]),
      "MTurk within-domain correlation, the same quantity the body text prints as .29", 2)
claim("s1_mturk_across_text", one(t1$across_domain[t1$sample == "MTurk sample"]),
      "MTurk across-domain correlation as the body text gives it", 2)
claim("s1_lucid_within_text", one(t1$within_domain[t1$sample == "Lucid sample"]),
      "Lucid within-domain correlation as the body text gives it", 2)
claim("s1_lucid_across_text", one(t1$across_domain[t1$sample == "Lucid sample"]),
      "Lucid across-domain correlation as the body text gives it", 2)

# Study 1: figure 4 ----

# "The figure has 16 panels, each of which corresponds to the effects of an op-ed
#  on a set of outcome measures. The rows of panels correspond to the four
#  attitudes, each of which is measured with four questions. The columns are
#  separate treatments. The points and bars within each panel represent treatment
#  effect estimates and 95% confidence intervals."
claim("s1_panels", n_distinct(f4$treatment) * n_distinct(f4$dv_cat_label),
      "panels in figure 4", 0)
claim("s1_attitudes", n_distinct(f4$dv_cat_label),
      "attitudes the rows of panels correspond to", 0)
claim("s1_ci_level",
      100 * mean(2 * pt((f4$conf.high - f4$estimate) / f4$std.error, f4$df) - 1),
      "confidence level implied by the plotted half widths, in per cent", 0)

# "Proceeding diagonally down from the top left panel to the bottom right panel,
#  we observe strong effects of information on target issues. The Wall Street
#  treatment had the largest effects on its target issue of any of the treatments,
#  averaging nearly 0.5 standard deviations in all three samples."
claim("s1_wallstreet_largest",
      sum(target_means$mean_estimate[target_means$treatment == "Treatment: Wall Street"] ==
            summarize(target_means, top = max(mean_estimate), .by = Study)$top),
      "samples of three in which Wall Street's target-cell average is the largest of the four treatments", 0)
claim("s1_wallstreet_magnitude",
      min(target_means$mean_estimate[target_means$treatment == "Treatment: Wall Street"]),
      "smallest of the three Wall Street target-cell averages, the one furthest from 0.5", 2)

# "Among both elites and MTurk respondents, the effects of the Amtrak treatment on
#  the Amtrak outcomes hover around one-third of a standard deviation."
claim("s1_amtrak_magnitude",
      mean(target_means$mean_estimate[target_means$treatment == "Treatment: Amtrak" &
                                        target_means$Study != "Lucid Sample"]),
      "mean of the elite and MTurk Amtrak target-cell averages", 2)

# "Treatment effects on the third question in the Amtrak set of outcomes
#  (regarding how subjects expect new taxes to be spent) were especially large, at
#  0.52 standard deviations among elites and 0.73 standard deviations on MTurk."
amtrak_taxes <- amtrak |> filter(dv_labels == "Taxes Used for Existing/New Projects")
claim("s1_amtrak_question_index",
      as.numeric(str_extract(one(unique(amtrak_taxes$outcome)), "(?<=amtrak_)\\d+")),
      "position of that question among the four Amtrak outcomes, from the deposit's own variable name", 0)
claim("s1_amtrak_elite", one(amtrak_taxes$estimate[amtrak_taxes$Study == "Elite Sample"]),
      "effect on the third Amtrak question among elites", 2)
claim("s1_amtrak_mturk", one(amtrak_taxes$estimate[amtrak_taxes$Study == "MTurk Sample"]),
      "effect on the third Amtrak question on MTurk", 2)

# "Overall, out of the 48 estimated treatment effects on target issues, 39 are
#  statistically significant at p < .05 or better. If we apply the Benjamini and
#  Hochberg (1995) multiple comparisons correction to the associated p-values, 35
#  estimated effects remain significant."
claim("s1_target_estimates", one(counts$n_estimates[counts$issue_type == "Target issues"]),
      "estimated treatment effects on target issues", 0)
claim("s1_target_significant", one(counts$n_significant[counts$issue_type == "Target issues"]),
      "target-issue effects significant at p < 0.05", 0)
claim("s1_target_significant_bh",
      one(counts$n_significant_bh[counts$issue_type == "Target issues"]),
      "target-issue effects still significant after Benjamini-Hochberg", 0)

# "In nearly every case, the estimates are very close to zero. Out of 144
#  estimated effects on nontarget issues, only nine of the effects are
#  significant, and when we apply the Benjamini-Hochberg correction, none of these
#  estimates is deemed significant."
claim("s1_nontarget_estimates",
      one(counts$n_estimates[counts$issue_type == "Nontarget issues"]),
      "estimated treatment effects on nontarget issues", 0)
claim("s1_nontarget_significant",
      one(counts$n_significant[counts$issue_type == "Nontarget issues"]),
      "nontarget-issue effects significant at p < 0.05", 0)
claim("s1_nontarget_significant_bh",
      one(counts$n_significant_bh[counts$issue_type == "Nontarget issues"]),
      "nontarget-issue effects still significant after Benjamini-Hochberg", 0)
claim("s1_nontarget_near_zero", max(abs(f4$estimate[!f4$target])),
      "largest nontarget estimate in absolute value, against a target-cell range of 0.01 to 0.49", 2)

print(f4 |>
        filter(!target) |>
        summarize(claim = "s1_nontarget_near_zero: distribution of the 144 nontarget estimates",
                  min = min(estimate), q25 = quantile(estimate, 0.25),
                  median = median(estimate), q75 = quantile(estimate, 0.75),
                  max = max(estimate), within_0.1_of_zero = sum(abs(estimate) < 0.1)))

# Study 1: the precision-weighted averages ----

# "The precision weighted average of all treatment effects on target issues is
#  0.42 on MTurk, 0.22 in the elite sample, and 0.245 on Lucid. By contrast, the
#  precision weighted average of effects on nontarget issues is tiny in all three
#  samples: 0.004 standard deviations on MTurk, 0.01 in the elite sample, and
#  -0.02 on Lucid."
claim("s1_pwa_target_mturk", pwa_value("MTurk", "Within Domain", "estimate"),
      "precision-weighted average target effect on MTurk", 2)
claim("s1_pwa_target_elite", pwa_value("Elite", "Within Domain", "estimate"),
      "precision-weighted average target effect among elites", 2)
claim("s1_pwa_target_lucid", pwa_value("Lucid", "Within Domain", "estimate"),
      "precision-weighted average target effect on Lucid", 3)
claim("s1_pwa_nontarget_mturk", pwa_value("MTurk", "Across Domain", "estimate"),
      "precision-weighted average nontarget effect on MTurk", 3)
claim("s1_pwa_nontarget_elite", pwa_value("Elite", "Across Domain", "estimate"),
      "precision-weighted average nontarget effect among elites", 2)
claim("s1_pwa_nontarget_lucid", pwa_value("Lucid", "Across Domain", "estimate"),
      "precision-weighted average nontarget effect on Lucid", 2)

# "On MTurk, our best guess is that the average indirect effect is 0.8% of the
#  average direct effect, with a very tight confidence interval from -8.1% to
#  8.7%. On the elite sample, we estimate that indirect effects are 6.4% the size
#  of the direct effects on average, with a wider confidence interval extending
#  from -25.7% to 26.2%. On Lucid, the fraction is in fact negative, at -9.4%,
#  with a confidence interval from -24% to 15%."
claim("s1_ratio_mturk", 100 * pwa_value("MTurk", "ratio", "estimate"),
      "MTurk indirect-to-direct ratio, per cent", 1)
claim("s1_ratio_mturk_low", 100 * pwa_value("MTurk", "ratio", "conf_low"),
      "MTurk ratio, lower endpoint, per cent", 1)
claim("s1_ratio_mturk_high", 100 * pwa_value("MTurk", "ratio", "conf_high"),
      "MTurk ratio, upper endpoint, per cent", 1)
claim("s1_ratio_elite", 100 * pwa_value("Elite", "ratio", "estimate"),
      "elite indirect-to-direct ratio, per cent", 1)
claim("s1_ratio_elite_low", 100 * pwa_value("Elite", "ratio", "conf_low"),
      "elite ratio, lower endpoint, per cent", 1)
claim("s1_ratio_elite_high", 100 * pwa_value("Elite", "ratio", "conf_high"),
      "elite ratio, upper endpoint, per cent", 1)
claim("s1_ratio_lucid", 100 * pwa_value("Lucid", "ratio", "estimate"),
      "Lucid indirect-to-direct ratio, per cent", 1)
claim("s1_ratio_lucid_low", 100 * pwa_value("Lucid", "ratio", "conf_low"),
      "Lucid ratio, lower endpoint, per cent", 0)
claim("s1_ratio_lucid_high", 100 * pwa_value("Lucid", "ratio", "conf_high"),
      "Lucid ratio, upper endpoint, per cent, against a published 15", 0)

# "If we pool the three studies together, our estimate is 1.4% (95% CI: -8%, 6%)."
fe <- pooled |> filter(model == "Fixed effects")
claim("s1_ratio_pooled", 100 * one(fe$estimate),
      "pooled indirect-to-direct ratio, per cent, against a published 1.4", 1)
claim("s1_ratio_pooled_low", 100 * one(fe$conf_low),
      "pooled ratio, lower endpoint, per cent", 0)
claim("s1_ratio_pooled_high", 100 * one(fe$conf_high),
      "pooled ratio, upper endpoint, per cent", 0)

# "In summary, this study was well powered to detect even very small indirect
#  effects on the order of 10% to 20% the size of direct effects, but we found no
#  indication at all that these treatments induced any opinion change in nontarget
#  domains."
claim("s1_power_low", stated("s1_power_low") / (100 * one(fe$std.error)),
      "pooled standard errors in a ratio of 10 per cent", 1)
claim("s1_power_high", stated("s1_power_high") / (100 * one(fe$std.error)),
      "pooled standard errors in a ratio of 20 per cent", 1)

# Study 2 ----

# "The 1,087 subjects for this experiment were also recruited via Lucid."
claim("s2_n_original", one(inconsistency$n_respondents[inconsistency$sample == "Original"]),
      "study 2 original sample size", 0)

# "One outcome measure raises the same objection in a different domain (e.g.,
#  religious objections among officials who oversee marriage licenses), while the
#  other two outcome measures broach the subject of same-sex marriage more
#  generally or the acceptability of prayer in public school settings. ... so it
#  is reasonable to imagine that subjects would apply the religious conviction
#  consideration when answering the three further questions."
n_nontarget_s2 <- n_distinct(t2$outcome) - 1
claim("s2_nontarget_measures_text", n_nontarget_s2,
      "nontarget outcome measures in study 2, against the three the design paragraph counts", 0)

# "However, the four additional outcome measures did not change in a manner
#  consistent with the initial response, as shown in columns 2-5."
claim("s2_nontarget_measures_table", n_nontarget_s2,
      "the same count, against the four the results paragraph gives", 0)

# "Responses to these nontarget outcomes are positively correlated among each
#  other and with the target outcome of the manipulation (correlations range from
#  0.3 to 0.7), so it is reasonable to imagine that subjects would apply the
#  religious conviction consideration when answering the three further questions."
s2_original_cors <- study_2_cors$correlation[study_2_cors$sample == "Original"]
claim("s2_correlation_low", min(s2_original_cors),
      "smallest of the ten pairwise correlations among the study 2 outcomes", 1)
claim("s2_correlation_high", max(s2_original_cors),
      "largest of the same ten", 1)

print(study_2_cors |>
        filter(sample == "Original") |>
        mutate(claim = "s2_correlation_low and s2_correlation_high: the ten pairwise correlations") |>
        select(claim, outcome_1, outcome_2, target_pair, correlation))

# "Table 2 shows that the religious exception treatment increased subjects'
#  agreement that employers should have the right to discriminate by 0.44 scale
#  points in the original study and 0.41 points in the replication."
claim("s2_effect_original",
      t2_value("Original", "Employers", "Religious exemption", "estimate"),
      "religious exemption effect on the employers question, original sample", 2)
claim("s2_effect_replication",
      t2_value("Replication", "Employers", "Religious exemption", "estimate"),
      "religious exemption effect on the employers question, replication", 2)

# "In both samples, two of the estimated effects are positive, and two are
#  negative, but all are close to zero and statistically insignificant."
s2_nontarget <- t2 |>
  filter(term == "Religious exemption", outcome != "Employers")
claim("s2_sign_split_original",
      sum(s2_nontarget$estimate[s2_nontarget$sample == "Original"] > 0),
      "positive nontarget estimates of four in the original sample", 0)
claim("s2_sign_split_replication",
      sum(s2_nontarget$estimate[s2_nontarget$sample == "Replication"] > 0),
      "positive nontarget estimates of four in the replication", 0)

print(s2_nontarget |>
        mutate(claim = "s2_sign_split_original and s2_sign_split_replication") |>
        select(claim, sample, outcome, estimate, std.error, p.value))

# Study 3 ----

# "The average effects of the treatment on belief that income inequality increased
#  are very large (approximately 25 percentage points in both samples, with
#  standard errors smaller than 4 points)."
claim("s3_belief_effect_original",
      100 * t3_value("Original", "Belief in Inequality", "Inequality treatment", "estimate"),
      "belief in inequality effect in the original study, percentage points", 1)
claim("s3_belief_effect_replication",
      100 * t3_value("Replication", "Belief in Inequality", "Inequality treatment", "estimate"),
      "the same effect in the replication, percentage points", 1)
claim("s3_se_bound",
      100 * max(t3_value("Original", "Belief in Inequality", "Inequality treatment", "std.error"),
                t3_value("Replication", "Belief in Inequality", "Inequality treatment", "std.error")),
      "larger of the two standard errors, percentage points, against a stated bound of 4", 1)

# "By contrast, the effects on economic system justification are small and close to
#  zero: -0.18 standard units in the original study (SE: 0.10) and -0.07 units in
#  the replication (SE: 0.06)."
claim("s3_esj_original",
      t3_value("Original", "Econ. System Justification", "Inequality treatment", "estimate"),
      "economic system justification effect in the original study", 2)
claim("s3_esj_original_se",
      t3_value("Original", "Econ. System Justification", "Inequality treatment", "std.error"),
      "its standard error", 2)
claim("s3_esj_replication",
      t3_value("Replication", "Econ. System Justification", "Inequality treatment", "estimate"),
      "economic system justification effect in the replication", 2)
claim("s3_esj_replication_se",
      t3_value("Replication", "Econ. System Justification", "Inequality treatment", "std.error"),
      "its standard error", 2)

# Table 3 note: "A random third of original study respondents was asked the
#  economic system justification outcome question."
claim("t3_note_third",
      t3_value("Original", "Belief in Inequality", "Inequality treatment", "n") /
        t3_value("Original", "Econ. System Justification", "Inequality treatment", "n"),
      "original study respondents per economic system justification respondent", 0)

# Discussion ----

# "What did we find? In study 1, the intervention induced significant opinion
#  change on target issues in 39 of 48 opportunities, providing a quite powerful
#  first-stage platform for the exploration of dynamic effects. But in only nine
#  of 144 opportunities did it induce significant changes on nontarget issues, and
#  the average spillover effect is just 1.4% of the first-stage direct effect."
claim("disc_target_significant", one(counts$n_significant[counts$issue_type == "Target issues"]),
      "target-issue effects that were significant, restated in the discussion", 0)
claim("disc_target_estimates", one(counts$n_estimates[counts$issue_type == "Target issues"]),
      "target-issue opportunities, restated in the discussion", 0)
claim("disc_nontarget_significant",
      one(counts$n_significant[counts$issue_type == "Nontarget issues"]),
      "nontarget-issue effects that were significant, restated in the discussion", 0)
claim("disc_nontarget_estimates",
      one(counts$n_estimates[counts$issue_type == "Nontarget issues"]),
      "nontarget-issue opportunities, restated in the discussion", 0)
claim("disc_spillover_ratio", 100 * one(fe$estimate),
      "average spillover effect as a share of the direct effect, per cent, against a published 1.4", 1)

# "As is well documented (Ahler and Broockman 2018; Sears and Citrin 1982),
#  inconsistency between preferences for smaller government and increased social
#  spending is common. Among the MTurk sample, 30% of the respondents were
#  inconsistent; this figure was 24% on Lucid."
claim("disc_inconsistent_original",
      100 * one(inconsistency$share_inconsistent[inconsistency$sample == "Original"]),
      "per cent of the study 2 original sample holding inconsistent preferences", 0)
claim("disc_inconsistent_replication",
      100 * one(inconsistency$share_inconsistent[inconsistency$sample == "Replication"]),
      "per cent of the study 2 replication sample holding inconsistent preferences", 0)
claim("disc_inconsistent_sample_label",
      one(inconsistency$n_respondents[inconsistency$sample == "Original"]),
      "respondents the 30 per cent is taken over: the study 2 original sample, which the article elsewhere says was recruited via Lucid", 0)

# Table 4 row label: "N (MTurk) 299 26"
claim("disc_correction_sample_label",
      sum(t4$n[t4$sample == "Original" & t4$term == "Correction treatment"]),
      "respondents in table 4's top panel, drawn from the inconsistent subset of that same Lucid sample", 0)

# "We conducted our experiment among the inconsistent subset only. Half of these
#  subjects were randomly assigned to receive a query."
claim("disc_half_assigned",
      one(inconsistency$n_inconsistent[inconsistency$sample == "Original"]) /
        one(inconsistency$n_assigned_query[inconsistency$sample == "Original"]),
      "inconsistent respondents per respondent assigned the query, original sample", 0)

# "[Five spending categories, Social Security, Medicare, health care for
#  low-income families, national defense, foreign aid]. We average all five
#  responses so that -1 on the resulting scale indicates that a respondent
#  preferred decreasing spending in all five categories and +1 indicates a
#  preference for increases in all categories."
claim("disc_spending_categories",
      one(inconsistency$n_spending_items[inconsistency$sample == "Original"]),
      "spending items averaged into the study 4 outcome", 0)

# "The results are shown in table 4. When induced to become more consistent,
#  people who prefer smaller government prefer less spending (-0.172 scale points
#  on MTurk, -0.153 scale points on Lucid)."
claim("disc_correction_small",
      one(t4$estimate[t4$sample == "Original" & t4$gov_pref == "Prefer small government" &
                        t4$term == "Correction treatment"]),
      "correction effect among small-government respondents, original sample", 3)
claim("disc_correction_small_replication",
      one(t4$estimate[t4$sample == "Replication" & t4$gov_pref == "Prefer small government" &
                        t4$term == "Correction treatment"]),
      "correction effect among small-government respondents, replication", 3)

# "We see very similar results among inconsistent subjects who prefer larger
#  government, though owing to their comparatively small numbers, our effect
#  estimates are less precise in this subgroup."
t4_treatment <- t4 |> filter(term == "Correction treatment")
t4_precision <- t4_treatment |>
  select(sample, gov_pref, std.error) |>
  pivot_wider(names_from = gov_pref, values_from = std.error)
claim("disc_large_gov_imprecise",
      sum(t4_precision$`Prefer large government` > t4_precision$`Prefer small government`),
      "samples of two in which the large-government standard error exceeds the small-government one", 0)

print(t4_treatment |>
        mutate(claim = "disc_large_gov_imprecise") |>
        select(claim, sample, gov_pref, estimate, std.error, n))

# Appendix C ----

# "Above, we report that the Study 1 replication sample had 2,524 subjects,
#  whereas here we report 2,976. The discrepancy is due to our exclusion of
#  subjects assigned to an unanalyzed treatment condition in study 1."
claim("app_c_n_study_1", pct_cell(b2, "Lucid", "n", "N"),
      "study 1 Lucid sample size as the appendix restates it", 0)
claim("app_c_n_study_2", pct_cell(c3, "Replication", "n", "N"),
      "study 2 replication sample size", 0)

# "Subjects who saw the welfare version of the question supported redistribution
#  0.59 scale points less than subjects who saw the aid to the poor version (0.46
#  in the replication)."
claim("app_c_welfare_original",
      abs(one(c4$estimate[c4$sample == "Original" & c4$outcome == "Too Little on Welfare" &
                            c4$term == "Welfare wording"])),
      "welfare wording effect in the original sample, absolute value", 2)
claim("app_c_welfare_replication",
      abs(one(c4$estimate[c4$sample == "Replication" & c4$outcome == "Too Little on Welfare" &
                            c4$term == "Welfare wording"])),
      "welfare wording effect in the replication, absolute value", 2)

# Appendix D ----

# "We use two outcomes from the Trump and White (2018) study: belief in income
#  inequality and economic system justification."
claim("app_d_outcomes_used", n_distinct(t3$outcome),
      "outcomes taken from Trump and White (2018)", 0)

# Float coverage ----
# Each of these compares what the pipeline writes for a float against the number
# of cells the article prints in it, which is the covered fraction rather than the
# "at least one row" a float inventory would settle for. The individual cells are
# compared one by one in the ground truth.
table_cells <- function(tab, sample_cols) nrow(tab) * length(sample_cols)

regression_cells <- function(tab, panel_col, column_col) {
  2 * nrow(tab) + nrow(distinct(tab, .data[[panel_col]], .data[[column_col]]))
}

claim("float_table_1", 2 * nrow(t1), "cells the rewrite writes for table 1", 0)
claim("float_table_2", regression_cells(t2, "sample", "outcome"),
      "cells the rewrite writes for table 2: an estimate and a standard error per row plus one N per column", 0)
claim("float_table_3", regression_cells(t3, "sample", "outcome"),
      "cells the rewrite writes for table 3", 0)
claim("float_table_4", regression_cells(t4, "sample", "gov_pref"),
      "cells the rewrite writes for table 4", 0)
claim("float_table_c4", regression_cells(c4, "sample", "outcome"),
      "cells the rewrite writes for appendix table C.4", 0)
claim("float_table_b2", table_cells(b2, c("MTurk", "Elites", "Lucid")),
      "cells the rewrite writes for appendix table B.2", 0)
claim("float_table_c3", table_cells(c3, c("Original", "Replication")),
      "cells the rewrite writes for appendix table C.3", 0)
claim("float_table_d5", table_cells(d5, c("Original", "Replication")),
      "cells the rewrite writes for appendix table D.5", 0)
claim("float_figure_4", nrow(f4), "estimates figure 4 plots", 0)
claim("float_figure_b2", nrow(fb2), "estimates appendix figure B.2 plots", 0)
claim("float_figure_b3", nrow(fb3), "estimates appendix figure B.3 plots", 0)
