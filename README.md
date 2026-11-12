# Active Maintenance Report: coppock_green_2022


- [Summary](#summary)
  - [Does the deposited archive run?](#does-the-deposited-archive-run)
  - [Does the maintained rewrite reproduce the
    paper?](#does-the-maintained-rewrite-reproduce-the-paper)
- [Paper overview](#paper-overview)
- [Original archive reproducibility](#original-archive-reproducibility)
- [Errata](#errata)
  - [The elite ideology column of appendix table B.2 is
    repaired](#the-elite-ideology-column-of-appendix-table-b2-is-repaired)
  - [The pooled ratio, and what the unseeded bootstrap does to
    it](#the-pooled-ratio-and-what-the-unseeded-bootstrap-does-to-it)
  - [Two appendix findings filed
    late](#two-appendix-findings-filed-late)
- [Ground truth](#ground-truth)
- [The extraction, and the two instruments that read
  it](#the-extraction-and-the-two-instruments-that-read-it)
- [Maintained rewrite](#maintained-rewrite)
  - [What changed and why](#what-changed-and-why)
- [Figure verification](#figure-verification)
- [Rewrite verification](#rewrite-verification)
- [R environment](#r-environment)

**Archived at Zenodo:** [doi:10.5281/zenodo.24168344](https://doi.org/10.5281/zenodo.24168344). This DOI always resolves to the latest release of the maintained version.

*Drafted by Claude Opus 5 under the supervision of Alex Coppock.*

This repository holds the actively maintained replication code for
Coppock and Green (2022), together with the reproducibility report that
documents what the original archive did and did not do. It is part of a
program applying the maintenance proposal in Peer, Orr and Coppock
(2021, *PS: Political Science & Politics*, doi
[10.1017/S1049096521000366](https://doi.org/10.1017/S1049096521000366))
to a set of published archives.

|  |  |
|----|----|
| Article | [10.1086/716294](https://doi.org/10.1086/716294) |
| Replication archive | [10.7910/DVN/NBMH7M](https://doi.org/10.7910/DVN/NBMH7M) |
| Pre-analysis plan (study 1) | [osf.io/vdcyq](https://osf.io/vdcyq) |
| Pre-analysis plan (studies 2 to 4) | [osf.io/ch4ms](https://osf.io/ch4ms) |

**The data are not redistributed here.** The deposit is 40 MB across 46
files and lives at Harvard Dataverse, which is the only copy this
repository points at. `download_original.R` fetches it and verifies
every file; `original_manifest.csv` pins the file identifiers, sizes and
checksums, so the exact bytes this code was written against are recorded
in version control even though the bytes themselves are not.

**Repository layout.** `maintained/` is the maintained rewrite: one
script per published table or figure, writing to `output/`, which is
committed so a reader can compare a fresh run against it without
downloading anything. `ground_truth/` ties every published number to the
code that produces it. `original/` is created by the download script and
is deliberately absent from the repository. This file is the
reproducibility report, also available as a PDF in `report/`.

**License.** CC0 1.0 Universal, matching the terms of the deposit this
repository maintains. See `LICENSE`.

**To reproduce.** Clone or download the repository, open
`coppock_green_2022.Rproj`, and run:

``` r
source("run_all.R")
```

That fetches the deposit, verifies its 46 files, produces every table
and figure into `maintained/output/`, verifies the deposit again, and
rebuilds the ground truth. Required packages: tidyverse, estimatr,
rsample, metafor, janitor, knitr, kableExtra, here. Paths resolve
through `here`, so nothing depends on the working directory. Most of the
running time is the respondent-level bootstrap behind the in-text
precision-weighted averages, which refits 16 regressions in each of
3,000 resamples. A successful run overwrites `maintained/output/`, which
is committed: **`git diff` on that folder is the reproduction check.**

`ground_truth/run_original_archive.R` runs the deposited code itself and
is deliberately outside `run_all.R`, because it needs the packages the
deposit was written against (stargazer, xtable, coefplot, psych, car,
rmeta) and the rewrite needs none of them. Its output is committed so
the archive column of the ground truth can be read without installing
them.

# Summary

Two questions, answered before the detail.

## Does the deposited archive run?

Only after a reader rebuilds the folder tree it assumes. Every file in
the deposit sits flat in one directory, but every script addresses
`programs/analysis/`, `programs/cleaning/`, `programs/helpers/`,
`data/raw/` and `data/clean/`, and writes to `../Drafts/figures/` and
`../Drafts/tables/`. One cleaning script additionally reads
`data/raw/Trump605/`, a folder the deposit flattens into a single file
at the top level. Nothing in the archive creates any of those
directories, and the deposit’s own `README.txt` concedes the point,
saying the paths will “(probably!)” be correct. Rebuild the tree and all
12 analysis scripts run without error on a current R installation and
reproduce the published tables and figures.

The cleaning scripts are a different matter, and they are why the
deposit’s clean data cannot be rebuilt from its raw data. Given only the
raw files and the code, 1 of the 6 cleaning scripts finishes, 2 of the 7
analysis-ready files it takes to run the paper get written, and **every
one of the 12 analysis scripts then fails**, each on the first line that
tries to read a clean data file that no longer exists. The archive
passes only because it ships the outputs of code that no longer runs.

## Does the maintained rewrite reproduce the paper?

Yes, with 7 exceptions, none of which is a defect in this code.

Of 417 published quantities recorded in the ground truth, the rewrite
reproduces 393 to the precision the article prints and fails to
reproduce 7. A further 12 claims are about shape, sign, count or order
rather than about a value, and 10 of those hold. Every cell of tables 1
through 4 and of appendix tables B.2, C.3, C.4 and D.5 reproduces,
including all 24 sample sizes those four regression tables report.

The 7 failures are all the article disagreeing with itself, and every
one of them is a sentence rather than a table cell. The pooled
indirect-to-direct effect ratio is printed with the wrong sign, in the
results section and again in the discussion. The body text gives an
average correlation of .29 where table 1 gives .30 for the same
underlying 0.2966. Two graduate degree shares and one standard error are
stated at a rounding their own tables contradict. And study 2’s design
paragraph counts three nontarget outcome measures where the table and
the results paragraph both have four.

2 of the descriptive claims also fail, and they are the same mislabel
twice: the inconsistency rates behind study 4 and the top panel of table
4 are both attributed to MTurk, and both come from the study 2 original
sample that the body text and appendix table C.3 identify as Lucid. The
numbers themselves are right, which is why no value comparison could
have found them. All 9 are set out as 8 entries in
`coppock_green_2022_errata.pdf` at the root of this repository, one
entry per published sentence. The note carries 11 entries in all: entry
9 supplies two figure cross-references the appendix omits, entry 10
corrects the table a cross-reference names, and entry 11 the unit on a
significance threshold, none of which a value comparison could have
found either.

The deposited archive does worse. On the 309 quantities a deposited
script produces at all, it reproduces 302 and fails 7. The extra
failures are one appendix table column that the deposit’s own clean data
destroys before any analysis script sees it, described below.

# Paper overview

**Citation**: Coppock, A. and Green, D. P. (2022). “Do belief systems
exhibit dynamic constraint?” *The Journal of Politics*, 84(2), 725-738.
DOI: 10.1086/716294

**Summary**: Belief systems are conventionally measured by static
constraint, the cross-sectional correlation among a person’s opinions on
related issues. The paper asks whether they exhibit dynamic constraint
instead: whether an exogenous change in one opinion causes concomitant
change in the opinions correlated with it. Four experiments say they do
not. Study 1 randomly assigns op-eds on four issue domains (a flat tax,
Wall Street regulation, veterans’ benefits and Amtrak) to an MTurk
sample, a sample of political elites, and a Lucid sample, then measures
16 outcomes across three waves; the treatments move their target
outcomes substantially and their nontarget outcomes not at all, in all
three samples and at all three waves. Study 2 primes religious
conviction in the context of employment discrimination against gay men
and lesbians and finds the prime moves the target question by 0.44 scale
points without moving four cognate church-state questions. Study 3
reanalyses and replicates Trump and White (2018, doi
[10.1017/xps.2018.2](https://doi.org/10.1017/xps.2018.2)), where a
graphical manipulation of income inequality moves belief in inequality
by roughly 25 percentage points and economic system justification not at
all. Study 4 shows that respondents holding inconsistent spending
preferences will resolve the inconsistency when it is pointed out to
them, which locates the constraint in the encouragement rather than in
the belief system.

**The quantity that carries the argument** is the precision-weighted
average treatment effect on nontarget outcomes expressed as a fraction
of the same average on target outcomes: 0.8% on MTurk, 6.4% among elites
and -9.4% on Lucid, all with intervals that comfortably exclude the 10%
to 20% the paper describes itself as powered to detect.

# Original archive reproducibility

Every deposited script was run twice in a clean R session against a
reconstructed folder tree: once with the deposit stripped to its raw
data and its code, and once with the analysis-ready files the deposit
ships restored. Neither run touched `original/`; both were assembled in
a scratch directory, because several deposited scripts write files whose
names are deposited files.

| Script | Role | Stripped to data and code | As deposited | Note |
|:---|:---|:---|:---|:---|
| study_1_mturk_cleaning.R | cleaning | Fails | Fails | Error in file(file, …) : argument “file” is missing, with no default Calls: source -\>… |
| study_1_elite_cleaning.R | cleaning | Fails | Fails | Error in file(file, …) : argument “file” is missing, with no default Calls: source -\>… |
| study_2_lucid_cleaning.R | cleaning | Runs | Runs |  |
| study_3_original_cleaning.R | cleaning | Fails | Fails | Error: Failed to parse <archive>/data/raw/Trump605/TESS3 174 - Trump_Client.sav: Unknow… |
| study_3_lucid_cleaning.R | cleaning | Fails | Runs |  |
| studies_1_2_replications_lucid_cleaning.R | cleaning | Fails | Fails | Error in `mutate()`: ℹ In argument: `lucid_age = as.numeric(age_1)`. Caused by error: !… |
| in_text_numbers.R | analysis | Fails | Runs |  |
| study_1_figure_4.R | analysis | Fails | Runs |  |
| study_1_table_1.R | analysis | Fails | Runs |  |
| study_2_table_2.R | analysis | Fails | Runs |  |
| study_3_table_3.R | analysis | Fails | Runs |  |
| study_4_table_4.R | analysis | Fails | Runs |  |
| appendix_figure_B2.R | analysis | Fails | Runs |  |
| appendix_figure_B3.R | analysis | Fails | Runs |  |
| appendix_table_B2.R | analysis | Fails | Runs |  |
| appendix_table_C3.R | analysis | Fails | Runs |  |
| appendix_table_C4.R | analysis | Fails | Runs |  |
| appendix_table_D5.R | analysis | Fails | Runs |  |

Every script in the deposited archive, run against a reconstructed
folder tree with and without the analysis-ready files the deposit ships.

**The stripped run is the informative one.** With the shipped
intermediates removed, the deposit cannot rebuild them, so nothing
downstream runs. `study_1_mturk_cleaning.R` and
`study_1_elite_cleaning.R` end in `write_rds(x, path = ...)`, and `path`
was removed from readr in version 2.0, so both stop on their last line.
`study_3_original_cleaning.R` stops earlier: `haven::read_sav` cannot
parse the deposited SPSS file at all, returning “Failed to parse …
Unknown error”, although `foreign::read.spss` reads the same bytes into
a 1,020 by 98 data frame without difficulty.
`studies_1_2_replications_lucid_cleaning.R` stops on
`object 'age_1' not found`, which is a bug in the script rather than a
change underneath it.

**Running the deposit in its own directory would damage the deposit.**
The cleaning scripts write into `data/clean/` and `data/raw/`, which is
where the deposited data live. Three deposited files are written over by
a single run of the deposit as it stands, one of which comes back with
different contents.

| File | Rebuilt when stripped | Rebuilt copy matches deposit | Written over by a run as deposited | Contents unchanged after that run |
|:---|:---|:---|:---|:---|
| study_1_mturk_cleaned.rds | No |  | No | Yes |
| study_1_elite_cleaned.rds | No |  | No | Yes |
| study_2_lucid_clean.rds | Yes | Yes | Yes | Yes |
| original_trump_white_clean.rds | No |  | No | Yes |
| replication_trump_white_clean.rds | No |  | Yes | Yes |
| studies_1_2_replications_lucid_cleaned.rds | No |  | No | Yes |
| factor_analysis_models.rdata | Yes | No | Yes | No |

The seven deposited files that deposited code also writes.

**One published column cannot be recovered from the deposit’s clean
data.** `study_1_elite_cleaning.R` builds the elite ideology variable
with `recode(ideology, "c(1,9)='Liberal';2='Moderate';...")`, which is
`car::recode` syntax, but the script never loads `car` and its own
`library(tidyverse)` puts `dplyr::recode` in the way. `dplyr::recode`
reads that specification as a single positional replacement, and the
deposited `study_1_elite_cleaned.rds` accordingly records every elite
respondent as “Other” or missing. Appendix table B.2 reports a real
distribution for that column, so the deposit’s clean data contradict the
published table, and 5 of the 7 archive failures are that one column.
The rewrite rebuilds the variable from the deposited wave 1 raw file and
reproduces the published distribution exactly; see the Errata section.

**The deposit contains four files nothing reads.**
`dc_replication_cleaned.rds`, `elite_opeds_cleaned.rds`,
`lucid_cleaned.rds` and `mturk_opeds_cleaned.rds` are byte-different but
content-equivalent copies of four files the scripts do read under other
names, and no script in the deposit mentions them. The deposit’s
`README.txt` also lists a file that is not there (`appendix_table_D2.R`)
and omits one that is (`appendix_table_B2.R`), and it describes a
`Trump605` folder that the deposit flattens into a single `.sav` at the
top level.

**Two comments in the deposit were tested rather than repeated.**
`run_all_scripts_in_order.R` marks `in_text_numbers.R` as one that
“takes a few minutes”. At the `sims <- 10` the deposit actually ships it
takes a few seconds; at the `sims <- 1000` that sits commented out on
the line above it takes minutes. The comment describes the version that
was run, which is evidence that the deposited setting is a leftover from
debugging rather than the setting behind the published intervals. The
rewrite uses 1,000.

**One quantile is a typo.** `in_text_numbers.R` asks for
`quantile(., probs = c(0.9755))` where 0.975 is meant, in all three of
its uncertainty summaries. At 1,000 resamples the difference is at most
one order statistic and never reaches the second decimal place of a
reported percentage, but the rewrite uses 0.975 and writes the archive’s
quantile alongside so the size of the typo is measured rather than
asserted.

# Errata

Two kinds of correction, kept apart because they are different things.

**Corrections this code makes to the deposited code** are below.
**Sentences in the published article that its own data do not support**,
together with one that refers to figures it never names, are in
`coppock_green_2022_errata.pdf` at the root of this repository, which
lists eleven of them, computes every corrected value from
`maintained/output/` when it is rendered, and changes no conclusion of
the article. An entry it carried until 2026-08-14 was removed: an
endpoint it corrected turned out to be a misreading of the published
page rather than an error in it, the entry was removed, and the reason
is below.

## The elite ideology column of appendix table B.2 is repaired

The deposited elite clean data cannot produce the published ideology
distribution, for the reason given above. The recode the cleaning script
intended is unambiguous, it needs nothing beyond the deposit’s own
`study_1_elite_wave_1_raw.csv`, and rebuilding it returns the published
distribution exactly. `read_elite_ideology()` in `maintained/helpers.R`
writes that recode out in `case_when`, and
`table_b2_sample_demographics.R` uses it in place of the broken column.
All 5 published elite ideology percentages reproduce.

This is a correction to a coding error, not to an analytical decision:
the specification string names each source value and its label, so there
is nothing to interpret.

The same defect reaches two more columns, and they are the reason the
article’s description of its elite sample can be checked at all.
`news_how_often` and `talk_politics_how_often` are recoded in the same
way in the same script, so the deposited clean file stores the
specification string in place of every respondent’s answer. The two
figures the article gives for them appear in no table, so nothing else
in the paper would have caught it. `read_elite_engagement()` rebuilds
both from the same raw file, and both figures reproduce.

## The pooled ratio, and what the unseeded bootstrap does to it

The article’s pooled indirect-to-direct ratio is the errata note’s first
item, and the evidence for it is worth setting out at length here.

| Source                        | Estimate | SE    | 95% CI            |
|:------------------------------|:---------|:------|:------------------|
| MTurk sample                  | 0.8%     | 4.4%  | \[-8.7%, 8.6%\]   |
| Elite sample                  | 6.4%     | 13.4% | \[-25.4%, 26.1%\] |
| Lucid sample                  | -9.4%    | 6.6%  | \[-23.2%, 2.2%\]  |
| Pooled, Fixed effects         | -1.7%    | 3.5%  | \[-8.6%, 5.3%\]   |
| Pooled, Random effects (REML) | -1.9%    | 4.0%  | \[-9.8%, 6.0%\]   |
| Pooled, as published          | 1.4%     |       | \[-8%, 6%\]       |

The three sample ratios, the pooled estimate this repository computes,
and the pooled estimate the article prints.

The three inputs are the sample-specific ratios of the
precision-weighted average nontarget effect to the precision-weighted
average target effect, together with their respondent-level bootstrap
standard errors. The deposited `in_text_numbers.R` pools them with
`rmeta::meta.summaries` and passes no `method` argument, so it is an
inverse-variance fixed-effect meta-analysis and
`metafor::rma(method = "FE")` is the maintained equivalent. Two of the
three inputs are near zero and the third, Lucid, is negative with the
second-smallest standard error, so an inverse-variance pool of 0.8%,
6.4% and -9.4% cannot land above zero at these weights. The published
interval is the one this repository computes; the published point
estimate is not.

Neither the archive nor the article sets a seed, so no interval endpoint
in this paper is a fixed quantity: each is one draw of a bootstrap
quantile, and how variable a draw depends on how many resamples went
into it. That variability is measured rather than assumed, by resampling
the saved draws and recomputing the endpoint.

| Resamples | Endpoint runs from | to   | Published |
|:----------|:-------------------|:-----|:----------|
| 10        | -6.3%              | 6.9% | +5%       |
| 1,000     | 1.1%               | 3.6% | +5%       |

The Lucid interval’s upper endpoint over repeated unseeded runs, at the
resample count the deposit ships and the one the rewrite uses.

The 2.2% this repository computes for the Lucid interval’s upper
endpoint sits inside that range, and so does the +5% the article prints.
All six endpoints the article gives fall inside their own ranges and the
pooled point estimate falls outside its own, which is why the errata
note corrects the estimate and leaves every interval as published.

**That endpoint was misread here, not misprinted there.** Until
2026-08-14 this report and the errata note both gave the published
endpoint as 15% and recorded it as an interval bound the deposit’s
procedure cannot produce. The article prints +5%. Both signs in the
sentence are set in a symbol font that the text extraction behind the
ground truth mangles: the minus of -24% comes off the page as a 2 and
the plus of +5% as a 1, so `-24% to +5%` extracts as `224% to 15%` and
was transcribed as fifteen per cent. What found it was an audit of the
published PDF by font name: a glyph the extractor mangles comes from a
face the body text does not use, and this article sets its plus, minus
and equals signs in one of those. The entry that corrected it is gone
from the errata note, and the ground truth now records the endpoint as
agreeing.

## Two appendix findings filed late

Neither changes a quantity, which is why the errata note was first
written without them. Both were filed on 2026-10-31 as entries 10 and
11, on the same footing as entry 9’s cross-reference.

**A cross-reference in the appendix (entry 10).** Appendix section A.2
says the elaboration “blow\[s\] up the possible number of graphs from
the 32 in Table 1 to 256”. The 32 are enumerated in appendix table A.1;
table 1 of the main text reports levels of static constraint. The count
itself is right, and the reference points at the wrong table.

**A stray percent sign (entry 11).** Appendix section C.2 says an
estimate “is not significant at the 0.05% level in either sample”. The
article’s tables mark significance at `p < .05` throughout, and both
estimates are indeed insignificant at that level, at p = 0.064 and p =
0.097. The claim the sentence makes holds; only the unit on the
threshold is wrong.

**One sample label is right and reads as though it were not.** Appendix
table D.5 reports N = 1,187 for the study 3 replication, which is the
number of respondents in the file. Table 3 reports N = 1,041 and 1,026
for the same study, which are the two regression sample sizes. Both are
right for what they describe.

# Ground truth

`ground_truth/coppock_green_2022_ground_truth.csv` records 417 claims.
Each row names a published quantity, the value the deposited archive’s
own scripts produce, the value printed in the article or appendix, and
the value this rewrite produces. `value_paper` is carried as the string
the article prints, so the tolerance a comparison uses is read off the
printed precision rather than guessed from a double, and it is filled
only by reading the article or the appendix.

| Section | Claims | Archive matches | Archive fails | Archive silent | No published value | Rewrite matches | Rewrite fails |
|:---|---:|---:|---:|---:|---:|---:|---:|
| Appendix tables | 209 | 200 | 5 | 4 | 0 | 209 | 0 |
| Figures | 5 | 0 | 0 | 5 | 0 | 5 | 0 |
| In-text quantities | 99 | 2 | 2 | 95 | 7 | 75 | 7 |
| Main text tables | 104 | 100 | 0 | 4 | 0 | 104 | 0 |

Ground truth claims by section of the paper.

Coverage is checked against the article’s own float list rather than
against what the pipeline happens to produce: `build_ground_truth.R`
enumerates every numbered table and figure in the paper and the appendix
and stops if any float carrying data has no row. The floats with no rows
are the ones with no numbers in them, namely the three theoretical
diagrams, the enumeration of the 32 causal models in table A.1, and the
reproduced treatment images in figure D.4.

**Coverage of a float is a fraction, not a yes or no.** Each of the
eight published tables and three published figures carrying data also
gets a row of its own recording how many values the article prints in it
against how many the pipeline writes, so a float covered by one row out
of a hundred cells could not pass. All eleven are complete, from table
1’s 6 cells to appendix table B.2’s 87. The three coefficient plots
print no numbers on their faces, so what they assert is a count of
estimates plotted: 192 for figure 4 and 128 and 64 for appendix figures
B.2 and B.3.

Every quantity in the ground truth is read out of a file in
`maintained/output/` by `build_ground_truth.R`; a published value that
finds no counterpart there stops the build rather than being recorded as
unverifiable.

| Where | Claim | Published | Archive | Rewrite | Fault lies with |
|:---|:---|:---|:---|:---|:---|
| Table B.2 | ideology: Conservative (Elites) | 16.8% | 0 | 16.80% | archive |
| Table B.2 | ideology: Liberal (Elites) | 37.3% | 0 | 37.30% | archive |
| Table B.2 | ideology: Libertarian (Elites) | 10.2% | 0 | 10.20% | archive |
| Table B.2 | ideology: Moderate (Elites) | 32.0% | 0 | 32.00% | archive |
| Table B.2 | ideology: Other (Elites) | 3.7% | 75.4 | 3.70% | archive |
| Text | MTurk average within-domain correlation, as given in the body text | .29 | 0.3 | 0.297 | paper_internal |
| Text | Pooled indirect-to-direct ratio | 1.4% | -2.77 | -1.68% | paper_internal |
| Text | Elite sample holding a graduate degree | 71% |  | 71.57% | paper_internal |
| Text | MTurk sample holding a graduate degree | 13% |  | 12.40% | paper_internal |
| Text | Nontarget outcome measures in study 2, as the design paragraph counts them | 3 |  | 4 | paper_internal |
| Text | Its standard error, restated in prose | 0.06 |  | 0.0663 | paper_internal |
| Text | Average spillover effect as a share of the direct effect, restated in the discussion | 1.4% |  | -1.68% | paper_internal |

Every claim the archive or the rewrite does not match.

The 7 rows the rewrite does not match, and the 2 descriptive claims that
do not hold, are set out in `coppock_green_2022_errata.pdf`. The 7 the
archive does not match are the elite ideology column, described above.
`defect_locus` is required on every row carrying an adverse verdict, in
either column and including a descriptive claim that fails, and a row
that fails without one stops the build: an unexplained zero reads as a
failure of this code, and here none of them is. The commonest reading of
the column across this paper is that the archive fails where the rewrite
succeeds, which is the deposit’s clean data rather than either analysis.

**The unseeded quantities are judged against their own distribution.**
Nine of the published quantities come out of a bootstrap that neither
the archive nor the article seeds: the six interval endpoints of the
three sample ratios, and the pooled estimate and its two endpoints,
whose weights are themselves unseeded bootstrap variances. Comparing any
of them to printed precision would be the wrong test. Each is instead
scored against the range its own resampling produces at the `sims <- 10`
the deposit ships, and the range is written into the row’s note, so the
note and the verdict come from the same computation.

# The extraction, and the two instruments that read it

A ground truth built from the pipeline can only ever check the numbers
the pipeline happens to produce, and the most consequential errors in
this article are in sentences rather than in tables. So the published
article and its appendix were read separately from the code, every
numeric token in them extracted with its surrounding context, and each
one classified by hand into `ground_truth/published_claims.csv`.

| Claim type | Meaning | Tokens | With a claim block |
|:---|:---|---:|---:|
| definitional | Scale endpoints, category counts, a stipulated illustration | 32 | 1 |
| descriptive | A claim about shape, sign, count or order rather than a value | 17 | 17 |
| pipeline | A quantity the analysis produces | 84 | 84 |
| structural | Panel counts, model indices, references to a float’s own layout | 26 | 14 |
| transcribed | Copied from another paper or from a reproduced op-ed | 10 | 0 |

The extraction: every numeric token in the article and the appendix,
classified by hand.

The extraction covers digits and spelled-out numbers alike; “the
experiment was conducted three times” and “all five categories” are
claims no token scan sees. The four op-eds the appendix reproduces carry
62 numeric tokens between them, all of them the newspapers’ own figures,
and they get one row each rather than one row per token. Each published
float’s cells are compared one by one in the ground truth and summarised
in the extraction by a single coverage row, which is what the float
coverage figures above report.

**The coverage gate.** `build_ground_truth.R` will not finish unless
every `pipeline` and `descriptive` row of the extraction has a
ground-truth row, every `claim_id` in the ground truth names a claim the
article actually makes, the two files transcribe every shared published
value identically, and `maintained/in_text_claims.R` prints exactly 116
claim lines whose identifiers are the 116 the extraction declares. The
claims file is run as a program under `capture.output` and into an
environment of its own, not scanned as text: a block that errors, or
that ends in a bare expression and so prints nothing when sourced,
satisfies a textual check completely and this one not at all.

**The second instrument.** `maintained/in_text_claims.R` carries the
article’s own sentence verbatim in a block comment and then recomputes
the number underneath it from `maintained/output/`, in the article’s
units and at the article’s precision. It never reads the ground truth,
and it reaches each quantity by a different route from the one
`build_ground_truth.R` takes, so the two can disagree; every one of the
116 values is compared across them and a disagreement stops the build.
Running it prints the whole audit trail, which is the last thing
`run_all.R` does.

The article’s 9 misstatements are collected as 8 entries in
`coppock_green_2022_errata.pdf` at the root of this repository. Each
entry quotes the published sentence, gives the corrected sentence with
the changed token in bold, and computes every value in it from
`maintained/output/` when the document is rendered, so the note cannot
go stale in the way the article did.

# Maintained rewrite

`maintained/` holds one script per published output plus `helpers.R`,
which every script sources first.

| Script | Produces |
|:---|:---|
| helpers.R | Packages, paths, the shared estimation and plotting helpers |
| table_1_static_constraint_correlations.R | Table 1 |
| table_2_employment_discrimination.R | Table 2 |
| table_3_inequality.R | Table 3 |
| table_4_correction_experiment.R | Table 4 |
| table_b2_sample_demographics.R | Appendix table B.2 |
| table_c3_study2_demographics.R | Appendix table C.3 |
| table_c4_welfare_experiment.R | Appendix table C.4 |
| table_d5_study3_demographics.R | Appendix table D.5 |
| figure_4_dynamic_constraint_estimates.R | Figure 4, and the wave 1 estimates three other scripts read |
| figure_b2_wave2_estimates.R | Appendix figure B.2 |
| figure_b3_wave3_estimates.R | Appendix figure B.3 |
| text_significance_counts.R | The counts of significant target and nontarget effects |
| text_amtrak_largest_effects.R | The two Amtrak effects quoted in the results |
| text_inconsistency_rates.R | The two inconsistency rates behind study 4 |
| text_sample_descriptions.R | The study 1 sample composition figures the article states in prose |
| text_study_2_correlations.R | The correlations among study 2’s five outcome measures |
| text_precision_weighted_averages.R | The precision-weighted averages, ratios and bootstrap intervals |
| text_pooled_ratio_metaanalysis.R | The pooled ratio across the three samples |
| in_text_claims.R | No file: one printed line per claim the article makes in prose |

The maintained rewrite, script by script.

## What changed and why

**`rmeta` becomes `metafor`.** The deposited `in_text_numbers.R` pools
the three samples with `rmeta::meta.summaries`. `rmeta` was last updated
in March 2018 and survives on CRAN by the archiving policy rather than
by maintenance, so an archive that depends on it is exposed to a
decision nobody in this program controls. The call passes no `method`
argument, so it fits the fixed-effect model that is `meta.summaries`’s
default, and `metafor::rma(method = "FE")` is its maintained equivalent.
That the two agree is measured rather than assumed:
`run_original_archive.R` fits both to the same inputs in the same
session and records the largest absolute difference across the pooled
estimate, its standard error and both interval endpoints, which is NA,
NA, NA, 2.8e-17. The random-effects fit is reported alongside as an
addition rather than a correction.

| Model | k | Estimate | SE | 95% CI | tau^2 | Q | Q p |
|:---|---:|:---|:---|:---|:---|:---|:---|
| Fixed effects | 3 | -0.0168 | 0.0354 | \[-0.0863, 0.0527\] | 0.00000 | 2.03 | 0.362 |
| Random effects (REML) | 3 | -0.0188 | 0.0404 | \[-0.0980, 0.0603\] | 0.00078 | 2.03 | 0.362 |

Fixed and random effects pooling of the three sample ratios.

**`stargazer` becomes `kableExtra`, and `xtable` plus `print.xtable`
becomes `kableExtra` too.** Both of the archive’s table routes write to
a hardcoded path outside the archive. Every table in the rewrite is
written twice, as a `.csv` carrying full precision and as a `.tex`
carrying the display rounding, both into `maintained/output/`.

**`position_dodgev` becomes `position_dodge`, and `geom_errorbarh`
becomes `geom_linerange`.** `position_dodgev` came from `ggstance`,
which existed to give ggplot2 horizontal geoms it now has natively;
`geom_errorbarh` was deprecated in ggplot2 4.0.0. The three coefficient
plots keep the dependent variable on `y` and the estimate on `x`, so
`position_dodge` dodges along the discrete axis without a `coord_flip`.

**`spread` becomes `pivot_wider`, `%>%` becomes `|>`, and
`rm(list = ls())` is gone from every script.**

**Sample sizes come from the fitted model.** Each table reports the
regression sample size, which is what the published tables report and
which differs per outcome. Counting non-missing outcome values instead
would ignore missingness on the treatment indicator, and it would miss
that only a random third of study 3’s original respondents were asked
the economic system justification question, which is why that column’s
published N is 336 where the manipulation check’s is 1,020.

**Five in-text quantities are computed that the archive never
computed.** The counts of significant target and nontarget effects
before and after the Benjamini-Hochberg correction, the two Amtrak
effects the results section quotes, and the two inconsistency rates
behind study 4 were all stated in the article and produced by no
deposited script. All of them reproduce. The counts are read from the
same estimates figure 4 plots, so the figure and the sentences about it
cannot drift apart.

**No published number is an input to a computation.** Every value in
`maintained/output/` traces to the deposited data through code in this
repository. The published values live only in
`ground_truth/coppock_green_2022_ground_truth.csv`, where they are
compared against, never used.

# Figure verification

The three coefficient plots are the paper’s figure 4 and appendix
figures B.2 and B.3. Each shows a 4 by 4 grid of panels: op-ed
treatments across the columns, outcome families down the rows, with the
within-domain diagonal shaded. The argument is visual and it is the same
at all three waves, so all three are reproduced.

![Figure 4. Wave 1 treatment effects on all 16 outcomes in three
samples. Estimates on the shaded diagonal are effects of a treatment on
its own domain’s outcomes; everything off the diagonal is a nontarget
effect.](maintained/output/figure_4_dynamic_constraint_estimates.png)

![Appendix figure B.2. The same estimates at wave 2, for the two samples
that were
reinterviewed.](maintained/output/figure_b2_wave2_estimates.png)

![Appendix figure B.3. The same estimates at wave 3, MTurk
only.](maintained/output/figure_b3_wave3_estimates.png)

The published figures carry no printed numbers, so they cannot be
checked cell by cell against the article the way a table can. What can
be checked is the estimate behind every point, and that is what each
figure’s CSV records. Figure 4’s carries 192 estimates, of which the 48
on the diagonal and the 144 off it are exactly the two sets the results
section counts.

# Rewrite verification

| Check                                                          | Result   |
|:---------------------------------------------------------------|:---------|
| Archive files verified against Dataverse                       | 46 of 46 |
| Published checksums that disagree with what Dataverse serves   | 0        |
| Files in original/ that are not in the deposit                 | 0        |
| Deposited files ingested into a derived tabular representation | 9        |
| Scripts in the maintained rewrite                              | 20       |
| Ground truth claims                                            | 417      |
| Claims the rewrite reproduces                                  | 393      |
| Claims the rewrite does not reproduce                          | 7        |
| Claims with no published value to check against                | 17       |

Verification summary.

**Determinism.** Only `text_precision_weighted_averages.R` and
`text_pooled_ratio_metaanalysis.R` sample, and both set a seed before
doing so. Running the pipeline twice returns byte-identical CSV and TeX
output, and PDF figures that differ only in the creation timestamp a PDF
records, which is why a post-run hash of a figure PDF is not quoted
anywhere in this report: it would differ on every run.

**Checksums.** `original_manifest.csv` records, for each deposited file,
the MD5 of the bytes Dataverse serves for `?format=original` and the MD5
Dataverse displays. `download_original.R` verifies against the served
checksum and reports any published checksum that disagrees with it,
because the two do not always agree. Here they agree for all 46 files.
The script also stops if `original/` holds anything the manifest does
not list, since running deposited code in the wrong directory adds files
as well as overwriting them. `run_all.R` runs that check before the
analysis and again after it.

**Archive verification.** `ground_truth/archive_values.csv` is read out
of the LaTeX the deposited scripts write, not transcribed, and is
regenerated by `ground_truth/run_original_archive.R`. The deposit’s one
unseeded quantity is drawn under a seed set by that script, so the
recorded value is reproducible even though the deposit’s own run is not.

# R environment

| Component  | Version    |
|:-----------|:-----------|
| R          | 4.6.1      |
| tidyverse  | 2.0.0      |
| estimatr   | 2.0.1.9000 |
| rsample    | 1.3.2      |
| metafor    | 5.0.1      |
| janitor    | 2.2.1      |
| kableExtra | 1.4.0      |
| knitr      | 1.51       |
| here       | 1.0.2      |

Versions this report was built against.

The deposit’s `README.txt` records R 4.0.2 on macOS Catalina, with
`coefplot`, `ggstance` by way of `coefplot`, `xtable`, `rmeta`, `psych`
and `car` among its dependencies. The rewrite drops `coefplot`,
`ggstance`, `stargazer` and `rmeta`, and needs neither `psych` nor
`car`, because it reads the deposited clean data rather than rebuilding
it.
