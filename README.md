# TECH6 term paper: replication of Meyersson (2014)

Balázs Dániel Biró (s242441), TECH6 Data Science and Econometrics, NHH, autumn 2026.

Replication of Tables I, II, III and VII and Figures 1–4 of

> Meyersson, E. (2014). "Islamic Rule and the Empowerment of the Poor and Pious." *Econometrica* 82(1), 229–269.

and two extensions. Everything is in R, except the lasso in Extension B, which is in Python (scikit-learn).

## How to run

`regdata0.dta` is in `data/`, so the pipeline runs as it stands.

1. Install the packages once:

   ```
   Rscript -e 'install.packages(c("tidyverse", "haven", "fixest", "patchwork", "knitr"))'
   pip install -r requirements.txt
   ```

2. From the repository root:

   ```
   Rscript run_all.R
   ```

`run_all.R` runs the scripts in `R/` in order, each in its own R session, and then writes the package versions to `output/session_info.txt`. `R/08_extension_b.R` and `run_all.R` call `python3`, so Python has to be on the path. The scripts print the main results and sample sizes to the console and write the tables as LaTeX to `output/tables` and the figures to `output/figures`.

The write-up itself is not in this repository; it is submitted separately. It takes its tables and figures straight from `output/`, which is committed so the results can be read without running anything.

## Scripts

| Script | Output |
| --- | --- |
| `R/00_setup.R` | Packages, controls, the main bandwidths for women and men, the RD regression `rd_reg()` and the `etable()` settings. Sourced by every script |
| `R/01_prepare_data.R` | Estimation samples in `data/derived/` (not committed) |
| `R/02_table1.R` | `table1.tex`, summary statistics |
| `R/03_table2.R` | `table2_women.tex`, `table2_men.tex`; the enrolment bandwidths and the women = men test are in this script |
| `R/04_table3.R` | `table3.tex` |
| `R/05_table7.R` | `table7_hs1519.tex`, `table7_hs1529.tex`, `table7_nevm1519.tex`; the six Table VII bandwidths are in this script |
| `R/06_figures.R` | `figure1`–`figure4` (PDF and PNG); prints the McCrary test and the eight covariate regressions behind Figure 3. The McCrary test, `mccrary_test()`, is written out in this script |
| `R/07_extension_a.R` | `ext_a.tex`, `ext_a_cohorts` figure |
| `R/08_extension_b.R`, `py/ext_b_lasso.py` | `ext_b.tex`; `ext_b_lasso_fit.csv` and `ext_b_lasso_coefficients.csv` from the lasso |

## Specification

| Choice | Setting |
| --- | --- |
| Sample | Complete cases on both outcomes, `i94`, `iwm94`, `pcode` and the 11 controls: 2,629 municipalities |
| Table VII sample | Complete cases on the six 2011 outcomes and the same variables: 2,338 |
| Estimator | OLS on municipalities with \|`iwm94`\| < h, with a separate slope on each side of the cutoff |
| Bandwidths | The ones printed in the paper, to three decimals: 0.240 women, 0.323 men, 0.205 and 0.230 for enrolment, and the six in Table VII |
| Controls | `vshr_islam1994`, `partycount`, `lpop1994`, `ageshr19`, `ageshr60`, `sexr`, `shhs`, `merkezi`, `merkezp`, `subbuyuk`, `buyuk` |
| Province fixed effects | The 81 dummies `pd_1`–`pd_81`, entered as one factor |
| Standard errors | Clustered by province (`pcode`, 67 values) |
| Table II, women = men test | One pooled regression with every regressor interacted with a dummy for men |

The table notes in the paper do not list household size among the controls or mention the province dummies in Table III. I looked up the full control list, and the bins and windows of the figures, in the do-files of the paper's replication package (`9878_data_and_programs.zip`, on the [article's page at the Econometric Society](https://www.econometricsociety.org/publications/econometrica/2014/01/01/islamic-rule-and-empowerment-poor-and-pious)).

## Where the replication differs from the paper

- **Sample sizes, third decimals and stars.** The bandwidths are the rounded values printed in the paper, so some windows hold a few municipalities more or fewer than the paper's: 1,021 against 1,020 in the main window for women, and up to six in Table VII at half and double the bandwidth. Three estimates in Table VII differ in the third decimal, and for women aged 15–29 the stars change at half the bandwidth and at the bandwidth.
- **Table II, women = men.** The difference between the two effects is the same as in the paper, but its standard error is not: fixest also corrects for the number of regressors and uses the t distribution, so my p-values are somewhat larger. The conclusion at the main specification is the same; with a cubic in the margin (column 8) the difference is significant at 10% instead of 5%.
- **Figure 2 (b).** Same test and sample as the paper: McCrary's test on the 2,543 municipalities with the women's outcome and \|`iwm94`\| < 0.6. The test is written in R following `DCdensity.ado` from the replication package, since rdd, the R package with the test, is no longer on CRAN. The panel draws the two weighted lines that the test compares at the cutoff, in place of the paper's density estimate over the whole range.
- **Figure 3.** Eight separate RD regressions, one per covariate, at the women's bandwidth and without controls, in place of the paper's joint test with a bandwidth for each covariate.
- **Figures 3 and 4.** A straight line on each side of the cutoff, in place of the paper's local linear smoother. The bins and windows are the paper's.
- **Table I.** No significance stars and no row for the Islamic-mayor dummy; the difference has its standard error.

## Extensions

**A. Was the discontinuity there before 1994?** The paper's RD, with its controls and bandwidth for women, on outcomes the 1994 mayor could not have affected: the high school share of 15–20 year-olds in the 1990 census, next to the 2000 estimate on the same municipalities, and the high school share of older cohorts in 2000, for women and men. The minimum detectable effect, (1.96 + 0.84) × SE, shows what the 1990 data can detect. Checks: the placebo at h/2 and 2h, and an RD on being in the 1990 sample.

**B. For whom does the effect hold?** The effect on young women in two median splits inside the women's window: poverty, measured by predicted schooling from a lasso on characteristics fixed before 1994 and trained on municipalities without an Islamic mayor, and piety, measured by the Islamic vote share in 1994. Checks: the number of treated municipalities in each group, and an RD on predicted schooling.

## Software

R 4.6.2 with fixest 0.14.2, tidyverse 2.0.0 (ggplot2 4.0.4), haven 2.5.5, patchwork 1.3.2 and knitr 1.52; Python 3.11 with pandas 2.2.2 and scikit-learn 1.5.1. `output/session_info.txt` records the versions of the last run. The seed in `py/ext_b_lasso.py` fixes the cross-validation folds; nothing else is random.

## Layout

```
run_all.R          runs everything
R/                 scripts 00–08
py/                the lasso step of Extension B
requirements.txt   Python packages
data/              regdata0.dta, the course data file
output/tables/     tables as LaTeX, lasso results as CSV
output/figures/    figures as PDF and PNG
```
