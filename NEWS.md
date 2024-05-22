# cardx 0.1.0.9042

### Breaking Changes

* Updated function names to follow the pattern `ard_<pkgname>_<fnname>()`. Former functions names have _not_ been deprecated. (#106)

```r
ard_ttest()             -> ard_stats_t_test()
ard_paired_ttest()      -> ard_stats_paired_t_test()
ard_wilcoxtest()        -> ard_stats_wilcox_test()
ard_paired_wilcoxtest() -> ard_stats_paired_wilcox_test()
ard_chisqtest()         -> ard_stats_chisq_test()
ard_fishertest()        -> ard_stats_fisher_test()
ard_kruskaltest()       -> ard_stats_kruskal_test()
ard_mcnemartest()       -> ard_stats_mcnemar_test()
ard_moodtest()          -> ard_stats_mood_test()
```

### New Features

* Added the following functions for calculating Analysis Results Data (ARD).
  - `ard_stats_aov()` for calculating ANOVA results using `stats::aov()`. (#3)
  - `ard_stats_anova()` for calculating ANOVA results using `stats::anova()`. (#12) 
  - `ard_stats_mcnemar_test_long()` for McNemar's test from long data using `stats::mcnemar.test()`. 
  - `ard_aod_wald_test()` for calculating Wald Tests for regression models using `aod::wald.test()`. (#84)
  - `ard_car_anova()` for calculating ANOVA results using `car::Anova()`. (#3)
  - `ard_stats_oneway_test()` for calculating ANOVA results using `stats::oneway.test()`. (#3)
  - `ard_effectsize_cohens_d()`, `ard_effectsize_paired_cohens_d()`, `ard_effectsize_hedges_g()`, and `ard_effectsize_paired_hedges_g()` for standardized differences using `effectsize::cohens_d()` and `effectsize::hedges_g()`. (#50)
  - `ard_stats_prop_test()` for tests of proportions using `stats::prop.test()`. (#64)
  - `ard_regression_basic()` for basic regression models. The function focuses on matching terms to underlying variables names. (#46)
  - `ard_smd_smd()` for calculating standardized mean differences using `smd::smd()`. (#4)
  - `ard_survival_survfit()` for survival analyses using `survival::survfit()`. (#43)
  - `ard_continuous.survey.design()` for calculating univariate summary statistics from weighted/survey data using many functions from the {survey} package. (#68)
  - `ard_attributes.survey.design()` for summarizing labels and attributes from weighted/survey data using many functions from the {survey} package.
  - `ard_survey_svychisq()` for weighted/survey chi-squared test using `survey::svychisq()`. (#72)
  - `ard_survey_svyttest()` for weighted/survey t-tests using `survey::svyttest()`. (#70)
  - `ard_survey_svyranktest()` for weighted/survey rank tests using `survey::svyranktest()`. (#71)
  - `ard_car_vif()` for calculating the variance inflation factor using `car::vif()`. (#10)
  - `ard_emmeans_mean_difference()` for calculating the least-squares mean differences using the {emmeans} package. (#34)

* Updated functions `ard_stats_t_test()`, `ard_stats_paired_t_test()`, `ard_stats_wilcox_test()`, `ard_stats_paired_wilcox_test()`, `ard_stats_chisq_test()`, `ard_stats_fisher_test()`, `ard_stats_kruskal_test()`, `ard_stats_mcnemar_test()`, and `ard_stats_mood_test()` to accept multiple variables at once. Independent tests are calculated for each variable. The `variable` argument is renamed to `variables`. (#77)

* Updated `ard_stats_t_test()` and `ard_stats_wilcox_test()` to no longer require the `by` argument, which yields central estimates with their confidence intervals. (#82)

* Imported cli call environment functions from `https://github.com/ddsjoberg/standalone/blob/main/R/standalone-cli_call_env.R` and implemented `set_cli_abort_call` in user-facing functions. (#111)

* Added `ard_survival_survdiff()` for creating results from `survival::survdiff()`. (#113)

# cardx 0.1.0

* Initial release.
