# cardx 0.1.0.9012

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
  - `ard_aod_wald_test()` for calculating Wald Tests for regression models using `aod::wald.test()`. (#3)
  - `ard_car_anova()` for calculating ANOVA results using `car::Anova()`. (#3)
  - `ard_onewaytest()` for calculating ANOVA results using `stats::oneway.test()`. (#3)
  - `ard_cohens_d()`, `ard_paired_cohens_d()`, `ard_hedges_g()`, and `ard_paired_hedges_g()` for standardized differences using `effectsize::cohens_d()` and `effectsize::hedges_g()`. (#50)
  - `ard_proptest()` for tests of proportions using `stats::prop.test()`. (#64)
  - `ard_regression_basic()` for basic regression models. The function focuses on matching terms to underlying variables names. (#46)
  - `ard_smd()` for calculating standardized mean differences using `smd::smd()`. (#4)
  - `ard_survfit()` for survival analyses using `survival::survfit()`. (#43)
  - `ard_svycontinuous()` for calculating univariate summary statistics from weighted/survey data using many functions from the {survey} package. (#68)
  - `ard_svychisq()` for weighted/survey chi-squared test using `survey::svychisq()`. (#72)
  - `ard_svyttest()` for weighted/survey t-tests using `survey::svyttest()`. (#70)
  - `ard_svyranktest()` for weighted/survey rank tests using `survey::svyranktest()`. (#71)
  - `ard_vif()` for calculating the variance inflation factor using `car::vif()`. (#10)

* Updated functions `ard_stats_t_test()`, `ard_stats_paired_t_test()`, `ard_stats_wilcox_test()`, `ard_stats_paired_wilcox_test()`, `ard_stats_chisq_test()`, `ard_stats_fisher_test()`, `ard_stats_kruskal_test()`, `ard_stats_mcnemar_test()`, and `ard_stats_mood_test()` to accept multiple variables at once. Independent tests are calculated for each variable. The `variable` argument is renamed to `variables`. (#77)

* Updated `ard_stats_t_test()` and `ard_stats_wilcox_test()` to no longer require the `by` argument, which yields central estimates with their confidence intervals. (#82)

# cardx 0.1.0

* Initial release.
