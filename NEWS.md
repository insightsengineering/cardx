# cardx 0.1.0.9005

* Added the following functions for calculating Analysis Results Data (ARD).
  - `ard_car_anova()` for calculating ANOVA results using `car::Anova()`. (#3)
  - `ard_cohens_d()`, `ard_paired_cohens_d()`, `ard_hedges_g()`, and `ard_paired_hedges_g()` for standardized differences using `effectsize::cohens_d()` and `effectsize::hedges_g()`. (#50)
  - `ard_proptest()` for tests of proportions using `stats::prop.test()`. (#64)
  - `ard_regression_basic()` for basic regression models. The function focuses on matching terms to underlying variables names. (#46)
  - `ard_smd()` for calculating standardized mean differences using `smd::smd()`. (#4)
  - `ard_svycontinuous()` for calculating univariate summary statistics from weighted/survey data using many functions from the {survey} package. (#68)
  - `ard_svychisq()` for weighted/survey chi-squared test using `survey::svychisq()`. (#72)
  - `ard_svyttest()` for weighted/survey t-tests using `survey::svyttest()`. (#70)
  - `ard_vif()` for calculating the variance inflation factor using `car::vif()`. (#10)

* Updated functions `ard_ttest()`, `ard_paired_ttest()`, `ard_wilcoxtest()`, `ard_paired_wilcoxtest()`, `ard_chisqtest()`, `ard_fishertest()`, `ard_kruskaltest()`, `ard_mcnemartest()`, and `ard_moodtest()` to accept multiple variables at once. Independent tests are calculated for each variable. The `variable` argument is renamed to `variables`. (#77)

# cardx 0.1.0

* Initial release.
