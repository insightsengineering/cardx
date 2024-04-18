# ard_effectsize_cohens_d() works

    Code
      as.data.frame(dplyr::select(ard_effectsize_cohens_d(cards::ADSL, by = ARM,
      variables = AGE), c("variable", "stat_name", "error")))
    Output
        variable   stat_name                                           error
      1      AGE    estimate Grouping variable y must have exactly 2 levels.
      2      AGE  conf.level Grouping variable y must have exactly 2 levels.
      3      AGE    conf.low Grouping variable y must have exactly 2 levels.
      4      AGE   conf.high Grouping variable y must have exactly 2 levels.
      5      AGE          mu Grouping variable y must have exactly 2 levels.
      6      AGE      paired Grouping variable y must have exactly 2 levels.
      7      AGE   pooled_sd Grouping variable y must have exactly 2 levels.
      8      AGE alternative Grouping variable y must have exactly 2 levels.

---

    Code
      as.data.frame(dplyr::slice_head(dplyr::group_by(dplyr::select(
        ard_effectsize_cohens_d(dplyr::filter(cards::ADSL, ARM %in% c("Placebo",
          "Xanomeline High Dose")), by = ARM, variables = c(BMIBL, HEIGHTBL)), c(1:3,
        5:6)), variable), n = 3))
    Output
        group1 variable             context           stat_label      stat
      1    ARM    BMIBL effectsize_cohens_d Effect Size Estimate -0.436653
      2    ARM    BMIBL effectsize_cohens_d  CI Confidence Level      0.95
      3    ARM    BMIBL effectsize_cohens_d       CI Lower Bound -0.740282
      4    ARM HEIGHTBL effectsize_cohens_d Effect Size Estimate -0.299056
      5    ARM HEIGHTBL effectsize_cohens_d  CI Confidence Level      0.95
      6    ARM HEIGHTBL effectsize_cohens_d       CI Lower Bound -0.600975

# ard_effectsize_paired_cohens_d() works

    Code
      as.data.frame(dplyr::select(ard_effectsize_paired_cohens_d(dplyr::mutate(
        ADSL_paired, ARM = ifelse(dplyr::row_number() == 1L, "3rd ARM", ARM)), by = ARM,
      variable = AGE, id = USUBJID), c("variable", "stat_name", "error")))
    Output
        variable   stat_name                                                error
      1      AGE    estimate The `by` argument must have two and only two levels.
      2      AGE  conf.level The `by` argument must have two and only two levels.
      3      AGE    conf.low The `by` argument must have two and only two levels.
      4      AGE   conf.high The `by` argument must have two and only two levels.
      5      AGE          mu The `by` argument must have two and only two levels.
      6      AGE      paired The `by` argument must have two and only two levels.
      7      AGE   pooled_sd The `by` argument must have two and only two levels.
      8      AGE alternative The `by` argument must have two and only two levels.

