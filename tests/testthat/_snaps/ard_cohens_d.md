# ard_cohens_d() works

    Code
      as.data.frame(dplyr::select(ard_cohens_d(cards::ADSL, by = ARM, variable = AGE),
      c("variable", "stat_name", "error")))
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

# ard_paired_cohens_d() works

    Code
      as.data.frame(dplyr::select(ard_paired_cohens_d(dplyr::mutate(ADSL_paired, ARM = ifelse(
        dplyr::row_number() == 1L, "3rd ARM", ARM)), by = ARM, variable = AGE, id = USUBJID),
      c("variable", "stat_name", "error")))
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

