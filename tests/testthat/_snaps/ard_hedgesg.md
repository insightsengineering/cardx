# ard_paired_hedgesg() works

    Code
      as.data.frame(dplyr::select(ard_paired_hedgesg(dplyr::mutate(ADSL_paired, ARM = ifelse(
        dplyr::row_number() == 1L, "3rd ARM", ARM)), by = ARM, variable = AGE, id = USUBJID),
      c("variable", "stat_name", "error")))
    Output
        variable  stat_name                                                error
      1      AGE   estimate The `by` argument must have two and only two levels.
      2      AGE conf.level The `by` argument must have two and only two levels.
      3      AGE   conf.low The `by` argument must have two and only two levels.
      4      AGE  conf_high The `by` argument must have two and only two levels.
      5      AGE         mu The `by` argument must have two and only two levels.
      6      AGE     paired The `by` argument must have two and only two levels.
      7      AGE  pooled_sd The `by` argument must have two and only two levels.

