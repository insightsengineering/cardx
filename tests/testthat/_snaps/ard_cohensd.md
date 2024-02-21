# ard_cohensd() works

    Code
      as.data.frame(ard_cohensd(cards::ADSL, by = ARM, variable = AGE))
    Output
        group1 variable context  stat_name           stat_label statistic
      1    ARM      AGE cohensd   estimate Effect Size Estimate      NULL
      2    ARM      AGE cohensd conf.level  CI Confidence Level      NULL
      3    ARM      AGE cohensd   conf.low       CI Lower Bound      NULL
      4    ARM      AGE cohensd  conf_high            conf_high      NULL
      5    ARM      AGE cohensd         mu                   mu         0
      6    ARM      AGE cohensd     paired               paired     FALSE
      7    ARM      AGE cohensd  pooled_sd            pooled_sd      TRUE
        statistic_fmt_fn warning                                           error
      1             NULL    NULL Grouping variable y must have exactly 2 levels.
      2             NULL    NULL Grouping variable y must have exactly 2 levels.
      3             NULL    NULL Grouping variable y must have exactly 2 levels.
      4             NULL    NULL Grouping variable y must have exactly 2 levels.
      5                1    NULL Grouping variable y must have exactly 2 levels.
      6             NULL    NULL Grouping variable y must have exactly 2 levels.
      7             NULL    NULL Grouping variable y must have exactly 2 levels.

# ard_paired_cohensd() works

    Code
      as.data.frame(ard_paired_cohensd(dplyr::mutate(ADSL_paired, ARM = ifelse(dplyr::row_number() ==
        1L, "3rd ARM", ARM)), by = ARM, variable = AGE, id = USUBJID))
    Output
        group1 variable context  stat_name           stat_label statistic
      1    ARM      AGE cohensd   estimate Effect Size Estimate      NULL
      2    ARM      AGE cohensd conf.level  CI Confidence Level      NULL
      3    ARM      AGE cohensd   conf.low       CI Lower Bound      NULL
      4    ARM      AGE cohensd  conf_high            conf_high      NULL
      5    ARM      AGE cohensd         mu                   mu         0
      6    ARM      AGE cohensd     paired               paired      TRUE
      7    ARM      AGE cohensd  pooled_sd            pooled_sd      TRUE
        statistic_fmt_fn warning                                                error
      1             NULL    NULL The `by` argument must have two and only two levels.
      2             NULL    NULL The `by` argument must have two and only two levels.
      3             NULL    NULL The `by` argument must have two and only two levels.
      4             NULL    NULL The `by` argument must have two and only two levels.
      5                1    NULL The `by` argument must have two and only two levels.
      6             NULL    NULL The `by` argument must have two and only two levels.
      7             NULL    NULL The `by` argument must have two and only two levels.

