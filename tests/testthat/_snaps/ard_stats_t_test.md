# ard_stats_t_test() works

    Code
      as.data.frame(ard_stats_t_test(cards::ADSL, by = ARM, variable = AGE,
      var.equal = TRUE))
    Output
         group1 variable      context   stat_name          stat_label  stat fmt_fn
      1     ARM      AGE stats_t_test    estimate     Mean Difference  NULL   NULL
      2     ARM      AGE stats_t_test   estimate1        Group 1 Mean  NULL   NULL
      3     ARM      AGE stats_t_test   estimate2        Group 2 Mean  NULL   NULL
      4     ARM      AGE stats_t_test   statistic         t Statistic  NULL   NULL
      5     ARM      AGE stats_t_test     p.value             p-value  NULL   NULL
      6     ARM      AGE stats_t_test   parameter  Degrees of Freedom  NULL   NULL
      7     ARM      AGE stats_t_test    conf.low      CI Lower Bound  NULL   NULL
      8     ARM      AGE stats_t_test   conf.high      CI Upper Bound  NULL   NULL
      9     ARM      AGE stats_t_test      method              method  NULL   NULL
      10    ARM      AGE stats_t_test alternative         alternative  NULL   NULL
      11    ARM      AGE stats_t_test          mu             H0 Mean     0      1
      12    ARM      AGE stats_t_test      paired       Paired t-test FALSE   NULL
      13    ARM      AGE stats_t_test   var.equal     Equal Variances  TRUE   NULL
      14    ARM      AGE stats_t_test  conf.level CI Confidence Level  0.95      1
         warning                                      error
      1     NULL grouping factor must have exactly 2 levels
      2     NULL grouping factor must have exactly 2 levels
      3     NULL grouping factor must have exactly 2 levels
      4     NULL grouping factor must have exactly 2 levels
      5     NULL grouping factor must have exactly 2 levels
      6     NULL grouping factor must have exactly 2 levels
      7     NULL grouping factor must have exactly 2 levels
      8     NULL grouping factor must have exactly 2 levels
      9     NULL grouping factor must have exactly 2 levels
      10    NULL grouping factor must have exactly 2 levels
      11    NULL grouping factor must have exactly 2 levels
      12    NULL grouping factor must have exactly 2 levels
      13    NULL grouping factor must have exactly 2 levels
      14    NULL grouping factor must have exactly 2 levels

# ard_stats_paired_t_test() works

    Code
      as.data.frame(ard_stats_paired_t_test(dplyr::mutate(ADSL_paired, ARM = ifelse(
        dplyr::row_number() == 1L, "3rd ARM", ARM)), by = ARM, variable = AGE, id = USUBJID,
      var.equal = TRUE))
    Output
         group1 variable      context   stat_name          stat_label stat fmt_fn
      1     ARM      AGE stats_t_test    estimate     Mean Difference NULL   NULL
      2     ARM      AGE stats_t_test   estimate1        Group 1 Mean NULL   NULL
      3     ARM      AGE stats_t_test   estimate2        Group 2 Mean NULL   NULL
      4     ARM      AGE stats_t_test   statistic         t Statistic NULL   NULL
      5     ARM      AGE stats_t_test     p.value             p-value NULL   NULL
      6     ARM      AGE stats_t_test   parameter  Degrees of Freedom NULL   NULL
      7     ARM      AGE stats_t_test    conf.low      CI Lower Bound NULL   NULL
      8     ARM      AGE stats_t_test   conf.high      CI Upper Bound NULL   NULL
      9     ARM      AGE stats_t_test      method              method NULL   NULL
      10    ARM      AGE stats_t_test alternative         alternative NULL   NULL
      11    ARM      AGE stats_t_test          mu             H0 Mean    0      1
      12    ARM      AGE stats_t_test      paired       Paired t-test TRUE   NULL
      13    ARM      AGE stats_t_test   var.equal     Equal Variances TRUE   NULL
      14    ARM      AGE stats_t_test  conf.level CI Confidence Level 0.95      1
         warning                                                error
      1     NULL The `by` argument must have two and only two levels.
      2     NULL The `by` argument must have two and only two levels.
      3     NULL The `by` argument must have two and only two levels.
      4     NULL The `by` argument must have two and only two levels.
      5     NULL The `by` argument must have two and only two levels.
      6     NULL The `by` argument must have two and only two levels.
      7     NULL The `by` argument must have two and only two levels.
      8     NULL The `by` argument must have two and only two levels.
      9     NULL The `by` argument must have two and only two levels.
      10    NULL The `by` argument must have two and only two levels.
      11    NULL The `by` argument must have two and only two levels.
      12    NULL The `by` argument must have two and only two levels.
      13    NULL The `by` argument must have two and only two levels.
      14    NULL The `by` argument must have two and only two levels.

