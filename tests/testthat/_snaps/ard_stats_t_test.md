# ard_stats_t_test() works

    Code
      as.data.frame(ard_stats_t_test(cards::ADSL, by = ARM, variable = AGE,
      var.equal = TRUE))
    Output
         group1 variable context   stat_name          stat_label  stat fmt_fn warning
      1     ARM      AGE   ttest    estimate     Mean Difference  NULL   NULL    NULL
      2     ARM      AGE   ttest   estimate1        Group 1 Mean  NULL   NULL    NULL
      3     ARM      AGE   ttest   estimate2        Group 2 Mean  NULL   NULL    NULL
      4     ARM      AGE   ttest   statistic         t Statistic  NULL   NULL    NULL
      5     ARM      AGE   ttest     p.value             p-value  NULL   NULL    NULL
      6     ARM      AGE   ttest   parameter  Degrees of Freedom  NULL   NULL    NULL
      7     ARM      AGE   ttest    conf.low      CI Lower Bound  NULL   NULL    NULL
      8     ARM      AGE   ttest   conf.high      CI Upper Bound  NULL   NULL    NULL
      9     ARM      AGE   ttest      method              method  NULL   NULL    NULL
      10    ARM      AGE   ttest alternative         alternative  NULL   NULL    NULL
      11    ARM      AGE   ttest          mu             H0 Mean     0      1    NULL
      12    ARM      AGE   ttest      paired       Paired t-test FALSE   NULL    NULL
      13    ARM      AGE   ttest   var.equal     Equal Variances  TRUE   NULL    NULL
      14    ARM      AGE   ttest  conf.level CI Confidence Level  0.95      1    NULL
                                              error
      1  grouping factor must have exactly 2 levels
      2  grouping factor must have exactly 2 levels
      3  grouping factor must have exactly 2 levels
      4  grouping factor must have exactly 2 levels
      5  grouping factor must have exactly 2 levels
      6  grouping factor must have exactly 2 levels
      7  grouping factor must have exactly 2 levels
      8  grouping factor must have exactly 2 levels
      9  grouping factor must have exactly 2 levels
      10 grouping factor must have exactly 2 levels
      11 grouping factor must have exactly 2 levels
      12 grouping factor must have exactly 2 levels
      13 grouping factor must have exactly 2 levels
      14 grouping factor must have exactly 2 levels

# ard_stats_paired_t_test() works

    Code
      as.data.frame(ard_stats_paired_t_test(dplyr::mutate(ADSL_paired, ARM = ifelse(
        dplyr::row_number() == 1L, "3rd ARM", ARM)), by = ARM, variable = AGE, id = USUBJID,
      var.equal = TRUE))
    Output
         group1 variable context   stat_name          stat_label stat fmt_fn warning
      1     ARM      AGE   ttest    estimate     Mean Difference NULL   NULL    NULL
      2     ARM      AGE   ttest   estimate1        Group 1 Mean NULL   NULL    NULL
      3     ARM      AGE   ttest   estimate2        Group 2 Mean NULL   NULL    NULL
      4     ARM      AGE   ttest   statistic         t Statistic NULL   NULL    NULL
      5     ARM      AGE   ttest     p.value             p-value NULL   NULL    NULL
      6     ARM      AGE   ttest   parameter  Degrees of Freedom NULL   NULL    NULL
      7     ARM      AGE   ttest    conf.low      CI Lower Bound NULL   NULL    NULL
      8     ARM      AGE   ttest   conf.high      CI Upper Bound NULL   NULL    NULL
      9     ARM      AGE   ttest      method              method NULL   NULL    NULL
      10    ARM      AGE   ttest alternative         alternative NULL   NULL    NULL
      11    ARM      AGE   ttest          mu             H0 Mean    0      1    NULL
      12    ARM      AGE   ttest      paired       Paired t-test TRUE   NULL    NULL
      13    ARM      AGE   ttest   var.equal     Equal Variances TRUE   NULL    NULL
      14    ARM      AGE   ttest  conf.level CI Confidence Level 0.95      1    NULL
                                                        error
      1  The `by` argument must have two and only two levels.
      2  The `by` argument must have two and only two levels.
      3  The `by` argument must have two and only two levels.
      4  The `by` argument must have two and only two levels.
      5  The `by` argument must have two and only two levels.
      6  The `by` argument must have two and only two levels.
      7  The `by` argument must have two and only two levels.
      8  The `by` argument must have two and only two levels.
      9  The `by` argument must have two and only two levels.
      10 The `by` argument must have two and only two levels.
      11 The `by` argument must have two and only two levels.
      12 The `by` argument must have two and only two levels.
      13 The `by` argument must have two and only two levels.
      14 The `by` argument must have two and only two levels.

