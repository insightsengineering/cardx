# ard_ttest() works

    Code
      as.data.frame(ard_ttest(cards::ADSL, by = ARM, variable = AGE, var.equal = TRUE))
    Output
         group1 variable context   stat_name          stat_label statistic
      1     ARM      AGE   ttest    estimate     Mean Difference      NULL
      2     ARM      AGE   ttest   estimate1        Group 1 Mean      NULL
      3     ARM      AGE   ttest   estimate2        Group 2 Mean      NULL
      4     ARM      AGE   ttest   statistic         t Statistic      NULL
      5     ARM      AGE   ttest     p.value             p-value      NULL
      6     ARM      AGE   ttest   parameter  Degrees of Freedom      NULL
      7     ARM      AGE   ttest    conf.low      CI Lower Bound      NULL
      8     ARM      AGE   ttest   conf.high      CI Upper Bound      NULL
      9     ARM      AGE   ttest      method              method      NULL
      10    ARM      AGE   ttest alternative         alternative      NULL
      11    ARM      AGE   ttest          mu             H0 Mean         0
      12    ARM      AGE   ttest      paired       Paired t-test     FALSE
      13    ARM      AGE   ttest   var.equal     Equal Variances      TRUE
      14    ARM      AGE   ttest  conf.level CI Confidence Level      0.95
         statistic_fmt_fn warning                                      error
      1              NULL    NULL grouping factor must have exactly 2 levels
      2              NULL    NULL grouping factor must have exactly 2 levels
      3              NULL    NULL grouping factor must have exactly 2 levels
      4              NULL    NULL grouping factor must have exactly 2 levels
      5              NULL    NULL grouping factor must have exactly 2 levels
      6              NULL    NULL grouping factor must have exactly 2 levels
      7              NULL    NULL grouping factor must have exactly 2 levels
      8              NULL    NULL grouping factor must have exactly 2 levels
      9              NULL    NULL grouping factor must have exactly 2 levels
      10             NULL    NULL grouping factor must have exactly 2 levels
      11                1    NULL grouping factor must have exactly 2 levels
      12             NULL    NULL grouping factor must have exactly 2 levels
      13             NULL    NULL grouping factor must have exactly 2 levels
      14                1    NULL grouping factor must have exactly 2 levels

# ard_paired_ttest() works

    Code
      as.data.frame(ard_paired_ttest(dplyr::mutate(ADSL_paired, ARM = ifelse(dplyr::row_number() ==
        1L, "3rd ARM", ARM)), by = ARM, variable = AGE, id = USUBJID, var.equal = TRUE))
    Output
         group1 variable context   stat_name          stat_label statistic
      1     ARM      AGE   ttest    estimate     Mean Difference      NULL
      2     ARM      AGE   ttest   estimate1        Group 1 Mean      NULL
      3     ARM      AGE   ttest   estimate2        Group 2 Mean      NULL
      4     ARM      AGE   ttest   statistic         t Statistic      NULL
      5     ARM      AGE   ttest     p.value             p-value      NULL
      6     ARM      AGE   ttest   parameter  Degrees of Freedom      NULL
      7     ARM      AGE   ttest    conf.low      CI Lower Bound      NULL
      8     ARM      AGE   ttest   conf.high      CI Upper Bound      NULL
      9     ARM      AGE   ttest      method              method      NULL
      10    ARM      AGE   ttest alternative         alternative      NULL
      11    ARM      AGE   ttest          mu             H0 Mean         0
      12    ARM      AGE   ttest      paired       Paired t-test      TRUE
      13    ARM      AGE   ttest   var.equal     Equal Variances      TRUE
      14    ARM      AGE   ttest  conf.level CI Confidence Level      0.95
         statistic_fmt_fn warning
      1              NULL    NULL
      2              NULL    NULL
      3              NULL    NULL
      4              NULL    NULL
      5              NULL    NULL
      6              NULL    NULL
      7              NULL    NULL
      8              NULL    NULL
      9              NULL    NULL
      10             NULL    NULL
      11                1    NULL
      12             NULL    NULL
      13             NULL    NULL
      14                1    NULL
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

