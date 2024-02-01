# ard_wilcoxtest() works

    Code
      as.data.frame(ard_wilcoxtest(cards::ADSL, by = ARM, variable = AGE, correct = FALSEE))
    Output
         group1 variable context   stat_name          stat_label statistic
      1     ARM      AGE   ttest   statistic X-squared Statistic      NULL
      2     ARM      AGE   ttest     p.value             p-value      NULL
      3     ARM      AGE   ttest      method              method      NULL
      4     ARM      AGE   ttest alternative         alternative      NULL
      5     ARM      AGE   ttest          mu                  mu      NULL
      6     ARM      AGE   ttest      paired         Paired test      NULL
      7     ARM      AGE   ttest       exact               exact      NULL
      8     ARM      AGE   ttest     correct             correct      NULL
      9     ARM      AGE   ttest    conf.int            conf.int      NULL
      10    ARM      AGE   ttest  conf.level CI Confidence Level      NULL
      11    ARM      AGE   ttest    tol.root            tol.root      NULL
      12    ARM      AGE   ttest digits.rank         digits.rank      NULL
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
      11             NULL    NULL grouping factor must have exactly 2 levels
      12             NULL    NULL grouping factor must have exactly 2 levels

# ard_paired_wilcoxtest() works

    Code
      as.data.frame(ard_paired_wilcoxtest(dplyr::mutate(ADSL_paired, ARM = ifelse(
        dplyr::row_number() == 1L, "3rd ARM", ARM)), by = ARM, variable = AGE, id = USUBJID,
      correct = FALSE, conf.int = TRUE))
    Output
         group1 variable context   stat_name          stat_label statistic
      1     ARM      AGE   ttest   statistic X-squared Statistic      NULL
      2     ARM      AGE   ttest     p.value             p-value      NULL
      3     ARM      AGE   ttest      method              method      NULL
      4     ARM      AGE   ttest alternative         alternative      NULL
      5     ARM      AGE   ttest          mu                  mu         0
      6     ARM      AGE   ttest      paired         Paired test      TRUE
      7     ARM      AGE   ttest       exact               exact      NULL
      8     ARM      AGE   ttest     correct             correct     FALSE
      9     ARM      AGE   ttest    conf.int            conf.int      TRUE
      10    ARM      AGE   ttest  conf.level CI Confidence Level      0.95
      11    ARM      AGE   ttest    tol.root            tol.root     1e-04
      12    ARM      AGE   ttest digits.rank         digits.rank       Inf
         statistic_fmt_fn warning
      1              NULL    NULL
      2              NULL    NULL
      3              NULL    NULL
      4              NULL    NULL
      5                 1    NULL
      6              NULL    NULL
      7              NULL    NULL
      8              NULL    NULL
      9              NULL    NULL
      10                1    NULL
      11                1    NULL
      12                1    NULL
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

