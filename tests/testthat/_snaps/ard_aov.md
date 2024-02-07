# ard_aov() works

    Code
      as.data.frame(ard_aov(cards::ADSL, by = ARM, variable = AGEGR1))
    Output
         group1 variable context   stat_name             stat_label statistic
      1     ARM   AGEGR1   ANOVA        term                   Term      NULL
      2     ARM   AGEGR1   ANOVA          df     Degrees of Freedom      NULL
      3     ARM   AGEGR1   ANOVA       sumsq         Sum of Squares      NULL
      4     ARM   AGEGR1   ANOVA      meansq Mean of Sum of Squares      NULL
      5     ARM   AGEGR1   ANOVA   statistic            F Statistic      NULL
      6     ARM   AGEGR1   ANOVA     p.value                p-value      NULL
      7     ARM   AGEGR1   ANOVA     formula                formula          
      8     ARM   AGEGR1   ANOVA projections            projections     FALSE
      9     ARM   AGEGR1   ANOVA          qr                     qr      TRUE
      10    ARM   AGEGR1   ANOVA   contrasts              contrasts      NULL
         statistic_fmt_fn                    warning             error
      1              NULL NAs introduced by coercion NA/NaN/Inf in 'y'
      2              NULL NAs introduced by coercion NA/NaN/Inf in 'y'
      3              NULL NAs introduced by coercion NA/NaN/Inf in 'y'
      4              NULL NAs introduced by coercion NA/NaN/Inf in 'y'
      5              NULL NAs introduced by coercion NA/NaN/Inf in 'y'
      6              NULL NAs introduced by coercion NA/NaN/Inf in 'y'
      7              NULL NAs introduced by coercion NA/NaN/Inf in 'y'
      8              NULL NAs introduced by coercion NA/NaN/Inf in 'y'
      9              NULL NAs introduced by coercion NA/NaN/Inf in 'y'
      10             NULL NAs introduced by coercion NA/NaN/Inf in 'y'

