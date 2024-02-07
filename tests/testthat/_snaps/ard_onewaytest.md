# ard_onewaytest() works

    Code
      as.data.frame(ard_onewaytest(cards::ADSL, by = ARM, variable = AGEGR1))
    Output
        group1 variable     context stat_name                     stat_label
      1    ARM   AGEGR1 Oneway.test    num.df             Degrees of Freedom
      2    ARM   AGEGR1 Oneway.test    den.df Denominator Degrees of Freedom
      3    ARM   AGEGR1 Oneway.test statistic                    F Statistic
      4    ARM   AGEGR1 Oneway.test   p.value                        p-value
      5    ARM   AGEGR1 Oneway.test    method                         Method
      6    ARM   AGEGR1 Oneway.test   formula                        formula
      7    ARM   AGEGR1 Oneway.test    subset                         subset
      8    ARM   AGEGR1 Oneway.test na.action                      na.action
      9    ARM   AGEGR1 Oneway.test var.equal                      var.equal
                                                       statistic statistic_fmt_fn
      1                                                        2                1
      2                                                       NA                1
      3                                                       NA                1
      4                                                       NA                1
      5 One-way analysis of means (not assuming equal variances)             NULL
      6                                                                      NULL
      7                                                                      NULL
      8                                                                      NULL
      9                                                    FALSE             NULL
                                                                                                                                                                                                                                         warning
      1 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      2 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      3 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      4 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      5 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      6 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      7 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      8 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      9 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
        error
      1  NULL
      2  NULL
      3  NULL
      4  NULL
      5  NULL
      6  NULL
      7  NULL
      8  NULL
      9  NULL

