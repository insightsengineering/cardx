# ard_onewaytest() works

    Code
      head(as.data.frame(dplyr::select(ard_onewaytest(AGEGR1 ~ ARM, data = cards::ADSL),
      c("stat_name", "warning"))), 3)
    Output
        stat_name
      1    num.df
      2    den.df
      3 statistic
                                                                                                                                                                                                                                         warning
      1 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      2 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion
      3 argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, argument is not numeric or logical: returning NA, NAs introduced by coercion, NAs introduced by coercion, NAs introduced by coercion

