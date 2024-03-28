# ard_onewaytest() works

    Code
      as.data.frame(dplyr::select(ard_onewaytest(lm(AGE ~ ARM, data = cards::ADSL)),
      c("stat_name", "error")))
    Output
        stat_name error
      1    num.df  NULL
      2    den.df  NULL
      3 statistic  NULL
      4   p.value  NULL
      5    method  NULL
      6 var.equal  NULL

