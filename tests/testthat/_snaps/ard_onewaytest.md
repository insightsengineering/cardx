# ard_onewaytest() works

    Code
      as.data.frame(dplyr::select(ard_onewaytest(lm(AGEGR1 ~ ARM, data = cards::ADSL)),
      c("stat_name", "error")))
    Output
        stat_name             error
      1    num.df NA/NaN/Inf in 'y'
      2    den.df NA/NaN/Inf in 'y'
      3 statistic NA/NaN/Inf in 'y'
      4   p.value NA/NaN/Inf in 'y'
      5    method NA/NaN/Inf in 'y'
      6 var.equal NA/NaN/Inf in 'y'

