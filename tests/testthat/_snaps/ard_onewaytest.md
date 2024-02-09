# ard_onewaytest() works

    Code
      as.data.frame(dplyr::select(ard_onewaytest(cards::ADSL, by = ARM, variable = AGEGR1),
      c("group1", "variable", "stat_name", "error")))
    Output
        group1 variable stat_name error
      1    ARM   AGEGR1    num.df  NULL
      2    ARM   AGEGR1    den.df  NULL
      3    ARM   AGEGR1 statistic  NULL
      4    ARM   AGEGR1   p.value  NULL
      5    ARM   AGEGR1    method  NULL
      6    ARM   AGEGR1 var.equal  NULL

