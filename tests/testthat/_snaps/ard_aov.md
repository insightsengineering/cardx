# ard_aov() works

    Code
      as.data.frame(dplyr::select(ard_aov(cards::ADSL, by = ARM, variable = AGEGR1),
      c("group1", "variable", "stat_name", "error")))
    Output
        group1 variable stat_name             error
      1    ARM   AGEGR1      term NA/NaN/Inf in 'y'
      2    ARM   AGEGR1        df NA/NaN/Inf in 'y'
      3    ARM   AGEGR1     sumsq NA/NaN/Inf in 'y'
      4    ARM   AGEGR1    meansq NA/NaN/Inf in 'y'
      5    ARM   AGEGR1 statistic NA/NaN/Inf in 'y'
      6    ARM   AGEGR1   p.value NA/NaN/Inf in 'y'
      7    ARM   AGEGR1   formula NA/NaN/Inf in 'y'

