# ard_regression() works

    Code
      print(dplyr::mutate(ard_regression(lm(AGE ~ ARM, data = cards::ADSL),
      add_estimate_to_reference_rows = TRUE), statistic = lapply(statistic, function(
        x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 43 x 7
    Output
         variable variable_level   context      stat_name stat_label statistic
      1       ARM        Placebo regressi…           term       term ARMPlace…
      2       ARM        Placebo regressi…      var_label      Label Descript…
      3       ARM        Placebo regressi…      var_class      Class character
      4       ARM        Placebo regressi…       var_type       Type categori…
      5       ARM        Placebo regressi…    var_nlevels   N Levels         3
      6       ARM        Placebo regressi…      contrasts  contrasts contr.tr…
      7       ARM        Placebo regressi… contrasts_type  Contrast… treatment
      8       ARM        Placebo regressi…  reference_row  referenc…      TRUE
      9       ARM        Placebo regressi…          label  Level La…   Placebo
      10      ARM        Placebo regressi…          n_obs     N Obs.        86
      11      ARM        Placebo regressi…       estimate  Coeffici…         0
      12      ARM      Xanomeli… regressi…           term       term ARMXanom…
      13      ARM      Xanomeli… regressi…      var_label      Label Descript…
      14      ARM      Xanomeli… regressi…      var_class      Class character
      15      ARM      Xanomeli… regressi…       var_type       Type categori…
      16      ARM      Xanomeli… regressi…    var_nlevels   N Levels         3
      17      ARM      Xanomeli… regressi…      contrasts  contrasts contr.tr…
      18      ARM      Xanomeli… regressi… contrasts_type  Contrast… treatment
      19      ARM      Xanomeli… regressi…  reference_row  referenc…     FALSE
      20      ARM      Xanomeli… regressi…          label  Level La… Xanomeli…
      21      ARM      Xanomeli… regressi…          n_obs     N Obs.        84
      22      ARM      Xanomeli… regressi…       estimate  Coeffici…    -0.828
      23      ARM      Xanomeli… regressi…      std.error  Standard…     1.267
      24      ARM      Xanomeli… regressi…      statistic  statistic    -0.654
      25      ARM      Xanomeli… regressi…        p.value    p-value     0.514
      26      ARM      Xanomeli… regressi…       conf.low  CI Lower…    -3.324
      27      ARM      Xanomeli… regressi…      conf.high  CI Upper…     1.668
      28      ARM      Xanomeli… regressi…           term       term ARMXanom…
      29      ARM      Xanomeli… regressi…      var_label      Label Descript…
      30      ARM      Xanomeli… regressi…      var_class      Class character
      31      ARM      Xanomeli… regressi…       var_type       Type categori…
      32      ARM      Xanomeli… regressi…    var_nlevels   N Levels         3
      33      ARM      Xanomeli… regressi…      contrasts  contrasts contr.tr…
      34      ARM      Xanomeli… regressi… contrasts_type  Contrast… treatment
      35      ARM      Xanomeli… regressi…  reference_row  referenc…     FALSE
      36      ARM      Xanomeli… regressi…          label  Level La… Xanomeli…
      37      ARM      Xanomeli… regressi…          n_obs     N Obs.        84
      38      ARM      Xanomeli… regressi…       estimate  Coeffici…     0.457
      39      ARM      Xanomeli… regressi…      std.error  Standard…     1.267
      40      ARM      Xanomeli… regressi…      statistic  statistic     0.361
      41      ARM      Xanomeli… regressi…        p.value    p-value     0.719
      42      ARM      Xanomeli… regressi…       conf.low  CI Lower…    -2.039
      43      ARM      Xanomeli… regressi…      conf.high  CI Upper…     2.953
    Message
      i 1 more variable: statistic_fmt_fn

