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

# ard_regression_basic() works

    Code
      as.data.frame(ard_regression_basic(lm(AGE ~ ARM, data = cards::ADSL)))
    Output
         variable       variable_level    context      stat_name     stat_label
      1       ARM Xanomeline High Dose regression    var_nlevels       N Levels
      2       ARM Xanomeline High Dose regression      contrasts      contrasts
      3       ARM Xanomeline High Dose regression contrasts_type  Contrast Type
      4       ARM Xanomeline High Dose regression       estimate    Coefficient
      5       ARM Xanomeline High Dose regression      std.error Standard Error
      6       ARM Xanomeline High Dose regression      statistic      statistic
      7       ARM Xanomeline High Dose regression        p.value        p-value
      8       ARM Xanomeline High Dose regression       conf.low CI Lower Bound
      9       ARM Xanomeline High Dose regression      conf.high CI Upper Bound
      10      ARM  Xanomeline Low Dose regression    var_nlevels       N Levels
      11      ARM  Xanomeline Low Dose regression      contrasts      contrasts
      12      ARM  Xanomeline Low Dose regression contrasts_type  Contrast Type
      13      ARM  Xanomeline Low Dose regression       estimate    Coefficient
      14      ARM  Xanomeline Low Dose regression      std.error Standard Error
      15      ARM  Xanomeline Low Dose regression      statistic      statistic
      16      ARM  Xanomeline Low Dose regression        p.value        p-value
      17      ARM  Xanomeline Low Dose regression       conf.low CI Lower Bound
      18      ARM  Xanomeline Low Dose regression      conf.high CI Upper Bound
               statistic statistic_fmt_fn
      1                3                0
      2  contr.treatment             NULL
      3        treatment             NULL
      4       -0.8283499                1
      5         1.267394                1
      6        -0.653585                1
      7        0.5139775                1
      8        -3.324433                1
      9         1.667733                1
      10               3                0
      11 contr.treatment             NULL
      12       treatment             NULL
      13       0.4573643                1
      14        1.267394                1
      15       0.3608698                1
      16       0.7185003                1
      17       -2.038718                1
      18        2.953447                1

