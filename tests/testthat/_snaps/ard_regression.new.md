# ard_regression() works

    Code
      print(dplyr::mutate(ard_regression(lm(AGE ~ ARM, data = cards::ADSL),
      add_estimate_to_reference_rows = TRUE), statistic = lapply(statistic, function(
        x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 43 x 7
    Output
         variable variable_level      stat_name stat_label                  statistic
      1       ARM        Placebo           term       term                 ARMPlacebo
      2       ARM        Placebo      var_label      Label Description of Planned Arm
      3       ARM        Placebo      var_class      Class                  character
      4       ARM        Placebo       var_type       Type                categorical
      5       ARM        Placebo    var_nlevels   N Levels                          3
      6       ARM        Placebo      contrasts  contrasts            contr.treatment
      7       ARM        Placebo contrasts_type  Contrast…                  treatment
      8       ARM        Placebo  reference_row  referenc…                       TRUE
      9       ARM        Placebo          label  Level La…                    Placebo
      10      ARM        Placebo          n_obs     N Obs.                         86
      11      ARM        Placebo       estimate  Coeffici…                          0
      12      ARM      Xanomeli…           term       term    ARMXanomeline High Dose
      13      ARM      Xanomeli…      var_label      Label Description of Planned Arm
      14      ARM      Xanomeli…      var_class      Class                  character
      15      ARM      Xanomeli…       var_type       Type                categorical
      16      ARM      Xanomeli…    var_nlevels   N Levels                          3
      17      ARM      Xanomeli…      contrasts  contrasts            contr.treatment
      18      ARM      Xanomeli… contrasts_type  Contrast…                  treatment
      19      ARM      Xanomeli…  reference_row  referenc…                      FALSE
      20      ARM      Xanomeli…          label  Level La…       Xanomeline High Dose
      21      ARM      Xanomeli…          n_obs     N Obs.                         84
      22      ARM      Xanomeli…       estimate  Coeffici…                     -0.828
      23      ARM      Xanomeli…      std.error  Standard…                      1.267
      24      ARM      Xanomeli…      statistic  statistic                     -0.654
      25      ARM      Xanomeli…        p.value    p-value                      0.514
      26      ARM      Xanomeli…       conf.low  CI Lower…                     -3.324
      27      ARM      Xanomeli…      conf.high  CI Upper…                      1.668
      28      ARM      Xanomeli…           term       term     ARMXanomeline Low Dose
      29      ARM      Xanomeli…      var_label      Label Description of Planned Arm
      30      ARM      Xanomeli…      var_class      Class                  character
      31      ARM      Xanomeli…       var_type       Type                categorical
      32      ARM      Xanomeli…    var_nlevels   N Levels                          3
      33      ARM      Xanomeli…      contrasts  contrasts            contr.treatment
      34      ARM      Xanomeli… contrasts_type  Contrast…                  treatment
      35      ARM      Xanomeli…  reference_row  referenc…                      FALSE
      36      ARM      Xanomeli…          label  Level La…        Xanomeline Low Dose
      37      ARM      Xanomeli…          n_obs     N Obs.                         84
      38      ARM      Xanomeli…       estimate  Coeffici…                      0.457
      39      ARM      Xanomeli…      std.error  Standard…                      1.267
      40      ARM      Xanomeli…      statistic  statistic                      0.361
      41      ARM      Xanomeli…        p.value    p-value                      0.719
      42      ARM      Xanomeli…       conf.low  CI Lower…                     -2.039
      43      ARM      Xanomeli…      conf.high  CI Upper…                      2.953
         statistic_fmt_fn
      1              NULL
      2              NULL
      3              NULL
      4              NULL
      5                 0
      6              NULL
      7              NULL
      8              NULL
      9              NULL
      10                1
      11                1
      12             NULL
      13             NULL
      14             NULL
      15             NULL
      16                0
      17             NULL
      18             NULL
      19             NULL
      20             NULL
      21                1
      22                1
      23                1
      24                1
      25                1
      26                1
      27                1
      28             NULL
      29             NULL
      30             NULL
      31             NULL
      32                0
      33             NULL
      34             NULL
      35             NULL
      36             NULL
      37                1
      38                1
      39                1
      40                1
      41                1
      42                1
      43                1
    Message
      i 1 more variable: context

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

