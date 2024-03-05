# ard_survfit() works with times provided

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~
        TRTA, cards::ADTTE), times = c(60, 180)), stat = lapply(stat, function(x)
        ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 24 x 7
    Output
         variable variable_level  context stat_name stat_label  stat
      1      TRTA        Placebo survival  estimate  Survival… 0.893
      2      TRTA        Placebo survival conf.high  CI Upper… 0.966
      3      TRTA        Placebo survival  conf.low  CI Lower… 0.825
      4      TRTA        Placebo survival      time       Time    60
      5      TRTA        Placebo survival  estimate  Survival… 0.651
      6      TRTA        Placebo survival conf.high  CI Upper… 0.783
      7      TRTA        Placebo survival  conf.low  CI Lower… 0.541
      8      TRTA        Placebo survival      time       Time   180
      9      TRTA      Xanomeli… survival  estimate  Survival… 0.694
      10     TRTA      Xanomeli… survival conf.high  CI Upper… 0.849
      11     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.568
      12     TRTA      Xanomeli… survival      time       Time    60
      13     TRTA      Xanomeli… survival  estimate  Survival… 0.262
      14     TRTA      Xanomeli… survival conf.high  CI Upper… 0.749
      15     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.092
      16     TRTA      Xanomeli… survival      time       Time   180
      17     TRTA      Xanomeli… survival  estimate  Survival… 0.732
      18     TRTA      Xanomeli… survival conf.high  CI Upper… 0.878
      19     TRTA      Xanomeli… survival  conf.low  CI Lower…  0.61
      20     TRTA      Xanomeli… survival      time       Time    60
      21     TRTA      Xanomeli… survival  estimate  Survival… 0.381
      22     TRTA      Xanomeli… survival conf.high  CI Upper… 0.743
      23     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.195
      24     TRTA      Xanomeli… survival      time       Time   180
    Message
      i 1 more variable: fmt_fn

# ard_survfit() works with different type

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~
        TRTA, cards::ADTTE), times = c(60, 180), type = "risk"), stat = lapply(stat,
        function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 24 x 7
    Output
         variable variable_level context stat_name stat_label  stat
      1      TRTA        Placebo    risk  estimate  Survival… 0.107
      2      TRTA        Placebo    risk conf.high  CI Upper… 0.175
      3      TRTA        Placebo    risk  conf.low  CI Lower… 0.034
      4      TRTA        Placebo    risk      time       Time    60
      5      TRTA        Placebo    risk  estimate  Survival… 0.349
      6      TRTA        Placebo    risk conf.high  CI Upper… 0.459
      7      TRTA        Placebo    risk  conf.low  CI Lower… 0.217
      8      TRTA        Placebo    risk      time       Time   180
      9      TRTA      Xanomeli…    risk  estimate  Survival… 0.306
      10     TRTA      Xanomeli…    risk conf.high  CI Upper… 0.432
      11     TRTA      Xanomeli…    risk  conf.low  CI Lower… 0.151
      12     TRTA      Xanomeli…    risk      time       Time    60
      13     TRTA      Xanomeli…    risk  estimate  Survival… 0.738
      14     TRTA      Xanomeli…    risk conf.high  CI Upper… 0.908
      15     TRTA      Xanomeli…    risk  conf.low  CI Lower… 0.251
      16     TRTA      Xanomeli…    risk      time       Time   180
      17     TRTA      Xanomeli…    risk  estimate  Survival… 0.268
      18     TRTA      Xanomeli…    risk conf.high  CI Upper…  0.39
      19     TRTA      Xanomeli…    risk  conf.low  CI Lower… 0.122
      20     TRTA      Xanomeli…    risk      time       Time    60
      21     TRTA      Xanomeli…    risk  estimate  Survival… 0.619
      22     TRTA      Xanomeli…    risk conf.high  CI Upper… 0.805
      23     TRTA      Xanomeli…    risk  conf.low  CI Lower… 0.257
      24     TRTA      Xanomeli…    risk      time       Time   180
    Message
      i 1 more variable: fmt_fn

# ard_survfit() works with probs provided

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~
        TRTA, cards::ADTTE), probs = c(0.25, 0.75), type = "cumhaz"), stat = lapply(
        stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      The `type` argument is ignored for survival quantile estimation.
      {cards} data frame: 24 x 7
    Output
         variable variable_level  context stat_name stat_label stat
      1      TRTA        Placebo survival  estimate  Survival…  142
      2      TRTA        Placebo survival conf.high  CI Upper…  181
      3      TRTA        Placebo survival  conf.low  CI Lower…   70
      4      TRTA        Placebo survival      prob   Quantile 0.25
      5      TRTA      Xanomeli… survival  estimate  Survival…   44
      6      TRTA      Xanomeli… survival conf.high  CI Upper…  180
      7      TRTA      Xanomeli… survival  conf.low  CI Lower…   22
      8      TRTA      Xanomeli… survival      prob   Quantile 0.25
      9      TRTA      Xanomeli… survival  estimate  Survival…   49
      10     TRTA      Xanomeli… survival conf.high  CI Upper…  180
      11     TRTA      Xanomeli… survival  conf.low  CI Lower…   37
      12     TRTA      Xanomeli… survival      prob   Quantile 0.25
      13     TRTA        Placebo survival  estimate  Survival…  184
      14     TRTA        Placebo survival conf.high  CI Upper…  191
      15     TRTA        Placebo survival  conf.low  CI Lower…  183
      16     TRTA        Placebo survival      prob   Quantile 0.75
      17     TRTA      Xanomeli… survival  estimate  Survival…  188
      18     TRTA      Xanomeli… survival conf.high  CI Upper…   NA
      19     TRTA      Xanomeli… survival  conf.low  CI Lower…  167
      20     TRTA      Xanomeli… survival      prob   Quantile 0.75
      21     TRTA      Xanomeli… survival  estimate  Survival…  184
      22     TRTA      Xanomeli… survival conf.high  CI Upper…   NA
      23     TRTA      Xanomeli… survival  conf.low  CI Lower…  180
      24     TRTA      Xanomeli… survival      prob   Quantile 0.75
    Message
      i 1 more variable: fmt_fn

# ard_survfit() works with competing risks

    Code
      print(dplyr::mutate(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, data = ADTTE_MS) %>%
        ard_survfit(times = c(60, 180)), stat = lapply(stat, function(x) ifelse(
        is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      Multi-state model detected. Showing probabilities into state 'death from cancer'.
      {cards} data frame: 48 x 7
    Output
         variable variable_level  context stat_name stat_label  stat
      1      TRTA        Placebo survival  estimate  Survival… 0.053
      2      TRTA        Placebo survival conf.high  CI Upper… 0.139
      3      TRTA        Placebo survival  conf.low  CI Lower… 0.021
      4      TRTA        Placebo survival      time       Time    60
      5      TRTA        Placebo survival  estimate  Survival… 0.123
      6      TRTA        Placebo survival conf.high  CI Upper… 0.237
      7      TRTA        Placebo survival  conf.low  CI Lower… 0.064
      8      TRTA        Placebo survival      time       Time   180
      9      TRTA      Xanomeli… survival  estimate  Survival… 0.169
      10     TRTA      Xanomeli… survival conf.high  CI Upper… 0.304
      11     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.094
      12     TRTA      Xanomeli… survival      time       Time    60
      13     TRTA      Xanomeli… survival  estimate  Survival… 0.262
      14     TRTA      Xanomeli… survival conf.high  CI Upper… 0.749
      15     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.092
      16     TRTA      Xanomeli… survival      time       Time   180
      17     TRTA      Xanomeli… survival  estimate  Survival…  0.51
      18     TRTA      Xanomeli… survival conf.high  CI Upper… 0.892
      19     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.292
      20     TRTA      Xanomeli… survival      time       Time   180
      21     TRTA      Xanomeli… survival  estimate  Survival… 0.228
      22     TRTA      Xanomeli… survival conf.high  CI Upper… 0.427
      23     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.121
      24     TRTA      Xanomeli… survival      time       Time   180
      25     TRTA      Xanomeli… survival  estimate  Survival… 0.732
      26     TRTA      Xanomeli… survival conf.high  CI Upper… 0.878
      27     TRTA      Xanomeli… survival  conf.low  CI Lower…  0.61
      28     TRTA      Xanomeli… survival      time       Time    60
      29     TRTA      Xanomeli… survival  estimate  Survival… 0.162
      30     TRTA      Xanomeli… survival conf.high  CI Upper…  0.33
      31     TRTA      Xanomeli… survival  conf.low  CI Lower…  0.08
      32     TRTA      Xanomeli… survival      time       Time    60
      33     TRTA      Xanomeli… survival  estimate  Survival… 0.106
      34     TRTA      Xanomeli… survival conf.high  CI Upper… 0.232
      35     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.048
      36     TRTA      Xanomeli… survival      time       Time    60
      37     TRTA      Xanomeli… survival  estimate  Survival… 0.381
      38     TRTA      Xanomeli… survival conf.high  CI Upper… 0.743
      39     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.195
      40     TRTA      Xanomeli… survival      time       Time   180
      41     TRTA      Xanomeli… survival  estimate  Survival… 0.244
      42     TRTA      Xanomeli… survival conf.high  CI Upper… 0.516
      43     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.115
      44     TRTA      Xanomeli… survival      time       Time   180
      45     TRTA      Xanomeli… survival  estimate  Survival… 0.375
      46     TRTA      Xanomeli… survival conf.high  CI Upper… 0.719
      47     TRTA      Xanomeli… survival  conf.low  CI Lower… 0.196
      48     TRTA      Xanomeli… survival      time       Time   180
    Message
      i 1 more variable: fmt_fn

# ard_survfit() errors are properly handled

    Code
      ard_survfit("not_survfit")
    Condition
      Error in `ard_survfit()`:
      ! The `x` argument must be class <survfit> created using the `survival::survfit()` function.

---

    Code
      ard_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE),
      times = 100, type = "notatype")
    Condition
      Error in `ard_survfit()`:
      ! The `type` argument is "notatype" but must be one of "survival", "risk", or "cumhaz".

---

    Code
      ard_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE),
      times = 100, probs = c(0.25, 0.75))
    Condition
      Error in `ard_survfit()`:
      ! One and only one of `times` and `probs` must be specified.

