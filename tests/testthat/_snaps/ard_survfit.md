# ard_survfit() works with times provided

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~
        TRTA, cards::ADTTE), times = c(60, 180)), stat = lapply(stat, function(x)
        ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 18 x 9
    Output
         group1 group1_level variable variable_level stat_name stat_label  stat
      1    TRTA      Placebo     time             60  estimate  Survival… 0.893
      2    TRTA      Placebo     time             60 conf.high  CI Upper… 0.966
      3    TRTA      Placebo     time             60  conf.low  CI Lower… 0.825
      4    TRTA      Placebo     time            180  estimate  Survival… 0.651
      5    TRTA      Placebo     time            180 conf.high  CI Upper… 0.783
      6    TRTA      Placebo     time            180  conf.low  CI Lower… 0.541
      7    TRTA    Xanomeli…     time             60  estimate  Survival… 0.694
      8    TRTA    Xanomeli…     time             60 conf.high  CI Upper… 0.849
      9    TRTA    Xanomeli…     time             60  conf.low  CI Lower… 0.568
      10   TRTA    Xanomeli…     time            180  estimate  Survival… 0.262
      11   TRTA    Xanomeli…     time            180 conf.high  CI Upper… 0.749
      12   TRTA    Xanomeli…     time            180  conf.low  CI Lower… 0.092
      13   TRTA    Xanomeli…     time             60  estimate  Survival… 0.732
      14   TRTA    Xanomeli…     time             60 conf.high  CI Upper… 0.878
      15   TRTA    Xanomeli…     time             60  conf.low  CI Lower…  0.61
      16   TRTA    Xanomeli…     time            180  estimate  Survival… 0.381
      17   TRTA    Xanomeli…     time            180 conf.high  CI Upper… 0.743
      18   TRTA    Xanomeli…     time            180  conf.low  CI Lower… 0.195
    Message
      i 2 more variables: context, fmt_fn

# ard_survfit() works with different type

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~
        TRTA, cards::ADTTE), times = c(60, 180), type = "risk"), stat = lapply(stat,
        function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 18 x 9
    Output
         group1 group1_level variable variable_level stat_name stat_label  stat
      1    TRTA      Placebo     time             60  estimate  Survival… 0.107
      2    TRTA      Placebo     time             60 conf.high  CI Upper… 0.175
      3    TRTA      Placebo     time             60  conf.low  CI Lower… 0.034
      4    TRTA      Placebo     time            180  estimate  Survival… 0.349
      5    TRTA      Placebo     time            180 conf.high  CI Upper… 0.459
      6    TRTA      Placebo     time            180  conf.low  CI Lower… 0.217
      7    TRTA    Xanomeli…     time             60  estimate  Survival… 0.306
      8    TRTA    Xanomeli…     time             60 conf.high  CI Upper… 0.432
      9    TRTA    Xanomeli…     time             60  conf.low  CI Lower… 0.151
      10   TRTA    Xanomeli…     time            180  estimate  Survival… 0.738
      11   TRTA    Xanomeli…     time            180 conf.high  CI Upper… 0.908
      12   TRTA    Xanomeli…     time            180  conf.low  CI Lower… 0.251
      13   TRTA    Xanomeli…     time             60  estimate  Survival… 0.268
      14   TRTA    Xanomeli…     time             60 conf.high  CI Upper…  0.39
      15   TRTA    Xanomeli…     time             60  conf.low  CI Lower… 0.122
      16   TRTA    Xanomeli…     time            180  estimate  Survival… 0.619
      17   TRTA    Xanomeli…     time            180 conf.high  CI Upper… 0.805
      18   TRTA    Xanomeli…     time            180  conf.low  CI Lower… 0.257
    Message
      i 2 more variables: context, fmt_fn

# ard_survfit() works with probs provided

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~
        TRTA, cards::ADTTE), probs = c(0.25, 0.75), type = "cumhaz"), stat = lapply(
        stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      The `type` argument is ignored for survival quantile estimation.
      {cards} data frame: 18 x 9
    Output
         group1 group1_level variable variable_level stat_name stat_label stat
      1    TRTA      Placebo     prob           0.25  estimate  Survival…  142
      2    TRTA      Placebo     prob           0.25 conf.high  CI Upper…  181
      3    TRTA      Placebo     prob           0.25  conf.low  CI Lower…   70
      4    TRTA      Placebo     prob           0.75  estimate  Survival…  184
      5    TRTA      Placebo     prob           0.75 conf.high  CI Upper…  191
      6    TRTA      Placebo     prob           0.75  conf.low  CI Lower…  183
      7    TRTA    Xanomeli…     prob           0.25  estimate  Survival…   44
      8    TRTA    Xanomeli…     prob           0.25 conf.high  CI Upper…  180
      9    TRTA    Xanomeli…     prob           0.25  conf.low  CI Lower…   22
      10   TRTA    Xanomeli…     prob           0.75  estimate  Survival…  188
      11   TRTA    Xanomeli…     prob           0.75 conf.high  CI Upper…   NA
      12   TRTA    Xanomeli…     prob           0.75  conf.low  CI Lower…  167
      13   TRTA    Xanomeli…     prob           0.25  estimate  Survival…   49
      14   TRTA    Xanomeli…     prob           0.25 conf.high  CI Upper…  180
      15   TRTA    Xanomeli…     prob           0.25  conf.low  CI Lower…   37
      16   TRTA    Xanomeli…     prob           0.75  estimate  Survival…  184
      17   TRTA    Xanomeli…     prob           0.75 conf.high  CI Upper…   NA
      18   TRTA    Xanomeli…     prob           0.75  conf.low  CI Lower…  180
    Message
      i 2 more variables: context, fmt_fn

# ard_survfit() works with unstratified model

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(time, status) ~
        1, data = lung), times = c(60, 180)), stat = lapply(stat, function(x) ifelse(
        is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 6 x 7
    Output
        variable variable_level  context stat_name stat_label  stat
      1     time             60 survival  estimate  Survival… 0.925
      2     time             60 survival conf.high  CI Upper…  0.96
      3     time             60 survival  conf.low  CI Lower… 0.892
      4     time            180 survival  estimate  Survival… 0.722
      5     time            180 survival conf.high  CI Upper… 0.783
      6     time            180 survival  conf.low  CI Lower… 0.666
    Message
      i 1 more variable: fmt_fn

---

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(time, status) ~
        1, data = lung), probs = c(0.5, 0.75)), stat = lapply(stat, function(x)
        ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 6 x 7
    Output
        variable variable_level  context stat_name stat_label stat
      1     prob            0.5 survival  estimate  Survival…  310
      2     prob            0.5 survival conf.high  CI Upper…  363
      3     prob            0.5 survival  conf.low  CI Lower…  285
      4     prob           0.75 survival  estimate  Survival…  550
      5     prob           0.75 survival conf.high  CI Upper…  654
      6     prob           0.75 survival  conf.low  CI Lower…  460
    Message
      i 1 more variable: fmt_fn

# ard_survfit() works with competing risks

    Code
      print(dplyr::mutate(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, data = ADTTE_MS) %>%
        ard_survfit(times = c(60, 180)), stat = lapply(stat, function(x) ifelse(
        is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      Multi-state model detected. Showing probabilities into state 'death from cancer'.
      {cards} data frame: 18 x 9
    Output
         group1 group1_level variable variable_level stat_name stat_label  stat
      1    TRTA      Placebo     time             60  estimate  Survival… 0.054
      2    TRTA      Placebo     time             60 conf.high  CI Upper…  0.14
      3    TRTA      Placebo     time             60  conf.low  CI Lower… 0.021
      4    TRTA      Placebo     time            180  estimate  Survival… 0.226
      5    TRTA      Placebo     time            180 conf.high  CI Upper… 0.361
      6    TRTA      Placebo     time            180  conf.low  CI Lower… 0.142
      7    TRTA    Xanomeli…     time             60  estimate  Survival… 0.137
      8    TRTA    Xanomeli…     time             60 conf.high  CI Upper… 0.311
      9    TRTA    Xanomeli…     time             60  conf.low  CI Lower…  0.06
      10   TRTA    Xanomeli…     time            180  estimate  Survival…  0.51
      11   TRTA    Xanomeli…     time            180 conf.high  CI Upper… 0.892
      12   TRTA    Xanomeli…     time            180  conf.low  CI Lower… 0.292
      13   TRTA    Xanomeli…     time             60  estimate  Survival… 0.162
      14   TRTA    Xanomeli…     time             60 conf.high  CI Upper…  0.33
      15   TRTA    Xanomeli…     time             60  conf.low  CI Lower…  0.08
      16   TRTA    Xanomeli…     time            180  estimate  Survival… 0.244
      17   TRTA    Xanomeli…     time            180 conf.high  CI Upper… 0.516
      18   TRTA    Xanomeli…     time            180  conf.low  CI Lower… 0.115
    Message
      i 2 more variables: context, fmt_fn

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

