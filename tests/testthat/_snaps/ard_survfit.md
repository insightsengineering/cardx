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

# ard_survfit() works with multiple stratification variables

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(time, status) ~
        sex + ph.ecog, data = lung), times = c(60, 180)), stat = lapply(stat,
        function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 42 x 11
    Output
         group1 group1_level  group2 group2_level variable variable_level stat_name
      1     sex            1 ph.ecog            0     time             60  estimate
      2     sex            1 ph.ecog            0     time             60 conf.high
      3     sex            1 ph.ecog            0     time             60  conf.low
      4     sex            1 ph.ecog            0     time            180  estimate
      5     sex            1 ph.ecog            0     time            180 conf.high
      6     sex            1 ph.ecog            0     time            180  conf.low
      7     sex            1 ph.ecog            1     time             60  estimate
      8     sex            1 ph.ecog            1     time             60 conf.high
      9     sex            1 ph.ecog            1     time             60  conf.low
      10    sex            1 ph.ecog            1     time            180  estimate
      11    sex            1 ph.ecog            1     time            180 conf.high
      12    sex            1 ph.ecog            1     time            180  conf.low
      13    sex            1 ph.ecog            2     time             60  estimate
      14    sex            1 ph.ecog            2     time             60 conf.high
      15    sex            1 ph.ecog            2     time             60  conf.low
      16    sex            1 ph.ecog            2     time            180  estimate
      17    sex            1 ph.ecog            2     time            180 conf.high
      18    sex            1 ph.ecog            2     time            180  conf.low
      19    sex            1 ph.ecog            3     time             60  estimate
      20    sex            1 ph.ecog            3     time             60 conf.high
      21    sex            1 ph.ecog            3     time             60  conf.low
      22    sex            1 ph.ecog            3     time            180  estimate
      23    sex            1 ph.ecog            3     time            180 conf.high
      24    sex            1 ph.ecog            3     time            180  conf.low
      25    sex            2 ph.ecog            0     time             60  estimate
      26    sex            2 ph.ecog            0     time             60 conf.high
      27    sex            2 ph.ecog            0     time             60  conf.low
      28    sex            2 ph.ecog            0     time            180  estimate
      29    sex            2 ph.ecog            0     time            180 conf.high
      30    sex            2 ph.ecog            0     time            180  conf.low
      31    sex            2 ph.ecog            1     time             60  estimate
      32    sex            2 ph.ecog            1     time             60 conf.high
      33    sex            2 ph.ecog            1     time             60  conf.low
      34    sex            2 ph.ecog            1     time            180  estimate
      35    sex            2 ph.ecog            1     time            180 conf.high
      36    sex            2 ph.ecog            1     time            180  conf.low
      37    sex            2 ph.ecog            2     time             60  estimate
      38    sex            2 ph.ecog            2     time             60 conf.high
      39    sex            2 ph.ecog            2     time             60  conf.low
      40    sex            2 ph.ecog            2     time            180  estimate
      41    sex            2 ph.ecog            2     time            180 conf.high
      42    sex            2 ph.ecog            2     time            180  conf.low
         stat_label  stat
      1   Survival… 0.889
      2   CI Upper… 0.998
      3   CI Lower… 0.792
      4   Survival… 0.806
      5   CI Upper… 0.946
      6   CI Lower… 0.686
      7   Survival… 0.944
      8   CI Upper… 0.999
      9   CI Lower… 0.892
      10  Survival… 0.675
      11  CI Upper… 0.794
      12  CI Lower… 0.574
      13  Survival… 0.759
      14  CI Upper… 0.932
      15  CI Lower… 0.618
      16  Survival… 0.414
      17  CI Upper… 0.638
      18  CI Lower… 0.268
      19  Survival…     1
      20  CI Upper…     1
      21  CI Lower…     1
      22  Survival…    NA
      23  CI Upper…    NA
      24  CI Lower…    NA
      25  Survival… 0.963
      26  CI Upper…     1
      27  CI Lower… 0.894
      28  Survival… 0.889
      29  CI Upper…     1
      30  CI Lower… 0.778
      31  Survival… 0.976
      32  CI Upper…     1
      33  CI Lower… 0.931
      34  Survival… 0.881
      35  CI Upper… 0.985
      36  CI Lower… 0.788
      37  Survival…     1
      38  CI Upper…     1
      39  CI Lower…     1
      40  Survival…  0.69
      41  CI Upper… 0.931
      42  CI Lower… 0.511
    Message
      i 2 more variables: context, fmt_fn

---

    Code
      print(dplyr::mutate(ard_survfit(survival::survfit(survival::Surv(time, status) ~
        sex + ph.ecog, data = lung), probs = c(0.5, 0.75)), stat = lapply(stat,
        function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 42 x 11
    Output
         group1 group1_level  group2 group2_level variable variable_level stat_name
      1     sex            1 ph.ecog            0     prob            0.5  estimate
      2     sex            1 ph.ecog            0     prob            0.5 conf.high
      3     sex            1 ph.ecog            0     prob            0.5  conf.low
      4     sex            1 ph.ecog            0     prob           0.75  estimate
      5     sex            1 ph.ecog            0     prob           0.75 conf.high
      6     sex            1 ph.ecog            0     prob           0.75  conf.low
      7     sex            1 ph.ecog            1     prob            0.5  estimate
      8     sex            1 ph.ecog            1     prob            0.5 conf.high
      9     sex            1 ph.ecog            1     prob            0.5  conf.low
      10    sex            1 ph.ecog            1     prob           0.75  estimate
      11    sex            1 ph.ecog            1     prob           0.75 conf.high
      12    sex            1 ph.ecog            1     prob           0.75  conf.low
      13    sex            1 ph.ecog            2     prob            0.5  estimate
      14    sex            1 ph.ecog            2     prob            0.5 conf.high
      15    sex            1 ph.ecog            2     prob            0.5  conf.low
      16    sex            1 ph.ecog            2     prob           0.75  estimate
      17    sex            1 ph.ecog            2     prob           0.75 conf.high
      18    sex            1 ph.ecog            2     prob           0.75  conf.low
      19    sex            1 ph.ecog            3     prob            0.5  estimate
      20    sex            1 ph.ecog            3     prob            0.5 conf.high
      21    sex            1 ph.ecog            3     prob            0.5  conf.low
      22    sex            1 ph.ecog            3     prob           0.75  estimate
      23    sex            1 ph.ecog            3     prob           0.75 conf.high
      24    sex            1 ph.ecog            3     prob           0.75  conf.low
      25    sex            2 ph.ecog            0     prob            0.5  estimate
      26    sex            2 ph.ecog            0     prob            0.5 conf.high
      27    sex            2 ph.ecog            0     prob            0.5  conf.low
      28    sex            2 ph.ecog            0     prob           0.75  estimate
      29    sex            2 ph.ecog            0     prob           0.75 conf.high
      30    sex            2 ph.ecog            0     prob           0.75  conf.low
      31    sex            2 ph.ecog            1     prob            0.5  estimate
      32    sex            2 ph.ecog            1     prob            0.5 conf.high
      33    sex            2 ph.ecog            1     prob            0.5  conf.low
      34    sex            2 ph.ecog            1     prob           0.75  estimate
      35    sex            2 ph.ecog            1     prob           0.75 conf.high
      36    sex            2 ph.ecog            1     prob           0.75  conf.low
      37    sex            2 ph.ecog            2     prob            0.5  estimate
      38    sex            2 ph.ecog            2     prob            0.5 conf.high
      39    sex            2 ph.ecog            2     prob            0.5  conf.low
      40    sex            2 ph.ecog            2     prob           0.75  estimate
      41    sex            2 ph.ecog            2     prob           0.75 conf.high
      42    sex            2 ph.ecog            2     prob           0.75  conf.low
         stat_label stat
      1   Survival…  353
      2   CI Upper…  558
      3   CI Lower…  303
      4   Survival…  574
      5   CI Upper…   NA
      6   CI Lower…  428
      7   Survival…  239
      8   CI Upper…  363
      9   CI Lower…  207
      10  Survival…  460
      11  CI Upper…  624
      12  CI Lower…  363
      13  Survival…  166
      14  CI Upper…  288
      15  CI Lower…  105
      16  Survival…  291
      17  CI Upper…   NA
      18  CI Lower…  183
      19  Survival…  118
      20  CI Upper…   NA
      21  CI Lower…   NA
      22  Survival…  118
      23  CI Upper…   NA
      24  CI Lower…   NA
      25  Survival…  705
      26  CI Upper…   NA
      27  CI Lower…  350
      28  Survival…   NA
      29  CI Upper…   NA
      30  CI Lower…  705
      31  Survival…  450
      32  CI Upper…  687
      33  CI Lower…  345
      34  Survival…  728
      35  CI Upper…   NA
      36  CI Lower…  524
      37  Survival…  239
      38  CI Upper…  444
      39  CI Lower…  199
      40  Survival…  361
      41  CI Upper…   NA
      42  CI Lower…  285
    Message
      i 2 more variables: context, fmt_fn

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

