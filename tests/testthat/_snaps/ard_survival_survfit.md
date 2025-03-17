# ard_survival_survfit() works with times provided

    Code
      print(dplyr::mutate(ard_survival_survfit(survival::survfit(survival::Surv(AVAL,
        CNSR) ~ TRTA, cards::ADTTE), times = c(60, 180)), stat = lapply(stat,
        function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 36 x 11
    Output
         group1 group1_level variable variable_level  stat_name stat_label  stat
      1    TRTA      Placebo     time             60     n.risk  Number o…    59
      2    TRTA      Placebo     time             60   estimate  Survival… 0.893
      3    TRTA      Placebo     time             60  std.error  Standard… 0.036
      4    TRTA      Placebo     time             60  conf.high  CI Upper… 0.966
      5    TRTA      Placebo     time             60   conf.low  CI Lower… 0.825
      6    TRTA      Placebo     time             60 conf.level  CI Confi…  0.95
      7    TRTA      Placebo     time            180     n.risk  Number o…    35
      8    TRTA      Placebo     time            180   estimate  Survival… 0.651
      9    TRTA      Placebo     time            180  std.error  Standard… 0.061
      10   TRTA      Placebo     time            180  conf.high  CI Upper… 0.783
      11   TRTA      Placebo     time            180   conf.low  CI Lower… 0.541
      12   TRTA      Placebo     time            180 conf.level  CI Confi…  0.95
      13   TRTA    Xanomeli…     time             60     n.risk  Number o…    14
      14   TRTA    Xanomeli…     time             60   estimate  Survival… 0.694
      15   TRTA    Xanomeli…     time             60  std.error  Standard… 0.071
      16   TRTA    Xanomeli…     time             60  conf.high  CI Upper… 0.849
      17   TRTA    Xanomeli…     time             60   conf.low  CI Lower… 0.568
      18   TRTA    Xanomeli…     time             60 conf.level  CI Confi…  0.95
      19   TRTA    Xanomeli…     time            180     n.risk  Number o…     3
      20   TRTA    Xanomeli…     time            180   estimate  Survival… 0.262
      21   TRTA    Xanomeli…     time            180  std.error  Standard…  0.14
      22   TRTA    Xanomeli…     time            180  conf.high  CI Upper… 0.749
      23   TRTA    Xanomeli…     time            180   conf.low  CI Lower… 0.092
      24   TRTA    Xanomeli…     time            180 conf.level  CI Confi…  0.95
      25   TRTA    Xanomeli…     time             60     n.risk  Number o…    20
      26   TRTA    Xanomeli…     time             60   estimate  Survival… 0.732
      27   TRTA    Xanomeli…     time             60  std.error  Standard… 0.068
      28   TRTA    Xanomeli…     time             60  conf.high  CI Upper… 0.878
      29   TRTA    Xanomeli…     time             60   conf.low  CI Lower…  0.61
      30   TRTA    Xanomeli…     time             60 conf.level  CI Confi…  0.95
      31   TRTA    Xanomeli…     time            180     n.risk  Number o…     5
      32   TRTA    Xanomeli…     time            180   estimate  Survival… 0.381
      33   TRTA    Xanomeli…     time            180  std.error  Standard…  0.13
      34   TRTA    Xanomeli…     time            180  conf.high  CI Upper… 0.743
      35   TRTA    Xanomeli…     time            180   conf.low  CI Lower… 0.195
      36   TRTA    Xanomeli…     time            180 conf.level  CI Confi…  0.95
    Message
      i 4 more variables: context, fmt_fn, warning, error

# ard_survival_survfit() works with different type

    Code
      print(dplyr::mutate(ard_survival_survfit(survival::survfit(survival::Surv(AVAL,
        CNSR) ~ TRTA, cards::ADTTE), times = c(60, 180), type = "risk"), stat = lapply(
        stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 36 x 11
    Output
         group1 group1_level variable variable_level  stat_name stat_label  stat
      1    TRTA      Placebo     time             60     n.risk  Number o…    59
      2    TRTA      Placebo     time             60   estimate  Survival… 0.107
      3    TRTA      Placebo     time             60  std.error  Standard… 0.036
      4    TRTA      Placebo     time             60  conf.high  CI Upper… 0.175
      5    TRTA      Placebo     time             60   conf.low  CI Lower… 0.034
      6    TRTA      Placebo     time             60 conf.level  CI Confi…  0.95
      7    TRTA      Placebo     time            180     n.risk  Number o…    35
      8    TRTA      Placebo     time            180   estimate  Survival… 0.349
      9    TRTA      Placebo     time            180  std.error  Standard… 0.061
      10   TRTA      Placebo     time            180  conf.high  CI Upper… 0.459
      11   TRTA      Placebo     time            180   conf.low  CI Lower… 0.217
      12   TRTA      Placebo     time            180 conf.level  CI Confi…  0.95
      13   TRTA    Xanomeli…     time             60     n.risk  Number o…    14
      14   TRTA    Xanomeli…     time             60   estimate  Survival… 0.306
      15   TRTA    Xanomeli…     time             60  std.error  Standard… 0.071
      16   TRTA    Xanomeli…     time             60  conf.high  CI Upper… 0.432
      17   TRTA    Xanomeli…     time             60   conf.low  CI Lower… 0.151
      18   TRTA    Xanomeli…     time             60 conf.level  CI Confi…  0.95
      19   TRTA    Xanomeli…     time            180     n.risk  Number o…     3
      20   TRTA    Xanomeli…     time            180   estimate  Survival… 0.738
      21   TRTA    Xanomeli…     time            180  std.error  Standard…  0.14
      22   TRTA    Xanomeli…     time            180  conf.high  CI Upper… 0.908
      23   TRTA    Xanomeli…     time            180   conf.low  CI Lower… 0.251
      24   TRTA    Xanomeli…     time            180 conf.level  CI Confi…  0.95
      25   TRTA    Xanomeli…     time             60     n.risk  Number o…    20
      26   TRTA    Xanomeli…     time             60   estimate  Survival… 0.268
      27   TRTA    Xanomeli…     time             60  std.error  Standard… 0.068
      28   TRTA    Xanomeli…     time             60  conf.high  CI Upper…  0.39
      29   TRTA    Xanomeli…     time             60   conf.low  CI Lower… 0.122
      30   TRTA    Xanomeli…     time             60 conf.level  CI Confi…  0.95
      31   TRTA    Xanomeli…     time            180     n.risk  Number o…     5
      32   TRTA    Xanomeli…     time            180   estimate  Survival… 0.619
      33   TRTA    Xanomeli…     time            180  std.error  Standard…  0.13
      34   TRTA    Xanomeli…     time            180  conf.high  CI Upper… 0.805
      35   TRTA    Xanomeli…     time            180   conf.low  CI Lower… 0.257
      36   TRTA    Xanomeli…     time            180 conf.level  CI Confi…  0.95
    Message
      i 4 more variables: context, fmt_fn, warning, error

# ard_survival_survfit() works with probs provided

    Code
      print(dplyr::mutate(ard_survival_survfit(survival::survfit(survival::Surv(AVAL,
        CNSR) ~ TRTA, cards::ADTTE), probs = c(0.25, 0.75)), stat = lapply(stat,
        function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 24 x 11
    Output
         group1 group1_level variable variable_level  stat_name stat_label stat
      1    TRTA      Placebo     prob           0.25   estimate  Survival…  142
      2    TRTA      Placebo     prob           0.25  conf.high  CI Upper…  181
      3    TRTA      Placebo     prob           0.25   conf.low  CI Lower…   70
      4    TRTA      Placebo     prob           0.25 conf.level  CI Confi… 0.95
      5    TRTA      Placebo     prob           0.75   estimate  Survival…  184
      6    TRTA      Placebo     prob           0.75  conf.high  CI Upper…  191
      7    TRTA      Placebo     prob           0.75   conf.low  CI Lower…  183
      8    TRTA      Placebo     prob           0.75 conf.level  CI Confi… 0.95
      9    TRTA    Xanomeli…     prob           0.25   estimate  Survival…   44
      10   TRTA    Xanomeli…     prob           0.25  conf.high  CI Upper…  180
      11   TRTA    Xanomeli…     prob           0.25   conf.low  CI Lower…   22
      12   TRTA    Xanomeli…     prob           0.25 conf.level  CI Confi… 0.95
      13   TRTA    Xanomeli…     prob           0.75   estimate  Survival…  188
      14   TRTA    Xanomeli…     prob           0.75  conf.high  CI Upper…   NA
      15   TRTA    Xanomeli…     prob           0.75   conf.low  CI Lower…  167
      16   TRTA    Xanomeli…     prob           0.75 conf.level  CI Confi… 0.95
      17   TRTA    Xanomeli…     prob           0.25   estimate  Survival…   49
      18   TRTA    Xanomeli…     prob           0.25  conf.high  CI Upper…  180
      19   TRTA    Xanomeli…     prob           0.25   conf.low  CI Lower…   37
      20   TRTA    Xanomeli…     prob           0.25 conf.level  CI Confi… 0.95
      21   TRTA    Xanomeli…     prob           0.75   estimate  Survival…  184
      22   TRTA    Xanomeli…     prob           0.75  conf.high  CI Upper…   NA
      23   TRTA    Xanomeli…     prob           0.75   conf.low  CI Lower…  180
      24   TRTA    Xanomeli…     prob           0.75 conf.level  CI Confi… 0.95
    Message
      i 4 more variables: context, fmt_fn, warning, error

# ard_survival_survfit() works with unstratified model

    Code
      print(dplyr::mutate(ard_survival_survfit(survival::survfit(survival::Surv(time,
        status) ~ 1, data = survival::lung), times = c(60, 180)), stat = lapply(stat,
        function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 12 x 9
    Output
         variable variable_level  context  stat_name stat_label  stat
      1      time             60 survival     n.risk  Number o…   213
      2      time             60 survival   estimate  Survival… 0.925
      3      time             60 survival  std.error  Standard… 0.017
      4      time             60 survival  conf.high  CI Upper…  0.96
      5      time             60 survival   conf.low  CI Lower… 0.892
      6      time             60 survival conf.level  CI Confi…  0.95
      7      time            180 survival     n.risk  Number o…   160
      8      time            180 survival   estimate  Survival… 0.722
      9      time            180 survival  std.error  Standard…  0.03
      10     time            180 survival  conf.high  CI Upper… 0.783
      11     time            180 survival   conf.low  CI Lower… 0.666
      12     time            180 survival conf.level  CI Confi…  0.95
    Message
      i 3 more variables: fmt_fn, warning, error

---

    Code
      print(dplyr::mutate(ard_survival_survfit(survival::survfit(survival::Surv(time,
        status) ~ 1, data = survival::lung), probs = c(0.5, 0.75)), stat = lapply(
        stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 8 x 9
    Output
        variable variable_level   context  stat_name stat_label stat
      1     prob            0.5 survival…   estimate  Survival…  310
      2     prob            0.5 survival…  conf.high  CI Upper…  363
      3     prob            0.5 survival…   conf.low  CI Lower…  285
      4     prob            0.5 survival… conf.level  CI Confi… 0.95
      5     prob           0.75 survival…   estimate  Survival…  550
      6     prob           0.75 survival…  conf.high  CI Upper…  654
      7     prob           0.75 survival…   conf.low  CI Lower…  460
      8     prob           0.75 survival… conf.level  CI Confi… 0.95
    Message
      i 3 more variables: fmt_fn, warning, error

# ard_survival_survfit() works with multiple stratification variables

    Code
      print(head(dplyr::select(dplyr::mutate(ard_survival_survfit(survival::survfit(
        survival::Surv(time, status) ~ sex + ph.ecog, data = survival::lung), times = c(
        60, 180)), stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(
        x, 3), x))), "group1", "group1_level", "group2", "group2_level"), 20), n = Inf)
    Message
      {cards} data frame: 20 x 4
    Output
         group1 group1_level  group2 group2_level
      1     sex            1 ph.ecog            0
      2     sex            1 ph.ecog            0
      3     sex            1 ph.ecog            0
      4     sex            1 ph.ecog            0
      5     sex            1 ph.ecog            0
      6     sex            1 ph.ecog            0
      7     sex            1 ph.ecog            0
      8     sex            1 ph.ecog            0
      9     sex            1 ph.ecog            0
      10    sex            1 ph.ecog            0
      11    sex            1 ph.ecog            0
      12    sex            1 ph.ecog            0
      13    sex            1 ph.ecog            1
      14    sex            1 ph.ecog            1
      15    sex            1 ph.ecog            1
      16    sex            1 ph.ecog            1
      17    sex            1 ph.ecog            1
      18    sex            1 ph.ecog            1
      19    sex            1 ph.ecog            1
      20    sex            1 ph.ecog            1

---

    Code
      print(head(dplyr::select(dplyr::mutate(ard_survival_survfit(survival::survfit(
        survival::Surv(time, status) ~ sex + ph.ecog, data = survival::lung), probs = c(
        0.5, 0.75)), stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(
        x, 3), x))), "group1", "group1_level", "group2", "group2_level"), 20), n = Inf)
    Message
      {cards} data frame: 20 x 4
    Output
         group1 group1_level  group2 group2_level
      1     sex            1 ph.ecog            0
      2     sex            1 ph.ecog            0
      3     sex            1 ph.ecog            0
      4     sex            1 ph.ecog            0
      5     sex            1 ph.ecog            0
      6     sex            1 ph.ecog            0
      7     sex            1 ph.ecog            0
      8     sex            1 ph.ecog            0
      9     sex            1 ph.ecog            1
      10    sex            1 ph.ecog            1
      11    sex            1 ph.ecog            1
      12    sex            1 ph.ecog            1
      13    sex            1 ph.ecog            1
      14    sex            1 ph.ecog            1
      15    sex            1 ph.ecog            1
      16    sex            1 ph.ecog            1
      17    sex            1 ph.ecog            2
      18    sex            1 ph.ecog            2
      19    sex            1 ph.ecog            2
      20    sex            1 ph.ecog            2

# ard_survival_survfit() works with competing risks

    Code
      print(dplyr::mutate(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, data = ADTTE_MS) %>%
        ard_survival_survfit(times = c(60, 180)), stat = lapply(stat, function(x)
        ifelse(is.numeric(x), cards::round5(x, 3), x))), n = Inf)
    Message
      Multi-state model detected. Showing probabilities into state 'death from cancer'.
      {cards} data frame: 36 x 11
    Output
         group1 group1_level variable variable_level  stat_name stat_label  stat
      1    TRTA      Placebo     time             60     n.risk  Number o…    59
      2    TRTA      Placebo     time             60   estimate  Survival… 0.054
      3    TRTA      Placebo     time             60  std.error  Standard… 0.026
      4    TRTA      Placebo     time             60  conf.high  CI Upper…  0.14
      5    TRTA      Placebo     time             60   conf.low  CI Lower… 0.021
      6    TRTA      Placebo     time             60 conf.level  CI Confi…  0.95
      7    TRTA      Placebo     time            180     n.risk  Number o…    35
      8    TRTA      Placebo     time            180   estimate  Survival… 0.226
      9    TRTA      Placebo     time            180  std.error  Standard… 0.054
      10   TRTA      Placebo     time            180  conf.high  CI Upper… 0.361
      11   TRTA      Placebo     time            180   conf.low  CI Lower… 0.142
      12   TRTA      Placebo     time            180 conf.level  CI Confi…  0.95
      13   TRTA    Xanomeli…     time             60     n.risk  Number o…    14
      14   TRTA    Xanomeli…     time             60   estimate  Survival… 0.137
      15   TRTA    Xanomeli…     time             60  std.error  Standard… 0.057
      16   TRTA    Xanomeli…     time             60  conf.high  CI Upper… 0.311
      17   TRTA    Xanomeli…     time             60   conf.low  CI Lower…  0.06
      18   TRTA    Xanomeli…     time             60 conf.level  CI Confi…  0.95
      19   TRTA    Xanomeli…     time            180     n.risk  Number o…     3
      20   TRTA    Xanomeli…     time            180   estimate  Survival…  0.51
      21   TRTA    Xanomeli…     time            180  std.error  Standard… 0.145
      22   TRTA    Xanomeli…     time            180  conf.high  CI Upper… 0.892
      23   TRTA    Xanomeli…     time            180   conf.low  CI Lower… 0.292
      24   TRTA    Xanomeli…     time            180 conf.level  CI Confi…  0.95
      25   TRTA    Xanomeli…     time             60     n.risk  Number o…    20
      26   TRTA    Xanomeli…     time             60   estimate  Survival… 0.162
      27   TRTA    Xanomeli…     time             60  std.error  Standard… 0.059
      28   TRTA    Xanomeli…     time             60  conf.high  CI Upper…  0.33
      29   TRTA    Xanomeli…     time             60   conf.low  CI Lower…  0.08
      30   TRTA    Xanomeli…     time             60 conf.level  CI Confi…  0.95
      31   TRTA    Xanomeli…     time            180     n.risk  Number o…     5
      32   TRTA    Xanomeli…     time            180   estimate  Survival… 0.244
      33   TRTA    Xanomeli…     time            180  std.error  Standard… 0.093
      34   TRTA    Xanomeli…     time            180  conf.high  CI Upper… 0.516
      35   TRTA    Xanomeli…     time            180   conf.low  CI Lower… 0.115
      36   TRTA    Xanomeli…     time            180 conf.level  CI Confi…  0.95
    Message
      i 4 more variables: context, fmt_fn, warning, error

---

    Code
      survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, data = ADTTE_MS) %>%
        ard_survival_survfit(times = c(60, 180), type = "risk")
    Condition
      Error in `ard_survival_survfit()`:
      ! Cannot use `type` argument with `survfit` models with class <survfitms/survfitcoxms>.

# ard_survival_survfit() errors are properly handled

    Code
      ard_survival_survfit(x, times = 25)
    Condition
      Error in `ard_survival_survfit()`:
      ! The call in the survfit object `x` must be an evaluated formula. Please see `ard_survival_survfit()` (`?cardx::ard_survival_survfit()`) documentation for details on properly specifying formulas.

---

    Code
      ard_survival_survfit(times = 25)
    Condition
      Error in `ard_survival_survfit()`:
      ! The `x` argument cannot be missing.

---

    Code
      ard_survival_survfit("not_survfit")
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'ard_survival_survfit' applied to an object of class "character"

---

    Code
      ard_survival_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA,
      cards::ADTTE), times = 100, type = "notatype")
    Condition
      Error in `ard_survival_survfit()`:
      ! `type` must be one of "survival", "risk", or "cumhaz", not "notatype".

---

    Code
      ard_survival_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA,
      cards::ADTTE), probs = c(0.25, 0.75), type = "risk")
    Condition
      Error in `ard_survival_survfit()`:
      ! Cannot use `type` argument when `probs` argument specifed.

---

    Code
      ard_survival_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA,
      cards::ADTTE), times = 100, probs = c(0.25, 0.75))
    Condition
      Error in `ard_survival_survfit()`:
      ! One and only one of `times` and `probs` must be specified.

---

    Code
      ard_survival_survfit(x = cards::ADTTE, formula = survival::Surv(ttdeath, death) ~
        trt, variables = "trt", probs = c(0.25, 0.5, 0.75))
    Condition
      Error in `ard_survival_survfit()`:
      ! The `y` argument cannot be missing.

---

    Code
      ard_survival_survfit(x = cards::ADTTE, y = survival::Surv(ttdeath, death) ~ tte,
      probs = c(0.25, 0.5, 0.75))
    Condition
      Error in `ard_survival_survfit()`:
      ! The `y` argument must be a string or expression that evaluates to an object of class <Surv> most often created with `survival::Surv()` or `ggsurvfit::Surv_CNSR()`.

# ard_survival_survfit() errors with stratified Cox model

    Code
      ard_survival_survfit(survfit(coxph(Surv(time, status) ~ age + strata(sex),
      survival::lung)))
    Condition
      Error in `ard_survival_survfit()`:
      ! Argument `x` cannot be class <survfitcox>.

# ard_survival_survfit() works with '=' in strata variable level labels

    Code
      ard_survival_survfit(survival::survfit(survival::Surv(time, status) ~ age_bin,
      data = lung2), times = 100)
    Message
      {cards} data frame: 12 x 11
    Output
          group1 group1_level variable variable_level  stat_name stat_label  stat
      1  age_bin          <60     time            100     n.risk  Number o…    77
      2  age_bin          <60     time            100   estimate  Survival… 0.928
      3  age_bin          <60     time            100  std.error  Standard… 0.028
      4  age_bin          <60     time            100  conf.high  CI Upper… 0.985
      5  age_bin          <60     time            100   conf.low  CI Lower… 0.874
      6  age_bin          <60     time            100 conf.level  CI Confi…  0.95
      7  age_bin         >=60     time            100     n.risk  Number o…   119
      8  age_bin         >=60     time            100   estimate  Survival… 0.827
      9  age_bin         >=60     time            100  std.error  Standard… 0.031
      10 age_bin         >=60     time            100  conf.high  CI Upper… 0.891
      11 age_bin         >=60     time            100   conf.low  CI Lower… 0.768
      12 age_bin         >=60     time            100 conf.level  CI Confi…  0.95
    Message
      i 4 more variables: context, fmt_fn, warning, error

# ard_survival_survfit() extends to times outside range

    Code
      print(ard_survival_survfit(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA,
      cards::ADTTE), times = 200), n = Inf)
    Message
      {cards} data frame: 18 x 11
    Output
         group1 group1_level variable variable_level  stat_name stat_label stat
      1    TRTA      Placebo     time            200     n.risk  Number o…    0
      2    TRTA      Placebo     time            200   estimate  Survival…    0
      3    TRTA      Placebo     time            200  std.error  Standard…  NaN
      4    TRTA      Placebo     time            200  conf.high  CI Upper…   NA
      5    TRTA      Placebo     time            200   conf.low  CI Lower…   NA
      6    TRTA      Placebo     time            200 conf.level  CI Confi… 0.95
      7    TRTA    Xanomeli…     time            200     n.risk  Number o…    0
      8    TRTA    Xanomeli…     time            200   estimate  Survival…    0
      9    TRTA    Xanomeli…     time            200  std.error  Standard…  NaN
      10   TRTA    Xanomeli…     time            200  conf.high  CI Upper…   NA
      11   TRTA    Xanomeli…     time            200   conf.low  CI Lower…   NA
      12   TRTA    Xanomeli…     time            200 conf.level  CI Confi… 0.95
      13   TRTA    Xanomeli…     time            200     n.risk  Number o…    0
      14   TRTA    Xanomeli…     time            200   estimate  Survival…    0
      15   TRTA    Xanomeli…     time            200  std.error  Standard…  NaN
      16   TRTA    Xanomeli…     time            200  conf.high  CI Upper…   NA
      17   TRTA    Xanomeli…     time            200   conf.low  CI Lower…   NA
      18   TRTA    Xanomeli…     time            200 conf.level  CI Confi… 0.95
    Message
      i 4 more variables: context, fmt_fn, warning, error

# ard_survival_survfit.data.frame() works as expected

    Code
      res_quo <- print(dplyr::mutate(ard_survival_survfit(x = mtcars, y = "survival::Surv(mpg, am)",
        variables = "vs", times = 20, method.args = list(start.time = 0, id = cyl)),
      stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))),
      n = Inf)
    Message
      {cards} data frame: 12 x 11
    Output
         group1 group1_level variable variable_level  stat_name stat_label  stat
      1      vs            0     time             20     n.risk  Number o…     3
      2      vs            0     time             20   estimate  Survival… 0.615
      3      vs            0     time             20  std.error  Standard… 0.082
      4      vs            0     time             20  conf.high  CI Upper…   0.8
      5      vs            0     time             20   conf.low  CI Lower… 0.474
      6      vs            0     time             20 conf.level  CI Confi…  0.95
      7      vs            1     time             20     n.risk  Number o…    11
      8      vs            1     time             20   estimate  Survival…     1
      9      vs            1     time             20  std.error  Standard…     0
      10     vs            1     time             20  conf.high  CI Upper…     1
      11     vs            1     time             20   conf.low  CI Lower…     1
      12     vs            1     time             20 conf.level  CI Confi…  0.95
    Message
      i 4 more variables: context, fmt_fn, warning, error

