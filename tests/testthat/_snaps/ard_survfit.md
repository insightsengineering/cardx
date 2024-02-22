# ard_survfit() works with times provided

    Code
      print(dplyr::mutate(ard_survfit(survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE),
      times = c(60, 180)), stat = lapply(stat, function(x) ifelse(is.numeric(x),
      cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 24 x 7
    Output
         variable variable_level stat_name stat_label  stat fmt_fn
      1      TRTA        Placebo  estimate  Survival… 0.893      1
      2      TRTA        Placebo conf.high  CI Upper… 0.966      1
      3      TRTA        Placebo  conf.low  CI Lower… 0.825      1
      4      TRTA        Placebo      time       Time    60      1
      5      TRTA        Placebo  estimate  Survival… 0.651      1
      6      TRTA        Placebo conf.high  CI Upper… 0.783      1
      7      TRTA        Placebo  conf.low  CI Lower… 0.541      1
      8      TRTA        Placebo      time       Time   180      1
      9      TRTA      Xanomeli…  estimate  Survival… 0.694      1
      10     TRTA      Xanomeli… conf.high  CI Upper… 0.849      1
      11     TRTA      Xanomeli…  conf.low  CI Lower… 0.568      1
      12     TRTA      Xanomeli…      time       Time    60      1
      13     TRTA      Xanomeli…  estimate  Survival… 0.262      1
      14     TRTA      Xanomeli… conf.high  CI Upper… 0.749      1
      15     TRTA      Xanomeli…  conf.low  CI Lower… 0.092      1
      16     TRTA      Xanomeli…      time       Time   180      1
      17     TRTA      Xanomeli…  estimate  Survival… 0.732      1
      18     TRTA      Xanomeli… conf.high  CI Upper… 0.878      1
      19     TRTA      Xanomeli…  conf.low  CI Lower…  0.61      1
      20     TRTA      Xanomeli…      time       Time    60      1
      21     TRTA      Xanomeli…  estimate  Survival… 0.381      1
      22     TRTA      Xanomeli… conf.high  CI Upper… 0.743      1
      23     TRTA      Xanomeli…  conf.low  CI Lower… 0.195      1
      24     TRTA      Xanomeli…      time       Time   180      1
    Message
      i 1 more variable: context

# ard_survfit() works with probs provided

    Code
      print(dplyr::mutate(ard_survfit(survfit(Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE),
      probs = c(0.25, 0.75)), stat = lapply(stat, function(x) ifelse(is.numeric(x),
      cards::round5(x, 3), x))), n = Inf)
    Message
      {cards} data frame: 24 x 7
    Output
         variable variable_level stat_name stat_label stat fmt_fn
      1      TRTA        Placebo  estimate  Survival…  142      1
      2      TRTA        Placebo conf.high  CI Upper…  181      1
      3      TRTA        Placebo  conf.low  CI Lower…   70      1
      4      TRTA        Placebo      prob   Quantile 0.25      1
      5      TRTA        Placebo  estimate  Survival…  184      1
      6      TRTA        Placebo conf.high  CI Upper…  191      1
      7      TRTA        Placebo  conf.low  CI Lower…  183      1
      8      TRTA        Placebo      prob   Quantile 0.75      1
      9      TRTA      Xanomeli…  estimate  Survival…   44      1
      10     TRTA      Xanomeli… conf.high  CI Upper…  180      1
      11     TRTA      Xanomeli…  conf.low  CI Lower…   22      1
      12     TRTA      Xanomeli…      prob   Quantile 0.25      1
      13     TRTA      Xanomeli…  estimate  Survival…  188      1
      14     TRTA      Xanomeli… conf.high  CI Upper…   NA      1
      15     TRTA      Xanomeli…  conf.low  CI Lower…  167      1
      16     TRTA      Xanomeli…      prob   Quantile 0.75      1
      17     TRTA      Xanomeli…  estimate  Survival…   49      1
      18     TRTA      Xanomeli… conf.high  CI Upper…  180      1
      19     TRTA      Xanomeli…  conf.low  CI Lower…   37      1
      20     TRTA      Xanomeli…      prob   Quantile 0.25      1
      21     TRTA      Xanomeli…  estimate  Survival…  184      1
      22     TRTA      Xanomeli… conf.high  CI Upper…   NA      1
      23     TRTA      Xanomeli…  conf.low  CI Lower…  180      1
      24     TRTA      Xanomeli…      prob   Quantile 0.75      1
    Message
      i 1 more variable: context

