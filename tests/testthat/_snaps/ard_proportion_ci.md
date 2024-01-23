# ard_proportion_ci(method='strat_wilson') works

    Code
      ard_proportion_ci(data = data.frame(rsp = rsp, strata = interaction(strata_data)),
      variables = rsp, strata = strata, weights = weights, method = "strat_wilson")
    Message
      {cards} data frame: 1 x 8
    Output
        variable   context stat_name stat_label statistic     error
      1      rsp proporti…   prop_ci    prop_ci      NULL could no…
    Message
      i 2 more variables: statistic_fmt_fn, warning

---

    Code
      ard_proportion_ci(data = data.frame(rsp = rsp, strata = interaction(strata_data)),
      variables = rsp, strata = strata, weights = weights, method = "strat_wilsoncc")
    Message
      {cards} data frame: 1 x 8
    Output
        variable   context stat_name stat_label statistic     error
      1      rsp proporti…   prop_ci    prop_ci      NULL could no…
    Message
      i 2 more variables: statistic_fmt_fn, warning

