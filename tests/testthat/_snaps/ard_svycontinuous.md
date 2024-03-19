# unstratified ard_svycontinuous() works

    Code
      ard_uni_svy_cont
    Message
      {cards} data frame: 10 x 8
    Output
         variable   context      stat_name stat_label     stat fmt_fn
      1     api00 continuo…           mean       Mean  644.169      1
      2     api00 continuo…         median     Median      652      1
      3     api00 continuo…            min    Minimum      411      1
      4     api00 continuo…            max    Maximum      905      1
      5     api00 continuo…            sum        Sum  3989985      1
      6     api00 continuo…            var   Variance 11182.82      1
      7     api00 continuo…             sd  Standard…  105.749      1
      8     api00 continuo… mean.std.error   SE(Mean)   23.542      1
      9     api00 continuo…           deff  Design E…    9.346      1
      10    api00 continuo…            p75  75% Perc…      719      1
    Message
      i 2 more variables: warning, error

# ard_svycontinuous(fmt_fn)

    Code
      ard_svycontinuous(dclus1, variables = api00, statistic = ~ c("mean", "median",
        "min", "max"), fmt_fn = list(api00 = list(mean = 2, median = "xx.xx", min = as.character)))
    Message
      {cards} data frame: 4 x 8
    Output
        variable   context stat_name stat_label    stat fmt_fn
      1    api00 continuo…      mean       Mean 644.169      2
      2    api00 continuo…    median     Median     652  xx.xx
      3    api00 continuo…       min    Minimum     411   <fn>
      4    api00 continuo…       max    Maximum     905      1
    Message
      i 2 more variables: warning, error

# ard_svycontinuous(stat_label)

    Code
      ard_svycontinuous(dclus1, variables = api00, statistic = ~ c("mean", "median",
        "min", "max"), stat_label = list(api00 = list(mean = "MeAn", median = "MEDian",
        min = "MINimum")))
    Message
      {cards} data frame: 4 x 8
    Output
        variable   context stat_name stat_label    stat fmt_fn
      1    api00 continuo…      mean       MeAn 644.169      1
      2    api00 continuo…    median     MEDian     652      1
      3    api00 continuo…       min    MINimum     411      1
      4    api00 continuo…       max    Maximum     905      1
    Message
      i 2 more variables: warning, error

