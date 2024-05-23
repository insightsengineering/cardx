# unstratified ard_continuous.survey.design() works

    Code
      ard_uni_svy_cont
    Message
      {cards} data frame: 10 x 8
    Output
         variable   context      stat_name stat_label     stat fmt_fn
      1     api00 survey_s…           mean       Mean  644.169      1
      2     api00 survey_s…         median     Median      652      1
      3     api00 survey_s…            min    Minimum      411      1
      4     api00 survey_s…            max    Maximum      905      1
      5     api00 survey_s…            sum        Sum  3989985      1
      6     api00 survey_s…            var   Variance 11182.82      1
      7     api00 survey_s…             sd  Standard…  105.749      1
      8     api00 survey_s… mean.std.error   SE(Mean)   23.542      1
      9     api00 survey_s…           deff  Design E…    9.346      1
      10    api00 survey_s…            p75  75% Perc…      719      1
    Message
      i 2 more variables: warning, error

# ard_continuous.survey.design(fmt_fn)

    Code
      ard_continuous(dclus1, variables = api00, statistic = ~ c("mean", "median",
        "min", "max"), fmt_fn = list(api00 = list(mean = 2, median = "xx.xx", min = as.character)))
    Message
      {cards} data frame: 4 x 8
    Output
        variable   context stat_name stat_label    stat fmt_fn
      1    api00 survey_s…      mean       Mean 644.169      2
      2    api00 survey_s…    median     Median     652  xx.xx
      3    api00 survey_s…       min    Minimum     411   <fn>
      4    api00 survey_s…       max    Maximum     905      1
    Message
      i 2 more variables: warning, error

# ard_continuous.survey.design(stat_label)

    Code
      ard_continuous(dclus1, variables = api00, statistic = ~ c("mean", "median",
        "min", "max"), stat_label = list(api00 = list(mean = "MeAn", median = "MEDian",
        min = "MINimum")))
    Message
      {cards} data frame: 4 x 8
    Output
        variable   context stat_name stat_label    stat fmt_fn
      1    api00 survey_s…      mean       MeAn 644.169      1
      2    api00 survey_s…    median     MEDian     652      1
      3    api00 survey_s…       min    MINimum     411      1
      4    api00 survey_s…       max    Maximum     905      1
    Message
      i 2 more variables: warning, error

