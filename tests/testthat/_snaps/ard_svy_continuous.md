# unstratified ard_svy_continuous() works

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
      7     api00 continuo…             sd         SD  105.749      1
      8     api00 continuo… mean.std.error   SE(Mean)   23.542      1
      9     api00 continuo…           deff  Design E…    9.346      1
      10    api00 continuo…            p75  75% Perc…      719      1
    Message
      i 2 more variables: warning, error

# ard_svy_continuous() error handling

    Code
      dplyr::select(ard_svy_continuous(dclus1, variables = sname, statistic = ~ c(
        "mean", "median", "min", "max", "sum", "var", "sd", "mean.std.error", "deff",
        "p75")), -fmt_fn, -context, -stat_label)
    Message
      {cards} data frame: 10 x 5
    Output
         variable      stat_name      stat     error
      1     sname           mean      0.05          
      2     sname         median           error in…
      3     sname            min Bancroft…          
      4     sname            max Wilson E…          
      5     sname            sum    33.847          
      6     sname            var           error in…
      7     sname             sd           error in…
      8     sname mean.std.error      0.03          
      9     sname           deff     0.376          
      10    sname            p75           error in…
    Message
      i 1 more variable: warning

---

    Code
      dplyr::select(ard_svy_continuous(dclus1, variables = sname, by = both,
        statistic = ~ c("mean", "median", "min", "max", "sum", "var", "sd",
          "mean.std.error", "deff", "p75")), -fmt_fn, -context, -stat_label)
    Message
      {cards} data frame: 20 x 7
    Output
         group1 group1_level variable      stat_name   stat   warning     error
      1    both           No    sname           mean      0                    
      2    both          Yes    sname           mean  0.063                    
      3    both           No    sname         median        '<=' not… error in…
      4    both          Yes    sname         median        '<=' not… error in…
      5    both           No    sname            min                  'min' no…
      6    both          Yes    sname            min                  'min' no…
      7    both           No    sname            max                  'max' no…
      8    both          Yes    sname            max                  'max' no…
      9    both           No    sname            sum      0                    
      10   both          Yes    sname            sum 33.847                    
      11   both           No    sname            var      0 '-' not …          
      12   both          Yes    sname            var      0 '-' not …          
      13   both           No    sname             sd      0 '-' not …          
      14   both          Yes    sname             sd      0 '-' not …          
      15   both           No    sname mean.std.error      0                    
      16   both          Yes    sname mean.std.error  0.038                    
      17   both           No    sname           deff    NaN                    
      18   both          Yes    sname           deff  0.374                    
      19   both           No    sname            p75        '<=' not… error in…
      20   both          Yes    sname            p75        '<=' not… error in…

# ard_svy_continuous(fmt_fn)

    Code
      ard_svy_continuous(dclus1, variables = api00, statistic = ~ c("mean", "median",
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

# ard_svy_continuous(stat_label)

    Code
      ard_svy_continuous(dclus1, variables = api00, statistic = ~ c("mean", "median",
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

