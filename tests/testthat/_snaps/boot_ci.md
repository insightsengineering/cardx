# boot_ci() works with standard use

    Code
      res
    Output
      $N
      [1] 254
      
      $estimate
      [1] 75.08661
      
      $conf.level
      [1] 0.95
      
      $R
      [1] 2000
      
      $ci.type.1
      [1] "percent"
      
      $conf.low.1
      [1] 74.01998
      
      $conf.high.1
      [1] 76.05906
      

---

    Code
      res
    Message
      {cards} data frame: 16 x 8
    Output
         variable   context   stat_name stat_label    stat   warning
      1       AGE continuo…           N          N     254 bootstra…
      2       AGE continuo…    estimate   estimate  75.087 bootstra…
      3       AGE continuo…  conf.level  conf.lev…    0.95 bootstra…
      4       AGE continuo…           R          R    2000 bootstra…
      5       AGE continuo…   ci.type.1  ci.type.1  normal bootstra…
      6       AGE continuo…  conf.low.1  conf.low…  74.081 bootstra…
      7       AGE continuo… conf.high.1  conf.hig…   76.11 bootstra…
      8       AGE continuo…   ci.type.2  ci.type.2   basic bootstra…
      9       AGE continuo…  conf.low.2  conf.low…  74.114 bootstra…
      10      AGE continuo… conf.high.2  conf.hig…  76.153 bootstra…
      11      AGE continuo…   ci.type.3  ci.type.3 percent bootstra…
      12      AGE continuo…  conf.low.3  conf.low…   74.02 bootstra…
      13      AGE continuo… conf.high.3  conf.hig…  76.059 bootstra…
      14      AGE continuo…   ci.type.4  ci.type.4     bca bootstra…
      15      AGE continuo…  conf.low.4  conf.low…  73.992 bootstra…
      16      AGE continuo… conf.high.4  conf.hig…   76.04 bootstra…
    Message
      i 2 more variables: fmt_fn, error

# boot_ci() warnings work

    Code
      boot_ci(x[1], type = "perc")
    Condition
      Warning in `boot_ci()`:
      All values of t are equal to 63. Cannot calculate confidence intervals.
    Output
      $N
      [1] 1
      
      $estimate
      [1] 63
      
      $conf.level
      [1] 0.95
      
      $R
      [1] 2000
      
      $conf.low
      NULL
      
      $conf.high
      NULL
      

