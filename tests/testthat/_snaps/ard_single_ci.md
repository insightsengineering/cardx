# ard_single_ci(method='mean_with_T') works

    Code
      ard_single_mean_T
    Message
      {cards} data frame: 6 x 8
    Output
        variable   context  stat_name stat_label      stat fmt_fn
      1      AGE single_ci          N          N       254      0
      2      AGE single_ci   estimate   estimate    75.087      1
      3      AGE single_ci   conf.low   conf.low    74.068      1
      4      AGE single_ci  conf.high  conf.high    76.106      1
      5      AGE single_ci conf.level  conf.lev…      0.95      1
      6      AGE single_ci     method     method Mean Con…   <fn>
    Message
      i 2 more variables: warning, error

# ard_single_ci(method='mean_with_Z') works

    Code
      ard_single_mean_Z
    Message
      {cards} data frame: 6 x 8
    Output
        variable   context  stat_name stat_label      stat fmt_fn
      1      AGE single_ci          N          N       254      0
      2      AGE single_ci   estimate   estimate    75.087      1
      3      AGE single_ci   conf.low   conf.low    74.072      1
      4      AGE single_ci  conf.high  conf.high    76.101      1
      5      AGE single_ci conf.level  conf.lev…      0.95      1
      6      AGE single_ci     method     method Mean Con…   <fn>
    Message
      i 2 more variables: warning, error

# ard_single_ci(method='sd') works

    Code
      ard_single_sd
    Message
      {cards} data frame: 6 x 8
    Output
        variable   context  stat_name stat_label      stat fmt_fn
      1      AGE single_ci          N          N       254      0
      2      AGE single_ci   estimate   estimate     8.246      1
      3      AGE single_ci   conf.low   conf.low     7.586      1
      4      AGE single_ci  conf.high  conf.high     9.033      1
      5      AGE single_ci conf.level  conf.lev…      0.95      1
      6      AGE single_ci     method     method SD Confi…   <fn>
    Message
      i 2 more variables: warning, error

# ard_single_ci(method='var') works

    Code
      ard_single_var
    Message
      {cards} data frame: 6 x 8
    Output
        variable   context  stat_name stat_label      stat fmt_fn
      1      AGE single_ci          N          N       254      0
      2      AGE single_ci   estimate   estimate     8.246      1
      3      AGE single_ci   conf.low   conf.low     7.586      1
      4      AGE single_ci  conf.high  conf.high     9.033      1
      5      AGE single_ci conf.level  conf.lev…      0.95      1
      6      AGE single_ci     method     method SD Confi…   <fn>
    Message
      i 2 more variables: warning, error

