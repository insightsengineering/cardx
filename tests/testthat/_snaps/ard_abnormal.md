# ard_abnormal() works

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 18 x 11
    Output
         group1 group1_level variable variable_level  context stat_name stat_label  stat fmt_fun warning error
      1    TRTA      Placebo  LBNRIND            Low abnormal         n          n     2       0              
      2    TRTA      Placebo  LBNRIND            Low abnormal         N          N     7       0              
      3    TRTA      Placebo  LBNRIND            Low abnormal         p          % 0.286    <fn>              
      4    TRTA      Placebo  LBNRIND           High abnormal         n          n     3       0              
      5    TRTA      Placebo  LBNRIND           High abnormal         N          N     7       0              
      6    TRTA      Placebo  LBNRIND           High abnormal         p          % 0.429    <fn>              
      7    TRTA    Xanomeli…  LBNRIND            Low abnormal         n          n     4       0              
      8    TRTA    Xanomeli…  LBNRIND            Low abnormal         N          N     7       0              
      9    TRTA    Xanomeli…  LBNRIND            Low abnormal         p          % 0.571    <fn>              
      10   TRTA    Xanomeli…  LBNRIND           High abnormal         n          n     3       0              
      11   TRTA    Xanomeli…  LBNRIND           High abnormal         N          N     7       0              
      12   TRTA    Xanomeli…  LBNRIND           High abnormal         p          % 0.429    <fn>              
      13   TRTA    Xanomeli…  LBNRIND            Low abnormal         n          n     4       0              
      14   TRTA    Xanomeli…  LBNRIND            Low abnormal         N          N     6       0              
      15   TRTA    Xanomeli…  LBNRIND            Low abnormal         p          % 0.667    <fn>              
      16   TRTA    Xanomeli…  LBNRIND           High abnormal         n          n     3       0              
      17   TRTA    Xanomeli…  LBNRIND           High abnormal         N          N     6       0              
      18   TRTA    Xanomeli…  LBNRIND           High abnormal         p          %   0.5    <fn>              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 9 x 9
    Output
        variable variable_level  context stat_name stat_label stat fmt_fun warning error
      1  LBNRIND            low abnormal         n          n   10       0              
      2  LBNRIND            low abnormal         N          N   20       0              
      3  LBNRIND            low abnormal         p          %  0.5    <fn>              
      4  LBNRIND           high abnormal         n          n    9       0              
      5  LBNRIND           high abnormal         N          N   20       0              
      6  LBNRIND           high abnormal         p          % 0.45    <fn>              
      7  LBNRIND          other abnormal         n          n    0       0              
      8  LBNRIND          other abnormal         N          N   20       0              
      9  LBNRIND          other abnormal         p          %    0    <fn>              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 18 x 11
    Output
         group1 group1_level variable variable_level  context stat_name stat_label  stat fmt_fun warning error
      1    TRTA      Placebo  LBNRIND            Low abnormal         n          n     2       0              
      2    TRTA      Placebo  LBNRIND            Low abnormal         N          N     7       0              
      3    TRTA      Placebo  LBNRIND            Low abnormal         p          % 0.286    <fn>              
      4    TRTA      Placebo  LBNRIND           High abnormal         n          n     3       0              
      5    TRTA      Placebo  LBNRIND           High abnormal         N          N     7       0              
      6    TRTA      Placebo  LBNRIND           High abnormal         p          % 0.429    <fn>              
      7    TRTA    Xanomeli…  LBNRIND            Low abnormal         n          n     4       0              
      8    TRTA    Xanomeli…  LBNRIND            Low abnormal         N          N     7       0              
      9    TRTA    Xanomeli…  LBNRIND            Low abnormal         p          % 0.571    <fn>              
      10   TRTA    Xanomeli…  LBNRIND           High abnormal         n          n     3       0              
      11   TRTA    Xanomeli…  LBNRIND           High abnormal         N          N     7       0              
      12   TRTA    Xanomeli…  LBNRIND           High abnormal         p          % 0.429    <fn>              
      13   TRTA    Xanomeli…  LBNRIND            Low abnormal         n          n     4       0              
      14   TRTA    Xanomeli…  LBNRIND            Low abnormal         N          N     6       0              
      15   TRTA    Xanomeli…  LBNRIND            Low abnormal         p          % 0.667    <fn>              
      16   TRTA    Xanomeli…  LBNRIND           High abnormal         n          n     3       0              
      17   TRTA    Xanomeli…  LBNRIND           High abnormal         N          N     6       0              
      18   TRTA    Xanomeli…  LBNRIND           High abnormal         p          %   0.5    <fn>              

# ard_abnormal() errors are handled correctly

    Code
      res <- ard_abnormal(adlb, postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID,
        by = TRTA, abnormal = list("HIGH", "LOW"))
    Condition
      Error in `ard_abnormal()`:
      ! `abnormal` must be a named list, where each name corresponds to a different abnormality/direction.

---

    Code
      res <- ard_abnormal(adlb, postbaseline = LBNRIND, baseline = BNRIND, id = USUBJID,
        by = TRTA, abnormal = list(high = 1:5, low = 0))
    Condition
      Error in `ard_abnormal()`:
      ! Each abnormal level of `LBNRIND` specified via `abnormal` must be a <string>.

