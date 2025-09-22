# ard_incidence_rate() works

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 9 x 8
    Output
        variable   context       stat_name stat_label   stat fmt_fun warning error
      1     AVAL incidenc…        estimate  Incidenc…  0.605       1              
      2     AVAL incidenc…       std.error  Standard…  0.001       1              
      3     AVAL incidenc…        conf.low  CI Lower…  0.488       1              
      4     AVAL incidenc…       conf.high  CI Upper…  0.723       1              
      5     AVAL incidenc…       conf.type    CI Type normal    <fn>              
      6     AVAL incidenc…      conf.level  CI Confi…   0.95       1              
      7     AVAL incidenc… tot_person_time  Person-Y…  16853       1              
      8     AVAL incidenc…        n_events  Number o…    102       1              
      9     AVAL incidenc…               N  Number o…    254       0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 9 x 8
    Output
        variable   context       stat_name stat_label   stat fmt_fun warning error
      1     time incidenc…        estimate  Incidenc…  2.187       1              
      2     time incidenc…       std.error  Standard…  0.001       1              
      3     time incidenc…        conf.low  CI Lower…   2.06       1              
      4     time incidenc…       conf.high  CI Upper…  2.314       1              
      5     time incidenc…       conf.type    CI Type normal    <fn>              
      6     time incidenc…      conf.level  CI Confi…   0.95       1              
      7     time incidenc… tot_person_time  Person-D…  51895       1              
      8     time incidenc…        n_events  Number o…   1135       0              
      9     time incidenc…               N  Number o…    217       0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 9 x 8
    Output
        variable   context       stat_name stat_label   stat fmt_fun warning error
      1     time incidenc…        estimate  Incidenc…  2.154       1              
      2     time incidenc…       std.error  Standard…  0.001       1              
      3     time incidenc…        conf.low  CI Lower…  2.031       1              
      4     time incidenc…       conf.high  CI Upper…  2.277       1              
      5     time incidenc…       conf.type    CI Type normal    <fn>              
      6     time incidenc…      conf.level  CI Confi…   0.95       1              
      7     time incidenc… tot_person_time  Person-D…  54773       1              
      8     time incidenc…        n_events  Number o…   1180       0              
      9     time incidenc…               N  Number o…   1180       0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 9 x 8
    Output
        variable   context       stat_name stat_label   stat fmt_fun warning error
      1     time incidenc…        estimate  Incidenc…  1.077       1              
      2     time incidenc…       std.error  Standard…  0.001       1              
      3     time incidenc…        conf.low  CI Lower…  1.016       1              
      4     time incidenc…       conf.high  CI Upper…  1.139       1              
      5     time incidenc…       conf.type    CI Type normal    <fn>              
      6     time incidenc…      conf.level  CI Confi…   0.95       1              
      7     time incidenc… tot_person_time  Person-D…  54773       1              
      8     time incidenc…        n_events  Number o…   1180       0              
      9     time incidenc…               N  Number o…    224       0              

# ard_incidence_rate(conf.type) works

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 9 x 8
    Output
        variable   context       stat_name stat_label      stat fmt_fun warning error
      1     AVAL incidenc…        estimate  Incidenc…     0.605       1              
      2     AVAL incidenc…       std.error  Standard…     0.001       1              
      3     AVAL incidenc…        conf.low  CI Lower…     0.498       1              
      4     AVAL incidenc…       conf.high  CI Upper…     0.735       1              
      5     AVAL incidenc…       conf.type    CI Type normal-l…    <fn>              
      6     AVAL incidenc…      conf.level  CI Confi…      0.95       1              
      7     AVAL incidenc… tot_person_time  Person-Y…     16853       1              
      8     AVAL incidenc…        n_events  Number o…       102       1              
      9     AVAL incidenc…               N  Number o…       254       0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 9 x 8
    Output
        variable   context       stat_name stat_label  stat fmt_fun warning error
      1     AVAL incidenc…        estimate  Incidenc… 0.605       1              
      2     AVAL incidenc…       std.error  Standard… 0.001       1              
      3     AVAL incidenc…        conf.low  CI Lower… 0.493       1              
      4     AVAL incidenc…       conf.high  CI Upper… 0.735       1              
      5     AVAL incidenc…       conf.type    CI Type exact    <fn>              
      6     AVAL incidenc…      conf.level  CI Confi…  0.95       1              
      7     AVAL incidenc… tot_person_time  Person-Y… 16853       1              
      8     AVAL incidenc…        n_events  Number o…   102       1              
      9     AVAL incidenc…               N  Number o…   254       0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 9 x 8
    Output
        variable   context       stat_name stat_label  stat fmt_fun warning error
      1     AVAL incidenc…        estimate  Incidenc… 0.605       1              
      2     AVAL incidenc…       std.error  Standard… 0.001       1              
      3     AVAL incidenc…        conf.low  CI Lower… 0.496       1              
      4     AVAL incidenc…       conf.high  CI Upper… 0.731       1              
      5     AVAL incidenc…       conf.type    CI Type  byar    <fn>              
      6     AVAL incidenc…      conf.level  CI Confi…  0.95       1              
      7     AVAL incidenc… tot_person_time  Person-Y… 16853       1              
      8     AVAL incidenc…        n_events  Number o…   102       1              
      9     AVAL incidenc…               N  Number o…   254       0              

# ard_incidence_rate() errors are handled correctly

    Code
      res <- ard_incidence_rate(adtte, time = SEX, count = CNSR, id = USUBJID)
    Condition
      Error in `ard_incidence_rate()`:
      ! The `time` variable must be of type <numeric/integer> but `SEX` is a character vector.

---

    Code
      res <- ard_incidence_rate(adtte, time = AVAL, count = CNSR, id = USUBJID,
        unit_label = 10)
    Condition
      Error in `ard_incidence_rate()`:
      ! The `unit_label` argument must be a string, not a number.

---

    Code
      res <- ard_incidence_rate(adtte, time = AVAL, count = CNSR, id = USUBJID,
        unit_label = "years", conf.type = "standard")
    Condition
      Error in `ard_incidence_rate()`:
      ! `conf.type` must be one of "normal", "normal-log", "exact", or "byar", not "standard".

