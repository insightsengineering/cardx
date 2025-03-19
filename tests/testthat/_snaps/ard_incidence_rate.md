# ard_incidence_rate() works

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label   stat fmt_fn warning error
      1     AVAL incidenc…         estimate  AE Rate …  0.605      1              
      2     AVAL incidenc…         conf.low  CI Lower…  0.488      1              
      3     AVAL incidenc…        conf.high  CI Upper…  0.723      1              
      4     AVAL incidenc…        conf.type    CI Type normal   <fn>              
      5     AVAL incidenc…       conf.level  CI Confi…   0.95      1              
      6     AVAL incidenc… tot_person_years  Person-Y…  16853      1              
      7     AVAL incidenc…         n_events  Number o…    102      1              
      8     AVAL incidenc…      n_unique_id  Number o…    102      0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label    stat fmt_fn warning error
      1     time incidenc…         estimate  AE Rate … 798.841      1              
      2     time incidenc…         conf.low  CI Lower… 752.367      1              
      3     time incidenc…        conf.high  CI Upper… 845.315      1              
      4     time incidenc…        conf.type    CI Type  normal   <fn>              
      5     time incidenc…       conf.level  CI Confi…    0.95      1              
      6     time incidenc… tot_person_years  Person-Y… 142.081      1              
      7     time incidenc…         n_events  Number o…    1135      0              
      8     time incidenc…      n_unique_id  Number o…     217      0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label    stat fmt_fn warning error
      1     time incidenc…         estimate  AE Rate … 786.875      1              
      2     time incidenc…         conf.low  CI Lower… 741.978      1              
      3     time incidenc…        conf.high  CI Upper… 831.771      1              
      4     time incidenc…        conf.type    CI Type  normal   <fn>              
      5     time incidenc…       conf.level  CI Confi…    0.95      1              
      6     time incidenc… tot_person_years  Person-Y…  149.96      1              
      7     time incidenc…         n_events  Number o…    1180      0              
      8     time incidenc…      n_unique_id  Number o…    1180      0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label    stat fmt_fn warning error
      1     time incidenc…         estimate  AE Rate … 393.437      1              
      2     time incidenc…         conf.low  CI Lower… 370.989      1              
      3     time incidenc…        conf.high  CI Upper… 415.886      1              
      4     time incidenc…        conf.type    CI Type  normal   <fn>              
      5     time incidenc…       conf.level  CI Confi…    0.95      1              
      6     time incidenc… tot_person_years  Person-Y…  149.96      1              
      7     time incidenc…         n_events  Number o…    1180      0              
      8     time incidenc…      n_unique_id  Number o…     224      0              

# ard_incidence_rate(conf.type) works

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label      stat fmt_fn warning error
      1     AVAL incidenc…         estimate  AE Rate …     0.605      1              
      2     AVAL incidenc…         conf.low  CI Lower…     0.498      1              
      3     AVAL incidenc…        conf.high  CI Upper…     0.735      1              
      4     AVAL incidenc…        conf.type    CI Type normal-l…   <fn>              
      5     AVAL incidenc…       conf.level  CI Confi…      0.95      1              
      6     AVAL incidenc… tot_person_years  Person-Y…     16853      1              
      7     AVAL incidenc…         n_events  Number o…       102      1              
      8     AVAL incidenc…      n_unique_id  Number o…       102      0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label  stat fmt_fn warning error
      1     AVAL incidenc…         estimate  AE Rate … 0.605      1              
      2     AVAL incidenc…         conf.low  CI Lower… 0.493      1              
      3     AVAL incidenc…        conf.high  CI Upper… 0.735      1              
      4     AVAL incidenc…        conf.type    CI Type exact   <fn>              
      5     AVAL incidenc…       conf.level  CI Confi…  0.95      1              
      6     AVAL incidenc… tot_person_years  Person-Y… 16853      1              
      7     AVAL incidenc…         n_events  Number o…   102      1              
      8     AVAL incidenc…      n_unique_id  Number o…   102      0              

---

    Code
      print(res, columns = "all")
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context        stat_name stat_label  stat fmt_fn warning error
      1     AVAL incidenc…         estimate  AE Rate … 0.605      1              
      2     AVAL incidenc…         conf.low  CI Lower… 0.496      1              
      3     AVAL incidenc…        conf.high  CI Upper… 0.731      1              
      4     AVAL incidenc…        conf.type    CI Type  byar   <fn>              
      5     AVAL incidenc…       conf.level  CI Confi…  0.95      1              
      6     AVAL incidenc… tot_person_years  Person-Y… 16853      1              
      7     AVAL incidenc…         n_events  Number o…   102      1              
      8     AVAL incidenc…      n_unique_id  Number o…   102      0              

# ard_incidence_rate() errors are handled correctly

    Code
      res <- ard_incidence_rate(adtte, time = AVAL, count = CNSR, id = USUBJID,
        conf.type = "standard")
    Condition
      Error in `ard_incidence_rate()`:
      ! `conf.type` must be one of "normal", "normal-log", "exact", or "byar", not "standard".

---

    Code
      res <- ard_incidence_rate(adtte, time = AVAL, count = CNSR, id = USUBJID,
        units = "month")
    Condition
      Error in `ard_incidence_rate()`:
      ! `units` must be one of "years", "months", "weeks", or "days", not "month".
      i Did you mean "months"?

