# ard_svyranktest() works

    Code
      as.data.frame(svyranktest[[1]])
    Output
          group1 variable     context   stat_name               stat_label
      1 comp.imp   enroll svyranktest   statistic      X-squared Statistic
      2 comp.imp   enroll svyranktest   parameter       Degrees of Freedom
      3 comp.imp   enroll svyranktest    estimate Median of the Difference
      4 comp.imp   enroll svyranktest  null.value               Null Value
      5 comp.imp   enroll svyranktest alternative   Alternative Hypothesis
      6 comp.imp   enroll svyranktest      method                   method
      7 comp.imp   enroll svyranktest   data.name                Data Name
      8 comp.imp   enroll svyranktest     p.value                  p-value
                                   stat fmt_fn warning error
      1                       -1.718689      1    NULL  NULL
      2                              36      1    NULL  NULL
      3                      -0.1060602      1    NULL  NULL
      4                               0      1    NULL  NULL
      5                       two.sided   NULL    NULL  NULL
      6 Design-based KruskalWallis test   NULL    NULL  NULL
      7               enroll ~ comp.imp   NULL    NULL  NULL
      8                      0.09426084      1    NULL  NULL

---

    Code
      as.data.frame(svyranktest[[2]])
    Output
          group1 variable     context   stat_name               stat_label
      1 comp.imp   enroll svyranktest   statistic      X-squared Statistic
      2 comp.imp   enroll svyranktest   parameter       Degrees of Freedom
      3 comp.imp   enroll svyranktest    estimate Median of the Difference
      4 comp.imp   enroll svyranktest  null.value               Null Value
      5 comp.imp   enroll svyranktest alternative   Alternative Hypothesis
      6 comp.imp   enroll svyranktest      method                   method
      7 comp.imp   enroll svyranktest   data.name                Data Name
      8 comp.imp   enroll svyranktest     p.value                  p-value
                                   stat fmt_fn warning error
      1                       -1.583859      1    NULL  NULL
      2                              36      1    NULL  NULL
      3                      -0.3791163      1    NULL  NULL
      4                               0      1    NULL  NULL
      5                       two.sided   NULL    NULL  NULL
      6 Design-based vanderWaerden test   NULL    NULL  NULL
      7               enroll ~ comp.imp   NULL    NULL  NULL
      8                       0.1219723      1    NULL  NULL

---

    Code
      as.data.frame(svyranktest[[3]])
    Output
          group1 variable     context   stat_name               stat_label
      1 comp.imp   enroll svyranktest   statistic      X-squared Statistic
      2 comp.imp   enroll svyranktest   parameter       Degrees of Freedom
      3 comp.imp   enroll svyranktest    estimate Median of the Difference
      4 comp.imp   enroll svyranktest  null.value               Null Value
      5 comp.imp   enroll svyranktest alternative   Alternative Hypothesis
      6 comp.imp   enroll svyranktest      method                   method
      7 comp.imp   enroll svyranktest   data.name                Data Name
      8 comp.imp   enroll svyranktest     p.value                  p-value
                            stat fmt_fn warning error
      1               -0.9139828      1    NULL  NULL
      2                       36      1    NULL  NULL
      3               -0.1240709      1    NULL  NULL
      4                        0      1    NULL  NULL
      5                two.sided   NULL    NULL  NULL
      6 Design-based median test   NULL    NULL  NULL
      7        enroll ~ comp.imp   NULL    NULL  NULL
      8                0.3668071      1    NULL  NULL

---

    Code
      as.data.frame(svyranktest[[4]])
    Output
          group1 variable     context   stat_name               stat_label
      1 comp.imp   enroll svyranktest   statistic      X-squared Statistic
      2 comp.imp   enroll svyranktest   parameter       Degrees of Freedom
      3 comp.imp   enroll svyranktest    estimate Median of the Difference
      4 comp.imp   enroll svyranktest  null.value               Null Value
      5 comp.imp   enroll svyranktest alternative   Alternative Hypothesis
      6 comp.imp   enroll svyranktest      method                   method
      7 comp.imp   enroll svyranktest   data.name                Data Name
      8 comp.imp   enroll svyranktest     p.value                  p-value
                                   stat fmt_fn warning error
      1                       -1.718689      1    NULL  NULL
      2                              36      1    NULL  NULL
      3                      -0.1060602      1    NULL  NULL
      4                               0      1    NULL  NULL
      5                       two.sided   NULL    NULL  NULL
      6 Design-based KruskalWallis test   NULL    NULL  NULL
      7               enroll ~ comp.imp   NULL    NULL  NULL
      8                      0.09426084      1    NULL  NULL

