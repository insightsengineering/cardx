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
      [1] "normal"
      
      $conf.low.1
      [1] 74.08126
      
      $conf.high.1
      [1] 76.11047
      
      $ci.type.2
      [1] "basic"
      
      $conf.low.2
      [1] 74.11417
      
      $conf.high.2
      [1] 76.15325
      
      $ci.type.3
      [1] "percent"
      
      $conf.low.3
      [1] 74.01998
      
      $conf.high.3
      [1] 76.05906
      
      $ci.type.4
      [1] "bca"
      

---

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
      [1] "normal"
      
      $conf.low.1
      [1] 74.08126
      
      $conf.high.1
      [1] 76.11047
      
      $ci.type.2
      [1] "basic"
      
      $conf.low.2
      [1] 74.11417
      
      $conf.high.2
      [1] 76.15325
      
      $ci.type.3
      [1] "percent"
      
      $conf.low.3
      [1] 74.01998
      
      $conf.high.3
      [1] 76.05906
      
      $ci.type.4
      [1] "bca"
      

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
      

