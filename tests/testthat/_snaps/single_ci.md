# check the single_ci_*() functions work

    Code
      single_ci_mean(x_dbl, conf.level = 0.9, use_t = TRUE)
    Output
      $N
      [1] 10
      
      $estimate
      [1] 5.5
      
      $conf.low
      [1] 3.744928
      
      $conf.high
      [1] 7.255072
      
      $conf.level
      [1] 0.9
      
      $method
      Mean Confidence Interval with Student t Distribution
      

---

    Code
      single_ci_mean(x_dbl, conf.level = 0.9, use_t = FALSE)
    Output
      $N
      [1] 10
      
      $estimate
      [1] 5.5
      
      $conf.low
      [1] 3.925173
      
      $conf.high
      [1] 7.074827
      
      $conf.level
      [1] 0.9
      
      $method
      Mean Confidence Interval with Normal Distribution
      

