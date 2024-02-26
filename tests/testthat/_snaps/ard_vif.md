# ard_vif() works

    Code
      ard_vif(lm(AGE ~ ARM + SEX, data = cards::ADSL))
    Output
      # A tibble: 6 x 7
        variable context stat_name stat_label    statistic warning      error       
        <chr>    <chr>   <chr>     <chr>             <dbl> <named list> <named list>
      1 ARM      vif     GVIF      GVIF               1.02 <NULL>       <NULL>      
      2 ARM      vif     df        df                 2    <NULL>       <NULL>      
      3 ARM      vif     aGVIF     Adjusted GVIF      1.00 <NULL>       <NULL>      
      4 SEX      vif     GVIF      GVIF               1.02 <NULL>       <NULL>      
      5 SEX      vif     df        df                 1    <NULL>       <NULL>      
      6 SEX      vif     aGVIF     Adjusted GVIF      1.01 <NULL>       <NULL>      

# ard_vif() appropriate errors are given for model with only 1 term

    Code
      ard_vif(lm(AGE ~ ARM, data = cards::ADSL))
    Output
      # A tibble: 2 x 7
        variable                context stat_name stat_label statistic warning   error
        <chr>                   <chr>   <chr>     <chr>      <list>    <named l> <nam>
      1 ARMXanomeline High Dose vif     VIF       VIF        <NULL>    <NULL>    <chr>
      2 ARMXanomeline Low Dose  vif     VIF       VIF        <NULL>    <NULL>    <chr>

