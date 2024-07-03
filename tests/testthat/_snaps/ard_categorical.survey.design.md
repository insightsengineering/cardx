# ard_categorical.survey.design() returns an error when variables have all NAs

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "row")
    Condition
      Error in `FUN()`:
      ! Column "Class" is all missing and cannot be tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

---

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "column")
    Condition
      Error in `FUN()`:
      ! Column "Class" is all missing and cannot be tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

---

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "cell")
    Condition
      Error in `FUN()`:
      ! Column "Class" is all missing and cannot be tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

