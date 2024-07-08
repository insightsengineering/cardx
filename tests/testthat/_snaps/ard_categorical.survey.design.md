# ard_categorical.survey.design() returns an error when variables have all NAs

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "row")
    Condition
      Error in `ard_categorical()`:
      ! Column "Class" is all missing and cannot be tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

---

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "column")
    Condition
      Error in `ard_categorical()`:
      ! Column "Class" is all missing and cannot be tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

---

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "cell")
    Condition
      Error in `ard_categorical()`:
      ! Column "Class" is all missing and cannot be tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

# ard_categorical.survey.design(by) messages about protected names

    Code
      ard_categorical(svy_trial, by = variable, variables = stage)
    Condition
      Error in `ard_categorical()`:
      ! The `by` argument cannot include variables named "variable" and "variable_level"

