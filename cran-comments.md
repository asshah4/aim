# rmdl v0.1.0

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission (R1)

We have additionally responded to the initial manual review:

1. The DESCRIPTION file was modified to include references in the format suggested.

2. The @return or \value field was documented in all functions

3. The function that referenced the `.GlobalEnv` and have instead referenced it with `env = parent.frame()` instead with the intent to comply with CRAN policies. Our goal was to maintain the user's environment when they called the function `formula.tm()`. This is similar to the `stats::formula()` default argument (and hope its okay to have modeled our function after it).