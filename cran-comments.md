# rmdl v0.1.0

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission (R2)

We have additionally responded to the errors that occurred when attempting to build the manual. Per Uwe Ligges, it appears that the latex symbol "\rightarrow" is problematic, and thus have removed it from the documentation. 

1. The DESCRIPTION file was modified to include references in the format suggested.

2. The @return or \value field was documented in all functions

3. The function that referenced the `.GlobalEnv` and have instead referenced it with `env = parent.frame()` instead with the intent to comply with CRAN policies. Our goal was to maintain the user's environment when they called the function `formula.tm()`. This is similar to the `stats::formula()` default argument (and hope its okay to have modeled our function after it).
