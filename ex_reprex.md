*Local `.Rprofile` detected at `/Users/asshah4/projects/rmdl/.Rprofile`*

``` r
library(rmdl)
#> Loading required package: vctrs
#> Loading required package: tibble
#> 
#> Attaching package: 'tibble'
#> The following object is masked from 'package:vctrs':
#> 
#>     data_frame

cars <-
    mtcars |>
    dplyr::mutate(heavy = ifelse(wt > 3.2, 1, 0))

m1 <-
    fmls(heavy ~ .x(hp) + .i(vs)) |>
    fit(.fn = glm, family = 'binomial', data = cars, raw = FALSE)
#> Interaction term `vs` was applied to exposure term `hp`
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
m2 <-
    fmls(heavy ~ .x(hp) + .i(am)) |>
    fit(.fn = glm, family = 'binomial', data = cars, raw = FALSE)
#> Interaction term `am` was applied to exposure term `hp`
#> Warning: glm.fit: algorithm did not converge

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

mt <- rmdl::model_table(one = m1, two = m2)

tbl_interaction_forest(
    object = mt,
    outcomes = am ~ "Automatic",
    exposures = hp ~ 'Horsepower',
    interactions = list(vs ~ "V/S", am ~ "Transmission"),
    level_labels = list(
        vs ~ c("yes", "no"),
        am ~ c("Manual", "Automatic")
    )
)
#> Error in tbl_interaction_forest(object = mt, outcomes = am ~ "Automatic", : Assertion on 'all(out_nms %in% object$outcome)' failed: Must be TRUE.
```

<sup>Created on 2024-01-30 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>
