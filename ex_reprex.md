*Local `.Rprofile` detected at `/Users/asshah4/projects/vlndr/.Rprofile`*

``` r
library(vlndr)
#> Loading required package: vctrs
#> Loading required package: tibble
#> 
#> Attaching package: 'tibble'
#> The following object is masked from 'package:vctrs':
#> 
#>     data_frame
f <- vec_c(fmls(hp + mpg ~ .x(wt) + .i(am) + cyl),
           fmls(hp + mpg ~ .x(wt) + .i(vs) + cyl)) |>
  suppressMessages()
m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
object <- model_table(linear = m, data = mtcars)

# Variables of interest for filtering are function arguments

gtbl <-
  tbl_interaction_forest(
    object,
        outcomes = list(hp ~ "Horsepower"),
        exposures = list(wt ~ "Weight"),
        interactions = list(am ~ "Transmission", vs ~ "Engine"),
        level_labels = list(am ~ c("Automatic", "Manual"),
                           vs ~ c("V8", "V6")),
        columns = list(beta ~ "Estimate", conf ~ "95% CI", n ~ "No.", p ~ "Interaction p-value"),
        axis = list(scale ~ "continuous", title ~ "Forest"),
        width = list(),
        forest = list()
  )
#> Error: object 'colWidths' not found

gtbl
#> Error in eval(expr, envir, enclos): object 'gtbl' not found
```

<sup>Created on 2024-01-28 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>
