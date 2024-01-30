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
x <- tm(witch ~ wicked)
ys <- apply_sequential_pattern(x)
#> Error in `dplyr::filter()`:
#> ℹ In argument: `is.na(covariate_1) & !is.na(covariate_2)`.
#> Caused by error:
#> ! object 'covariate_2' not found
```

<sup>Created on 2024-01-30 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>
