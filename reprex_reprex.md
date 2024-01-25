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
fmls(hp ~ .x(wt) + .i(am), pattern = "parallel")
#> Interaction term `am` was applied to exposure term `wt`
#> hp ~ wt + am
#> hp ~ wt + wt:am
```

<sup>Created on 2024-01-25 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>
