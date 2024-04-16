
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmdl

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/asshah4/rmdl)](https://github.com/asshah4/rmdl/graphs/commit-activity)
[![R-CMD-check](https://github.com/asshah4/rmdl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asshah4/rmdl/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Installation

This package can be downloaded from CRAN or from
[Github](https://github.com/asshah4/rmdl) as below

``` r
# CRAN installation
install.packages("rmdl")
# Or remote/developmental version
remotes::install_github("asshah4/rmdl")
```

## Introduction

The package `rmdl` was intended as a way to handle causal- and
epidemiology-based modeling by the following principles:

1.  Role determination of variables
2.  Generativity in formula creation
3.  Multiple model management

## Usage

The package is simple to use. First, lets load the basic packages. The
`mtcars` dataset will serve as the example, and we will use linear
regressions as the primary test.

``` r
library(rmdl)
#> Loading required package: vctrs
#> Loading required package: tibble
#> 
#> Attaching package: 'tibble'
#> The following object is masked from 'package:vctrs':
#> 
#>     data_frame
```

There are several important extended classes that this package
introduces, however they are primarily used for internal validation and
for shortcuts to allow more effective communication.

- `<fmls>` are a *version* of the base `R` formula object, but contain
  additional information and have extra features
- `<tm>` are atomic elements used to describe individual variables, and
  departs from how terms are generally treated in the `{stats}` package
- `<mdl>` and `<mdl_tbl>` exist primarily as *tidy* versions of class
  regression modeling

We will see how they come together below. Next, we will evaluate a toy
dataset and evaluate how a `<fmls>` object is generated.

``` r
# Look at potential data from the `mtcars` dataset
head(mtcars)
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

baseFormula <- mpg ~ wt + hp
rmdlFormula <- fmls(mpg ~ wt + hp)

# Similar to the base formula
rmdlFormula
#> mpg ~ wt + hp
```

Now we can fit the hypothesis to its data - in this case, a simple
linear regression. The option to return the model as raw or not is
given. If `TRUE`, the default, then the expected result from the
modeling fit will be returned. For our purposes though, we want to use
the custom fit method, which retains more key information.

``` r
# Uses a custom fit function 
rmdlModel <-
  rmdlFormula |>
  fit(.fn = lm, data = mtcars, raw = FALSE)

rmdlModel
#> <model[1]>
#> lm(mpg ~ wt + hp)
```

## Advanced usage

The power of the `{rmdl}` package is partially in its flexibility with
formulas. Please see the vignettes for further details on usage.
