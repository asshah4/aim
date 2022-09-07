
<!-- README.md is generated from README.Rmd. Please edit that file -->

# volundr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/asshah4/volundr)](https://github.com/asshah4/volundr/graphs/commit-activity)
[![R-CMD-check](https://github.com/asshah4/volundr/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/volundr/actions)

<!-- badges: end -->

## Installation

This package has not yet been released on CRAN, but can be downloaded
from Github.

``` r
remotes::install_github("asshah4/volundr")
```

## Introduction

The package `volundr` is intended to help forge together multiple models
to better understand complex relationships in the causal space.

## Usage

The package is simple to use. First, lets load the basic packages. The
`mtcars` dataset will serve as the example, and we will use linear
regressions as the primary test.

``` r
library(volundr)
#> Loading required package: vctrs
#> Loading required package: arcana
#> Loading required package: tibble
#> 
#> Attaching package: 'tibble'
#> The following object is masked from 'package:vctrs':
#> 
#>     data_frame
library(parsnip)
```
