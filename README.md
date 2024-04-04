
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vlndr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/asshah4/vlndr)](https://github.com/asshah4/vlndr/graphs/commit-activity)
[![R-CMD-check](https://github.com/asshah4/vlndr/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/vlndr/actions)

<!-- badges: end -->

## Installation

This package can be downloaded from CRAN or from
[Github](https://github.com/asshah4/vlndr) as below

``` r
# CRAN installation
install.packages("vlndr")
# Or remote/developmental version
remotes::install_github("asshah4/vlndr")
```

## Introduction

The package `vlndr` was intended as a way to handle causal- and
epidemiology-based modeling by the following principles:

1.  Role determination of variables
2.  Generativity in formula creation
3.  Multiple model management

The name is an acronym for “**V**ariables of **L**arge **N**umbers;
**D**idn’t **R**egress” It also lexically represents the word
**völundr**, the Norse name for Wayland the Smith, in the spirit of
*forging* together many models.

## Usage

The package is simple to use. First, lets load the basic packages. The
`mtcars` dataset will serve as the example, and we will use linear
regressions as the primary test.

``` r
library(vlndr)
#> Loading required package: vctrs
#> Loading required package: tibble
#> 
#> Attaching package: 'tibble'
#> The following object is masked from 'package:vctrs':
#> 
#>     data_frame
library(parsnip)
```
