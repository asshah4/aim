
<!-- README.md is generated from README.Rmd. Please edit that file -->

# murmur

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/asshah4/murmur)](https://github.com/asshah4/murmur/graphs/commit-activity)
[![R-CMD-check](https://github.com/asshah4/murmur/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/murmur/actions)

<!-- badges: end -->

## Installation

This package has not yet been released on CRAN, but can be downloaded
from Github.

``` r
remotes::install_github("asshah4/murmur")
```

## Introduction

The `murmur` package is intended to help build causal models with an
underlying focus on exploration. However, instead of starting purely
from a **directed acyclic graph**, this package intends to allow
modeling to occur simultaneously, and then leveraging the patterns
within the data to help cut away *confounders* and other terms for a
more parsimonious causal model. The premise is that there are two
components in studying causality:

1.  Causal model diagrams
2.  Hypothesis testing and statistical analysis

These two components are reflected within the primary functions of the
package, and are built to help refine and modulate hypotheses as the
study develops. The underlying data structure is the concept of a
**map** that has multiple murmurs describing variable relationships,
however they may exist in different **layers** on the map.

## Usage

The package is simple to use. First, lets load the basic packages. The
`mtcars` dataset will serve as the example, and we will use linear
regressions as the primary test.

``` r
library(murmur)
library(parsnip)
```

The basic function of the package serves to help create a `model_map`
object.

``` r
create_models()
#> # A map with 0 hypotheses
#> #
#> # A tibble: 0 × 8
#> # … with 8 variables: name <chr>, outcome <chr>, exposure <chr>, level <chr>,
#> #   number <int>, formulae <list>, fit <list>, tidy <list>
```

This is accompanied by `hypothesis` objects, which are essentially
modified `formula` objects that allow for a better understanding of
variable relationships. We create two objects below, which have not yet
been analyzed yet.

``` r
h1 <-
    hypothesize(
        mpg ~ wt + hp + disp,
        exposures = "wt",
        combination = "sequential",
        test = linear_reg() %>% set_engine("lm"),
        data = mtcars,
    )

h2 <- update_hypothesis(h1, combination = "sequential")

# Print h1
h1
#> ----------
#> Hypothesis
#> ----------
#> 
#> mpg ~ wt + hp + disp
#> 
#> -----------
#> Description
#> -----------
#> 
#> Combination      sequential
#> Test         linear_reg, model_spec
#> Data         mtcars
#> Strata       none
# Print h2
h2
#> ----------
#> Hypothesis
#> ----------
#> 
#> mpg ~ wt + hp + disp
#> 
#> -----------
#> Description
#> -----------
#> 
#> Combination      sequential
#> Test         linear_reg, model_spec
#> Data         mtcars
#> Strata       none
```

These hypotheses can then be *drawn* on to the *study map* as below.

``` r
m1 <-
    create_models() %>%
    add_hypothesis(h1) %>%
    add_hypothesis(h2) 

# Print study
m1
#> # A map with 2 hypotheses
#> #
#> # A tibble: 6 × 8
#>   name  outcome exposure level number formulae  fit    tidy  
#>   <chr> <chr>   <chr>    <lgl>  <int> <list>    <list> <list>
#> 1 h1    mpg     NA       NA         1 <formula> <NULL> <NULL>
#> 2 h1    mpg     NA       NA         2 <formula> <NULL> <NULL>
#> 3 h1    mpg     NA       NA         3 <formula> <NULL> <NULL>
#> 4 h2    mpg     NA       NA         1 <formula> <NULL> <NULL>
#> 5 h2    mpg     NA       NA         2 <formula> <NULL> <NULL>
#> 6 h2    mpg     NA       NA         3 <formula> <NULL> <NULL>
```

Then, for analysis and display of results, the findings can easily be
extracted.

``` r
m2 <-
    m1 %>%
    construct_tests() %>%
    extract_models(which_ones = "h1", tidy = TRUE)

# Print findings
m2
#> # A tibble: 9 × 12
#>   name  outcome exposure level number term         estimate std.error statistic
#>   <chr> <chr>   <chr>    <lgl>  <int> <chr>           <dbl>     <dbl>     <dbl>
#> 1 h1    mpg     NA       NA         1 (Intercept) 37.3        1.88      19.9   
#> 2 h1    mpg     NA       NA         1 wt          -5.34       0.559     -9.56  
#> 3 h1    mpg     NA       NA         2 (Intercept) 37.2        1.60      23.3   
#> 4 h1    mpg     NA       NA         2 wt          -3.88       0.633     -6.13  
#> 5 h1    mpg     NA       NA         2 hp          -0.0318     0.00903   -3.52  
#> 6 h1    mpg     NA       NA         3 (Intercept) 37.1        2.11      17.6   
#> 7 h1    mpg     NA       NA         3 wt          -3.80       1.07      -3.56  
#> 8 h1    mpg     NA       NA         3 hp          -0.0312     0.0114    -2.72  
#> 9 h1    mpg     NA       NA         3 disp        -0.000937   0.0103    -0.0905
#> # … with 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
```
