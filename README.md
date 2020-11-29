
<!-- README.md is generated from README.Rmd. Please edit that file -->

# marksman

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/asshah4/marksman/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/marksman/actions)
[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/asshah4/marksman)](https://github.com/asshah4/marksman/graphs/commit-activity)
[![Travis build
status](https://travis-ci.com/asshah4/marksman.svg?branch=master)](https://travis-ci.com/asshah4/marksman)
[![Codecov test
coverage](https://codecov.io/gh/asshah4/marksman/branch/master/graph/badge.svg)](https://codecov.io/gh/asshah4/marksman?branch=master)
<!-- badges: end -->

The goal of `marksman` is to help organize data, hypotheses, and
analyses during research forays, providing a strategic and smart
approach at hitting the target of research questions.

The inspiration came from the fact that when applying for any research
project, specific aims were required, which came with their own
hypotheses. The results of these were important, whether or not they
were significant. This is a more *supervised learning* approach,
compared to the *unsupervised learning* in more machine learning /
statistical schools of thought.

It relies on and borrows from the `tidymodels` approach to modeling, and
sharpens the focus to help with specific research projects.

## Installation

You can install the released version of marksman from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("marksman")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asshah4/marksman")
```

# A Brief Example

The `marksman` package is intended to used to aid alongside any research
paper or project as a mechanism to organize the research aims.

Let’s run through a brief example of the process of using `marksman`.
First select the libraries needed. The `card` package has the datasets
that will be used.

``` r
library(marksman)
library(card)
library(tidymodels)
#> ── Attaching packages ────────────────────────────────────── tidymodels 0.1.2 ──
#> ✓ broom     0.7.2.9000     ✓ recipes   0.1.15    
#> ✓ dials     0.0.9          ✓ rsample   0.0.8     
#> ✓ dplyr     1.0.2          ✓ tibble    3.0.4     
#> ✓ ggplot2   3.3.2          ✓ tidyr     1.1.2     
#> ✓ infer     0.5.3          ✓ tune      0.1.2     
#> ✓ modeldata 0.1.0          ✓ workflows 0.2.1     
#> ✓ parsnip   0.1.4          ✓ yardstick 0.0.7     
#> ✓ purrr     0.3.4
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> x purrr::discard() masks scales::discard()
#> x dplyr::filter()  masks stats::filter()
#> x dplyr::lag()     masks stats::lag()
#> x recipes::step()  masks stats::step()
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ readr   1.4.0     ✓ forcats 0.5.0
#> ✓ stringr 1.4.0
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x readr::col_factor() masks scales::col_factor()
#> x purrr::discard()    masks scales::discard()
#> x dplyr::filter()     masks stats::filter()
#> x stringr::fixed()    masks recipes::fixed()
#> x dplyr::lag()        masks stats::lag()
#> x readr::spec()       masks yardstick::spec()
library(circular)
#> 
#> Attaching package: 'circular'
#> The following objects are masked from 'package:stats':
#> 
#>     sd, var
```

Next, setting up the regression model the traditional way. This example
uses an angle as an outcome, and requires use of the `circular` package
for the regression analysis.

``` r
# Setup of variables
f1 <- az_svg ~ lab_hba1c + age + sex + bmi + cad + htn
f2 <- log_wvg ~ lab_hba1c + age + sex + bmi + cad + htn
df <- geh
df$az_svg <-
    circular(df$az_svg, units = "degrees") %>%
    conversion.circular(., units = "radians")
```

The **az\_svg** variable is angular. The **log\_wvg** variable is linear
(after its been log-transformed). We can use the model specification for
circular regressions, using the `parnsip` approach along with a linear
model approach.

``` r
# Circular regression spec
circ_mod <-
    circular_reg(pattern = "c-l", tolerance = 1e-0, initial = varying()) %>%
    set_engine("circular")

# Linear models
lm_mod <-
    linear_reg() %>%
    set_engine("lm")
```

This next portion uses the functions `marksman::bullet()` and
`marksman::aim()` to create the modeling scaffolding needed.

``` r
# Creating the bullets
b1 <- bullet(f1, exposure = "lab_hba1c", approach = "sequential", model = circ_mod)
b2 <- bullet(f2, exposure = "lab_hba1c", approach = "sequential", model = lm_mod)
a <- aim(list(circ = b1, lin = b2))
print(a)
#> # A tibble: 10 x 7
#>    ammo  outcomes model_num predictors formulas  model_spec checkpoint
#>  * <chr> <chr>        <int> <list>     <list>    <list>     <lgl>     
#>  1 circ  az_svg           1 <chr [2]>  <formula> <spec[?]>  FALSE     
#>  2 circ  az_svg           2 <chr [3]>  <formula> <spec[?]>  FALSE     
#>  3 circ  az_svg           3 <chr [4]>  <formula> <spec[?]>  FALSE     
#>  4 circ  az_svg           4 <chr [5]>  <formula> <spec[?]>  FALSE     
#>  5 circ  az_svg           5 <chr [6]>  <formula> <spec[?]>  FALSE     
#>  6 lin   log_wvg          1 <chr [2]>  <formula> <spec[+]>  TRUE      
#>  7 lin   log_wvg          2 <chr [3]>  <formula> <spec[+]>  TRUE      
#>  8 lin   log_wvg          3 <chr [4]>  <formula> <spec[+]>  TRUE      
#>  9 lin   log_wvg          4 <chr [5]>  <formula> <spec[+]>  TRUE      
#> 10 lin   log_wvg          5 <chr [6]>  <formula> <spec[+]>  TRUE
```

There is a **checkpoint** column from the `aim()` function that shows
whether the model is ready to use. In this case, the circular regression
is not. The `varying()` element of the argument requires us to stop and
check, or make modifications to the aims table.

``` r
# Fix the circular regression
c <- 
    a[a$ammo == "circ", ] %>%
    mutate(model_spec = map2(model_spec, formulas, ~update(.x, initial = rep(0, length(all.vars(.y)))))) %>%
    ballistics()
#> Putting the aim in sight. Checking if models can be fit.

a[a$ammo == "circ", ] <- c
print(a)
#> # A tibble: 10 x 7
#>    ammo  outcomes model_num predictors formulas  model_spec checkpoint
#>    <chr> <chr>        <int> <list>     <list>    <list>     <lgl>     
#>  1 circ  az_svg           1 <chr [2]>  <formula> <spec[+]>  TRUE      
#>  2 circ  az_svg           2 <chr [3]>  <formula> <spec[+]>  TRUE      
#>  3 circ  az_svg           3 <chr [4]>  <formula> <spec[+]>  TRUE      
#>  4 circ  az_svg           4 <chr [5]>  <formula> <spec[+]>  TRUE      
#>  5 circ  az_svg           5 <chr [6]>  <formula> <spec[+]>  TRUE      
#>  6 lin   log_wvg          1 <chr [2]>  <formula> <spec[+]>  TRUE      
#>  7 lin   log_wvg          2 <chr [3]>  <formula> <spec[+]>  TRUE      
#>  8 lin   log_wvg          3 <chr [4]>  <formula> <spec[+]>  TRUE      
#>  9 lin   log_wvg          4 <chr [5]>  <formula> <spec[+]>  TRUE      
#> 10 lin   log_wvg          5 <chr [6]>  <formula> <spec[+]>  TRUE

# Now run the analyses
final <- suppressWarnings(fire(a, geh))
#> Iteration  1 :    Log-Likelihood =  4.480036 
#> Iteration  2 :    Log-Likelihood =  4.664062 
#> Iteration  1 :    Log-Likelihood =  4.381976 
#> Iteration  2 :    Log-Likelihood =  6.208358 
#> Iteration  1 :    Log-Likelihood =  4.89604 
#> Iteration  2 :    Log-Likelihood =  6.018497 
#> Iteration  1 :    Log-Likelihood =  6.510346 
#> Iteration  2 :    Log-Likelihood =  7.50495 
#> Iteration  1 :    Log-Likelihood =  6.142007 
#> Iteration  2 :    Log-Likelihood =  9.783403 
#> Iteration  3 :    Log-Likelihood =  10.13032 
#> Iteration  4 :    Log-Likelihood =  10.20306

# Tidy them up and see what they look like
ballistics(final)
#> Aimed and fired. Model statistics have been added.
#> # A tibble: 10 x 10
#>    ammo  outcomes model_num predictors formulas model_spec checkpoint fit  
#>  * <chr> <chr>        <int> <list>     <list>   <list>     <lgl>      <lis>
#>  1 circ  az_svg           1 <chr [2]>  <formul… <spec[+]>  TRUE       <fit…
#>  2 circ  az_svg           2 <chr [3]>  <formul… <spec[+]>  TRUE       <fit…
#>  3 circ  az_svg           3 <chr [4]>  <formul… <spec[+]>  TRUE       <fit…
#>  4 circ  az_svg           4 <chr [5]>  <formul… <spec[+]>  TRUE       <fit…
#>  5 circ  az_svg           5 <chr [6]>  <formul… <spec[+]>  TRUE       <fit…
#>  6 lin   log_wvg          1 <chr [2]>  <formul… <spec[+]>  TRUE       <fit…
#>  7 lin   log_wvg          2 <chr [3]>  <formul… <spec[+]>  TRUE       <fit…
#>  8 lin   log_wvg          3 <chr [4]>  <formul… <spec[+]>  TRUE       <fit…
#>  9 lin   log_wvg          4 <chr [5]>  <formul… <spec[+]>  TRUE       <fit…
#> 10 lin   log_wvg          5 <chr [6]>  <formul… <spec[+]>  TRUE       <fit…
#> # … with 2 more variables: runtime <dbl>, tidy <list>
```
