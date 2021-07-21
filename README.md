
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aims

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Github commit
frequency](https://img.shields.io/github/commit-activity/w/asshah4/aims)](https://github.com/asshah4/aims/graphs/commit-activity)
[![R-CMD-check](https://github.com/asshah4/aims/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/aims/actions)

<!-- badges: end -->

## Usage

The package is simple to use. First, lets load the basic packages. The
two hypotheses in this case are a linear model, and a paired t-test. The
`iris` dataset will serve as the example.

You can see that `parsnip` is used, as this allows **many** model
specifications to be called in a very similar manner, making modeling
much easier.

``` r
library(aims)
library(parsnip)

aim <- 
    project() %>%
    set_data(iris) 

# Print
print(aim)
#> # A tibble: 1 x 3
#>   data  hypothesis findings
#>   <chr>      <int>    <int>
#> 1 iris           0        0
```

This simply shows that there are not yet any hypotheses that have been
tested yet.

``` r
# Adding tests
aim <-
    aim %>%
    add_hypothesis(
        name = "sepals",
        formula = Sepal.Length ~ Sepal.Width,
        test = "t.test",
        paired = TRUE
    ) %>%
    add_hypothesis(
        name = "linear",
        formula = Petal.Length ~ Sepal.Length + Sepal.Width,
        combination = "sequential",
        test = linear_reg() %>% set_engine("lm"),
        .strata = "Species"
    )
#> Using the only available data set.
#> Using the only available data set.

# Show our new hypothesis
print(aim)
#> # A tibble: 4 x 8
#>   data  name   type       outcomes     number run   split level     
#>   <chr> <chr>  <chr>      <chr>         <int> <lgl> <lgl> <fct>     
#> 1 iris  sepals htest      Sepal.Length      1 FALSE FALSE <NA>      
#> 2 iris  linear model_spec Petal.Length      2 FALSE TRUE  setosa    
#> 3 iris  linear model_spec Petal.Length      2 FALSE TRUE  versicolor
#> 4 iris  linear model_spec Petal.Length      2 FALSE TRUE  virginica
```

This helps to identify how the tests were set up, and what have been
run. It serves as a quick summary of the analysis.

``` r
# Running the tests
aim <-
    aim %>%
    build_models()

# Shows which analyses have been run
print(aim)
#> # A tibble: 4 x 8
#>   data  name   type       outcomes     number run   split level     
#>   <chr> <chr>  <chr>      <chr>         <int> <lgl> <lgl> <fct>     
#> 1 iris  sepals htest      Sepal.Length      1 TRUE  FALSE <NA>      
#> 2 iris  linear model_spec Petal.Length      2 TRUE  TRUE  setosa    
#> 3 iris  linear model_spec Petal.Length      2 TRUE  TRUE  versicolor
#> 4 iris  linear model_spec Petal.Length      2 TRUE  TRUE  virginica
```

This now has models that have been run, along with how the data was
stratified if indicated. These can then be pulled in a tidy format (or a
raw model if need be).

``` r
# Show the findings in a tidy format
aim %>% collect_findings()
#> # A tibble: 16 x 14
#>    name   number outcomes     estimate statistic  p.value parameter conf.low
#>    <chr>   <int> <chr>           <dbl>     <dbl>    <dbl>     <dbl>    <dbl>
#>  1 sepals      1 Sepal.Length   2.79     34.8    1.85e-73       149  2.63   
#>  2 linear      1 Petal.Length   0.803     2.34   2.38e- 2        NA  0.112  
#>  3 linear      1 Petal.Length   0.132     1.92   6.07e- 2        NA -0.00615
#>  4 linear      1 Petal.Length   0.185     0.360  7.20e- 1        NA -0.849  
#>  5 linear      1 Petal.Length   0.686     7.95   2.59e-10        NA  0.513  
#>  6 linear      1 Petal.Length   0.610     1.46   1.50e- 1        NA -0.228  
#>  7 linear      1 Petal.Length   0.750    11.9    6.30e-16        NA  0.623  
#>  8 linear      2 Petal.Length   0.791     2.25   2.92e- 2        NA  0.0836 
#>  9 linear      2 Petal.Length   0.149     1.44   1.57e- 1        NA -0.0594 
#> 10 linear      2 Petal.Length  -0.0211   -0.220  8.27e- 1        NA -0.214  
#> 11 linear      2 Petal.Length  -0.111    -0.215  8.31e- 1        NA -1.15   
#> 12 linear      2 Petal.Length   0.578     5.90   3.87e- 7        NA  0.381  
#> 13 linear      2 Petal.Length   0.339     2.10   4.07e- 2        NA  0.0150 
#> 14 linear      2 Petal.Length   0.592     1.26   2.12e- 1        NA -0.350  
#> 15 linear      2 Petal.Length   0.747    10.4    8.01e-14        NA  0.603  
#> 16 linear      2 Petal.Length   0.0128    0.0904 9.28e- 1        NA -0.271  
#> # … with 6 more variables: conf.high <dbl>, method <chr>, alternative <chr>,
#> #   term <chr>, std.error <dbl>, level <fct>

# Show the findings with raw models (if needed)
aim %>% collect_findings(tidy = FALSE)
#> Only returning raw models.
#> # A tibble: 6 x 5
#>   name   number level      outcomes     fit   
#>   <chr>   <int> <fct>      <chr>        <list>
#> 1 linear      1 setosa     Petal.Length <lm>  
#> 2 linear      1 versicolor Petal.Length <lm>  
#> 3 linear      1 virginica  Petal.Length <lm>  
#> 4 linear      2 setosa     Petal.Length <lm>  
#> 5 linear      2 versicolor Petal.Length <lm>  
#> 6 linear      2 virginica  Petal.Length <lm>
```
