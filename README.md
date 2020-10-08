
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis557

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/wuyinfeistella/bis555.svg?branch=master)](https://travis-ci.com/wuyinfeistella/bis555)
[![Codecov test
coverage](https://codecov.io/gh/wuyinfeistella/bis555/branch/master/graph/badge.svg)](https://codecov.io/gh/wuyinfeistella/bis555?branch=master)
<!-- badges: end -->

The goal of bis557 is to have two functions here: one for linear model
and another for gradient descent

## Installation

You can install the released version of bis557 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("bis557")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bis557)
## basic example code
data(iris)
#The example for function linear_model
linear_model(Sepal.Length ~ ., iris,contrasts = NULL)$coefficients
#>       (Intercept)       Sepal.Width      Petal.Length       Petal.Width 
#>         2.1712663         0.4958889         0.8292439        -0.3151552 
#> Speciesversicolor  Speciesvirginica 
#>        -0.7235620        -1.0234978
```

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
