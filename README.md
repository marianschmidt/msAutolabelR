
<!-- README.md is generated from README.Rmd. Please edit that file -->

# msAutolabelleR

<!-- badges: start -->

<!-- badges: end -->

The goal of msAutolabelleR is to automate labelling introduced by the
sjlabelled package, which provides very helpful functions on labelling
dataframes. This package provides helper functions to automatically
label existing dataframes using an external Excel-Table that defines
variable labels and formats.

## Installation

You can install the released version of msAutolabelleR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("msAutolabelleR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("marianschmidt/msAutolabelleR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(msAutolabelleR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
