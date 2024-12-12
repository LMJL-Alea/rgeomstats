
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgeomstats <a href="https://lmjl-alea.github.io/rgeomstats/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/LMJL-Alea/rgeomstats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LMJL-Alea/rgeomstats/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/LMJL-Alea/rgeomstats/workflows/test-coverage/badge.svg)](https://github.com/LMJL-Alea/rgeomstats/actions)
[![Codecov test
coverage](https://codecov.io/gh/LMJL-Alea/rgeomstats/graph/badge.svg)](https://app.codecov.io/gh/LMJL-Alea/rgeomstats)
[![pkgdown](https://github.com/LMJL-Alea/rgeomstats/workflows/pkgdown/badge.svg)](https://github.com/LMJL-Alea/rgeomstats/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/rgeomstats)](https://CRAN.R-project.org/package=rgeomstats)
<!-- badges: end -->

The goal of **rgeomstats** is to provide accessibility to the
[**Geomstats**](https://geomstats.github.io) Python library for the
community of `R` users through an R interface that mimics as closely as
possible the carefully designed Python API.

## Installation

You can install the development version of **rgeomstats** via:

``` r
# install.packages("remotes")
remotes::install_github("LMJL-Alea/rgeomstats")
```

## Example

You can instantiate the space $\mathrm{SO}(3)$ of 3D rotations and
sample random points in this space as follows:

``` r
library(rgeomstats)
so3 <- SpecialOrthogonal(n = 3)
spl <- so3$random_point(n_samples = 5)
dim(spl)
#> [1] 5 3 3
```

All Geomstats-like computations are stored in arrays. In particular,
sample IDs are always stored along the first dimension. Hence, it is
always possible to convert a sample into a list via:

``` r
purrr::array_tree(spl, margin = 1)
#> [[1]]
#>            [,1]       [,2]      [,3]
#> [1,] -0.7967935  0.3991562 0.4536457
#> [2,] -0.2129792 -0.8880954 0.4073407
#> [3,]  0.5654732  0.2279493 0.7926406
#> 
#> [[2]]
#>            [,1]       [,2]       [,3]
#> [1,] -0.1324655 -0.6554600 0.74352205
#> [2,]  0.9860750 -0.1632376 0.03177479
#> [3,]  0.1005436  0.7373775 0.66795608
#> 
#> [[3]]
#>               [,1]       [,2]       [,3]
#> [1,] -0.5261063800 -0.5436776 -0.6539318
#> [2,]  0.8504182853 -0.3371599 -0.4038712
#> [3,] -0.0009038893 -0.7685947  0.6397354
#> 
#> [[4]]
#>            [,1]       [,2]        [,3]
#> [1,] -0.2365716  0.9667568  0.09703191
#> [2,] -0.9494114 -0.2087826 -0.23457992
#> [3,] -0.2065232 -0.1476181  0.96724199
#> 
#> [[5]]
#>            [,1]       [,2]       [,3]
#> [1,] -0.6202049 -0.4575861 -0.6371506
#> [2,]  0.7467855 -0.5930449 -0.3010135
#> [3,] -0.2401193 -0.6625049  0.7095280
```
