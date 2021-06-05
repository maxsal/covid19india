
# covid19india

<!-- badges: start -->
![lifecycle](https://raw.githubusercontent.com/maxsal/covid19india/main/man/figures/lifecycle-experimental.svg)
<!-- badges: end -->

`covid19india` is designed while working with data on COVID-19 in India. [covid19india.org](https://covid19india.org) is 
a data resource with APIs available. It is updated daily and provides count, testing, and vaccine data 
at the national, state, and district level. This package eases the process for R users to obtain data 
ready for analysis in hopes of democratizing data science and speed up public health research.

:warning: **The package is under active development.** :warning: After creating functions for pulling data,
additional functions to calculate *basic* public health metrics and data visualizations will be added.

Please share thoughts and comments with me [@mmsalva@umich.edu](mailto:mmsalva@umich.edu) or [@MaxSalTweets](twitter.com/MaxSalTweets)

## Installation

You can install the development version of `covid19india` with:

``` r
remotes::install_github("maxsal/covid19india")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# remotes::install_github("maxsal/covid19india")
library(covid19india)

# load data from covid19india.org -----------
nat_count   <- get_nat_counts()
state_count <- get_state_counts()
state_tests <- get_state_tests()

# estimate r0 ----------
nat_count %>% get_r0()
state_count %>% get_r0()

```

