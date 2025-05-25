
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biasTools

<!-- badges: start -->
<!-- badges: end -->

BiasTools is developed to help me (and other people maybe in the futere)
to perform publication bias analysis automaticcally

## Installation

You can install the development version of biasTools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mberlino/biasTools")
```

## Main function

- `bias.analysis()` – perform Egger-like publication bias analysis
  across all levels of a selected moderator within a specific realm

## Example

This is a basic example which shows you how to solve a common problem:

``` r

``` r
library(biasTools)

# Basic example:
dat$row_id <- 1:nrow(dat)
result <- bias.analysis(dat, "freshwater", "habitat", id_col = "row_id")

# Output examples:
#   Moderator   Habitat    n      g   CI_95_before  z_value  p_value  Outliers  n_after  g_after  CI_95_after  Change
#   Habitat     lake       120  0.21      0.13         2.91     0.004     5         115      0.19     0.12         No
#   Habitat     stream     300 -0.02      0.11         -0.45    0.65      8         292     -0.01     0.10         No

dat_clean <- dat[!(dat$row_id %in% res$outliers), ]

## basic example code
```
