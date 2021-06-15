
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AnlysisOfWaves

<!-- badges: start -->

<!-- badges: end -->

The goal of AnlysisOfWaves is to easily reproduce the analysis of Ca2+
waves in the publication …

## Installation

This package is only available in this git repository. You can install
it from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rjlopez2/AnlysisOfWaves")
```

## Example

This is a basic example which shows you how plot the cumulative
occurrence of Ca2+ waves obtained from linescan images:

``` r
library(AnlysisOfWaves)

time_threshold <- 10 # Set time for highlighting the relative wave occurrence.

df40_o %>% 
  my_cum_occu_wave_plot_func(time_threshold)
#> $cum_occu_plot
```

<img src="man/figures/README-example-1.png" width="600px" />
