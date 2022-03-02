
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kesernetwork

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The kesernetwork builds a shiny app to visualize the knowledge networks
for the code concepts. Using co-occurrence matrices of EHR codes from
Veterans Affairs (VA) and Massachusetts General Brigham (MGB), the
knowledge extraction via sparse embedding regression (KESER) algorithm
was used to construct knowledge networks for the code concepts.

## Installation

Install the released version of kesernetwork from CRAN:

``` r
install.packages("kesernetwork")
```

Or install the development version from GitHub with:

``` r
install.packages("remotes")
remotes::install_github("celehs/kesernetwork")
```

## Usage

This is a basic example which shows you how to run the `kesernetwork`
app. Remember you need to get access to the data and save it to your
local computer. In order to guarantee some dependencies are loaded, you
must use `library(kesernetwork)` beforehand, instead of directly running
`kesernetwork::run_app()`.

``` r
library(kesernetwork)
run_app(Rdata_path = "path/to/kesernetwork.RData")
```

See the [getting started
guide](https://celehs.github.io/kesernetwork/articles/main.html) to
learn how to use kesernetwork.

## Citations

  - Hong, C., Rush, E., Liu, M. et al. Clinical knowledge extraction via
    sparse embedding regression (KESER) with multi-center large scale
    electronic health record data. npj Digit. Med. 4, 151 (2021).
    <https://doi.org/10.1038/s41746-021-00519-z>
