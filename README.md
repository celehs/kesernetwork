
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

Install development version from GitHub:

``` r
install.packages("remotes")
remotes::install_github("celehs/kesernetwork")
```

## Usage

See the [getting started
guide](https://celehs.github.io/kesernetwork/articles/main.html) to
learn how to use kesernetwork.

## Citations

  - Chuan Hong, Everett Rush, Molei Liu, Doudou Zhou, Jiehuan Sun, Aaron
    Sonabend, et al. Clinical Knowledge Extraction via Sparse Embedding
    Regression (KESER) with Multi-Center Large Scale Electronic Health
    Record Data. medRxiv 2021.03.13.21253486; doi:
    <https://doi.org/10.1101/2021.03.13.21253486>
