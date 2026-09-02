# fars

<!-- badges: start -->
[![Build Status](https://travis-ci.org/YOUR_GITHUB_USERNAME/fars.svg?branch=master)](https://travis-ci.org/YOUR_GITHUB_USERNAME/fars)
<!-- badges: end -->

## Overview

`fars` is an R package for reading, summarizing, and mapping data from the
US National Highway Traffic Safety Administration's Fatality Analysis
Reporting System (FARS).

## Installation

```r
# install.packages("devtools")
devtools::install_github("YOUR_GITHUB_USERNAME/fars")
```

## Usage

See `vignette("fars_vignette")` for a full walkthrough of the package's
functions:

- `fars_read()`
- `make_filename()`
- `fars_read_years()`
- `fars_summarize_years()`
- `fars_map_state()`

## Tests

Run the package's test suite with:

```r
devtools::test()
```
