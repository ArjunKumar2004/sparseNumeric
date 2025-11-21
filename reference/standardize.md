# Standardize a sparse_numeric vector

Each element is transformed to (x - mean(x)) / sd(x), where mean and sd
include implicit zeros.

## Usage

``` r
standardize(x, ...)
```

## Arguments

- x:

  sparse_numeric

- ...:

  unused

## Value

sparse_numeric
