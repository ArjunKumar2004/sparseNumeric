
<!-- README.md is generated from README.Rmd. Please edit that file -->

\##sparseNumeric <!-- badges: start -->

[![R-CMD-check](https://github.com/ArjunKumar2004/sparseNumeric/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ArjunKumar2004/sparseNumeric/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

sparseNumeric provides an S4 class, sparse_numeric, for representing
numeric vectors in sparse form. Only nonzero entries are stored, along
with their positions and the full vector length. The package includes
methods for common vector operations without converting to a dense
vector, including:

addition, subtraction, and elementwise multiplication

dot products via sparse_crossprod()

mean() including implicit zeros

norm() (Euclidean norm)

standardize() using mean and sample standard deviation including zeros

This package was built for coursework and lightweight sparse
computations.

\##Installation

You can install the development version from GitHub:

# install.packages(“devtools”)

devtools::install_github(“<your-github-username>/sparseNumeric”)

Then load it:

``` r
library(sparseNumeric)
#> 
#> Attaching package: 'sparseNumeric'
#> The following object is masked from 'package:base':
#> 
#>     norm
```

Example usage

Create a sparse vector by coercing from numeric:

Writing

``` r
x <- as(c(0, 0, 5, 0, -2), "sparse_numeric")
x
#> sparse_numeric (length=5, nnz=2)
#>   entries: [3]=5, [5]=-2
```

Coerce back to dense when you want to view values:

Writing

``` r
as(x, "numeric")
#> [1]  0  0  5  0 -2
```

## Basic arithmetic

Writing

``` r
y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")

sparse_add(x, y)
#> sparse_numeric (length=5, nnz=4)
#>   entries: [1]=1, [2]=1, [3]=5, [5]=2
sparse_sub(x, y)
#> sparse_numeric (length=5, nnz=4)
#>   entries: [1]=-1, [2]=-1, [3]=5, [5]=-6
sparse_mult(x, y)
#> sparse_numeric (length=5, nnz=1)
#>   entries: [5]=-8
```

Operators +, -, and \* work too:

Writing

``` r
x + y
#> sparse_numeric (length=5, nnz=4)
#>   entries: [1]=1, [2]=1, [3]=5, [5]=2
x - y
#> sparse_numeric (length=5, nnz=4)
#>   entries: [1]=-1, [2]=-1, [3]=5, [5]=-6
x * y
#> sparse_numeric (length=5, nnz=1)
#>   entries: [5]=-8
```

## Dot product

Writing

`{r{}} sparse_crossprod(x, y)`

## Mean and norm

Both include zeros without densifying:

Writing

``` r
mean(x)
#> [1] 0.6
norm(x)
#> [1] 5.385165
```

## Standardization

standardize() returns a sparse vector where each element is (xi​−xˉ)/sx
using the mean and sample standard deviation including implicit zeros.

Writing

``` r
sx <- standardize(x)
sx
#> sparse_numeric (length=5, nnz=5)
#>   entries: [1]=-0.230089, [2]=-0.230089, [3]=1.68732, [4]=-0.230089, [5]=-0.997054
as(sx, "numeric")
#> [1] -0.2300895 -0.2300895  1.6873230 -0.2300895 -0.9970545
```

## Plotting overlap

The plot method compares overlapping nonzero positions:

Writing

``` r
x2 <- as(c(0, 2, 0, 0, 3), "sparse_numeric")
y2 <- as(c(0, 5, 0, 0, -1), "sparse_numeric")
plot(x2, y2)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

## Notes

The internal representation always keeps positions strictly increasing
and never stores explicit zeros.

Some operations (like standardization) can increase the number of stored
entries, because standardized zeros become nonzero values.

## License

MIT license. See LICENSE for details.
