library(testthat)

test_that("validity method exists and works for good objects", {
  validity_method <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity_method))

  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("validity catches common errors", {
  expect_error(
    new("sparse_numeric", value = 1, pos = 1L, length = -1L),
    "non-negative"
  )

  expect_error(
    new("sparse_numeric", value = c(1,2), pos = 1L, length = 3L),
    "same length"
  )

  expect_error(
    new("sparse_numeric", value = 1, pos = 4L, length = 3L),
    "out of bounds"
  )

  expect_error(
    new("sparse_numeric", value = c(1,2), pos = c(2L,2L), length = 3L),
    "strictly increasing"
  )

  expect_error(
    new("sparse_numeric", value = c(1,0.0), pos = c(1L,2L), length = 2L),
    "must not contain zeros"
  )

  expect_error(
    new("sparse_numeric", value = 1, pos = NA_integer_, length = 3L),
    "must not contain NA"
  )
})

test_that("coercion numeric -> sparse_numeric -> numeric round trips", {
  v <- c(0, 0, 5, 0, -2)
  x <- as(v, "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
  expect_equal(as(x, "numeric"), v)

  z <- as(numeric(0), "sparse_numeric")
  expect_equal(z@length, 0L)
  expect_equal(length(z@pos), 0L)

  allz <- as(rep(0, 4), "sparse_numeric")
  expect_equal(length(allz@pos), 0L)
  expect_equal(length(allz@value), 0L)
})

test_that("generics and operator methods exist", {
  expect_true(isGeneric("sparse_add"))
  expect_true(isGeneric("sparse_sub"))
  expect_true(isGeneric("sparse_mult"))
  expect_true(isGeneric("sparse_crossprod"))
  expect_true(isGeneric("norm"))
  expect_true(isGeneric("standardize"))

  expect_no_error(getMethod("+", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("-", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("*", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("show", "sparse_numeric"))
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
})

test_that("sparse_add matches dense addition", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")

  res <- sparse_add(x, y)
  expect_s4_class(res, "sparse_numeric")
  expect_equal(as(res, "numeric"), c(1,1,0,1,6))

  x2 <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
  y2 <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
  expect_equal(as(sparse_add(x2, y2), "numeric"), c(2,4,6,10,12))
})

test_that("addition cancellation removes zeros from storage", {
  x <- as(c(0, 5, 0, 0), "sparse_numeric")
  y <- as(c(0, -5, 0, 0), "sparse_numeric")
  z <- sparse_add(x, y)

  expect_equal(as(z, "numeric"), c(0,0,0,0))
  expect_equal(length(z@pos), 0L)
  expect_equal(length(z@value), 0L)
})

test_that("sparse_sub and sparse_mult behave correctly", {
  x <- as(c(0, 2, 0, 3), "sparse_numeric")
  y <- as(c(1, 0, 0, 5), "sparse_numeric")

  expect_equal(as(sparse_sub(x, y), "numeric"), c(-1,2,0,-2))
  expect_equal(as(sparse_mult(x, y), "numeric"), c(0,0,0,15))

  a <- as(c(1,0,0,0), "sparse_numeric")
  b <- as(c(0,0,0,2), "sparse_numeric")
  expect_equal(as(sparse_mult(a, b), "numeric"), c(0,0,0,0))
})

test_that("multiplication can drop to all zeros via overlap", {
  x <- as(c(0, 2, 0, 3), "sparse_numeric")
  y <- as(c(0, 0, 0, 0), "sparse_numeric")
  z <- sparse_mult(x, y)

  expect_equal(as(z, "numeric"), c(0,0,0,0))
  expect_equal(length(z@pos), 0L)
})

test_that("sparse_crossprod equals dense dot product", {
  x <- as(c(0, 2, 0, 3), "sparse_numeric")
  y <- as(c(1, 0, 0, 5), "sparse_numeric")

  expect_equal(sparse_crossprod(x, y), 15)

  a <- as(c(1,0,0), "sparse_numeric")
  b <- as(c(0,2,0), "sparse_numeric")
  expect_equal(sparse_crossprod(a, b), 0)
})

test_that("mean includes zeros and works for edge cases", {
  x <- as(c(0, 0, 5, 0, -2), "sparse_numeric")
  expect_equal(mean(x), (5 + (-2)) / 5)

  z <- as(numeric(0), "sparse_numeric")
  expect_true(is.nan(mean(z)))
})

test_that("mean propagates NA values in storage", {
  o <- as(c(7), "sparse_numeric")
  so <- standardize(o)
  expect_true(is.na(mean(so)))
})

test_that("norm returns euclidean norm including zeros", {
  x <- as(c(0, 0, 5, 0, -2), "sparse_numeric")
  expect_equal(norm(x), sqrt(25 + 4))

  z <- as(rep(0, 5), "sparse_numeric")
  expect_equal(norm(z), 0)
})

test_that("standardize matches dense scale() result", {
  v <- c(0, 0, 5, 0, -2)
  x <- as(v, "sparse_numeric")

  sx <- standardize(x)
  expect_s4_class(sx, "sparse_numeric")
  expect_equal(as(sx, "numeric"), as.numeric(scale(v)))
  expect_true(length(sx@pos) >= length(x@pos))
})

test_that("standardize handles special cases", {
  z <- as(numeric(0), "sparse_numeric")
  sz <- standardize(z)
  expect_equal(sz@length, 0L)
  expect_equal(length(sz@pos), 0L)

  o <- as(c(7), "sparse_numeric")
  so <- standardize(o)
  expect_true(is.na(as(so, "numeric")[1]))

  cst <- as(c(3,3,3,3), "sparse_numeric")
  scst <- standardize(cst)
  expect_true(all(is.nan(as(scst, "numeric"))))
})

test_that("standardize works with negatives and non-integers", {
  v <- c(-1.5, 0, 2.25, 0, 4.5)
  x <- as(v, "sparse_numeric")
  sx <- standardize(x)
  expect_equal(as(sx, "numeric"), as.numeric(scale(v)))
})

test_that("show handles all-zero and long nnz cases", {
  allz <- as(rep(0, 6), "sparse_numeric")
  out0 <- capture.output(show(allz))
  expect_true(any(grepl("<all zeros>", out0)))

  v <- c(1,2,3,4,5,6,7,8,9,10)
  x <- as(v, "sparse_numeric")
  out1 <- capture.output(show(x))
  expect_true(any(grepl("sparse_numeric", out1)))
  expect_true(any(grepl("entries:", out1)))
  expect_true(any(grepl("\\.\\.\\.", out1)))  # hits the ellipsis branch
})

test_that("plot handles overlap and no-overlap branches", {
  x <- as(c(0, 2, 0, 0, 3), "sparse_numeric")
  y <- as(c(0, 0, 0, 4, 0), "sparse_numeric")  # no overlap
  expect_silent(plot(x, y))

  y2 <- as(c(0, 5, 0, 0, -1), "sparse_numeric") # overlap at pos 2 and 5
  expect_silent(plot(x, y2))
})

test_that("length mismatch triggers error in cores", {
  x <- as(rep(0, 10), "sparse_numeric")
  y <- as(rep(0, 9), "sparse_numeric")

  expect_error(sparse_add(x, y))
  expect_error(sparse_sub(x, y))
  expect_error(sparse_mult(x, y))
  expect_error(sparse_crossprod(x, y))
})
