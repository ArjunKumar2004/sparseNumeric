## sparseNumeric.R

#' sparse_numeric class
#'
#' An S4 sparse vector storing only non-zero entries.
#'
#' @slot value numeric non-zero values (may include NA/NaN from operations)
#' @slot pos integer positions of values (1-based, strictly increasing)
#' @slot length integer total vector length
#'
#' @import methods
#' @exportClass sparse_numeric
setClass(
  Class = "sparse_numeric",
  slots = c(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  )
)

# Validity
setValidity("sparse_numeric", function(object) {
  v <- object@value; p <- object@pos; n <- object@length

  if (length(n) != 1L || !is.integer(n) || n < 0L)
    return("slot 'length' must be a single non-negative integer")

  if (length(v) != length(p))
    return("slots 'value' and 'pos' must have the same length")

  if (anyNA(p))
    return("slot 'pos' must not contain NA")

  if (any(p < 1L | p > n, na.rm = TRUE))
    return("slot 'pos' contains indices out of bounds [1, length]")

  if (length(p) > 1L && any(diff(p) <= 0L, na.rm = TRUE))
    return("'pos' must be strictly increasing and unique")

  if (any(v == 0, na.rm = TRUE))
    return("'value' must not contain zeros")

  TRUE
})

# Internal helpers
.same_len_or_stop <- function(x, y) {
  if (x@length != y@length)
    stop("arguments must have the same length", call. = FALSE)
}

.make_sparse <- function(val, pos, len) {
  if (length(val)) {
    keep <- is.na(val) | (val != 0)
    val <- val[keep]; pos <- pos[keep]

    if (length(pos)) {
      o <- order(pos)
      pos <- as.integer(pos[o])
      val <- as.numeric(val[o])
    } else {
      pos <- integer(0)
      val <- numeric(0)
    }
  } else {
    pos <- integer(0)
    val <- numeric(0)
  }

  new("sparse_numeric", value = val, pos = pos, length = as.integer(len))
}

.add_core <- function(x, y, sy = 1) {
  .same_len_or_stop(x, y)
  u <- sort(unique(c(x@pos, y@pos)))
  i1 <- match(u, x@pos); i2 <- match(u, y@pos)
  v  <- ifelse(!is.na(i1), x@value[i1], 0) + sy * ifelse(!is.na(i2), y@value[i2], 0)
  .make_sparse(v, u, x@length)
}

.mul_core <- function(x, y) {
  .same_len_or_stop(x, y)
  ov <- intersect(x@pos, y@pos)
  if (!length(ov)) {
    return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
  }
  xv <- x@value[match(ov, x@pos)]
  yv <- y@value[match(ov, y@pos)]
  .make_sparse(xv * yv, ov, x@length)
}

.dot_core <- function(x, y) {
  .same_len_or_stop(x, y)
  ov <- intersect(x@pos, y@pos)
  if (!length(ov)) return(0)
  xv <- x@value[match(ov, x@pos)]
  yv <- y@value[match(ov, y@pos)]
  sum(xv * yv)
}

# Coercions
setAs("numeric", "sparse_numeric", function(from) {
  n  <- length(from)
  if (!n) return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = 0L))
  nz <- which(from != 0)
  new("sparse_numeric",
      value  = as.numeric(from[nz]),
      pos    = as.integer(nz),
      length = as.integer(n))
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos)) out[from@pos] <- from@value
  out
})

# Generics
#' Add two sparse_numeric vectors
#' @param x,y sparse_numeric objects of same length
#' @param ... unused
#' @return sparse_numeric
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' Subtract two sparse_numeric vectors
#' @param x,y sparse_numeric objects of same length
#' @param ... unused
#' @return sparse_numeric
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' Multiply two sparse_numeric vectors elementwise
#' @param x,y sparse_numeric objects of same length
#' @param ... unused
#' @return sparse_numeric
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' Dot product of two sparse_numeric vectors
#' @param x,y sparse_numeric objects of same length
#' @param ... unused
#' @return numeric scalar
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

# Methods for sparse_numeric × sparse_numeric
#' @export
setMethod("sparse_add", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) .add_core(x, y, sy = 1))

#' @export
setMethod("sparse_sub", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) .add_core(x, y, sy = -1))

#' @export
setMethod("sparse_mult", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) .mul_core(x, y))

#' @export
setMethod("sparse_crossprod", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) .dot_core(x, y))

# Explicit operator methods required by tests
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

# show()
setMethod("show", "sparse_numeric", function(object) {
  k <- length(object@pos)
  cat(sprintf("sparse_numeric (length=%d, nnz=%d)\n", object@length, k))
  if (!k) { cat("  <all zeros>\n"); return(invisible(object)) }
  idx <- if (k <= 8) seq_len(k) else c(1:4, NA, (k-3):k)
  cat("  entries: ")
  for (i in idx) {
    if (is.na(i)) { cat(" ... "); next }
    cat(sprintf("[%d]=%g", object@pos[i], object@value[i]))
    if (!identical(i, tail(idx, 1L))) cat(", ")
  }
  cat("\n"); invisible(object)
})

# plot(): overlapping non-zero values x vs y
setMethod("plot", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .same_len_or_stop(x, y)
            ov <- intersect(x@pos, y@pos)
            if (!length(ov)) {
              plot(0, 0, type = "n",
                   xlab = "x (overlap values)", ylab = "y (overlap values)",
                   main = "sparse_numeric overlap: none")
              return(invisible(NULL))
            }
            xv <- x@value[match(ov, x@pos)]
            yv <- y@value[match(ov, y@pos)]
            plot(xv, yv,
                 xlab = "x (overlap values)", ylab = "y (overlap values)",
                 main = sprintf("Overlap on %d positions", length(ov)),
                 ...)
            abline(h = 0, v = 0, lty = 3)
            invisible(NULL)
          })

# mean()
#' Mean of a sparse_numeric vector (including zeros)
#'
#' @param x sparse_numeric
#' @param ... unused
#' @return numeric scalar
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  n <- x@length
  if (n == 0L) return(NaN)
  sum(x@value) / n
})

# norm()
#' Euclidean norm of a sparse_numeric vector
#'
#' @param x sparse_numeric
#' @param ... unused
#' @return numeric scalar
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
  sqrt(sum(x@value^2))
})

# standardize()
#' Standardize a sparse_numeric vector
#'
#' Each element is transformed to (x - mean(x)) / sd(x),
#' where mean and sd include implicit zeros.
#'
#' @param x sparse_numeric
#' @param ... unused
#' @return sparse_numeric
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @export
setMethod("standardize", "sparse_numeric", function(x, ...) {
  n <- x@length
  if (n == 0L) {
    return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = 0L))
  }

  mu <- sum(x@value) / n

  if (n == 1L) {
    val_all <- rep(NA_real_, 1L)
    return(.make_sparse(val_all, 1L, n))
  }

  sum_sq <- sum(x@value^2)
  var_s <- (sum_sq - n * mu^2) / (n - 1L)
  s <- sqrt(var_s)

  if (is.na(s) || s == 0) {
    val_all <- rep(NaN, n)
    return(.make_sparse(val_all, seq_len(n), n))
  }

  c0 <- (0 - mu) / s
  val_all <- rep(c0, n)
  if (length(x@pos)) {
    val_all[x@pos] <- (x@value - mu) / s
  }
  .make_sparse(val_all, seq_len(n), n)
})

