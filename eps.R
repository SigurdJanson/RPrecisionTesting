# FLOATING POINT PRECISION 

#' eps
#' Floating-point relative accuracy. The distance from x to the next 
#' largest double-precision number.
#' @usage eps(x = 1.0)
#' @param x scalar or numerical vector or matrix
#' @details \code{d=eps(x)} is the positive distance from \code{abs(x)} 
#' to the next larger floating point number in double precision.
#' 
#' On machines that support IEEE floating point arithmetic, eps is approximately 
#' 2.2204e-16 for double precision. That is also true for R.
#' @value A numeric vector
#' @seealso \code{\link{.Machine}}
#' @example eps(1^seq(-2, 10, 1))
#' If x is an array, eps(x) will return eps(max(abs(x))). 
eps <- function(x = 1.0) {
  stopifnot(is.numeric(x))
  
  x <- abs(x)
  WhichNA <- which(is.infinite(x) | is.na(x))
  WhichMin <- which(x <  .Machine$double.xmin)
  e <- 2^floor(log2(x)) * .Machine$double.eps
  e[WhichMin] <- .Machine$double.xmin
  e[WhichNA] <- NA
  e
}

