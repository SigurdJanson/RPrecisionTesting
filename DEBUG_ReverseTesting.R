source("./ReverseTesting.R")

f1 <- function(x, y) x*y
f2 <- function(x, y) x/y
ArgsF1 <- "f1"
ArgsF2 <- "f2"
Args3 <- list(x = -4:4, y = -4:4)
LenExpected <- prod(unlist(lapply(Args3, length)))
Df <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 1)


#' eps
#' Distance from x to the next largest double-precision number.
#' @usage eps(x = 1.0)
#' @param x scalar or numerical vector or matrix
#' @details \code{d=eps(x)} is the positive distance from \code{abs(x)} 
#' to the next larger floating point number in double precision.
#' @value A scalar
#' @example eps(1^seq(-2, 10, 1))
#' If x is an array, eps(x) will return eps(max(abs(x))). 
eps <- function(x = 1.0) {
  stopifnot(is.numeric(x))
  
  x <- abs(x)
  WhichMin <- which(x <  .Machine$double.xmin)
  e <- 2^floor(log2(x)) * .Machine$double.eps
  e[WhichMin] <- .Machine$double.xmin
  e
}


eps.old <- function(x = 1.0) {
  x <- max(abs(x))
  if (x <  .Machine$double.xmin) {
    e <- .Machine$double.xmin
  } else {
    e <- 2^floor(log2(x)) * .Machine$double.eps
  }
  e  
}
