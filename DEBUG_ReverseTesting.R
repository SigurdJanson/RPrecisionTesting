source("./ReverseTesting.R")


# Uncomment one line and source this to debug ----
#Result <- ReversionTest("pnorm", "qnorm", ToIterate = list(c(-0.5, -0.25, 0, 0.25, 0.5), mean = -4:4), sd = 1)
#Result <- ReversionTest("qlogitnorm", "plogitnorm", ToIterate =  list(mean = -4:4, sd=))
#Result <- ReversionTest("pnorm", "qnorm", ToIterate =  list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9)), KeyVar = 3)
#Result <- ReversionTest("pnorm", "qnorm", ToIterate =  list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9)), KeyVar = 3, DiffFunc = sqrt)#function(x, y) y/x)


# f1 <- function(x, y) x*y # * and / actions are relatively safe when it comes to floating point "drift"
# f2 <- function(x, y) x/y # * and / actions are relatively safe
# ArgsF1 <- "f1"
# ArgsF2 <- "f2"
# Args3 <- list(x = c(-4:(-1), 1:4), y = c(-4:(-1), 1:4))
# LenExpected <- prod(unlist(lapply(Args3, length)))
# Df <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 1)

# Values should be close to zero with subtraction as delta-function
#Result <- unlist(Df$Delta)
#Result, rep(FALSE, LenExpected)


# x <- 10 + c(1e-11, 1e-10, 1e-9, 1e-8, 1e-7)
# y <- 10 + rep(0, length(x))
# Result <- .DeltaEps(x, y) == 0
# 
# x <- runif(1000, -20, 20)
# y <- x + runif(1000, -0.1, 0.1)
# Result <- .DeltaEps(x, y) 
# Result2 <- .NearlyEqual(x, y)

# x <- runif(1000, -0.01, 0.01)
# y <- x + runif(1000, -1e-10, 1e-10)
# Result <- .DeltaEps(x, y)
# .DeltaEps(c(0,1,1), c(1,0,1), eps = 1e-10) 


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
