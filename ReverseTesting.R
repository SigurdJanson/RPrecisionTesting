
# References
# https://www.r-bloggers.com/numerical-pitfalls-in-computing-variance/
# R Inferno
# https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
# https://softwareengineering.stackexchange.com/questions/202843/solutions-for-floating-point-rounding-errors
# https://docs.python.org/3/tutorial/floatingpoint.html
# * https://www.phys.uconn.edu/~rozman/Courses/P2200_15F/downloads/floating-point-guide-2015-10-15.pdf
# * https://www.volkerschatz.com/science/float.html
# 0.1000000000000000055511151231257827021181583404541015625 == 0.1 ==> TRUE
# 0.1 + 0.2 == 0.3 ==> FALSE
# (0.1 + 0.2) - 0.3 ==>  5.551115e-17
#
# Further work probably on speed improvements?
# https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf


# FLOATING POINT PRECISION ----

#' eps
#' Distance from x to the next largest double-precision number.
#' @usage eps(x = 1.0)
#' @param x scalar or numerical vector or matrix
#' @details \code{d=eps(x)} is the positive distance from \code{abs(x)} 
#' to the next larger floating point number in double precision.
#' @value A numeric vector
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



# DELTA FUNCTIONS ----

#' .DeltaSub
#' @author Jan Seifert
#' @describeIn .NearlyEqual
.DeltaSub <- function( x, y ) abs(x - y)

#' .DeltaRatio
#' @author Jan Seifert
#' @describeIn .NearlyEqual
.DeltaRatio <- function( x, y ) abs(x - y)/x


#' .NearlyEqual
#' Functions to determine whether two values can be treated as equal.
#' @param x,y Numbers to be compared
#' @param eps Acceptable margin of error. Default is 1e-10.
#' @details .DeltaSub may be misleading in many cases. Problems with floating
#' point accuracy are relative to the range of the numbers. And a delta of 0.01
#' is far more troublesome when x = 0.1 compared to an x = 1000.
#' @note .NearlyEqual passes tests for many important special cases, but it also 
#' uses some non-obvious logic. (1) it has to use a completely different 
#' definition of error margin when x or y is zero, because the classical 
#' definition of relative error becomes meaningless in those cases. (2) There are 
#' some cases where the method above still produces 
#' unexpected results (in particular, it’s much stricter when one value is 
#' nearly zero than when it is exactly zero), and (3) some of the tests it was 
#' developed to pass probably specify behaviour that is not appropriate for 
#' some applications. Before using it, make sure it’s appropriate for your application!
#' @value .NearlyEqual returns TRUE or FALSE.
#' .DeltaSub gives the absolute difference between the values, .DeltaRatio 
#' the absolute value of x/y.
#' @author Michael G. Rozman, moved to R and made vector compatible to R by Jan Seifert
#' @references Rozman, M. Z. (2015) [What Every Programmer Should Know About Floating-Point Arithmetic](https://www.phys.uconn.edu/~rozman/Courses/P2200_15F/downloads/floating-point-guide-2015-10-15.pdf); accessed 2020-01-06
.NearlyEqual <- function(x, y, eps = 1e-10) {
  X.Abs <- abs(x)
  Y.Abs <- abs(y)
  Diff = abs(x - y)
  
  # Use relative error, by default
  Result <- Diff / (X.Abs + Y.Abs) < eps
  # Correct, where x or x_ is zero or both are extremely close to it
  # Relative error is less meaningful here
  Which <- which(x == 0 || y == 0 || Diff < .Machine$double.xmin)
  Result[Which] <- (Diff[Which] < eps * .Machine$double.xmin)
  # Handles infinities
  Result[which(x == y)] <- TRUE
  Result
}


#' .DeltaEps    ############TODO############ Not yet finished
#' @describeIn .NearlyEqual Quantify difference when |x-y| > eps (unlike .NearlyEqual 
#' that gives only TRUE/FALSE).
.DeltaEps <- function(x, y, eps = 1e-10) {
  X.Abs <- abs(x)
  Y.Abs <- abs(y)
  Diff = abs(x - y)
  
  # Use relative error, by default
  Which <- which( Diff / (X.Abs + Y.Abs) < eps )
  Result[Which] <- 0
  Result[-Which] <- Diff / (X.Abs + Y.Abs)
  
  # Correct, where x or y is zero or both are extremely close to it
  # Relative error is less meaningful here
  Which <- which(x == 0 || y == 0 || Diff < .Machine$double.xmin)
  Result[Which] <- (Diff[Which] < eps * .Machine$double.xmin)

  # Handles infinities
  Result[which(x == y)] <- 0
  Result
}


# REVERSION TEST  ----

#' ReversionTest
#' Tests the precision of two reciprocal functions. It evaluates the differences between 
#' a computed value \eqn{x'} and an expected value \eqn{x} after \eqn{x' = finv(f(x))}.
#' @param f,finv The functions to be tested for precision with finv the inverse function for f.
#' @param ToIterate a list of vectors, on for each variable.
#' @param KeyVar The variable in ToIterate for which the test is for. Either an list index 
#' or a name as string (partial strings allowed). 
#' @param ...  additional arguments to be passed to f and finv. 
#' @param DiffFunc A function to quantify the difference between expected value and computed value. 
#' If Null, ReversionTest will use the absolute ratio.
#' @details Each list element of \code{ToIterate} is a vector that contains the values 
#' that shall be used for the tests. Each list element must be named after the argument 
#' that will be passed on to f and finv. 
#' @example TotalResult <- ReversionTest("qnorm", "pnorm", ToIterate=list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9)), KeyVar = 3)
ReversionTest <- function(f, finv, ToIterate = NULL, KeyVar = 1, DiffFunc, ...) {
  .forwardreverse <- function(x, ...) {
    ## example: do.call("dnorm", list(-1:1, sd = -2, mean = 0))
    Args.Forward <- append(x, list(...))
    Result <- do.call(f, Args.Forward)
    Args.Backward <- x
    Args.Backward[KeyVar] <- Result
    Args.Backward <- append(Args.Backward, list(...)) 
    Result <- do.call(finv, Args.Backward)
    return(Result)
  }

  # PRECONDITIONS
  if(!is.character(f) || !is.character(finv)) stop("Need function name for 'f' and 'finv' as string")
  f <- match.fun(f)
  finv <- match.fun(finv)
  if(!is.list(ToIterate)) stop("List expected for 'ToIterate'")
  if(length(ToIterate) < 1) stop("Nothing to iterate through")
  if(!all(unlist(lapply(ToIterate, is.atomic )))) stop("'ToIterate' must contain only atomic numeric vectors")
  if(!all(unlist(lapply(ToIterate, is.numeric)))) stop("'ToIterate' must contain only atomic numeric vectors")
  if(is.numeric(KeyVar)) {
    if(KeyVar <= 0) stop("Key variable must be 1 or greater")
    if(KeyVar > length(ToIterate)) stop("Index of key variable out of bounds")
  } else {
    KeyVar <- pmatch(KeyVar, names(ToIterate))
    if(is.na(KeyVar)) stop("Key variable not found in ToIterate")
  }

  # Fix order in case of unnamed arguments  
  ArgNameLen <- nchar(names(ToIterate))
  if(any(ArgNameLen > 0)) { # there are unnamed arguments
    # Better safe than sorry
    if(sum(ArgNameLen == 0) > 1) stop("This function can handle only a single unnamed argument")
    # An unnamed variable must come first
    OldOrder <- names(ToIterate)
    ToIterate <- ToIterate[c(which(ArgNameLen == 0), which(ArgNameLen != 0))]
    NewOrder <- names(ToIterate)
    # Correct index of KeyVar
    KeyVar <- which(names(NewOrder) == names(OldOrder[KeyVar]))
    if(length(KeyVar) == 0L) KeyVar <- 1 # Correction because 'which' does not catch empty strings
  }
  if(missing(DiffFunc)) {
    delta <- match.fun(.NearlyEqual)
  } else {
    delta <- match.fun(DiffFunc)
  }

  # CODE
  # get data.frame with all combinations of vector elements in ToIterate
  Df <- expand.grid(ToIterate)
  # Force the names to the original ones because expand.grid replaces "" with "Var?"
  names(Df) <- names(ToIterate)
  
  # Go, iterate!
  Df$Result <- apply(Df, 1, .forwardreverse, ...)
  Df["Delta"]  <- delta(Df[[KeyVar]], Df$Result)
  
  return(Df)
}

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