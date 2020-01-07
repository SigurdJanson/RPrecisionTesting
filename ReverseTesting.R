

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
#' @author Michael G. Rozman
#' @author Moved to R, made vector compatible, and modified by Jan Seifert
#' @references Rozman, M. Z. (2015) [What Every Programmer Should Know About Floating-Point Arithmetic](https://www.phys.uconn.edu/~rozman/Courses/P2200_15F/downloads/floating-point-guide-2015-10-15.pdf); accessed 2020-01-06
.NearlyEqual <- function(x, y, eps = 1e-10) {
  X.Abs <- abs(x)
  Y.Abs <- abs(y)
  Diff = abs(x - y)
  
  # Use relative error, by default
  Result <- Diff / (X.Abs + Y.Abs) < eps
  # Correct, where x or x_ is zero or both are extremely close to it
  # Relative error is less meaningful here
  Which <- which(x == 0 | y == 0 | (X.Abs + Y.Abs) < .Machine$double.xmin)
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
  
  # Handles infinities
  # Set all to 0 first, correct later where x != y
  Which1 <- which(x == y)
  Result <- rep(0, length(x))
  
  # Correct, where x or y is zero or both are extremely close to it
  # Relative error is less meaningful here
  # Jan Seifert: probably, the author made a mistake here. TODO: VERIFY
  Which2 <- which(x == 0 | y == 0 | (X.Abs + Y.Abs) < .Machine$double.xmin)
  Which2 <- setdiff(Which2, Which1)
  Result[Which2] <- eps * .Machine$double.xmin

  # Use relative error, by default
  ErrorRel <- Diff / (X.Abs + Y.Abs)
  Which3 <- which( ErrorRel > eps )
  Which3 <- setdiff(setdiff(Which3, Which2), Which1)
  Result[Which3] <- ErrorRel[Which3]

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


