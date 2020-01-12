# Testing functions for their the practical range of definition 

source("./eps.R")

#' is.monotonous
#' Tests the order of values given by numeric vector or function.
#' @details 'is.monotonous' 
#' @param x an R object with a function or numeric vector.
#' @param Range The range in which the function shall be tested.
#' @value Returns -1 if monotonously decreasing, +1 monotonously increasing
#' and 0 for everything else.
is.monotonous <- function(x, ...) {
  UseMethod("is.monotonous", x)
}


#' is.monotonous.function
#' @describeIn is.monotonous
is.monotonous.function <- function(x, Range, ...) {
  stopifnot(is.function(x))
  stopifnot(is.numeric(Range))
  Precision <- abs(diff(Range)) / eps( min((Range)) )
  Precision <- min(Precision, 1e6)
  v <- x(seq(Range[1], Range[2], length.out = Precision), ...)
  
  return(is.monotonous.default(v))
}


#' is.monotonous.default
#' @describeIn is.monotonous
is.monotonous.default <- function(x, ...) {
  stopifnot(is.numeric(x))
  d <- diff(x)
  if(all(d <= 0)) return(-1)
  if(all(d >= 0)) return(+1)
  return(0)
}


#' SaturationThold
#' Where does an asymptotic function run into saturation? 'SaturationThold' 
#' determines the practical range of definition of a function. 
#' Though a function may theoretically approach to a limit asymptotically,
#' an implemented function ususally does not work along the full range of 
#' possible input values. The limitations in the precision of floating point
#' numbers will most likely cause a function to run into saturation (long) 
#' before the input values reach the maximum possible values.
#' @param f A function. It's first argument will be used to find the 
#' saturation threshold.
#' @param Range The range to search for the saturation threshold.
#' @param Limit The value limiting the functions value range when 
#' input exceeds 'End'.
#' @details SaturationLimit assumes that saturation only occurs at the 
#' outer edges of the range of definition. If a function may run into
#' saturation besides that, try cutting the range of definition into 
#' several pieces. In general, it is necessary to provdide a 'Range' of
#' values in which the function behaves monotonously. Otherwise it may 
#' not converge.
#' @note If you get a message that the limit is not the end of the 
#' functions value range, try \code{Limit = x + eps(x)}.
#' @example logit.inv(-709.7825) == 0 is FALSE whereas logit.inv(-709.785) == 0 is TRUE
SaturationThold <- function(f, Range, Limit = Inf, ...) {
  S <- Range[1]; ValueS <- f(S, ...)
  E <- Range[2]; ValueE <- f(E, ...)
  if(ValueS == ValueE)
    stop("SaturationThold may not converge because function is not monotonous.")
  if(ValueE != Limit) stop("'Limit' is not the end of the functions value range")
  # Check direction of asymptotic slope
  if(ValueE > ValueS) { # DoE = Different or Equal
    `%DoE%` <- `>=`
  } else {
    `%DoE%` <- `<=`
  }
  # Check for discrete function
  if(is.integer(Range)) {
    `%DIV%` <- `%/%`
    Tolerance <- 1
  } else {
    `%DIV%` <- `/`
    Tolerance <- sqrt(.Machine$double.eps)
  }
  
  Pos   <- (E + S) %DIV% 2
  while(abs(E - S) > Tolerance) { 
    Value <- f(Pos, ...)
    if(Value %DoE% Limit) { # Move closer to Start
      E <- Pos
      Pos <- (E + S) %DIV% 2 
    } else { # Move closer to End
      S <- Pos
      Pos <- (E + S) %DIV% 2 
    }
  }
  # Fix data type in case of discrete function
  if(is.integer(Range)) Pos <- as.integer(Pos)
  
  return(Pos)
}

