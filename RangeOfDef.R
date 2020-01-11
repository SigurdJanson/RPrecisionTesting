# Testing functions for their the practical range of definition 



#' is.monotonous
is.monotonous <- function(f, Range, ...) {
  Precision <- abs(diff(Range)) / eps( min((Range)) )
  Precision <- min(Precision, 1e6)
  #if(Precision > 1e6) stop(paste("You really do not want this: ", Precision))
  v <- f(seq(Range[1], Range[2], length.out = Precision), ...)
  d <- diff(v)
  if(all(d <= 0)) return(-1)
  if(all(d >= 0)) return(+1)
  return(0)
}


#' FindSaturation
#' @param f
#' @param Start
#' @param End
#' @param Limit The value limiting the functions value range when 
#' input exceeds 'End'.
#' @details SaturationLimit assumes that saturation only occurs at the 
#' outer edged of the range of definition. If a function may run into
#' saturation besides that, try cutting the range of definition into 
#' several pieces. In general, it is necessary to provdide a Start and End 
#' values in which the function behaves monotonously.
#' @note If you get a message that the limit is not the end of the 
#' functions value range, try \code{Limit = x + eps(x)}.
#' @example logit.inv(-709.7825) == 0 is FALSE whereas logit.inv(-709.785) == 0 is TRUE
SaturationLimit <- function(f, Range, Limit = Inf, ...) {
  #TODO Define DIV operation as %/% for is.integer(Start/End)
  #TODO print(SaturationLimit(logit.inv, c(-2000, 0), Limit = 0), digits = 20) does not work
  # while 'SaturationLimit(logit.inv, c(0, -2000)' does
  # Is the direction up or down?
  S <- Range[1]
  E <- Range[2]
  
  if(E > S) {
    DoE <- `>=`
    D   <- `>`
  } else {
    DoE <- `<=`
    D   <- `<`
  }
  Tolerance <- sqrt(.Machine$double.eps)
  if(f(S, ...) == Limit && f(E, ...) == Limit)
    stop("SaturationLimit may not converge because function is not monotonous.")
  
  Pos   <- (E + S) / 2
  while(abs(E - S) > Tolerance) { 
    Value <- f(Pos, ...)
    if(D(Value, Limit)) stop("'Limit' is not the end of the functions value range")
    if(DoE(Value, Limit)) {
      E <- Pos
      Pos <- (E+S)/2 
    } else {
      S <- Pos
      Pos <- (E+S)/2 
    }
  }
  return(Pos)
}


