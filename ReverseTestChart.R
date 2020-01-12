#
# PLOTTING REVERSION TESTS
#
# Sources for charts:
# - https://www.r-graph-gallery.com/215-the-heatmap-function.html
# - http://compbio.ucsd.edu/making-heat-maps-r/

#source("./ReverseTesting.R")
require(gplots)
library(reshape2)


#' hist.ReversionTest
#' @note This is still a draft
hist.ReversionTest <- function( TestResult, freq = TRUE, ... ) {
  # The exponent of eps in a power of 2
  ExponentEps <- log2(TestResult$Precision) 
  
  Results <- TestResult$Data$Delta
  if(is.logical(Results)) {
    warning("Histogram does not work for logical data")
    return(invisible(NULL))
  }
  
  # Break points and bar labels
  if(length(Results) / (abs(ExponentEps)+1) < 20) {
    N <- max(length(Results) %/% 40L, -ExponentEps %/% 5L)
    Breaks <- rev( pretty( seq(ExponentEps, 0, 1), n = N, min.n = 5) )
  } else
    Breaks <- ExponentEps:0
  BreakLabels <- c("-∞", paste(Breaks)) # paste("2", Breaks, sep="^")
  Breaks <- c(0, 2^Breaks)
  
  Main <- sprintf("%s -> %s", TestResult$Functions[[1]], TestResult$Functions[[2]])
  XLab <- "Frequency"
  YLab <- "Difference < 2^y"
  
  h <- hist(Results, breaks = Breaks, plot = FALSE)
  if(freq)
    Values <- h$counts
  else 
    Values <- h$density
  yp <- barplot(Values, horiz = TRUE, las = 1, xlim = c(0, max(Values)*1.075),
          col="darkslateblue", border="white", # Colors
          main = Main, xlab = XLab, ylab = YLab, # Labels
          names.arg = BreakLabels[-1]) 
  text(x = Values, y = yp, label = Values, pos = 4, cex = 0.8, col = "darkslateblue")
  invisible(h)
}



#' plot.ReversionTest
#' @note This function is limited to specific purposes. It may not work for any use case.
plot.ReversionTest <- function( TestResult ) {
  # 1. Data frame in long form has to be transformed
  # 
  names(TestResult$Data)[1] <- "Key" #TODO: that looks risky
  # Remove 'Result' column and transform to matrix
  ResultCol <- which(names(TestResult$Data) == "Result") 
  if(length(TestResult$Variables) > 1) {
    Result.M <- dcast(TestResult$Data[-ResultCol], Key ~ mean + sd, value.var = "Delta")
  } else {
    Result.M <- as.matrix(TestResult$Data[-ResultCol], ncol = ncol(TestResult$Data))
  }
  rownames(Result.M) <-  Result.M[,"Key"] 
  Result.M <- data.matrix(Result.M[,-1])
  Result.M <- t(Result.M)
  if(all(Result.M == 0)) warning("All values are zero")
  
  # 2. Set color palette
  # Get vector of unique values
  Singletons <- unique(unlist( apply(Result.M, 1, unique) ))
  if(length(Singletons) <= 2) {
    Colors <- colorRampPalette(c("red", "white"))(2)
  } else {
    N <- min(length(Singletons) %/% 1, 100, na.rm = TRUE)
    Range <- range(Singletons, na.rm = TRUE)
    # All deltas >= 0.5 shall be black
    BlackN <- max((max(Range) - 0.5), 0) / diff(Range)
    BlackN <- BlackN * N
    # All deltas >= 0.1 shall be red
    RedN <- (0.5 - max(0.1, min(Range))) / diff(Range)
    RedN <- RedN * N
    # All deltas < 0.1 shall be light red
    LightN <- max((0.1 - min(Range)), 0) / diff(Range)
    LightN <- LightN * N
    Colors <- colorRampPalette(c("white", 
                                 rep("lightcoral", LightN),
                                 rep("red", RedN),
                                 rep("black", BlackN)))(N)
  }
  #. Plot heat map
  # Title
  Main <- sprintf("%s -> %s", TestResult$Functions[[1]], TestResult$Functions[[2]])
  KeyXLab <- ifelse(length(Singletons) == 2, "Delta", "Different - Same")
  XLab <- "Key"
  
  if(length(TestResult$Variables) > 1) {
    YLab <- paste(TestResult$Variables[-1], collapse=", ")
    heatmap.2(Result.M, Rowv = NA, Colv = NA, dendrogram = "none",
              main = Main, xlab = XLab, ylab = YLab,
              key.xlab=KeyXLab, key=TRUE, symkey=FALSE, key.title = "Colors & Histogram",
              density.info="histogram", trace = "none",
              col = Colors)
  } else {
    YLab <- ifelse(length(Singletons) == 2, "Different - Same", "Delta")
    barplot(Result.M,
            main = Main, xlab = XLab, ylab = YLab,
            ylim = c(0,1))
  }
}



#' print.ReversionTest
#' @note This is still a draft
print.ReversionTest <- function( TestResult ) {
  # Print description
  cat("Reversion test at", format(TestResult$TestTime, "%a %b %d %X %Y"), "\n")
  cat(format("Tested functions:", width=20), unlist(TestResult$Functions), "\n")
  cat("Variables", "\n")
  for (v in 1:length(TestResult$Variables)) {
    Values <- unique(TestResult$Data[[v]])
    if(all( diff(diff(Values)) < 2^(-20) ))
      Sequence <- paste0("seq( ", min(Values), ", ", max(Values), ", ", 
                         format(mean(diff(Values))), ")")
    else
      Sequence <- paste(Values, collapse = ", ")
    Label <- TestResult$Variables[v]
    if(is.null(Label) || Label == "") Label <- "Key"
    cat( format(Label, width=20), Sequence, "\n" )
  }

  #TODO: Cannot print closure and do not know how to get the function name
  #print(paste("Difference function:", TestResult$Diff, "\n")) 
  cat("\n")
  cat( format("Precision:", width=20), TestResult$Precision, "\n" )
  
  
  # Print test results
  Deltas <- TestResult$Data[["Delta"]]
  Singletons <- unique(Deltas)
  NValues <- length(Deltas)
  NValuesCorr <- sum(!is.na(Deltas)) # length(Deltas)
  Zeroes <- sum(Result$Data[["Delta"]] == 0, na.rm = TRUE)
  Ratio <- Zeroes / NValuesCorr
  NNA   <- sum(is.na(TestResult$Data[["Delta"]]))
  
  cat("\n")
  cat("Results", "\n")
  cat( format("Values created:", width=15), NValues, "\n" )
  cat( format("NA:", width=15), NNA, "\n" )
  
  if(Zeroes == NValues) {
    cat( "All values are zero. The result is flawless.", "\n" )
  } else {
    cat( format("Delta range:", width=15), range(Singletons, na.rm = TRUE), "\n" )
    cat( format("Delta == 0:", width=15), Zeroes, "\n" )
    cat( format("Ratio 0/All:", width=15), Ratio, "\n" )
  }
}



summary.ReversionTest <- function( TestResult ) {
  # Print test results
  Deltas <- TestResult$Data[["Delta"]]
  Singletons <- unique(Deltas)
  NValues <- nrow(TestResult$Data)*ncol(Deltas)
  NNA     <- sum(is.na(Deltas)) # count of NAs
  Zeroes  <- sum(Deltas == 0, na.rm = TRUE)
  Ratio   <- Zeroes / NValues
  
  cat("Results", "\n")
  if(Zeroes == NValues) {
    cat( "All values are zero. The result is flawless.", "\n" )
  } else {
    cat( format("Value range:", width=15), range(Singletons), "\n" )
    cat( format("Values == 0:", width=15), Zeroes, "\n" )
    cat( format("Ratio 0/All:", width=15), Ratio, "\n" )
    cat( format("Delta < 10%:", width=15), sum(Deltas < .10) / NValues, "\n" )
    cat( format("Delta < 25%:", width=15), sum(Deltas < .25) / NValues, "\n" )
    cat( format("Delta < 50%:", width=15), sum(Deltas < .50) / NValues, "\n" )
  }
}

# Code for Debugging ----
# Result <- ReversionTest("plogitnorm", "qlogitnorm",
#                         ToIterate = list(seq(0.05, 0.95, 0.05), 
#                                          mean = seq(-50,50,5), 
#                                          sd = c(0.1, 1, 10, 20, 50)), 
#                         DiffFunc = .DeltaEps)
# print(Result)
# hist(Result)
# plot(Result)
# 
# Result <- ReversionTest("plogitnorm.subopt", "qlogitnorm.subopt",
#                         ToIterate = list(seq(0.05, 0.95, 0.05), 
#                                          mean = seq(-50,50,5), 
#                                          sd = c(0.1, 1, 10, 20, 50)), 
#                         DiffFunc = .DeltaEps)
# print(Result)
# hist(Result)
# plot(Result)