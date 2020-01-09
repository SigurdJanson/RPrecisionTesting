#
# PLOTTING REVERSION TESTS
#
# Sources for charts:
# - https://www.r-graph-gallery.com/215-the-heatmap-function.html
# - http://compbio.ucsd.edu/making-heat-maps-r/

source("./ReverseTesting.R")
require(gplots)
library(reshape2)

hist.ReversionTest <- function( TestResult, freq = TRUE, ... ) {
  eps <- -26 # Assumed default epsilon
  
  Results <- TestResult$Data$Delta
  
  # Brea points and bar labels
  if(length(Results) / (abs(eps)+1) < 20)
    Breaks <- pretty( seq(eps, 0, 1), n, min.n = 5)
  else
    Breaks <- eps:0
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
  names(TestResult$Data)[1] <- "key"
  # Remove 'Result' column and transform to matrix
  ResultCol <- which(names(TestResult$Data) == "Result") 
  Result.M <- dcast(TestResult$Data[-ResultCol], key ~ mean + sd, value.var = "Delta")
  rownames(Result.M) <- Result.M$key
  Result.M <- data.matrix(Result.M[-1])
  Result.M <- t(Result.M)
  
  # 2. Set color palette
  # Get vector of unique values
  Singletons <- unique(unlist( apply(Result.M, 1, unique) ))
  if(length(Singletons) == 2) {
    Colors <- colorRampPalette(c("red", "white"))(2)
  } else {
    #Range <- range(Singletons, na.rm = TRUE)
    Range <- min(length(Singletons) %/% 1, 400)
    Colors <- colorRampPalette(c("white",
                                 "red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red",
                                 "black", "black", "black", "black", "black", "black", "black", "black", "black", "black"))(Range)
  }
  #. Plot heat map
  # Title
  Main <- sprintf("%s -> %s", TestResult$Functions[[1]], TestResult$Functions[[2]])
  KeyXLab <- ifelse(length(Singletons) > 2, "Delta", "Different - Same")
  YLab <- paste(TestResult$Variables[-1], collapse=", ")
  XLab <- "Key"
  
  if(length(TestResult$Variables) > 1) {
    heatmap.2(Result.M, Rowv = NA, Colv = NA, dendrogram = "none",
              main = Main, xlab = XLab, ylab = YLab,
              key.xlab=KeyXLab, key=TRUE, symkey=FALSE, key.title = "Colors & Histogram",
              density.info="histogram", trace = "none",
              col = Colors)
  } else {
    barplot(Result.M)
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
    if(Label == "") Label <- "Key"
    cat( format(Label, width=20), Sequence, "\n" )
  }

  #TODO: Cannot print closure and do not know how to get the function name
  #print(paste("Difference function:", TestResult$Diff, "\n")) 
  cat("\n")
  cat( format("Precision:", width=20), TestResult$Precision, "\n" )
  
  
  # Print test results
  #Singletons <- unique(unlist( apply(TestResult$Data$Result, 1, unique) ))
  Singletons <- unique(TestResult$Data[["Delta"]])
  NValues <- length(TestResult$Data[["Delta"]]) # nrow(TestResult$Data)*ncol(TestResult$Data)
  Zeroes <- sum(Result$Data[["Delta"]] == 0)
  Ratio <- Zeroes / NValues
  
  cat("\n")
  cat("Results", "\n")
  cat( format("Values created:", width=15), NValues, "\n" )
  cat( format("Delta range:", width=15), range(Singletons), "\n" )
  cat( format("Delta == 0:", width=15), Zeroes, "\n" )
  cat( format("Ratio 0/All:", width=15), Ratio, "\n" )
}



summary.ReversionTest <- function( TestResult ) {
  # Print test results
  Singletons <- unique(unlist( apply(TestResult$Data, 1, unique) ))
  NValues <- nrow(TestResult$Data)*ncol(TestResult$Data)
  Zeroes <- sum(Result$Data == 0)
  Ratio <- Zeroes / NValues
  
  cat("Results", "\n")
  cat( format("Value range:", width=15), range(Singletons), "\n" )
  cat( format("Values == 0:", width=15), Zeroes, "\n" )
  cat( format("Ratio 0/All:", width=15), Ratio, "\n" )
}


#Result <- ReversionTest("qnorm", "pnorm", ToIterate =  list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9)), KeyVar = 3)
#Result <- ReversionTest("qnorm", "pnorm", ToIterate =  list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9)), KeyVar = 3, DiffFunc = sqrt)#function(x, y) y/x)
#Result <- ReversionTest("qlogitnorm", "plogitnorm", ToIterate =  list(mean = -4:4, sd=1))

# Forward
# Result <- ReversionTest("qlogitnorm", "plogitnorm", 
#                         ToIterate = list(seq(0.05, 0.95, 0.05), 
#                                          mean = seq(-50,50,5), 
#                                          sd = c(0.1, 1, 10, 20, 50)))
# plot(Result)
# print(Result)
# 
# Result <- ReversionTest("qlogitnorm", "plogitnorm", 
#                         ToIterate = list(seq(0.05, 0.95, 0.05), 
#                                          mean = seq(-50,50,5), 
#                                          sd = c(0.1, 1, 10, 20, 50)), 
#                         DiffFunc = .DeltaEps)
# plot(Result)
# hist(Result)

# For testing: the chart shall be identical to a chart created
# with Diff Function .NearlyEqual
#Result$Data$Delta <- (Result$Data$Delta == 0)
#plot(Result)


# Backward
# Result <- ReversionTest("plogitnorm", "qlogitnorm", 
#                         ToIterate = list(seq(0.05, 0.95, 0.05), 
#                                          mean = seq(-50,50,5), 
#                                          sd = c(0.1, 1, 10, 20, 50)))
# plot(Result)
# 
# Result <- ReversionTest("plogitnorm", "qlogitnorm", 
#                         ToIterate = list(seq(0.05, 0.95, 0.05), 
#                                          mean = seq(-50,50,5), 
#                                          sd = c(0.1, 1, 10, 20, 50)), 
#                         DiffFunc = .DeltaEps)
# plot(Result)
# hist(Result)


#Colors <- colorRampPalette(brewer.pal(8, "RdPu"))(20)

# breaks   <- seq(from=min(range(TotalResult)), to=max(range(TotalResult)), length.out=100)
# midpoint <- which.max(breaks < 1e-5)
# Colors   <- colorRampPalette(c("white"))(midpoint)
# midpoint <- which.max(breaks < 0.1)
# Colors   <- c(Colors, colorRampPalette(c("lightcoral"))(midpoint-length(Colors)))
# midpoint <- which.max(breaks < 0.25)
# Colors   <- c(Colors, colorRampPalette(c("red"))(midpoint-length(Colors)))
# midpoint <- which.max(breaks < 0.5)
# Colors   <- c(Colors, colorRampPalette(c("darkred"))(midpoint-length(Colors)))
# midpoint <- which.max(breaks < 1)
# Colors   <- c(Colors, colorRampPalette(c("black"))(midpoint-length(Colors)))
#rampCol2 <- colorRampPalette(c("red", "red", "darkred"))(100-(midpoint+1))
#Colors <- c(rampCol1,rampCol2)

# Convert data to matrix


# CHART ----
#Colors <- colorRampPalette(c("white",
#                             "lightcoral",
#                             "red", "red",
#                             "darkred", "darkred", "darkred", 
#                             "black", "black", "black", "black", "black", "black", "black"))(14)


#heatmap(data, scale="column", cexRow=1.5, labRow=paste("new_", rownames(data),sep=""), col= colorRampPalette(brewer.pal(8, "Blues"))(25))
#levelplot(your_data, col.regions=heat.colors)