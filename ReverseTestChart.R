#
# PLOTTING REVERSION TESTS
#
# Sources for charts:
# - https://www.r-graph-gallery.com/215-the-heatmap-function.html
# - http://compbio.ucsd.edu/making-heat-maps-r/

source("./ReverseTesting.R")
require(gplots)
library(reshape2)


#Result <- ReversionTest("qnorm", "pnorm", ToIterate =  list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9)), KeyVar = 3)
#Result <- ReversionTest("qnorm", "pnorm", ToIterate =  list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9)), KeyVar = 3, DiffFunc = sqrt)#function(x, y) y/x)
#Result <- ReversionTest("qlogitnorm", "plogitnorm", ToIterate =  list(mean = -4:4, sd=1))
Result <- ReversionTest("qlogitnorm", "plogitnorm", ToIterate = list(seq(0.05, 0.95, 0.05), mean = -50:50), sd = 1)

# Convert expanded data frame to matrix or vector
names(Result)[1] <- "q"
Result.M <- dcast(Result[-3], q ~ mean, value.var = "Delta")
rownames(Result.M) <- Result.M$q
Result.M <- data.matrix(Result.M[-1])

# If matrix: heatmap; if vector: bar chart
Colors <- colorRampPalette(c("white",
                            "black"))(2)
heatmap.2(Result.M, Rowv = NA, Colv = NA, dendrogram = "none",
       main = "Logitnorm TRUE/FALSE", xlab = "p", ylab = "mean",
       key.xlab="Delta", key=TRUE, symkey=FALSE, key.title = "Colors & Histogram",
       density.info="histogram", trace = "none",
       col = Colors)


Result <- ReversionTest("qlogitnorm", "plogitnorm", ToIterate = list(seq(0.05, 0.95, 0.05), mean = -50:50), sd = 1, DiffFunc = .DeltaEps)
names(Result)[1] <- "q"
Result.M <- dcast(Result[-3], q ~ mean, value.var = "Delta")
rownames(Result.M) <- Result.M$q
Result.M <- data.matrix(Result.M[-1])

Colors <- colorRampPalette(c("white",
                             "red",
                             "black"))(100)
heatmap.2(Result.M, Rowv = NA, Colv = NA, dendrogram = "none",
          main = "Logitnorm Relative Diff", xlab = "p", ylab = "mean",
          key.xlab="Delta", key=TRUE, symkey=FALSE, key.title = "Colors & Histogram",
          density.info="histogram", trace = "none",
          col = Colors)


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