# UNIT TESTS
# 
library(testthat)

source("./RangeOfDef.R")

# is.monotonous ----
test_that("is.monotonous", {
  
  TestWithFunction <- function(f, Range, ExpDir, ...) {
    r <- seq(Range[1], Range[2], length.out = 50)
    x <- do.call(f, list(r, ...))
    # Call function with vector input
    expect_identical(is.monotonous(x), ExpDir)
    # Same result when posting function call directly
    expect_identical(is.monotonous(f, Range, ...), is.monotonous(x))
  }
  # Gauss: mean = 0 is FALSE
  TestWithFunction(dnorm, c(-10, 10), 0)
  TestWithFunction(dnorm, c(0, 10), -1)
  TestWithFunction(dnorm, c(-10, 0), 1)

  # log-normal
  TestWithFunction(dlnorm, c(0, 10), 0)
  TestWithFunction(dlnorm, c(1, 10), -1)
  
  # exponential
  TestWithFunction(dexp, c(0, 10), -1)
  TestWithFunction(dexp, c(-1, 10), 0)
  
  # uniform is monotonous
  TestWithFunction(dexp, c(0, 10), -1)
  TestWithFunction(dexp, c(-1, 10), 0)
  
  # 
  for(i in 1:100) {
    x <- runif(100, min = 0+i, max = 10^i)
    expect_identical(is.monotonous(sort(x, decreasing = FALSE)), 1)
    expect_identical(is.monotonous(sort(x, decreasing = TRUE)), -1)
  }
})



test_that("SaturationThold", {
#Example: SaturationThold(f, Range, Limit = Inf, ...)
  
dnorm.trunc <- function(x, mean = 0, sd = 1, RangeOf0 = c(-2, 2)) {
  Result <- dnorm(x, mean, sd)
  Result[x < RangeOf0[1]] <- 0
  Result[x > RangeOf0[2]] <- 0
  Result
}
dnorm.trunc.ud <- function(x, mean = 0, sd = 1, RangeOf0 = c(-2, 2)) {
  - dnorm.trunc(x, mean, sd, RangeOf0)
}
logit.inv <- function(x) 1/(1+exp(-x))

# SaturationThold ----
#logit.inv(-709.7825) == 0 is FALSE whereas logit.inv(-709.785) == 0 is TRUE
# Limit = -709.78271288913675
Observed <- SaturationThold(logit.inv, Range = c(0, -800), Limit = 0)
expect_identical(Observed < -709.7825, TRUE) 
expect_identical(Observed > -709.785,  TRUE)
# Limit von 1 = 36.736800573999062
Observed <- SaturationThold(logit.inv, Range = c(0, 800), Limit = 1)
expect_identical(Observed < 36.7369, TRUE) 
expect_identical(Observed > 36.7367, TRUE)


# Test in all 4 directions: 
# 1. upper/lower Threshold 
# 2. Direction of search forward / backward
MaxX <- 11
for(r0 in 1:(MaxX-1)) {
  Expected <- c(-r0, r0)
  Observed <- SaturationThold(dnorm.trunc, Range = c(0, MaxX), Limit = 0, 
                              mean = 0, sd = 1, RangeOf0 = Expected)
  expect_equal(Observed, Expected[2])
  Observed <- SaturationThold(dnorm.trunc, Range = c(0, -MaxX), Limit = 0, 
                              mean = 0, sd = 1, RangeOf0 = Expected)
  expect_equal(Observed, Expected[1])
}

MaxX <- 11
for(r0 in 1:(MaxX-1)) {
  Expected <- c(-r0, r0)
  Observed <- SaturationThold(dnorm.trunc.ud, Range = c(0, MaxX), Limit = 0, 
                              mean = 0, sd = 1, RangeOf0 = Expected)
  expect_equal(Observed, Expected[2])
  Observed <- SaturationThold(dnorm.trunc.ud, Range = c(0, -MaxX), Limit = 0, 
                              mean = 0, sd = 1, RangeOf0 = Expected)
  expect_equal(Observed, Expected[1])
}


# Test SaturationThold with discrete function ----
for(MaxX in 2:20) {
  #MaxX <- x
  Observed <- SaturationThold(dbinom, Range = as.integer(c(0, MaxX+2)), Limit = 0, 
                              size = MaxX, prob = 0.5) # dbinom args
  Expected <- MaxX
  expect_identical((Observed), Expected)
}

# Test SaturationThold for errors ----
for(i in 1:5) {
  expect_error(SaturationThold(dnorm.trunc, Range = c(-i, i), Limit = 0, 
                               mean = 0, sd = 1, RangeOf0 = c(-4, 4)),
               "SaturationThold may not converge")
}
expect_error(SaturationThold(dunif, Range = c(0, 1), Limit = 0, 
                             min = 0, max = 1),
             "SaturationThold may not converge")


expect_error(SaturationThold(dunif, Range = c(-0.001, 1), Limit = 0, 
                             min = 0, max = 1),
             "'Limit' is not the end")

})