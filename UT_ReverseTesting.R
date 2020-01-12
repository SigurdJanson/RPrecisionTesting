# UNIT TESTS:
# - Tests with wrong order of unnamed param
# - Tests with other DiffFunc
# - Tests with ... arguments
# 
library(testthat)

source("./ReverseTesting.R")
source("D:/Texte/02 Wissen/!Ideenschmiede/R-package 'Usability'/LNB/src/R/logitnormal.R")

# HELPER FUNCTIONS ----

test_that(".NearlyEqual", {
  .NearlyEqual.old <- function(x, y, eps = 2^-26) {
    X.Abs <- abs(x)
    Y.Abs <- abs(y)
    Diff = abs(x - y)
    
    Result <- as.logical(rep(NA, length(x)))
    # if(x == x_) {
    #   # shortcut, handles infinities
    #   return( TRUE )
    # }
    Result[which(x == y)] <- TRUE
    # else {
    # if (x == 0 || x_ == 0 || Diff < .Machine$double.xmin) #FLT_MIN in C
    # {
    #   # x or x_ is zero or both are extremely close to it
    #   # Relative error is less meaningful here
    #   return (Diff < (eps*.Machine$double.xmin));
    # } 
    Which <- which(x == 0 || y == 0 || Diff < .Machine$double.xmin)
    Which <- setdiff(which(Result == TRUE), Which) # ignore 'which(x == x_)'
    Result[Which] <- (Diff[Which] < eps*.Machine$double.xmin)
    # else {
    #     # use relative error
    #     return (Diff / (X.Abs + Y.Abs) < eps);
    #   }
    # }
    Result[which(is.na(Result))] <- (Diff / ( X.Abs + Y.Abs )) < eps
  }
  
  x <- 10 + c(1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6)
  y <- 10 + rep(0, length(x))
  e.r <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  expect_equal(e.r, .NearlyEqual(x, y))
  
  x <- 10^(-50:+50)
  expect_identical(all(.NearlyEqual(x, x)), TRUE)
  
  # Test the near-zero case
  expect_equal(c(FALSE, FALSE, TRUE),
               .NearlyEqual(c(0,1,1), c(1,0,1), eps = 1e-10))
  
  # Compare with original algorithm
  x <- 10 + c(1e-14, 1e-13, 1e-12, 1e-11, 1e-10, 1e-9, 1e-8, 1e-7)
  y <- 10 + rep(0, length(x))
  expect_identical(.NearlyEqual.old(x, y), .NearlyEqual(x, y))
})



test_that(".DeltaEps", {
  x <- 10 + c(1e-11, 1e-10, 1e-9, 1e-8, 1e-7)
  y <- 10 + rep(0, length(x))
  e.r <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  Result <- .DeltaEps(x, y)
  expect_equal(e.r, Result == 0)
  e.r[e.r==TRUE] <- 0
  e.r[e.r==FALSE] <- abs(x-y) / (x + y)
  expect_equal(e.r, Result)

  # Test the near-zero case
  expect_equal(c(1e-10 * .Machine$double.xmin, 1e-10 * .Machine$double.xmin, 0),
               .DeltaEps(c(0,1,1), c(1,0,1), eps = 1e-10))
  
  
  x <- 10^(-50:+50)
  expect_identical(all(.DeltaEps(x, x) == 0), TRUE)
  
  x <- runif(1000, -20, 20)
  y <- x + runif(1000, -0.1, 0.1)
  expect_equal(.NearlyEqual(x, y), .DeltaEps(x, y) == 0)
  
  x <- runif(1000, -0.1, 0.1)
  y <- x + runif(1000, -1e-5, 1e-5)
  expect_equal(.NearlyEqual(x, y), .DeltaEps(x, y) == 0)
  
  x <- ReversionTest("qlogitnorm", "plogitnorm", 
                     ToIterate = list(seq(0.05, 0.95, 0.05), mean = seq(-50,50,5), sd = c(0.1, 1:10)), 
                     DiffFunc = .DeltaEps)
  y <- ReversionTest("qlogitnorm", "plogitnorm", 
                     ToIterate = list(seq(0.05, 0.95, 0.05), mean = seq(-50,50,5), sd = c(0.1, 1:10)),
                     DiffFunc = .NearlyEqual)
  x$Data$Delta <- (x$Data$Delta == 0)
  expect_equal(x$Data, y$Data)
})



test_that("ReversionTest: Correct Output", {
  ArgsF1 <- "qnorm"
  ArgsF2 <- "pnorm"
  Args3 <- list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9))
  
  # ReversionTest: Do we basically get the right output? ----
  Df <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 3)$Data
  expect_is(Df, "data.frame")
  expect_type(Df, "list")
  expect_identical(ncol(Df), as.integer(length(Args3)+2))
  expect_identical(nrow(Df), as.integer(prod(unlist(lapply(Args3, length)))))
  # Argument without name must have been placed first
  expect_identical(names(Df[2]), names(Args3[1]))
  expect_identical(names(Df[3]), names(Args3[2]))

  # TODO: test if data is right
  expect_equal(all(Df$Delta == TRUE), TRUE, tolerance = 1E-15)
  # TODO: test this with arguments in wrong and right order
})



test_that("ReversionTest: Precondition Checks", {
  ArgsF1 <- "qnorm"
  ArgsF2 <- "pnorm"
  Args3 <- list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9))

  # Out of bounds and other errors ----
  # f is not a valid function
  expect_error(ReversionTest("NON..SENSE", ArgsF2, ToIterate = Args3, KeyVar = 3),
                 NULL)
  expect_error(ReversionTest(ArgsF1, "p.norm", ToIterate = Args3, KeyVar = 3),
                 NULL)
  
  # 'f' is not a string
  expect_error(ReversionTest(qnorm, ArgsF2, ToIterate = Args3, KeyVar = 3),
                 "Need function name")
  expect_error(ReversionTest(12, ArgsF2, ToIterate = Args3, KeyVar = 3),
                 "Need function name")
  
  # ToIterate
  expect_error(ReversionTest(ArgsF1, ArgsF2, ToIterate = 1:3, KeyVar = 3),
               "List expected for 'ToIterate'")
  expect_error(ReversionTest(ArgsF1, ArgsF2, ToIterate = list(), KeyVar = 3),
               "Nothing to iterate through")
  expect_error(ReversionTest(ArgsF1, ArgsF2, ToIterate = list(1, list("a", 1)), KeyVar = 3),
               "'ToIterate' must contain only atomic numeric vectors")
  expect_error(ReversionTest(ArgsF1, ArgsF2, ToIterate = list(1, "a"), KeyVar = 3),
               "'ToIterate' must contain only atomic numeric vectors")
  
  
  # Wrong Key
  expect_error(ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 0),
               "Key variable must be 1 or greater")
  expect_error(ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = -1),
               "Key variable must be 1 or greater")
  expect_error(ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = length(Args3)+1),
               "Index of key variable out of bounds")
  
  
  # DiffFunc
  expect_error(ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 3, DiffFunc = NON..SENSE),
               "Objekt 'NON..SENSE' nicht gefunden")
  # sqrt accepts only 1 argument, so this should not work
  expect_error(ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 3, DiffFunc = sqrt),
               "3 Argumente an")
})



test_that("ReversionTest: Diff-Functions", {
  # Diff Functions ----
  f1 <- function(x, y) x*y # * and / actions are relatively safe when it comes to floating point "drift"
  f2 <- function(x, y) x/y # * and / actions are relatively safe
  ArgsF1 <- "f1"
  ArgsF2 <- "f2"
  Args3 <- list(x = c(-4:(-1), 1:4), y = c(-4:(-1), 1:4))
  LenExpected <- prod(unlist(lapply(Args3, length)))
  Df <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 1)$Data
  expect_is(Df, "data.frame")
  expect_type(Df, "list")
  expect_identical(ncol(Df), as.integer(length(Args3)+2))
  expect_identical(nrow(Df), as.integer(LenExpected))
  # Values should be close to zero with subtraction as delta-function
  expect_equal(Df$Delta, rep(TRUE, LenExpected))
  
  # Values should be close to zero with ratio as delta-function
  MyDiff <- function(x, y, eps) y/x + (0*eps)
  Result <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 1, DiffFunc = MyDiff)
  Df <- Result$Data
  expect_equal(unlist(Df$Delta), rep(1, LenExpected))
  
  MyDiff <- function(x, y, eps) 9 + (0*eps)
  Args3 <- list(x = 1:99, y = 99:1)
  LenExpected <- prod(unlist(lapply(Args3, length)))
  Df <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 1, DiffFunc = MyDiff)$Data
  expect_equal(unlist(Df$Delta), rep(9, LenExpected))
})

