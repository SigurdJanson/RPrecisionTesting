# UNIT TESTS:
# - Tests with wrong order of unnamed param
# - Tests with other DiffFunc
# - Tests with ... arguments
# 
library(testthat)

source("./ReverseTesting.R")


# HELPER FUNCTIONS ----

test_that("eps", {
  #' eps.old
  #' This function has been taken from the pragma package. It is being
  #' used for unit testing 'eps'. Does the same as eps but does not work 
  #' on vectors.
  eps.pracma <- function(x = 1.0) {
    x <- max(abs(x))
    if (x <  .Machine$double.xmin) {
      e <- .Machine$double.xmin
    } else {
      e <- 2^floor(log2(x)) * .Machine$double.eps
    }
    e  
  }
  ## eps.old
  
  # values taken from https://rdrr.io/rforge/pracma/man/eps.html
  x <- 10^(-5:5)
  e.r <- c(1.694066e-21, 1.355253e-20, 2.168404e-19, 1.734723e-18, 1.387779e-17,
           2.220446e-16, 1.776357e-15, 1.421085e-14, 1.136868e-13, 1.818989e-12, 1.455192e-11 )
  expect_equal(e.r, eps(x))
  
  x <- 10^(-300:300)
  e.r <- sapply(x, eps.pracma) # function copied from 'pracma' package
  expect_identical(e.r, eps(x))
  
  x <- 10^((-20:20)+0.5)
  e.r <- sapply(x, eps.pracma) # function copied from 'pracma' package
  expect_identical(e.r, eps(x))
})



test_that(".NearlyEqual", {
  .NearlyEqual.old <- function(x, y, eps = 1e-10) {
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
  
  
  x <- 10 + c(1e-11, 1e-10, 1e-9, 1e-8, 1e-7)
  y <- 10 + rep(0, length(x))
  e.r <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  expect_equal(e.r, .NearlyEqual(x, y))
  
  x <- 10^(-50:+50)
  expect_identical(all(.NearlyEqual(x, x)), TRUE)
  
  x <- 10 + c(1e-11, 1e-10, 1e-9, 1e-8, 1e-7)
  y <- 10 + rep(0, length(x))
  expect_identical(.NearlyEqual.old(x, y), .NearlyEqual(x, y))
})


test_that("ReversionTest: Correct Output", {
  ArgsF1 <- "qnorm"
  ArgsF2 <- "pnorm"
  Args3 <- list(mean = -4:4, sd=c(0.5, 1.5), c(0.1, 0.2, 0.9))
  
  # ReversionTest: Do we basically get the right output? ----
  Df <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 3)
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
               "2 Argumente an")
})



test_that("ReversionTest: Diff-Functions", {
  # Diff Functions ----
  f1 <- function(x, y) x*y # * and / actions are relatively safe when it comes to floating point "drift"
  f2 <- function(x, y) x/y # * and / actions are relatively safe
  ArgsF1 <- "f1"
  ArgsF2 <- "f2"
  Args3 <- list(x = c(-4:(-1), 1:4), y = c(-4:(-1), 1:4))
  LenExpected <- prod(unlist(lapply(Args3, length)))
  Df <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 1)
  expect_is(Df, "data.frame")
  expect_type(Df, "list")
  expect_identical(ncol(Df), as.integer(length(Args3)+2))
  expect_identical(nrow(Df), as.integer(LenExpected))
  # Values should be close to zero with subtraction as delta-function
  expect_equal(Df$Delta, rep(TRUE, LenExpected))
  
  # Values should be close to zero with ratio as delta-function
  Df <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 1, DiffFunc = function(x, y) y/x)
  expect_equal(unlist(Df$Delta), rep(1, LenExpected))
  
  MyDiff <- function(x, y) 9
  Args3 <- list(x = 1:99, y = 99:1)
  LenExpected <- prod(unlist(lapply(Args3, length)))
  Df <- ReversionTest(ArgsF1, ArgsF2, ToIterate = Args3, KeyVar = 1, DiffFunc = MyDiff)
  expect_equal(unlist(Df$Delta), rep(9, LenExpected))
})

