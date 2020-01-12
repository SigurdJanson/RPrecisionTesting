# UNIT TESTS:
# - Tests with wrong order of unnamed param
# - Tests with other DiffFunc
# - Tests with ... arguments
# 
library(testthat)

source("./eps.R")



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
  
  # Taken from the Matlab documentation
  expect_equal(2^(-53), eps(1/2))
  expect_equal(2^(-51), eps(2))
  expect_equal(2^(-1074), eps(0))
  expect_equal(as.numeric(c(NA, NA)), eps(c(Inf, -Inf)))
  expect_equal(as.numeric(NA), eps(as.numeric(NA)))
  
  # On machines that support IEEE floating point arithmetic, eps is approximately 2.2204e-16 for double precision
  x <- 1.0
  e.r <- .Machine$double.eps
  expect_equal(e.r, eps(x))
  
  
  x <- 10^(-300:300)
  e.r <- sapply(x, eps.pracma) # function copied from 'pracma' package
  expect_identical(e.r, eps(x))
  
  x <- 10^((-20:20)+0.5)
  e.r <- sapply(x, eps.pracma) # function copied from 'pracma' package
  expect_identical(e.r, eps(x))
})

